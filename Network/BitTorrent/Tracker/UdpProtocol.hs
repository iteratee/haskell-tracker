module Network.BitTorrent.Tracker.UdpProtocol where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Crypto.Saltine.Core.Hash
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString                           as B
import qualified Data.ByteString.Char8                     as B8 (pack)
import qualified Data.ByteString.Lazy                      as BL
import           Data.Digest.SHA1
import           Data.Maybe
import           Data.Word
import           Network.BitTorrent.Tracker.Announce
import           Network.BitTorrent.Tracker.AnnounceServer
import           Network.Socket
import           Network.Socket.ByteString

-- | An environment for the UDP Server. Consists of the general announcement
--   environment and hash keys for the bittorrent udp protocol.
--   A connection is valid if its identifier was created with either the
--   current or the previuos hash key.
--   see <https://www.bittorrent.org/beps/bep_0015.html the UDP protocol spec>
--   also implements an ipv6
--   <http://opentracker.blog.h3q.com/2007/12/28/the-ipv6-situation/ extension>
--   that was never adopted, but makes the most sense.
data UdpEnv = UdpEnv
  { anEnv   :: AnnounceEnv
    -- ^ The `AnnounceEnv` used to serve the requests
  , currKey :: MVar ShorthashKey
    -- ^ The current key used to validate connection identifiers
  , prevKey :: MVar ShorthashKey
    -- ^ The previous key used to validat connection identifiers.
    --   Once a minute `prevKey` is forgotten, `prevKey` is assigned the value
    --   that was in `currKey` and a new value is chosen for `currKey`
  }

-- | Make a `UdpEnv` by randomly choosing two keys for `curKey` and `prevKey`.
--   The value chosen for `prevKey` is pointless, but unlikely to matter.
--   Choosing a value avoids an edge case.
makeUdpEnv :: AnnounceEnv -> IO UdpEnv
makeUdpEnv anEnv = do
  currKeyM <- newMVar =<< newShorthashKey
  prevKeyM <- newMVar =<< newShorthashKey
  return UdpEnv {anEnv = anEnv, currKey = currKeyM, prevKey = prevKeyM}

-- | Cycle `currKey` to `prevKey` and choose a new value for `currKey`.
--   Discard the value in `prevKey`
cycleKeys :: UdpT ()
cycleKeys = do
  currK <- liftIO . takeMVar =<< asks currKey
  prevK <- liftIO . takeMVar =<< asks prevKey
  newK <- liftIO newShorthashKey
  liftIO . (`putMVar` newK) =<< asks currKey
  liftIO . (`putMVar` currK) =<< asks prevKey

-- | Monad for interacting with the UDP server.
type UdpT a = ReaderT UdpEnv IO a

-- | A connection ID is 64 bits, specified in BEP 15
type ConnectionId = Word64

-- | A transaction ID is 64 bits, specified in BEP 15
type TransactionId = Word32

-- | Execute an 'AnnounceT' action in a 'UpdT' context
liftAnnounceT :: AnnounceT a -> UdpT a
liftAnnounceT = ReaderT . (\f -> f . anEnv) . runReaderT

-- | All requests in BEP 15 and the IPv6 extension start with the same header
data RequestHeader = RequestHeader
  { reqConnectionId  :: !ConnectionId
    -- ^ Connection id provided by connecting or '0x41727101980' when connecting
  , reqAction        :: !Word32
    -- ^ Action. Valid values are:
    --   * 0 -- __Connect__
    --   * 1 -- __Announce__
    --   * 2 -- __Scrape__
    --   * 3 -- __Error__ is only used on return
  , reqTransactionId :: !TransactionId
    -- ^ Supplied by the client. Echoed back to match up request and response.
  }

-- Serialization and Deserialization of the request header.
instance Binary RequestHeader where
  get = liftM3 RequestHeader get get get
  put rh = do
    put $ reqConnectionId rh
    put $ reqAction rh
    put $ reqTransactionId rh

-- | All responses in BEP 15 and the IPv6 extension start with the same header
data ResponseHeader = ResponseHeader
  { resAction        :: !Word32
    -- ^ Action. Same as `reqAction` above. On success, the value is echoed.
    --   On error, the value is @3@
  , resTransactionId :: !TransactionId
    -- ^ Transaction Id. Echoed from the corresponding request.
  }

-- | Copy the relevant values from a request into a response.
makeResponseHeader :: RequestHeader -> ResponseHeader
makeResponseHeader reqH =
  ResponseHeader
    {resAction = reqAction reqH, resTransactionId = reqTransactionId reqH}

-- | Copy the relevant values from a request into an error response.
makeErrorHeader :: RequestHeader -> ResponseHeader
makeErrorHeader reqH =
  ResponseHeader {resAction = 3, resTransactionId = reqTransactionId reqH}

instance Binary ResponseHeader where
  get = liftM2 ResponseHeader get get
  put rh = do
    put $ resAction rh
    put $ resTransactionId rh

instance Binary PortNumber where
  get = liftM fromIntegral getWord16be
  put pn = putWord16be (fromIntegral pn)

instance Binary ScrapeResponse where
  get = do
    l <- get
    s <- get
    return ScrapeResponse {srSeeders = s, srCompletions = 0, srLeechers = l}
  put sr = do
    put $ srLeechers sr
    put $ srSeeders sr

-- | Extract a binary string from a socket that we can feed to a keyed hash.
--   We use this to produce connection ids.
connectionHashString :: SockAddr -> B.ByteString
connectionHashString sock =
  BL.toStrict $
  case sock of
    SockAddrInet p h      -> encode h `BL.append` encode p
    SockAddrInet6 p _ h _ -> encode h `BL.append` encode p

-- | Helper function to read the current connection hash key.
fetchCurrKey :: UdpT ShorthashKey
fetchCurrKey = liftIO . readMVar =<< asks currKey

-- | Helper function to read the previous connection hash key.
fetchPrevKey :: UdpT ShorthashKey
fetchPrevKey = liftIO . readMVar =<< asks prevKey

-- | Helper function to check if a connection is valid
isValidConnId
  :: SockAddr
    -- ^ The address that the udp connection is coming from.
  -> ConnectionId
    -- ^ The connection id supplied from the network
  -> UdpT Bool
    -- ^ True if the keyed hash of @`connectionHashString` sock@ under
    --   either `currKey` or `prevKey` matches @connId@
isValidConnId sock connId = do
  let connStr = connectionHashString sock
  currKey <- fetchCurrKey
  prevKey <- fetchPrevKey
  let currHash = decode $ BL.fromStrict $ shorthash currKey connStr
      prevHash = decode $ BL.fromStrict $ shorthash prevKey connStr
  return (connId == currHash || connId == prevHash)

-- | Handle a connection request. Uses `currKey` to produce a keyed hash of the
--   connection details if the connection request is well formed.
handleConnect
  :: SockAddr
    -- ^ The address the request came in on.
  -> RequestHeader
    -- ^ The RequestHeader that was supplied in the connection request
  -> UdpT (Maybe (ResponseHeader, ConnectionId))
    -- ^ Returns @Just (ResponseHeader, ConnectionId)@ if the request was valid,
    --   otherwise returns @Nothing@
handleConnect sock rh = do
  case (reqConnectionId rh) of
    0x41727101980 -> do
      key <- fetchCurrKey
      let connStr = connectionHashString sock
          connId = decode $ BL.fromStrict $ shorthash key connStr
      return $ Just (makeResponseHeader rh, connId)
    _ -> return Nothing

-- | Parse an IPv4 Announce Request from binary data
getUdpAnnounce4 :: Get AnnounceRequest
getUdpAnnounce4 = getUdpAnnounceGen get SockAddrInet

-- | Parse an IPv6 Announce Request from binary data
getUdpAnnounce6 :: Get AnnounceRequest
getUdpAnnounce6 = getUdpAnnounceGen get (\p a -> SockAddrInet6 p 0 a 0)

-- | Parse a Generic Announce Request from binary data
getUdpAnnounceGen
  :: Get a
    -- ^ Read an address of type @a@
  -> (PortNumber -> a -> SockAddr)
    -- ^ Turn a `PortNumber` and an address of type @a@ into a `SockAddr`
  -> Get AnnounceRequest
    -- ^ The parsed AnnounceRequest
getUdpAnnounceGen getAddr buildSock = do
  ih <- get :: Get InfoHash
  pid <- get :: Get PeerId
  bdownloaded <- get :: Get Word64
  bleft <- get :: Get Word64
  buploaded <- get :: Get Word64
  eventCode <- get :: Get Word32
  let event =
        case eventCode of
          1 -> Just Completed
          2 -> Just Started
          3 -> Just Stopped
          _ -> Nothing
  ipaddr <- getAddr
  _key <- get :: Get Word32
  wantCode <- get :: Get Word32
  let want =
        case wantCode of
          0xffffffff -> Nothing
          x          -> Just x
  port <- get :: Get PortNumber
  return
    AnnounceRequest
      { anInfoHash = ih
      , anPeer = Peer {peerId = pid, peerAddr = buildSock port ipaddr}
      , anUploaded = buploaded
      , anDownloaded = bdownloaded
      , anLeft = bleft
      , anEvent = event
      , anWant = want
      }

-- | Write dense IPv4 addresses and port pairs
packPeers4 :: [Peer] -> Put
packPeers4 = mapM_ packPeer4
  where
    packPeer4 p =
      case peerAddr p of
        SockAddrInet port addr
        -- HostAddress is stored in network byte order
        -- write it out in host endian order.
         -> do
          putWord32host addr
          put port
        _ -> return ()

-- | Write dense IPv6 addresses and port pairs
packPeers6 :: [Peer] -> Put
packPeers6 = mapM_ packPeer6
  where
    packPeer6 p =
      case peerAddr p of
        SockAddrInet6 port _ (a1, a2, a3, a4) _ -> do
          putWord32host a1
          putWord32host a2
          putWord32host a3
          putWord32host a4
          put port
        _ -> return ()

-- | Write an AnnounceResponse to an IPv4 client
packAnnounceResponse4 :: AnnounceResponse -> Put
packAnnounceResponse4 = packAnnounceResponseGen packPeers4

-- | Write an AnnounceResponse to an IPv6 client
packAnnounceResponse6 :: AnnounceResponse -> Put
packAnnounceResponse6 = packAnnounceResponseGen packPeers6

-- | Write a Generic response
packAnnounceResponseGen
  :: ([Peer] -> Put)
    -- ^ Function to write peers to the correct format
  -> AnnounceResponse
    -- ^ `AnnounceResponse` to write
  -> Put
    -- ^ Correctly formatted binary response
packAnnounceResponseGen packPeers ar =
  case ar of
    Failure message -> put message
    PeerList { plInterval = ival
             , plSeeders = ns
             , plLeechers = nl
             , plPeers = peers
             } -> do
      put ival
      put (fromMaybe 0 nl)
      put (fromMaybe 0 ns)
      packPeers peers

-- | Process a UDP Request
handleUdpRequest
  :: Socket
    -- ^ Socket to send the response
  -> SockAddr
    -- ^ Address on which the request came in
  -> B.ByteString
    -- ^ Request body
  -> UdpT ()
    -- ^ Process the request
handleUdpRequest sock addr msg =
  -- Start by parsing a header
  case runGetOrFail get (BL.fromStrict msg) of
    Left _ -> return () -- Unparseable requests just get dropped
    Right (msg', _, rh) -> do
      let (announceAction, announceGetter, announcePack, ipVersion) =
            case addr of
              SockAddrInet {} ->
                (1, getUdpAnnounce4, packAnnounceResponse4, Ipv4)
              SockAddrInet6 {} ->
                (4, getUdpAnnounce6, packAnnounceResponse6, Ipv6)
      case reqAction rh of
        0 -> do
          mresp <- handleConnect addr rh
          case mresp of
            Nothing -> return ()
            Just (respHeader, connId) -> do
              let p = put respHeader >> put connId
              writeResponse p
        x
          | x == announceAction ->
            whenValid addr rh $
            whenParses rh announceGetter msg' $ \an
            -- We don't trust you. Rewrite the supplied address, unless
            -- you're coming from rfc1918
             -> do
              let an' =
                    case addr of
                      SockAddrInet6 _ _ addr6 _ -> updateAddr6 addr6 an
                      SockAddrInet port addr4 ->
                        if isRfc1918 addr4
                          then case peerAddr $ anPeer an of
                                 SockAddrInet _ 0 -> updateAddr4 addr4 an
                                 _                -> an
                          else updateAddr4 addr4 an
              anResp <- liftAnnounceT $ handleAnnounce an'
              writeResponse $ put (makeResponseHeader rh) >> announcePack anResp
        2 ->
          whenValid addr rh $
          case BL.length msg' `divMod` 20 of
            (n, 0) ->
              whenParses rh (replicateM (fromIntegral n) get) msg' $ \sreqs -> do
                sresps <- liftAnnounceT $ handleScrape ipVersion sreqs
                writeResponse (put (makeResponseHeader rh) >> mapM_ put sresps)
            _ -> do
              let p =
                    put (makeErrorHeader rh) >>
                    put (B8.pack "Scrape should be a multiple of 20 bytes.")
              writeResponse p
        _ -> do
          let p =
                put (makeErrorHeader rh) >>
                put (B8.pack "currently unsupported request.")
          writeResponse p
  where
    -- | Guard to guarantee that a `Binary` instances correctly deserializes
    whenParses
      :: RequestHeader
        -- ^ Header of the request. Used in the failure case
      -> Get a
        -- ^ `Binary` deserializer
      -> BL.ByteString
        -- ^ Message to be parsed
      -> (a -> UdpT ())
        -- ^ Action to perform with the correctly parsed value
      -> UdpT ()
        -- ^ Perform the action, or write a failure response
    whenParses rh getter msg action =
      case runGetOrFail getter msg of
        Left _ ->
          writeResponse $
          put (makeErrorHeader rh) >> put (B8.pack "error parsing request.")
        Right (_, _, a) -> action a
    -- | Guard to guaratee that a connection id is valid
    whenValid
      :: SockAddr
        -- ^ Incoming address
      -> RequestHeader
        -- ^ Header with connection id
      -> UdpT ()
        -- ^ Action to perform if the connection is correct
      -> UdpT ()
        -- ^ Perform the action or pass
    whenValid addr rh action = do
      validity <- isValidConnId addr (reqConnectionId rh)
      when validity action
    -- | Write a response on `sock` to `addr`
    writeResponse
      :: Put
        -- ^ A `Put` value to reify into a `B.ByteString`
      -> UdpT ()
        -- ^ Write the value to `sock` at `addr`
    writeResponse p = do
      let respMsg = BL.toStrict $ runPut p
      liftIO $ sendAllTo sock respMsg addr
    -- | Replace the IPv6 peer address in an announcement request
    updateAddr6 addr6 an =
      let peerAddr' =
            case peerAddr $ anPeer an of
              SockAddrInet6 port flow _ scopeId ->
                SockAddrInet6 port flow addr6 scopeId
       in an {anPeer = (anPeer an) {peerAddr = peerAddr'}}
    -- | Replace the IPv4 peer address in an announcement request
    updateAddr4 addr4 an =
      let peerAddr' =
            case peerAddr $ anPeer an of
              SockAddrInet port _ -> SockAddrInet port addr4
       in an {anPeer = (anPeer an) {peerAddr = peerAddr'}}
