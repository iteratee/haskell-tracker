{-# LANGUAGE TupleSections #-}

module Network.BitTorrent.Tracker.AnnounceServer
  ( AnnounceConfig(..)
  , AnnounceEnv(..)
  , AnnounceState(..)
  , AnnounceT(..)
  , IpVersion(..)
  , defaultConfig
  , emptyAnnounceState
  , handleAnnounce
  , handleScrape
  , pruneQueue
  ) where

import           Control.Concurrent.MVar
import           Control.Monad                         hiding (forM, mapM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString                       as B
import           Data.Int
import           Data.List                             (foldl')
import qualified Data.Map                              as M
import           Data.Maybe
import qualified Data.Set                              as S
import           Data.Time.Clock
import           Data.Traversable
import           Data.Word
import           Network.BitTorrent.Tracker.Announce
import           Network.BitTorrent.Tracker.PeerFinder
import           Network.Socket

-- | A Map to keep track of when a peer was last seen.
type ActivityRecord = M.Map PeerId UTCTime

-- | A Queue used to keep track of when a peer was last seen in order of the
-- sightings. Used together with the ActivityRecord to make sure that only
-- active hashes are kept around.
type ActivityQueue = S.Set (UTCTime, Maybe PeerId)

-- | All the state necessary to respond to Announce or Scrape Requests
data AnnounceState = AnnounceState
    -- | The current set of active hashes
  { activeHashes      :: MVar (M.Map InfoHash HashRecord)
    -- | A map from hashes to the ActivityRecord for that hash. Used to allow
    -- removal of entries from the activity queue.
  , peersLastSeen     :: MVar (M.Map InfoHash (MVar ActivityRecord))
    -- | A map from hashes to the ActivityQueue for that hash
  , peerActivityQueue :: MVar (M.Map InfoHash (MVar ActivityQueue))
  }

-- | Create an empty announce state serving no hashes and having no peers.
emptyAnnounceState :: IO AnnounceState
emptyAnnounceState = do
  ah <- newMVar M.empty
  pls <- newMVar M.empty
  paq <- newMVar M.empty
  return
    AnnounceState
      {activeHashes = ah, peersLastSeen = pls, peerActivityQueue = paq}

type Seconds = Int32

-- | Configuration parameters for how an Announce Server behaves.
data AnnounceConfig = AnnounceConfig
  { ancInterval        :: !Seconds
  , ancMaxPeers        :: !Word32
  , ancDefaultPeers    :: !Word32
  , ancIdlePeerTimeout :: !Seconds
  , ancAddrs           :: [(String, String)]
  }

-- | The default config
defaultConfig :: AnnounceConfig
defaultConfig =
  AnnounceConfig
    { ancInterval = 120
    , ancMaxPeers = 50
    , ancDefaultPeers = 30
    , ancIdlePeerTimeout = 360 -- Six minutes, three announce intervals
    , ancAddrs = [("0.0.0.0", "6969"), ("::", "6970")]
    }

-- | An announce environment is the state and the config.
data AnnounceEnv = AnnounceEnv
  { anSt   :: AnnounceState
  , anConf :: AnnounceConfig
  }

-- | To serve anounce requests, you need access to AnnounceEnv and the ability
--   to perform IO.
type AnnounceT = ReaderT AnnounceEnv IO

-- | Helper function to get the state from AnnounceEnv
getState :: AnnounceT AnnounceState
getState = asks anSt

-- | Helper function to get the config from AnnounceEnv
getConf :: AnnounceT AnnounceConfig
getConf = asks anConf

-- | Remove inactive peers from hashes.
--   TODO: remove hashes with no active peers.
pruneQueue :: AnnounceT ()
pruneQueue = do
  now <- liftIO getCurrentTime
  st <- getState
  hrMap <- liftIO $ takeMVar (activeHashes st)
  lastSeen <- liftIO $ takeMVar (peersLastSeen st)
  queue <- liftIO $ takeMVar (peerActivityQueue st)
  liftIO $ putMVar (activeHashes st) hrMap
  liftIO $ putMVar (peersLastSeen st) lastSeen
  liftIO $ putMVar (peerActivityQueue st) queue
  forM_ (M.assocs lastSeen) $ \(hash, hashActivityM) -> do
    let hashQueueM = queue M.! hash
    let hr = hrMap M.! hash
    pruneHashQueue now hashActivityM hashQueueM hr
  where
    pruneHashQueue ::
         UTCTime
      -> MVar ActivityRecord
      -> MVar ActivityQueue
      -> HashRecord
      -> AnnounceT ()
    pruneHashQueue now hashActivityM hashQueueM hr = do
      activity <- liftIO $ takeMVar hashActivityM
      queue <- liftIO $ takeMVar hashQueueM
      timeout <- liftM ancIdlePeerTimeout getConf
      let old_now = addUTCTime (fromIntegral $ negate timeout) now
          (old, queue') = S.split (old_now, Nothing) queue
          activity' =
            foldl' (flip (M.delete . fromJust . snd)) activity (S.elems old)
      liftIO $ putMVar hashActivityM activity'
      liftIO $ putMVar hashQueueM queue'
      liftIO $
        forM_ [hrInet4 hr, hrInet6 hr] $ \phrM -> do
          phr <- takeMVar phrM
          putMVar phrM $
            phr
              { phrSeeders = cleanUp old (phrSeeders phr)
              , phrLeechers = cleanUp old (phrLeechers phr)
              }
    cleanUp :: S.Set (UTCTime, Maybe PeerId) -> RandomPeerList -> RandomPeerList
    cleanUp old rpl =
      foldl' (flip (removePeerId . fromJust . snd)) rpl (S.elems old)

-- | Handle an announce request. Get peers if required.
handleAnnounce :: AnnounceRequest -> AnnounceT AnnounceResponse
handleAnnounce an = do
  let peer = anPeer an
      hash = anInfoHash an
  st <- getState
  hr <- liftIO $ getHashRecord st hash
  liftIO $ updateActivity st hash peer
  let phr = getProtocolHashRecord peer hr
      peerGetter
        | anEvent an == Just Completed = \count phr -> return []
        | isSeeder an = getLeechers
        | otherwise = getPeers
  maxPeers <- liftM ancMaxPeers getConf
  defPeers <- liftM ancDefaultPeers getConf
  let peersWanted =
        case anEvent an of
          Just Completed -> 0
          _              -> fromMaybe defPeers (anWant an)
      peerCount = min maxPeers peersWanted
  peers <- liftIO $ peerGetter (fromIntegral peerCount) phr
  interval <- liftM ancInterval getConf
  addOrRemovePeer an
  (nSeeders, nLeechers, _) <- liftIO $ getPeerCounts phr
  return
    PeerList
      { plInterval = fromIntegral interval
      , plSeeders = Just nSeeders
      , plLeechers = Just nLeechers
      , plPeers = peers
      }

-- | Record a peers activity so they don't get pruned.
updateActivity :: AnnounceState -> InfoHash -> Peer -> IO ()
updateActivity st hash peer = do
  lastSeenMap <- takeMVar (peersLastSeen st)
  queueMap <- takeMVar (peerActivityQueue st)
  case M.lookup hash lastSeenMap of
    Nothing -> do
      newLastSeen <- newMVar M.empty
      newQueue <- newMVar S.empty
      putMVar (peersLastSeen st) $ M.insert hash newLastSeen lastSeenMap
      putMVar (peerActivityQueue st) $ M.insert hash newQueue queueMap
      updateHashActivity peer newLastSeen newQueue
    Just lastSeen -> do
      putMVar (peersLastSeen st) lastSeenMap
      putMVar (peerActivityQueue st) queueMap
      let activityQueue = queueMap M.! hash
      updateHashActivity peer lastSeen activityQueue
  where
    updateHashActivity peer lastSeenM queueM = do
      let pid = peerId peer
      now <- getCurrentTime
      lastSeen <- takeMVar lastSeenM
      queue <- takeMVar queueM
      case M.lookup pid lastSeen of
        Nothing -> do
          putMVar lastSeenM $ M.insert pid now lastSeen
          putMVar queueM $ S.insert (now, Just pid) queue
        Just old_now -> do
          putMVar lastSeenM $ M.insert pid now lastSeen
          putMVar queueM $
            S.insert (now, Just pid) $ S.delete (old_now, Just pid) queue

-- | Lookup or create a HashRecord for a specific hash.
getHashRecord :: AnnounceState -> InfoHash -> IO HashRecord
getHashRecord st hash = do
  hrMap <- takeMVar (activeHashes st)
  case M.lookup hash hrMap of
    Just hr -> do
      putMVar (activeHashes st) hrMap
      return hr
    Nothing -> do
      hr <- emptyHashRecord
      putMVar (activeHashes st) $ M.insert hash hr hrMap
      return hr

-- | Grab the correct ProtocolHashRecord based on a peer's protocol.
getProtocolHashRecord :: Peer -> HashRecord -> MVar ProtocolHashRecord
getProtocolHashRecord peer hr =
  case peerAddr peer of
    SockAddrInet {}  -> hrInet4 hr
    SockAddrInet6 {} -> hrInet6 hr
    _                -> error "Unix sockets not supported."
      -- TODO: Make this a reasonable exception

-- | Predicate to determine if an announce request is from a seeder or leecher.
isSeeder :: AnnounceRequest -> Bool
isSeeder an = anLeft an == 0

-- | A peer is either added, shifted from leecher to seeder, or removed.
data PeerAction
  = Add
  | Shift
  | Remove

-- | Add or remove a peer (or shift them from leecher to seeder) based on
-- the action in the AnnounceRequest
addOrRemovePeer :: AnnounceRequest -> AnnounceT ()
addOrRemovePeer an = do
  let peer = anPeer an
      hash = anInfoHash an
  st <- getState
  case anEvent an
    -- On a non-event add them just in case
        of
    Nothing        -> addAnnounce st an
    Just Started   -> addAnnounce st an
    Just Completed -> shiftAnnounce st an
    Just Stopped   -> removeAnnounce st an
  where
    addAnnounce :: AnnounceState -> AnnounceRequest -> AnnounceT ()
    addAnnounce = updateAnnounce Add
    shiftAnnounce :: AnnounceState -> AnnounceRequest -> AnnounceT ()
    shiftAnnounce = updateAnnounce Shift
    removeAnnounce :: AnnounceState -> AnnounceRequest -> AnnounceT ()
    removeAnnounce = updateAnnounce Remove
    updateAnnounce ::
         PeerAction -> AnnounceState -> AnnounceRequest -> AnnounceT ()
    updateAnnounce act st an = do
      let activeMapM = activeHashes st
      activeMap <- liftIO $ takeMVar activeMapM
      hr <-
        case M.lookup (anInfoHash an) activeMap of
          Nothing ->
            liftIO $ do
              newHr <- emptyHashRecord
              putMVar activeMapM $ M.insert (anInfoHash an) newHr activeMap
              return newHr
          Just hr -> return hr
      liftIO $ putMVar activeMapM activeMap
      let phrM = getProtocolHashRecord (anPeer an) hr
      phr <- liftIO $ takeMVar phrM
      let (seedUpdater, leechUpdater, compInc) =
            case act of
              Shift ->
                ( addPeer (anPeer an)
                , return . removePeerId (peerId (anPeer an))
                , (+ 1))
              Add -> makeUpdaters (addPeer (anPeer an)) an
              Remove ->
                makeUpdaters (return . removePeerId (peerId (anPeer an))) an
      liftIO $ do
        seeders' <- seedUpdater (phrSeeders phr)
        leechers' <- leechUpdater (phrLeechers phr)
        putMVar phrM $
          phr
            { phrSeeders = seeders'
            , phrLeechers = leechers'
            , phrCompleteCount = compInc (phrCompleteCount phr)
            }
      where
        makeUpdaters ::
             (RandomPeerList -> IO RandomPeerList)
          -> AnnounceRequest
          -> ( RandomPeerList -> IO RandomPeerList
             , RandomPeerList -> IO RandomPeerList
             , Word32 -> Word32)
        makeUpdaters mod an =
          if isSeeder an
            then (mod, return, id)
            else (return, mod, id)

-- | IpVersion 4 or 6
data IpVersion
  = Ipv4
  | Ipv6

-- | Handle a scrape request. From the clients perspective the ipv6 and ipv4
-- servers are separated, so give them a count only for what they can see.
handleScrape :: IpVersion -> [ScrapeRequest] -> AnnounceT [ScrapeResponse]
handleScrape ipVersion hashes = do
  hashRecords <- liftIO . readMVar =<< asks (activeHashes . anSt)
  forM hashes $ \hash ->
    liftIO $
    case M.lookup hash hashRecords of
      Nothing -> return emptyScrapeResponse
      Just hr -> do
        let mphr =
              case ipVersion of
                Ipv4 -> hrInet4 hr
                Ipv6 -> hrInet6 hr
        (seeders, leechers, completed) <- getPeerCounts mphr
        return
          ScrapeResponse
            { srSeeders = seeders
            , srLeechers = leechers
            , srCompletions = completed
            }
