{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Network.BitTorrent.Tracker.SnapServer
  ( completeSnap
  ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Attoparsec.ByteString.Char8
import           Data.Binary
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString                           as B
import qualified Data.ByteString.Char8                     as B8
import qualified Data.ByteString.Lazy                      as BL
import           Data.ByteString.Lazy.Builder
import           Data.Digest.SHA1
import           Data.Either
import           Data.Endian
import           Data.Functor.Identity
import           Data.Functor
import qualified Data.Map                                  as M
import           Data.Maybe
import           Data.Monoid
import           Data.Word
import           Debug.Trace
import           Network.BitTorrent.Tracker.Announce
import           Network.BitTorrent.Tracker.AnnounceServer
import           Network.Socket
import           Snap.Core
import           System.Endian

-- | Continuation based Either. Takes a left and a right continuation, and
-- procduces a value.
newtype ContEitherT m l r = ContEitherT
  { runContEitherT :: forall z. (l -> m z) -> (r -> m z) -> m z
  }

-- | Run the left continuation
left :: l -> ContEitherT m l r
left l = ContEitherT $ \lk rk -> lk l

-- | Run the right continuation
right :: r -> ContEitherT m l r
right r = ContEitherT $ \lk rk -> rk r

-- | Lift a plain Either l r to a ContEitherT m l r
liftEither :: Either l r -> ContEitherT m l r
liftEither (Left l)  = left l
liftEither (Right r) = right r

-- | ContEitherT is really a bifunctor, but we only need plain functor on the
-- right in this case. Should look similar to the functor instance for ContT
instance Functor (ContEitherT m l) where
  fmap f (ContEitherT kka) = ContEitherT $ \lk rk -> kka lk (rk . f)

-- | ContEitherT is a monad on the right. Note that the failure propogates
-- exactly like it would for either, but without needing to be inspected.
instance Monad (ContEitherT m l) where
  return = right
  (ContEitherT kka) >>= f =
    ContEitherT $ \lk rk -> kka lk (\a -> runContEitherT (f a) lk rk)

-- | ContEitherT is an applicative. Written to satisfy ApplicativeMonad,
-- not used.
instance Applicative (ContEitherT m l) where
  pure = right
  (ContEitherT kkf) <*> (ContEitherT kka) =
    ContEitherT $ \lk rk -> kkf lk (\f -> kka lk (rk . f))
  (ContEitherT kka) *> (ContEitherT kkb) =
    ContEitherT $ \lk rk -> kka lk (\_ -> kkb lk rk)
  (ContEitherT kka) <* (ContEitherT kkb) =
    ContEitherT $ \lk rk -> kka lk (\a -> kkb lk (rk . const a))

-- | ContEitherT when we don't need the wrapped monad, just failure.
type ContEither = ContEitherT Identity

-- | Unwrap ContEither and run it with a success and failure continuation.
runContEither :: ContEither l r -> (l -> z) -> (r -> z) -> z
runContEither ma lk rk =
  runIdentity $ runContEitherT ma (Identity . lk) (Identity . rk)

-- | Lower a value of ContEither to a regular Either. Note that this can't be
-- done generally, only for ContEither
toEither :: ContEither l r -> Either l r
toEither ma = runContEither ma Left Right

-- | Catch for ContEitherT.
-- Note the similarity to (>>=) above.
eCatch :: ContEitherT m l r -> (l -> ContEitherT m l r) -> ContEitherT m l r
eCatch ka handler =
  ContEitherT $ \lk rk ->
    runContEitherT ka (\l -> runContEitherT (handler l) lk rk) rk

-- | Parse out an AnnounceRequest from an Http Request
rqAnnounce :: Request -> ContEitherT m B.ByteString AnnounceRequest
rqAnnounce req = do
  let params = rqQueryParams req
  hash <- grabAndParseParam "info_hash" params parseWord160
  pid <- grabAndParseParam "peer_id" params parseWord160
  port <- grabAndParseParam "port" params parsePort
  uploaded <- grabAndParseParam "uploaded" params parseDec
  downloaded <- grabAndParseParam "downloaded" params parseDec
  left <- grabAndParseParam "left" params parseDec
  addr <- optionalParseParam "ip" params (parseSockAddr port)
  reqAddr <- parseSockAddr port B8.empty (rqClientAddr req)
  compact <-
    optionalParseParam "compact" params parseDec :: ContEitherT m B.ByteString (Maybe Word8)
  failWhen
    "Compact not supported."
    (isJust compact && compact /= Just 1)
  event <- optionalParseParam "event" params parseEvent
  want <- optionalParseParam "numwant" params parseDec
  let realAddr =
        case reqAddr of
          SockAddrInet6 {} -> reqAddr
          SockAddrInet _ addr4 ->
            if isRfc1918 addr4
              then case addr of
                     Nothing                  -> reqAddr
                     Just SockAddrInet6 {}    -> reqAddr
                     Just a@SockAddrInet{} -> a
              else reqAddr
  return
    AnnounceRequest
      { anInfoHash = hash
      , anPeer = Peer {peerId = pid, peerAddr = realAddr}
      , anUploaded = uploaded
      , anDownloaded = downloaded
      , anLeft = left
      , anEvent = event
      , anWant = want
      }

-- | Handle announce requests via HTTP
announceAction :: AnnounceEnv -> Snap ()
announceAction env = do
  kAnnounce <- getsRequest rqAnnounce
  runContEitherT kAnnounce failure success
  where
    failure message = do
      writeBS message
      getResponse >>= finishWith
    success announce = do
      resp <- liftIO $ runReaderT (handleAnnounce announce) env
      writeLBS $ toLazyByteString $ encoder resp
      getResponse >>= finishWith
      where
        encoder =
          case peerAddr $ anPeer announce of
            SockAddrInet {}  -> bencodeResponse4
            SockAddrInet6 {} -> bencodeResponse6

-- | Handle scrape requests via HTTP
scrapeAction :: AnnounceEnv -> Snap ()
scrapeAction env = do
  mHashes <- getsRequest (rqQueryParam "info_hash")
  case mHashes of
    Nothing -> do
      writeBS "hashes required."
      getResponse >>= finishWith
    Just rawvals -> do
      let kvals = mapM parse rawvals
          parse = parseWord160 "info_hash"
      runContEitherT kvals failure success
  where
    success hashes = do
      kipVersion <- getsRequest rqGetIpVersion
      let ipVersion = runContEither kipVersion (const Ipv4) id
      resps <- liftIO $ runReaderT (handleScrape ipVersion hashes) env
      writeLBS $ toLazyByteString $ bencodeScrapes $ zip hashes resps
      getResponse >>= finishWith
    failure message = do
      writeBS message
      getResponse >>= finishWith

rqGetIpVersion :: Request -> ContEitherT m B.ByteString IpVersion
rqGetIpVersion req = do
  let port = (fromIntegral . rqClientPort) req
  reqAddr <- parseSockAddr port B8.empty (rqClientAddr req)
  case reqAddr of
    SockAddrInet {}  -> return Ipv4
    SockAddrInet6 {} -> return Ipv6

completeSnap :: AnnounceEnv -> Snap ()
completeSnap env =
  path "announce" (method GET $ announceAction env) <|>
  path "scrape" (method GET $ scrapeAction env)

-- Parsers and Helpers
-- | Conditional failure given a fail value and a boolean.
failWhen :: b -> Bool -> ContEitherT m b ()
fallWhen b True = left b

failWhen _ False = right ()

-- | Helper to construct "<key> missing." messages
missing :: B.ByteString -> B.ByteString
missing key = key <> " missing."

-- | Helper to construct "<key> too many times." messages
tooMany :: B.ByteString -> B.ByteString
tooMany key = key <> " too many times."

-- | Helper to construct "<key> not formatted correctly." messages
misformatted :: B.ByteString -> B.ByteString
misformatted key = key <> " not formatted correctly."

-- | Parsing helper that lifts a parse result to ContEitherT and returns a
-- message on parse failure.
maybeParse ::
     B.ByteString -> B.ByteString -> Parser a -> ContEitherT m B.ByteString a
maybeParse name = parseOnlyMessage (misformatted name)

-- | Parse a Word160 presented as 20 bytes.
-- Snap does url decoding for us.
parseWord160 ::
     B.ByteString -> B.ByteString -> ContEitherT m B.ByteString Word160
parseWord160 name val =
  case B.length val of
    20 ->
      case runGetOrFail get (BL.fromStrict val) of
        Left _ -> left (misformatted name) -- Shouldn't happen
        Right (rem, count, result) -> do
          failWhen
            (name <> " incorrect length")
            (count /= 20 || not (BL.null rem))
          return result
    _ -> left (misformatted name)

-- | Parse a decimal value.
parseDec ::
     (Integral a)
  => B.ByteString
  -> B.ByteString
  -> ContEitherT m B.ByteString a
parseDec name val = maybeParse name val decimal

-- | Parse a port.
parsePort ::
     B.ByteString -> B.ByteString -> ContEitherT m B.ByteString PortNumber
parsePort name val = maybeParse name val decimal

-- | Parse a socket address.
parseSockAddr ::
     PortNumber
  -> B.ByteString
  -> B.ByteString
  -> ContEitherT m B.ByteString SockAddr
parseSockAddr pnum name val = maybeParse name val parser
  where
    parser =
      (SockAddrInet pnum <$> parseIp4) <|>
      (SockAddrInet6 pnum 0 <$> parseIp6 <*> pure 0)

-- | Abbreviation for fromIntegral
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- | Parser for text ipv4 address
parseIp4 :: Parser HostAddress
parseIp4 =
  packBytes <$> decimal <* char '.' <*> decimal <* char '.' <*> decimal <*
    char '.' <*>
    decimal
  where
    packBytes :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
    packBytes b1 b2 b3 b4 =
      fi b1 `shiftL` 24 .|. fi b2 `shiftL` 16 .|. fi b3 `shiftL` 8 .|. fi b4

-- | Parser for text ipv6 address
-- An ipv6 address is either compacted with a double colon, or not-compact.
-- The non-compact case is easiest. The compact case proceeds in 2 steps.
-- Parse up to 7 sections, then a double colon
-- Parse 7 - n remaining sections
parseIp6 :: Parser HostAddress6
parseIp6 = complete6 <|> seperated6
  where
    hex = hexadecimal
    colon = char ':'
    doubleColon = char ':' *> char ':'
    complete6 =
      packComplete <$> hex <* colon <*> hex <* colon <*> hex <* colon <*> hex <*
      colon <*>
      hex <*
      colon <*>
      hex <*
      colon <*>
      hex <*
      colon <*>
      hex
    packComplete ::
         Word16
      -> Word16
      -> Word16
      -> Word16
      -> Word16
      -> Word16
      -> Word16
      -> Word16
      -> HostAddress6
    packComplete a b c d e f g h =
      (byteSwap a b, byteSwap c d, byteSwap e f, byteSwap g h)
    byteSwap :: Word16 -> Word16 -> Word32
    byteSwap a b = toBigEndian $ fi a `shiftL` 16 .|. fi b
    seperated6 =
      sepByUpto 7 hex colon >>= \pre ->
        packSeparated pre <$ doubleColon <*>
        sepByUpto (7 - length pre) hex colon
    packSeparated :: [Word16] -> [Word16] -> HostAddress6
    packSeparated first last = packSeparated' (first ++ replicate rem 0 ++ last)
      where
        rem = 8 - length first - length last
    packSeparated' [a, b, c, d, e, f, g, h] = packComplete a b c d e f g h

-- | Parse a client's reported event.
parseEvent :: B.ByteString -> B.ByteString -> ContEitherT m B.ByteString Event
parseEvent name val = maybeParse name val parser
  where
    parser =
      string "completed" $> Completed <|>
      string "started" $> Started <|>
      string "stopped" $> Stopped

-- | Separated by Up to, takes a count: k, a parser: p, and a separator: sep,
-- and parses up to k values of p, discarding sep, and returns them as a list.
sepByUpto :: (Alternative f) => Int -> f a -> f s -> f [a]
sepByUpto k _ _
  | k <= 0 = pure []

sepByUtpo k p sep =
  liftA2 (:) p ((sep *> sepBy1Upto (k - 1) p sep) <|> pure []) <|> pure []
  where
    sepBy1Upto 0 _ _ = pure []
    sepBy1Upto k p sep =
      liftA2 (:) p ((sep *> sepBy1Upto (k - 1) p sep) <|> pure [])

-- | Up Convert a Maybe value to a ContEitherT by supplying a left value for
-- for the failure case.
withMessage :: l -> Maybe a -> ContEitherT m l a
withMessage l = maybe (left l) right

-- | Map on the left. Similar to fmap above.
mapLeft :: (l1 -> l2) -> ContEitherT m l1 a -> ContEitherT m l2 a
mapLeft f ka = ContEitherT $ \lk rk -> runContEitherT ka (lk . f) rk

-- | Wrap parseOnly by supplying our own error message, and lifting the result
-- from Either to ContEitherT
parseOnlyMessage :: b -> B.ByteString -> Parser a -> ContEitherT m b a
parseOnlyMessage msg raw parser =
  mapLeft (const msg) $ liftEither $ parseOnly parser raw

-- | Fail when a list is not a singleton, and succeed if it is.
singletonList :: l -> [a] -> ContEitherT m l a
singletonList _ [x] = right x
singletonList l _      = left l

-- | Handle 0 or 1 parameters. Fails if there is more than one of the parameter
-- supplied.
optionalParam ::
     B.ByteString
  -> M.Map B.ByteString [B.ByteString]
  -> ContEitherT m B.ByteString (Maybe B.ByteString)
optionalParam key map =
  case M.lookup key map of
    Nothing   -> return Nothing
    Just vals -> Just <$> singletonList (tooMany key) vals

-- | Handle and parse 0 or 1 parameters. Fails if there is more than 1 or if
-- the parser fails.
optionalParseParam ::
     B.ByteString
  -> M.Map B.ByteString [B.ByteString]
  -> (B.ByteString -> B.ByteString -> ContEitherT m B.ByteString a)
  -> ContEitherT m B.ByteString (Maybe a)
optionalParseParam key map parser = do
  result <- optionalParam key map
  case result of
    Nothing  -> return Nothing
    Just val -> Just <$> parser key val

-- | Mandatory param. Fails if non-existant, or if more than 1
grabParam ::
     B.ByteString
  -> M.Map B.ByteString [B.ByteString]
  -> ContEitherT m B.ByteString B.ByteString
grabParam key map = do
  vals <- withMessage (missing key) (M.lookup key map)
  singletonList (tooMany key) vals

-- | Mandatory param with parser. Fails if non-existant, more than 1, or if
-- the parser fails.
grabAndParseParam ::
     B.ByteString
  -> M.Map B.ByteString [B.ByteString]
  -> (B.ByteString -> B.ByteString -> ContEitherT m B.ByteString a)
  -> ContEitherT m B.ByteString a
grabAndParseParam key map parser = grabParam key map >>= parser key
