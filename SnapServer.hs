{-# LANGUAGE RankNTypes #-}
module SnapServer where

import Announce
import AnnounceServer
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Attoparsec.ByteString.Char8
import Data.Binary
import Data.Binary.Get
import Data.Bits
import Data.ByteString.Lazy.Builder
import Data.Digest.SHA1
import Data.Either
import Data.Functor.Identity
import Data.Maybe
import Data.Monoid
import Data.Word
import Debug.Trace
import Network.Socket
import Snap.Core
import System.Endian
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M

newtype ContEitherT m l r = ContEitherT { runContEitherT :: (forall z. (l -> m z) -> (r -> m z) -> m z) }
left :: l -> ContEitherT m l r
left l = ContEitherT $ \lk rk -> lk l
right :: r -> ContEitherT m l r
right r = ContEitherT $ \lk rk -> rk r

liftEither :: Either l r -> ContEitherT m l r
liftEither (Left l) = left l
liftEither (Right r) = right r

instance Functor (ContEitherT m l) where
  fmap f (ContEitherT kka) = ContEitherT $ \lk rk -> kka lk (rk . f)

instance Monad (ContEitherT m l) where
  return = right
  (ContEitherT kka) >>= f = ContEitherT $ \lk rk -> kka lk (\a -> runContEitherT (f a) lk rk)

type ContEither = ContEitherT Identity
toEither :: ContEither l r -> Either l r
toEither ma = runIdentity $ runContEitherT ma (Identity . Left) (Identity . Right)

eCatch :: ContEitherT m l r -> (l -> ContEitherT m l r) -> ContEitherT m l r
eCatch ka handler = ContEitherT $ \lk rk -> runContEitherT ka (\l -> runContEitherT (handler l) lk rk) rk

rqAnnounce :: Request -> ContEitherT m B.ByteString Announce
rqAnnounce req = do
  let params = rqQueryParams req
  hash <- grabAndParseParam (B8.pack "info_hash") params urlBytes
  pid <- grabAndParseParam (B8.pack "peer_id") params urlBytes
  port <- grabAndParseParam (B8.pack "port") params parsePort
  uploaded <- grabAndParseParam (B8.pack "uploaded") params parseDec
  downloaded <- grabAndParseParam (B8.pack "downloaded") params parseDec
  left <- grabAndParseParam (B8.pack "left") params parseDec
  addr <- optionalParseParam (B8.pack "ip") params (parseSockAddr port)
  reqAddr <- parseSockAddr port (B8.empty) (rqRemoteAddr req)
  compact <- optionalParseParam (B8.pack "compact") params parseDec
    :: ContEitherT m B.ByteString (Maybe Word8)
  failWhen (B8.pack "Compact not supported.")
    (compact /= Nothing && compact /= Just 1)
  event <- optionalParseParam (B8.pack "event") params parseEvent
  want <- optionalParseParam (B8.pack "numwant") params parseDec
  let realAddr = case reqAddr of
        SockAddrInet6 _ _ _ _ -> reqAddr
        SockAddrInet _ addr4 ->
          case isRfc1918 addr4 of
            False -> reqAddr
            True -> case addr of
              Nothing -> reqAddr
              Just (SockAddrInet6 _ _ _ _) -> reqAddr
              Just a@(SockAddrInet _ _) -> a
  return Announce {
    anInfoHash = hash,
    anPeer = Peer {
      peerId = pid,
      peerAddr = realAddr
    },
    anUploaded = uploaded,
    anDownloaded = downloaded,
    anLeft = left,
    anEvent = event,
    anWant = want
  }

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
        encoder = case (peerAddr $ anPeer announce) of
          SockAddrInet _ _ -> bencodeResponse4
          SockAddrInet6 _ _ _ _ -> bencodeResponse6

scrapeAction :: AnnounceEnv -> Snap ()
scrapeAction env = do
  mHashes <- getsRequest (rqQueryParam (B8.pack "info_hash"))
  case mHashes of
    Nothing -> do
      writeBS (B8.pack "hashes required.")
      getResponse >>= finishWith
    Just rawvals -> do
      let kvals = mapM parse rawvals
          parse = (urlBytes (B8.pack "info_hash"))
      runContEitherT kvals failure success
  where
    success hashes = do
      resps <- liftIO $ runReaderT (handleScrape hashes) env
      writeLBS $ toLazyByteString $ bencodeScrapes $ zip hashes resps
      getResponse >>= finishWith
    failure message = do
      writeBS message
      getResponse >>= finishWith
      
  

completeSnap :: AnnounceEnv -> Snap ()
completeSnap env = 
  (path (B8.pack "announce") $ method GET $ announceAction env) <|>
  (path (B8.pack "scrape") $ method GET $ scrapeAction env)

failWhen :: b -> Bool -> ContEitherT m b ()
fallWhen b True  = left b
failWhen _ False = right ()

missing :: B.ByteString -> B.ByteString
missing key = key <> (B8.pack " missing.")

tooMany :: B.ByteString -> B.ByteString
tooMany key = key <> (B8.pack " too many times.")

misformatted :: B.ByteString -> B.ByteString
misformatted key = key <> (B8.pack " not formatted correctly.")

maybeParse :: B.ByteString -> B.ByteString -> Parser a -> ContEitherT m B.ByteString a
maybeParse name val parser = 
  parseOnlyMessage (misformatted name) val parser

urlBytes :: B.ByteString -> B.ByteString -> ContEitherT m B.ByteString Word160
urlBytes name val =
  case B.length val of
    20 -> case runGetOrFail get (BL.fromStrict val) of
      Left _ -> left (misformatted name) -- Shouldn't happen
      Right (rem, count, result) -> do
        failWhen (name <> (B8.pack "incorrect length")) (count /= 20 || not (BL.null rem))
        return result
    _ -> left (misformatted name)

parseDec :: (Integral a) => B.ByteString -> B.ByteString -> ContEitherT m B.ByteString a
parseDec name val = maybeParse name val decimal

parsePort :: B.ByteString -> B.ByteString -> ContEitherT m B.ByteString PortNumber
parsePort name val = maybeParse name val (PortNum <$> decimal)

parseSockAddr :: PortNumber -> B.ByteString -> B.ByteString -> ContEitherT m B.ByteString SockAddr
parseSockAddr pnum name val = maybeParse name val parser
  where
    parser = (SockAddrInet pnum <$> parseIp4) <|>
             (SockAddrInet6 pnum 0 <$> parseIp6 <*> pure 0)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

parseIp :: Parser (Either HostAddress HostAddress6)
parseIp = (Left <$> parseIp4) <|> (Right <$> parseIp6)

parseIp4 :: Parser HostAddress
parseIp4 = fromBE32 <$> (packBytes <$> decimal <* char '.'
                                   <*> decimal <* char '.'
                                   <*> decimal <* char '.'
                                   <*> decimal)
  where
    packBytes :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
    packBytes b1 b2 b3 b4 = (fi b1) `shiftL` 24 .|.
                            (fi b2) `shiftL` 16 .|.
                            (fi b3) `shiftL` 8 .|.
                            (fi b4)

parseIp6 :: Parser HostAddress6
parseIp6 = complete6 <|> seperated6
  where
    hex = hexadecimal
    colon = char ':'
    doubleColon = char ':' *> char ':'
    complete6 = packComplete <$>
      hex <* colon <*> hex <* colon <*> hex <* colon <*> hex
          <* colon <*> hex <* colon <*> hex <* colon <*> hex <* colon <*> hex
    packComplete :: Word16 -> Word16 -> Word16 -> Word16 ->
                    Word16 -> Word16 -> Word16 -> Word16 ->
                    HostAddress6
    packComplete a b c d e f g h = (
      (fi a) `shiftL` 16 .|. (fi b), (fi c) `shiftL` 16 .|. (fi d),
      (fi e) `shiftL` 16 .|. (fi f), (fi g) `shiftL` 16 .|. (fi h))
    seperated6 = sepByUpto 7 hex colon >>= \pre ->
      packSeparated pre <$ doubleColon <*> sepByUpto (7 - (length pre)) hex colon
    packSeparated :: [Word16] -> [Word16] -> HostAddress6
    packSeparated first last = packSeparated' (first ++ replicate rem 0 ++ last)
      where rem = (8 - length first - length last)
    packSeparated' (a:b:c:d:e:f:g:h:[]) = packComplete a b c d e f g h

parseEvent :: B.ByteString -> B.ByteString -> ContEitherT m B.ByteString Event
parseEvent name val = maybeParse name val parser
  where
    parser =
      string (B8.pack "completed") *> pure Completed <|>
      string (B8.pack "started") *> pure Started <|>
      string (B8.pack "stopped") *> pure Stopped

sepByUpto :: (Alternative f) => Int -> f a -> f s -> f [a]
sepByUpto k _ _ | k <= 0  = pure []
sepByUtpo k p sep = liftA2 (:) p ((sep *> sepBy1Upto (k-1) p sep) <|> pure []) <|> pure []
  where
    sepBy1Upto 0 _ _ = pure []
    sepBy1Upto k p sep = liftA2 (:) p ((sep *> sepBy1Upto (k-1) p sep) <|> pure [])

withMessage :: l -> Maybe a -> ContEitherT m l a
withMessage l = maybe (left l) right

mapLeft :: (l1 -> l2) -> ContEitherT m l1 a -> ContEitherT m l2 a
mapLeft f ka = ContEitherT $ \lk rk -> runContEitherT ka (lk . f) rk

parseOnlyMessage :: b -> B.ByteString -> Parser a -> ContEitherT m b a
parseOnlyMessage msg raw parser =
  mapLeft (const msg) $ liftEither $ parseOnly parser raw

singletonList :: l -> [a] -> ContEitherT m l a
singletonList _ (x:[]) = right x
singletonList l _ = left l

optionalParam :: B.ByteString
              -> M.Map B.ByteString [B.ByteString]
              -> ContEitherT m B.ByteString (Maybe B.ByteString)
optionalParam key map =
  case M.lookup key map of
    Nothing -> return Nothing
    Just vals -> do
      Just <$> singletonList (tooMany key) vals

optionalParseParam :: B.ByteString
                   -> M.Map B.ByteString [B.ByteString]
                   -> (B.ByteString -> B.ByteString -> ContEitherT m B.ByteString a)
                   -> ContEitherT m B.ByteString (Maybe a)
optionalParseParam key map parser = do
  result <- optionalParam key map
  case result of
    Nothing -> return Nothing
    Just val -> Just <$> parser key val

grabParam :: B.ByteString
          -> M.Map B.ByteString [B.ByteString]
          -> ContEitherT m B.ByteString B.ByteString
grabParam key map = do
  vals <- withMessage (missing key) (M.lookup key map)
  val <- singletonList (tooMany key) vals
  return val

grabAndParseParam :: B.ByteString
                  -> M.Map B.ByteString [B.ByteString]
                  -> (B.ByteString -> B.ByteString -> ContEitherT m B.ByteString a)
                  -> ContEitherT m B.ByteString a
grabAndParseParam key map parser =
  grabParam key map >>= parser key
