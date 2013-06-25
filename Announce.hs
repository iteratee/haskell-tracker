module Announce where

import Bencode
import Control.Monad
import Data.Binary
import Data.Binary.Builder
import Data.Binary.Put (runPut)
import Data.Bits
import Data.Digest.SHA1
import Data.Monoid
import Data.Word
import Network.Socket
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8

instance Ord Word160 where
  compare (Word160 a1 b1 c1 d1 e1) (Word160 a2 b2 c2 d2 e2) =
    case compare a1 a2 of
      EQ ->
        case compare b1 b2 of
          EQ ->
            case compare c1 c2 of
              EQ ->
                case compare d1 d2 of
                  EQ -> compare e1 e2
                  x -> x
              x -> x
          x -> x
      x -> x

instance Binary Word160 where
  get = liftM5 Word160 get get get get get
  put (Word160 a b c d e) = do
    put a; put b; put c; put d; put e

type InfoHash = Word160
type PeerId = Word160

data Announce = Announce { 
    anInfoHash :: ! InfoHash
  , anPeer :: ! Peer
  , anUploaded :: ! Word64
  , anDownloaded :: ! Word64
  , anLeft :: ! Word64
  , anEvent :: ! (Maybe Event)
  , anWant :: ! (Maybe Word32)
} deriving (Eq, Ord, Show)


type ScrapeRequest = InfoHash
data ScrapeResponse = ScrapeResponse {
    srSeeders :: ! Word32
  , srCompletions :: ! Word32
  , srLeechers :: ! Word32
} deriving (Eq, Ord, Show)

emptyScrapeResponse :: ScrapeResponse
emptyScrapeResponse = ScrapeResponse {
  srSeeders = 0, srCompletions = 0, srLeechers = 0
}

data Event = Started | Completed | Stopped
  deriving (Eq, Ord, Show)

data Peer = Peer {
    peerId :: ! PeerId
  , peerAddr :: ! SockAddr
} deriving (Eq, Ord, Show)

data AnnounceResponse =
    Failure ! B.ByteString
  | PeerList {
        plInterval :: ! Word32
      , plSeeders :: ! (Maybe Word32)
      , plLeechers :: ! (Maybe Word32)
      , plPeers :: [Peer]
    } deriving (Eq, Ord, Show)

bencodeResponse4 :: (BencodeC b) => AnnounceResponse -> b
bencodeResponse4 (Failure msg) =
  beDict $ beDictAlgCons (B8.pack "failure") (beString msg) $ beDictAlgNil
bencodeResponse4 (PeerList ival _ _ peers) =
  beDict $ beDictAlgCons (B8.pack "interval") (beInt $ fromIntegral ival) $
    beDictAlgCons (B8.pack "peers") (bencodePeers4 peers) $ beDictAlgNil
bencodeResponse6 :: (BencodeC b) => AnnounceResponse -> b
bencodeResponse6 (Failure msg) =
  beDict $ beDictAlgCons (B8.pack "failure") (beString msg) $ beDictAlgNil
bencodeResponse6 (PeerList ival _ _ peers) =
  beDict $ beDictAlgCons (B8.pack "interval") (beInt $ fromIntegral ival) $
    beDictAlgCons (B8.pack "peers6") (bencodePeers6 peers) $ beDictAlgNil

bencodePeers4 :: (BencodeC b) => [Peer] -> b
bencodePeers4 peers = beString . BL.toStrict . toLazyByteString $
  foldr bencodePeer4 mempty peers
bencodePeers6 :: (BencodeC b) => [Peer] -> b
bencodePeers6 peers = beString . BL.toStrict . toLazyByteString $
  foldr bencodePeer6 mempty peers

bencodePeer4 :: Peer -> Builder -> Builder
bencodePeer4 p bldr =
  case (peerAddr p) of
    SockAddrInet (PortNum p) h -> putWord32be h <> putWord16be p <> bldr
    _ -> bldr
bencodePeer6 :: Peer -> Builder -> Builder
bencodePeer6 p bldr =  
  case (peerAddr p) of
    SockAddrInet6 (PortNum p) _ (h0,h1,h2,h3) _ ->
      putWord32be h0 <> putWord32be h1 <> putWord32be h2 <> putWord32be h3 <> putWord16be p <> bldr
    _ -> bldr

bencodeScrape :: (BencodeC b) => ScrapeResponse -> b
bencodeScrape sr =
  beDict $ beDictAlgCons (B8.pack "complete") (beInt (fromIntegral $ srSeeders sr)) $
           beDictAlgCons (B8.pack "downloaded") (beInt (fromIntegral $ srCompletions sr)) $
           beDictAlgCons (B8.pack "incomplete") (beInt (fromIntegral $ srLeechers sr)) $ 
           beDictAlgNil

bencodeScrapes :: (BencodeC b) => [(InfoHash, ScrapeResponse)] -> b
bencodeScrapes srs =
  beDict $ beDictAlgCons (B8.pack "files") innerDict $ beDictAlgNil
  where
    innerDict = beDict $ foldr (uncurry beDictAlgCons) beDictAlgNil $ map packHash srs
    packHash (h,x) = (BL.toStrict . runPut . put $ h, bencodeScrape x)

isRfc1918 :: Word32 -> Bool
isRfc1918 addr =
  addr .&. 0xff000000 == 0x0a000000 ||  -- 10.0.0.0
  addr .&. 0xfff00000 == 0xac100000 ||
  addr .&. 0xffff0000 == 0xc0a80000
