module Announce where

import Control.Monad
import Data.Binary
import Data.Digest.SHA1
import Data.Word
import Network.Socket
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import qualified Data.ByteString as B

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

bencodePeers4 :: (BencodeC b) => AnnounceResponse -> b
bencodePeers4 (Failure msg) =
  beDict $ beDictAlgCons (B8.pack "failure") (beString msg) $ beDictAlgNil
bencodePeers4 (PeerList ival _ _ peers) =
  beDict $ beDictAlgCons (B8.pack "interval") (beInt $ fromIntegral ival) $
    beDictAlgCons (B8.pack "peers") packPeers4 $ beDictAlgNil
bencodePeers6 (PeerList ival _ _ peers) =
  beDict $ beDictAlgCons (B8.pack "interval") (beInt $ fromIntegral ival) $
    beDictAlgCons (B8.pack "peers6") packPeers6 $ beDictAlgNil

packPeers4 :: (BencodeC b) => [Peer] -> b
packPeers4 peers = beString . toStrict . toLazyByteString $
  foldr packPeer4 mempty peers
packPeers6 :: (BencodeC b) => [Peer] -> b
packPeers6 peers = beString . toStrict . toLazyByteString $
  foldr packPeer6 mempty peers

packPeer4 :: Peer -> Builder -> Builder
packPeer4 p bldr =
  case (peerAddr p) of
    SockAddrInet p h -> word32BE h <> word16LE p <> bldr
    _ -> bldr
packPeer6 :: Peer -> Builder -> Builder
packPeer6 p bldr =  
  case (peerAddr p) of
    SockAddrInet6 p _ (h0,h1,h2,h3) _ ->
      word32BE h0 <> word32BE h1 <> word32BE h2 <> word32BE h3 <>word16LE p <> bldr
    _ -> bldr
