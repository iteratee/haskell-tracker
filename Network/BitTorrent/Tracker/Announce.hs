{-# LANGUAGE OverloadedStrings #-}
module Network.BitTorrent.Tracker.Announce 
  ( AnnounceRequest(..)
  , AnnounceResponse(..)
  , Event(..)
  , InfoHash
  , Peer(..)
  , PeerId
  , ScrapeRequest(..)
  , ScrapeResponse(..)
  , bencodeResponse4
  , bencodeResponse6
  , bencodeScrapes
  , emptyScrapeResponse
  , isRfc1918
  ) where

import Control.Monad
import Data.Binary
import Data.Binary.Builder
import Data.Binary.Put (runPut)
import Data.Bits
import Data.Digest.SHA1
import Data.Monoid
import Data.Torrent.Bencode
import Data.Word
import Network.Socket
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8

-- | Word160 is order by ordering the individual Word32's
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

-- | Read and write Big-Endian data by writing the individual Word32's
instance Binary Word160 where
  get = liftM5 Word160 get get get get get
  put (Word160 a b c d e) = do
    put a; put b; put c; put d; put e

-- | An Info Hash is just Sha1, and is opaque as far as the tracker is
-- concerned.
type InfoHash = Word160
-- | PeerId's often contain theh client name in thy bytes, but are opaque to
-- the tracker.
type PeerId = Word160

-- | An Announce Request from a client
data AnnounceRequest = AnnounceRequest {
    -- | The hash being announce
    anInfoHash :: ! InfoHash
    -- | The peer doing the announcement
  , anPeer :: ! Peer
    -- | Bytes uploaded for this torrent
  , anUploaded :: ! Word64
    -- | Bytes downloaded on this torrent
  , anDownloaded :: ! Word64
    -- | Bytes remaining to download the torrent. 0 means the peer is a seeder.
  , anLeft :: ! Word64
    -- | The event that triggered the request. Ongoing status reports do not
    -- include it.
  , anEvent :: ! (Maybe Event)
    -- | An optional count of requested peers
  , anWant :: ! (Maybe Word32)
} deriving (Eq, Ord, Show)

-- | Which event triggered the request.
-- Started (or missing) adds the peer to the list.
-- Completed moves the peer from the leechers list to the seeders list.
-- Stopped removes the peer from the list.
data Event = Started | Completed | Stopped
  deriving (Eq, Ord, Show)

-- | A response to an Announce Request
data AnnounceResponse =
    -- | The failure case is just a failure message.
    Failure ! B.ByteString
    -- | A successful response is a list of peers.
  | PeerList {
        -- | Interval the client should wait until requesting again.
        -- Currently not enforced.
        plInterval :: ! Word32
        -- | Number of seeders in the list. Optional if there are none.
      , plSeeders :: ! (Maybe Word32)
        -- | Number of leechers in the list. Optional if there are none.
      , plLeechers :: ! (Maybe Word32)
        -- | The actual list of peers
      , plPeers :: [Peer]
    } deriving (Eq, Ord, Show)

-- | A Scrape Request is just the hash that the user is interested in.
type ScrapeRequest = InfoHash

-- | A response to a Scrape Request
data ScrapeResponse = ScrapeResponse {
    -- | The number of total seeders on the torrent.
    srSeeders :: ! Word32
    -- | The number of total completed downloads for the torrent.
  , srCompletions :: ! Word32
    -- | The number of total leechers on the torrent.
  , srLeechers :: ! Word32
} deriving (Eq, Ord, Show)

-- | An empty scrape response. Allows modifying only relevant values.
emptyScrapeResponse :: ScrapeResponse
emptyScrapeResponse = ScrapeResponse {
  srSeeders = 0, srCompletions = 0, srLeechers = 0
}

-- | A peer has a unique id, and a way to connect to them.
data Peer = Peer {
    -- | The unique peer id. Opaque
    peerId :: ! PeerId
    -- | A socket address, allowing us to connect to the peer.
  , peerAddr :: ! SockAddr
} deriving (Eq, Ord, Show)

-- | Encode an announce response as B-encoded data. Encoded as a Catamorphism,
-- as that allows the code to skip the intermediate representation.
-- This is the ipv4 version, as for security reasons we don't cross-pollinate
-- ipv4 and ipv6 peers.
bencodeResponse4 :: (BencodeC b) => AnnounceResponse -> b
bencodeResponse4 (Failure msg) =
  beDict $ beDictCataCons "failure" (beString msg) beDictCataNil
bencodeResponse4 (PeerList ival _ _ peers) =
  beDict $ beDictCataCons "interval" (beInt $ fromIntegral ival) $
    beDictCataCons "peers" (bencodePeers4 peers) beDictCataNil

-- | Encode an announce response as B-encoded data. Encoded as a Catamorphism,
-- as that allows the code to skip the intermediate representation.
-- This is the ipv6 version, as for security reasons we don't cross-pollinate
-- ipv4 and ipv6 peers.
bencodeResponse6 :: (BencodeC b) => AnnounceResponse -> b
bencodeResponse6 (Failure msg) =
  beDict $ beDictCataCons "failure" (beString msg) beDictCataNil
bencodeResponse6 (PeerList ival _ _ peers) =
  beDict $ beDictCataCons "interval" (beInt $ fromIntegral ival) $
    beDictCataCons "peers6" (bencodePeers6 peers) beDictCataNil

-- | Pack ipv4 peers into a byteString, and then a beString
bencodePeers4 :: (BencodeC b) => [Peer] -> b
bencodePeers4 peers = beString . BL.toStrict . toLazyByteString $
  foldr bencodePeer4 mempty peers

-- | Pack ipv6 peers into a byteString, and then a beString
bencodePeers6 :: (BencodeC b) => [Peer] -> b
bencodePeers6 peers = beString . BL.toStrict . toLazyByteString $
  foldr bencodePeer6 mempty peers

-- | Pack a single ivp4 peer as bytes.
bencodePeer4 :: Peer -> Builder -> Builder
bencodePeer4 p bldr =
  case peerAddr p of
    -- HostAddress is stored in network byte order. So it needs to be written
    -- out as stored.
    SockAddrInet p h -> putWord32host h <> putWord16be (fromIntegral p) <> bldr
    _ -> bldr

-- | Pack a single ivp6 peer as bytes.
bencodePeer6 :: Peer -> Builder -> Builder
bencodePeer6 p bldr =  
  case peerAddr p of
    SockAddrInet6 p _ (h0,h1,h2,h3) _ ->
      putWord32host h0 <> putWord32host h1 <> putWord32host h2
      <> putWord32host h3 <> putWord16be (fromIntegral p) <> bldr
    _ -> bldr

-- | Encode a ScrapeResponse as a B-encode catamorphism
bencodeScrape :: (BencodeC b) => ScrapeResponse -> b
bencodeScrape sr =
  beDict $ beDictCataCons "complete" (beInt (fromIntegral $ srSeeders sr)) $
           beDictCataCons "downloaded"
             (beInt (fromIntegral $ srCompletions sr)) $
           beDictCataCons "incomplete" (beInt (fromIntegral $ srLeechers sr))
             beDictCataNil

-- | Encode a list of scrapes in the required B-encode format.
-- Written as a catamorphism.
bencodeScrapes :: (BencodeC b) => [(InfoHash, ScrapeResponse)] -> b
bencodeScrapes srs =
  beDict $ beDictCataCons "files" innerDict beDictCataNil
  where
    innerDict = beDict $
        foldr (uncurry beDictCataCons . packHash) beDictCataNil srs
    packHash (h,x) = (BL.toStrict . runPut . put $ h, bencodeScrape x)

-- | Test to see if the address is RFC 1918. Used to see if we should trust the
-- users reported ip address.
isRfc1918 :: Word32 -> Bool
isRfc1918 addr =
  addr .&. 0xff000000 == 0x0a000000 ||  -- 10.0.0.0
  addr .&. 0xfff00000 == 0xac100000 ||
  addr .&. 0xffff0000 == 0xc0a80000
