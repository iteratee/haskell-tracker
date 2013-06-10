module Announce where

import Control.Monad
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
      , plPeers :: [Peer]
    } deriving (Eq, Ord, Show)
