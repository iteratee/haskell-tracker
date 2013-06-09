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

instance Arbitrary Word160 where
  arbitrary = do
    [a,b,c,d,e] <- replicateM 5 arbitrary
    return $ Word160 a b c d e

  shrink (Word160 a b c d e) = do
    [a', b', c', d', e'] <- mapM shrink [a, b, c, d, e]
    return $ Word160 a b c d e

instance Arbitrary PortNumber where
  arbitrary = liftM PortNum arbitrary
  shrink (PortNum p) = map PortNum $ shrink p

instance Arbitrary SockAddr where
  arbitrary =
    oneof [
      liftM2 SockAddrInet arbitrary arbitrary,
      liftM4 SockAddrInet6 arbitrary arbitrary arbitrary arbitrary
    ]

  shrink (SockAddrInet port addr) = do
    port' <- shrink port
    addr' <- shrink addr
    return $ SockAddrInet port' addr'

  shrink (SockAddrInet6 flow port scope addr) = do
    flow' <- shrink flow
    port' <- shrink port
    scope' <- shrink scope
    addr' <- shrink addr
    return $ SockAddrInet6 flow port' scope addr'

instance Arbitrary Peer where
  arbitrary = liftM2 Peer arbitrary arbitrary
  shrink peer = do
    peerId' <- shrink (peerId peer)
    peerAddr' <- shrink (peerAddr peer)
    return Peer { peerId = peerId', peerAddr = peerAddr' }

data Announce = Announce { 
    anInfoHash :: ! Word160
  , anPeerId :: ! Word160
  , anPeerAddr :: ! SockAddr
  , anUploaded :: ! Word64
  , anDownloaded :: ! Word64
  , anLeft :: ! Word64
  , anEvent :: ! (Maybe Event)
} deriving (Eq, Ord, Show)


data Event = Started | Completed | Stopped
  deriving (Eq, Ord, Show)

data Peer = Peer {
    peerId :: ! Word160
  , peerAddr :: ! SockAddr
} deriving (Eq, Ord, Show)

data AnnounceResponse =
    Failure ! B.ByteString
  | PeerList {
        plInterval :: ! Word32
      , plPeers :: [Peer]
    } deriving (Eq, Ord, Show)

-- Data for testing
peers = map (\n -> Peer { peerId = (Word160 0 0 0 0 n)
                        , peerAddr = SockAddrInet 80 0 }) [1..10]
