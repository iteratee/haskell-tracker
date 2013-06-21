module PeerFinder where

import Announce
import Control.Concurrent.MVar
import Control.Monad.Random
import Data.Digest.SHA1
import Data.List
import Data.Tuple
import Data.Word
import qualified Data.Map as M

-- A PeerList is a pair of maps (current and next) from (r, Maybe peerId) to a
-- Peer. When records are read from current, they are placed in next with r a
-- new random value. Maybe is included so that splitting can occur on a key
-- guaranteed not to be in the map. Also included is a reverse map to allow
-- efficient deletion.
type RandomPeerMap = M.Map (Word32, Maybe PeerId) Peer
type ReversePeerMap = M.Map (Maybe PeerId) Word32

data RandomPeerList = RandomPeerList {
    rplCurrent :: RandomPeerMap
  , rplNext :: RandomPeerMap
  , rplReverse :: ReversePeerMap
} deriving Show

getNPeers :: (MonadRandom m) => Int -> 
                                RandomPeerList ->
                                m ([Peer], RandomPeerList)
getNPeers count peerlist = do
  let currMap = (rplCurrent peerlist)
      nextMap = (rplNext peerlist)
      reverseMap = (rplReverse peerlist)
      got = M.size (currMap)
      count' = count - got
  if got >= count
    then do
      let ((splitKey, _), _) = M.elemAt count currMap
          (left, right) =
            if (got > count)
              then M.split (splitKey, Nothing) currMap
              else (currMap, M.empty)
          result = M.elems left
      newAssocs <- mapM (buildRandKey 0) result
      let newMap = foldl' (flip $ uncurry M.insert) nextMap newAssocs
          newReverse = updateReverseMap newAssocs reverseMap
          peerlist' =
            RandomPeerList { rplCurrent = right
                           , rplNext = newMap
                           , rplReverse = newReverse }
      return (result, peerlist')
    else do
      if M.size nextMap <= count'
        then do
          let currList = M.elems currMap
              nextList = M.elems nextMap
              result = currList ++ nextList
          return (result, peerlist)
        else do
          let ((insertKey_, _), _) = M.elemAt (count' - 1) nextMap
              insertKey = (min (maxBound - 1) insertKey_) + 1
              initialResult = M.elems currMap
          lastAssocs <- mapM (buildRandKey insertKey) initialResult
          let fixedMap = foldl' (flip $ uncurry M.insert) nextMap lastAssocs
              newReverse = updateReverseMap lastAssocs reverseMap
          (tail, peerlist') <-
              getNPeers count' RandomPeerList {  rplCurrent = fixedMap
                                               , rplNext = M.empty
                                               , rplReverse = newReverse }
          return (initialResult ++ tail, peerlist')
  where
    updateReverseMap :: [((Word32, Maybe PeerId), Peer)] -> ReversePeerMap -> ReversePeerMap
    updateReverseMap assocs reverseMap =
      let reverseKeys = map (swap . fst) assocs
      in foldl' (flip $ uncurry M.insert) reverseMap reverseKeys
    buildRandKey :: (MonadRandom m) => Word32 -> Peer -> m ((Word32, Maybe PeerId), Peer)
    buildRandKey lower peer = do
      randKey <- getRandomR (lower, maxBound)
      return ((randKey, Just (peerId peer)), peer)

addPeer :: (MonadRandom m) => Peer -> RandomPeerList -> m RandomPeerList
addPeer p rpl = do
  let nextMap = (rplNext rpl)
      reverseMap = (rplReverse rpl)
      k = (Just (peerId p))
  case M.lookup k reverseMap of
    Nothing -> do
      r <- getRandom
      return (rpl { rplNext = M.insert (r, Just (peerId p)) p nextMap,
                    rplReverse = M.insert (Just (peerId p)) r reverseMap } )
    Just _ -> return rpl

hasPeer :: Peer -> RandomPeerList -> Bool
hasPeer p rpl = hasPeerId (peerId p) rpl

hasPeerId :: PeerId -> RandomPeerList -> Bool
hasPeerId pid rpl =
  let reverseMap = (rplReverse rpl)
      k = (Just pid)
  in M.member k reverseMap

removePeerId :: PeerId -> RandomPeerList -> RandomPeerList
removePeerId pid rpl = do
  let currMap = (rplCurrent rpl)
      nextMap = (rplNext rpl)
      reverseMap = (rplReverse rpl)
      k = (Just pid)
  case M.lookup k reverseMap of
    Nothing -> rpl
    Just r ->
      RandomPeerList {
          rplCurrent = M.delete (r, k) currMap
        , rplNext = M.delete (r, k) nextMap
        , rplReverse = M.delete k reverseMap }

emptyPeerList :: RandomPeerList
emptyPeerList = RandomPeerList {
    rplCurrent = M.empty
  , rplNext = M.empty
  , rplReverse = M.empty
}

peerSize :: RandomPeerList -> Int
peerSize = M.size . rplReverse

data HashRecord = HashRecord {
    hrInet4 :: MVar ProtocolHashRecord
  , hrInet6 :: MVar ProtocolHashRecord
}

emptyHashRecord :: IO HashRecord
emptyHashRecord = do
  phr4 <- newMVar emptyProtocolHashRecord
  phr6 <- newMVar emptyProtocolHashRecord
  return HashRecord {
      hrInet4 = phr4
    , hrInet6 = phr6
  }

data ProtocolHashRecord = ProtocolHashRecord {
    phrSeeders :: ! RandomPeerList
  , phrLeechers :: ! RandomPeerList
  , phrCompleteCount :: ! Word32
}

emptyProtocolHashRecord :: ProtocolHashRecord
emptyProtocolHashRecord = ProtocolHashRecord {
    phrSeeders = emptyPeerList
  , phrLeechers = emptyPeerList
  , phrCompleteCount = 0
}

getPeers :: Int -> MVar ProtocolHashRecord -> IO [Peer]
getPeers count mphr =
  let nSeeders = (count + 9) `div` 10 -- 10% seeders if we can get it.
      nLeechers = (count - nSeeders)
  in getPeersGen nSeeders nLeechers mphr

getLeechers :: Int -> MVar ProtocolHashRecord -> IO [Peer]
getLeechers count mphr =
  getPeersGen 0 count mphr
  
getPeersGen :: Int -> Int -> MVar ProtocolHashRecord -> IO [Peer]
getPeersGen nSeeders nLeechers mphr = do
  g1<- newStdGen
  g2 <- newStdGen
  phr <- takeMVar mphr
  let count = nSeeders + nLeechers
      seeders = (phrSeeders phr)
      leechers = (phrLeechers phr)
      seedersAvail = peerSize seeders
      leechersAvail = peerSize leechers
      (nSeeders', nLeechers') =
        if (seedersAvail + leechersAvail) <= count
          then (seedersAvail, leechersAvail)
          else
            let seedersFloor = min seedersAvail nSeeders
                leechersFloor = min leechersAvail nLeechers
                seedersBackfill = count - leechersAvail
                leechersBackfill = count - seedersAvail
            in (max seedersFloor seedersBackfill,
                max leechersFloor leechersBackfill)
  let (seederList, seeders') = evalRand (getNPeers nSeeders' seeders) g1
      (leecherList, leechers') = evalRand (getNPeers nLeechers' leechers) g2
  putMVar mphr $ phr {
      phrSeeders = seeders', phrLeechers = leechers' }
  return (seederList ++ leecherList)

getPeerCounts :: MVar ProtocolHashRecord -> IO (Word32, Word32, Word32)
getPeerCounts mphr = do
  phr <- readMVar mphr
  let seeders = (phrSeeders phr)
      leechers = (phrLeechers phr)
      completed = (phrCompleteCount phr)
  return (fromIntegral $ peerSize seeders,
          fromIntegral $ peerSize leechers, completed)
