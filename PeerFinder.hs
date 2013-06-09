{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}
module PeerFinder where

import Announce
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Random
import Data.Digest.SHA1
import Data.List
import Data.Tuple
import Data.Word
import Debug.Trace
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import qualified Data.Map as M

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#endif

-- A PeerList is a pair of maps (current and next) from (r, Maybe peerId) to a
-- Peer. When records are read from current, they are placed in next with r a
-- new random value. Maybe is included so that splitting can occur on a key
-- guaranteed not to be in the map. Also included is a reverse map to allow
-- efficient deletion.
type RandomPeerMap = M.Map (Word32, Maybe Word160) Peer
type ReversePeerMap = M.Map (Maybe Word160) Word32

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
          newAssocs <- mapM (buildRandKey 0) result
          let newCurrent = M.fromList newAssocs
              newReverse = updateReverseMap newAssocs reverseMap
              peerlist' =
                RandomPeerList { rplCurrent = newCurrent
                               , rplNext = M.empty
                               , rplReverse = newReverse }
          return (result, peerlist')
        else do
          let ((insertKey_, _), _) = M.elemAt (count' - 1) nextMap
              insertKey = (min (maxBound - 1) insertKey_) + 1
              initialResult = M.elems currMap
          trace ("insertKey is: " ++ (show insertKey)) $ return ()
          lastAssocs <- mapM (buildRandKey insertKey) initialResult
          let fixedMap = foldl' (flip $ uncurry M.insert) nextMap lastAssocs
              newReverse = updateReverseMap lastAssocs reverseMap
          (tail, peerlist') <-
              getNPeers count' RandomPeerList {  rplCurrent = fixedMap
                                               , rplNext = M.empty
                                               , rplReverse = newReverse }
          return (initialResult ++ tail, peerlist')
  where
    updateReverseMap :: [((Word32, Maybe Word160), Peer)] -> ReversePeerMap -> ReversePeerMap
    updateReverseMap assocs reverseMap =
      let reverseKeys = map (swap . fst) assocs
      in foldl' (flip $ uncurry M.insert) reverseMap reverseKeys
    buildRandKey :: (MonadRandom m) => Word32 -> Peer -> m ((Word32, Maybe Word160), Peer)
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
hasPeer p rpl =
  let reverseMap = (rplReverse rpl)
      k = (Just (peerId p))
  in M.member k reverseMap

removePeer :: Peer -> RandomPeerList -> RandomPeerList
removePeer p rpl = do
  let currMap = (rplCurrent rpl)
      nextMap = (rplNext rpl)
      reverseMap = (rplReverse rpl)
      k = (Just (peerId p))
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


-- Properties

instance Arbitrary RandomPeerList where
  arbitrary = do
    count <- frequency [ (1, return 0), (3, return 10)
                       , (3, return 100), (3, return 1000) ]
    peers <- replicateM count arbitrary
    gen <- arbitrary :: Gen StdGen
    return $ evalRand (foldM (flip addPeer) emptyPeerList peers) gen

  shrink rpl =
    let rSize = peerSize rpl
    in if rSize == 0
      then []
      else do
        x <- [0..rSize - 1]
        let (k, r) = M.elemAt x (rplReverse rpl)
        return RandomPeerList {
            rplCurrent = M.delete (r, k) (rplCurrent rpl)
          , rplNext = M.delete (r, k) (rplNext rpl)
          , rplReverse = M.delete k (rplReverse rpl) }
    

instance Arbitrary StdGen where
  arbitrary = liftM mkStdGen arbitrary

prop_size_increases :: Peer -> RandomPeerList -> StdGen -> Bool
prop_size_increases p rpl gen =
  if hasPeer p rpl
    then peerSize rpl == peerSize (evalRand (addPeer p rpl) gen)
    else 1 + (peerSize rpl) == peerSize (evalRand (addPeer p rpl) gen)

prop_size_decreases :: Peer -> RandomPeerList -> Bool
prop_size_decreases p rpl =
  if hasPeer p rpl
    then (peerSize rpl) - 1 == peerSize (removePeer p rpl)
    else (peerSize rpl) == peerSize (removePeer p rpl)

prop_sum_sizes :: RandomPeerList -> Bool
prop_sum_sizes rpl =
  (M.size (rplCurrent rpl)) + (M.size (rplNext rpl)) ==
  peerSize rpl
      
data HashRecord = HashRecord {
    hrSeeders4 :: TVar RandomPeerList
  , hrSeeders6 :: TVar RandomPeerList
  , hrLeechers4 :: TVar RandomPeerList
  , hrLeechers6 :: TVar RandomPeerList
}

getPeers4 :: Int -> HashRecord -> IO [Peer]
getPeers4 count hr =
  getPeersGen count (hrSeeders4 hr) (hrLeechers4 hr)

getPeers6 :: Int -> HashRecord -> IO [Peer]
getPeers6 count hr =
  getPeersGen count (hrSeeders6 hr) (hrLeechers6 hr)

getPeersGen :: Int -> TVar RandomPeerList -> TVar RandomPeerList -> IO [Peer]
getPeersGen count seedersT leechersT = do
  g1<- newStdGen
  g2 <- newStdGen
  let nSeeders = (count + 9) `div` 10 -- 10% seeders if we can get it.
      nLeechers = (count - nSeeders)
  atomically $ do
    seeders <- readTVar seedersT
    let seedersAvail = peerSize seeders
    leechers <- readTVar leechersT
    let leechersAvail = peerSize leechers
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
    writeTVar seedersT seeders'
    writeTVar leechersT leechers'
    return (seederList ++ leecherList)


