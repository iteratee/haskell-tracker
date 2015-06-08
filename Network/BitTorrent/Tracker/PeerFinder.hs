module Network.BitTorrent.Tracker.PeerFinder 
  ( HashRecord(..)
  , ProtocolHashRecord(..)
  , RandomPeerList(..)
  , RandomPeerMap(..)
  , ReversePeerMap(..)
  , addPeer
  , emptyHashRecord
  , emptyPeerList
  , hasPeer
  , hasPeerId
  , getLeechers
  , getNPeers
  , getPeerCounts
  , getPeers
  , peerSize
  , removePeerId
  ) where

import Control.Concurrent.MVar
import Control.Monad.Random
import Data.Digest.SHA1
import Data.List
import Data.Tuple
import Data.Word
import Network.BitTorrent.Tracker.Announce
import qualified Data.Map as M

type RandomPeerMap = M.Map (Word32, Maybe PeerId) Peer
type ReversePeerMap = M.Map (Maybe PeerId) Word32

-- | A PeerList is a pair of maps (current and next) from (r, Maybe peerId) to a
-- Peer. When records are read from current, they are placed in next with r a
-- new random value. Maybe is included so that splitting can occur on a key
-- guaranteed not to be in the map. Also included is a reverse map to allow
-- efficient deletion.
data RandomPeerList = RandomPeerList {
    -- | The current set of peers, stored in random order.
    rplCurrent :: RandomPeerMap
    -- | The next set of peers, also in random order. When current is empty,
    -- it is replaced by next.
  , rplNext :: RandomPeerMap
    -- | Reverse mapping to allow for efficient deletion of peers from the list.
  , rplReverse :: ReversePeerMap
} deriving Show

-- | Retrieve at most n peers from a list.
getNPeers :: (MonadRandom m) =>
  -- | The number of requested peers
  Int ->
  -- | The peer list to take them from
  RandomPeerList ->
  -- | A monadic action that returns the requested peers and an updated
  -- RandomPeerList
  m ([Peer], RandomPeerList)
getNPeers count peerlist = do
  let currMap = rplCurrent peerlist
      nextMap = rplNext peerlist
      reverseMap = rplReverse peerlist
      got = M.size currMap
      count' = count - got
  if got >= count  -- If there are enough peers in currMap, use those.
    then do
      let (left, right) = splitMap count currMap
          result = M.elems left
      newAssocs <- mapM buildRandKey result
      let newMap = appendToMap nextMap newAssocs
          newReverse = updateReverseMap newAssocs reverseMap
          peerlist' =
            RandomPeerList { rplCurrent = right
                           , rplNext = newMap
                           , rplReverse = newReverse }
      return (result, peerlist')
    else  -- Not enough peers in currMap
      -- If there are fewer peers than requested, don't worry about
      -- randomizing, just return them all.
      if M.size nextMap <= count'
      then do
        let currList = M.elems currMap
            nextList = M.elems nextMap
            result = currList ++ nextList
        return (result, peerlist)
      else do
        -- There are enough peers total, they're just in nextMap.
        -- Take some of them.
        let initialResult = M.elems currMap
            (left, right) = splitMap count' nextMap
            result = initialResult ++ M.elems left
        newAssocs <- mapM buildRandKey result
        let newMap = M.fromList newAssocs
            newReverse = updateReverseMap newAssocs reverseMap
            peerlist' =
              RandomPeerList { rplCurrent = right
                             , rplNext = newMap
                             , rplReverse = newReverse }
        return (result, peerlist')

  where
    splitMap :: (Ord k2) =>
        Int ->
        M.Map (Word32, Maybe k2) v ->
        (M.Map (Word32, Maybe k2) v, M.Map (Word32, Maybe k2) v)
    splitMap count m =
      if M.size m > count
      then
        let ((splitKey, _), _) = M.elemAt count m
        in  M.split (splitKey, Nothing) m
      else
        (m, M.empty)
    appendToMap :: (Ord k) => M.Map k v -> [(k,v)] -> M.Map k v
    appendToMap = foldl' (flip $ uncurry M.insert)
    updateReverseMap :: [((Word32, Maybe PeerId), Peer)] -> ReversePeerMap -> ReversePeerMap
    updateReverseMap assocs reverseMap =
      let reverseKeys = map (swap . fst) assocs
      in foldl' (flip $ uncurry M.insert) reverseMap reverseKeys
    buildRandKey :: (MonadRandom m) => Peer -> m ((Word32, Maybe PeerId), Peer)
    buildRandKey peer = do
      randKey <- getRandomR (minBound, maxBound)
      return ((randKey, Just (peerId peer)), peer)

-- | Add a peer to a RandomPeerList
addPeer :: (MonadRandom m) => Peer -> RandomPeerList -> m RandomPeerList
addPeer p rpl = do
  let nextMap = rplNext rpl
      reverseMap = rplReverse rpl
      k = Just (peerId p)
  case M.lookup k reverseMap of
    Nothing -> do
      r <- getRandom
      return (rpl { rplNext = M.insert (r, Just (peerId p)) p nextMap,
                    rplReverse = M.insert (Just (peerId p)) r reverseMap } )
    Just _ -> return rpl

-- | Check for a peer in a RandomPeerList
hasPeer :: Peer -> RandomPeerList -> Bool
hasPeer p = hasPeerId (peerId p)

-- | Check for a peerId in a RandomPeerList
-- Checks the reverseMap for simplicity
hasPeerId :: PeerId -> RandomPeerList -> Bool
hasPeerId pid rpl =
  let reverseMap = rplReverse rpl
      k = Just pid
  in M.member k reverseMap

-- Remove a peer from a RandomPeerList
removePeerId :: PeerId -> RandomPeerList -> RandomPeerList
removePeerId pid rpl =
  let currMap = rplCurrent rpl
      nextMap = rplNext rpl
      reverseMap = rplReverse rpl
      k = Just pid
  in case M.lookup k reverseMap of
    Nothing -> rpl
    Just r ->
      RandomPeerList {
          rplCurrent = M.delete (r, k) currMap
        , rplNext = M.delete (r, k) nextMap
        , rplReverse = M.delete k reverseMap }

-- | An empty peer list, used when a new hash is added.
emptyPeerList :: RandomPeerList
emptyPeerList = RandomPeerList {
    rplCurrent = M.empty
  , rplNext = M.empty
  , rplReverse = M.empty
}

-- | Size of a peer list. Used in tests
peerSize :: RandomPeerList -> Int
peerSize = M.size . rplReverse

-- | Info used to serve requests about a single hash
-- Protocols are split because it's not safe to trust a client's word about its
-- Ipv(4/6) address if its connecting over Ipv(6/4). It could lead to an
-- amplification attack.
data HashRecord = HashRecord {
    hrInet4 :: MVar ProtocolHashRecord
  , hrInet6 :: MVar ProtocolHashRecord
}

-- | Empty hash record. Used when a new hash is added.
emptyHashRecord :: IO HashRecord
emptyHashRecord = do
  phr4 <- newMVar emptyProtocolHashRecord
  phr6 <- newMVar emptyProtocolHashRecord
  return HashRecord {
      hrInet4 = phr4
    , hrInet6 = phr6
  }

-- | ProtocolHashRecord
-- A RandomPeerList of Seeders and Leechers and a count of completed downloads.
-- Protocols are separated for security reasons.
data ProtocolHashRecord = ProtocolHashRecord {
    -- | A RandomPeerList of seeders
    phrSeeders :: ! RandomPeerList
    -- | A RandomPeerList of Leechers
  , phrLeechers :: ! RandomPeerList
    -- | Complete Count
  , phrCompleteCount :: ! Word32
}

-- | An empty ProtoclHashRecord. Used  when a new hash is added.
emptyProtocolHashRecord :: ProtocolHashRecord
emptyProtocolHashRecord = ProtocolHashRecord {
    phrSeeders = emptyPeerList
  , phrLeechers = emptyPeerList
  , phrCompleteCount = 0
}

-- | Get n peers while IO-updating the RandomPeerLists in the ProtocolHashRecord
getPeers :: Int -> MVar ProtocolHashRecord -> IO [Peer]
getPeers count mphr =
  let nSeeders = (count + 9) `div` 10 -- 10% seeders if we can get it.
      nLeechers = (count - nSeeders)
  in getPeersGen nSeeders nLeechers mphr

-- | Get only leechers. Useful for seeders
getLeechers :: Int -> MVar ProtocolHashRecord -> IO [Peer]
getLeechers =
  getPeersGen 0
  
-- | Get n seeders and m leechers, where seeders will be replaced by leechers
-- if necessary, and vice versa
getPeersGen :: Int -> Int -> MVar ProtocolHashRecord -> IO [Peer]
getPeersGen nSeeders nLeechers mphr = do
  g1<- newStdGen
  g2 <- newStdGen
  phr <- takeMVar mphr
  let count = nSeeders + nLeechers
      seeders = phrSeeders phr
      leechers = phrLeechers phr
      seedersAvail = peerSize seeders
      leechersAvail = peerSize leechers
      (nSeeders', nLeechers') =
        if seedersAvail + leechersAvail <= count
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

-- | Get a count seeders and leechers in a ProtocolHashRecord
getPeerCounts :: MVar ProtocolHashRecord -> IO (Word32, Word32, Word32)
getPeerCounts mphr = do
  phr <- readMVar mphr
  let seeders = phrSeeders phr
      leechers = phrLeechers phr
      completed = phrCompleteCount phr
  return (fromIntegral $ peerSize seeders,
          fromIntegral $ peerSize leechers, completed)
