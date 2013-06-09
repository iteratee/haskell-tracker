{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}
module PeerFinder where

import Announce
import Control.Monad.Random
import Data.Digest.SHA1
import Data.List
import Data.Word
import Debug.Trace
import qualified Data.Map as M

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#endif

-- A PeerList is a list (current) and a map (next) from (r, peerId) to a Peer
-- When records are read from current, they are placed in next with r a new
-- random value
type RandomPeerMap = M.Map (Word32, Word160) Peer

data RandomPeerList = RandomPeerList {
    rplCurrent :: [Peer]
  , rplNext :: RandomPeerMap
} deriving Show

splitAndCount :: Int -> [a] -> (Int, [a], [a])

#ifdef __GLASGOW_HASKELL__
splitAndCount (I# n#) as
  | n# <# 0# = (0, [], as)
  | otherwise =
    ((I# l#), h, t)
  where
    (# l#, h, t #) = splitAndCount# n# as
    splitAndCount# :: Int# -> [a] -> (# Int#, [a], [a] #)
    splitAndCount# 0# xs     = (# 0#, [], xs #)
    splitAndCount# _  xs@[]  = (# 0#, xs, xs #)
    splitAndCount# m# (x:xs) = (# l'# +# 1#, x:h, t #)
      where
        (# l'#, h, t #) = splitAndCount# (m# -# 1#) xs
#else
splitAndCount n as
  | n < 0 = (0, [], as)
  (len, h, t)
  where
    (len, h, t) = splitAndCount' n xs
    splitAndCount' 0 xs     = (0, [], xs)
    splitAndCount# _ xs@[]  = (0, xs, xs)
    splitAndCount# m (x:xs) = len' `seq`
      (len' + 1, x:h, t)
      where
        (len', h, t) = splitAndCount' (m - 1) xs
#endif

getNPeers :: (MonadRandom m) => Int -> 
                                RandomPeerList ->
                                m ([Peer], RandomPeerList)
getNPeers count peerlist = do
  let (got, result, rest) = splitAndCount count (rplCurrent peerlist)
      nextMap = (rplNext peerlist)
      count' = count - got
  if got == count
    then do
      newAssocs <- mapM (buildRandKey 0) result
      let newMap = foldl' (flip $ uncurry M.insert) nextMap newAssocs
          peerlist' = RandomPeerList { rplCurrent = rest, rplNext = newMap }
      return (result, peerlist')
    else do
      if M.size nextMap <= count'
        then do
          let nextList = M.elems nextMap
              result' = result ++ nextList
          newAssocs <- mapM (buildRandKey 0) result'
          let newCurrent = M.elems $ M.fromList newAssocs
              peerlist' =
                RandomPeerList { rplCurrent = newCurrent, rplNext = M.empty }
          return (result', peerlist')
        else do
          let ((insertKey_, _), _) = M.elemAt count' nextMap
              insertKey = (min (maxBound - 1) insertKey_) + 1
          trace ("insertKey is: " ++ (show insertKey)) $ return ()
          lastAssocs <- mapM (buildRandKey insertKey) result
          let fixedMap = foldl' (flip $ uncurry M.insert) nextMap lastAssocs
              current' = M.elems fixedMap
              (remaining, rest') = splitAt count' current'
              result' = result ++ remaining
          newAssocs <- mapM (buildRandKey 0) remaining
          let newMap = M.fromList newAssocs
              peerlist' =
                RandomPeerList { rplCurrent = rest', rplNext = newMap }
          return (result', peerlist')
  where
    buildRandKey :: (MonadRandom m) => Word32 -> Peer -> m ((Word32, Word160), Peer)
    buildRandKey lower peer = do
      randKey <- getRandomR (lower, maxBound)
      return ((randKey, (peerId peer)), peer)

addPeer :: (MonadRandom m =>) Peer -> RandomPeerList -> m RandomPeerList
addPeer p rpl = do
  let nextMap = (rplNext rpl)
  r <- getRandom
  return (rpl { rplNext = M.insert ((r, (peerId p)), p) nextMap } )

removePeer p rpl = do
  let current = (rplCurrent rpl)
      nextMap = (rplNext rpl)
  return RandomPeerList {
      rplCurrent = filter (/=p) current
    , rplNext = M.filter (/=p) nextMap }
      
data HashRecord = HashRecord {
    hrSeeders4 :: RandomPeerList
  , hrSeeders6 :: RandomPeerList
  , hrLeechers4 :: RandomPeerList
  , hrLeechers6 :: RandomPeerList
}
