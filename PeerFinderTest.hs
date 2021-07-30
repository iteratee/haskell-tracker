module Main where

import           Control.Monad
import           Control.Monad.Random
import           Data.Digest.SHA1
import           Data.List                             (nub)
import qualified Data.Map                              as M
import qualified Data.Set                              as S
import           Data.Word                             (Word16)
import           Network.BitTorrent.Tracker.Announce
import           Network.BitTorrent.Tracker.PeerFinder
import           Network.Socket
import           Test.Framework                        (defaultMain, testGroup)
import           Test.Framework.Providers.QuickCheck2  (testProperty)
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

main = defaultMain tests

tests =
  [ testGroup
      "AddGroup"
      [ testProperty "SizeIncreases" prop_size_increases
      , testProperty "SizeRemains" prop_add_size_same
      ]
  , testGroup
      "RemoveGroup"
      [ testProperty "SizeRemains" prop_remove_size_same
      , testProperty "SizeDecreases" prop_size_decreases
      ]
  , testGroup "Consistency" [testProperty "SizeSame" prop_sum_sizes]
  , testGroup
      "GetPeers"
      [ testProperty "GetCorrectNumber" prop_get_correct_peers
      , testProperty "NoDuplicates" prop_no_duplicate_peers
      , testProperty "ConsistentBeforAfter" prop_get_peers_consistent
      ]
  , testGroup
      "HasPeer"
      [ testProperty "AddPeerHas" prop_add_peer_has
      , testProperty "RemovePeerDoesn'tHave" prop_remove_peer_nohas
      , testProperty "HasAllPeers" prop_has_all_peers
      ]
  ]

prop_size_increases :: Peer -> RandomPeerList -> StdGen -> Bool
prop_size_increases p rpl gen =
  if hasPeer p rpl
    then peerSize rpl == peerSize (evalRand (addPeer p rpl) gen)
    else 1 + peerSize rpl == peerSize (evalRand (addPeer p rpl) gen)

prop_remove_size_same :: PeerId -> RandomPeerList -> Bool
prop_remove_size_same pid rpl =
  if hasPeerId pid rpl
    then peerSize rpl - 1 == peerSize (removePeerId pid rpl)
    else peerSize rpl == peerSize (removePeerId pid rpl)

prop_add_size_same :: RandomPeerList -> Gen Bool
prop_add_size_same rpl =
  if peerSize rpl == 0
    then return True
    else do
      mp <- choosePeer rpl
      case mp of
        Nothing -> return False
        Just p -> do
          g <- arbitrary :: Gen StdGen
          return $ peerSize rpl == peerSize (evalRand (addPeer p rpl) g)

prop_size_decreases :: RandomPeerList -> Gen Bool
prop_size_decreases rpl =
  if peerSize rpl == 0
    then return True
    else do
      mp <- choosePeer rpl
      case mp of
        Nothing -> return False
        Just p -> do
          g <- arbitrary :: Gen StdGen
          return $ peerSize rpl - 1 == peerSize (removePeerId (peerId p) rpl)

choosePeer :: RandomPeerList -> Gen (Maybe Peer)
choosePeer rpl = do
  n <- choose (0, peerSize rpl - 1)
  let (k, r) = M.elemAt n (rplReverse rpl)
      currMap = rplCurrent rpl
      nextMap = rplNext rpl
  return $ M.lookup (r, k) currMap `mplus` M.lookup (r, k) nextMap

prop_sum_sizes :: RandomPeerList -> Bool
prop_sum_sizes rpl =
  M.size (rplCurrent rpl) + M.size (rplNext rpl) == peerSize rpl

getSomePeers :: RandomPeerList -> Gen ([Peer], RandomPeerList, Int)
getSomePeers rpl = do
  count <- oneof [choose (0, peerSize rpl), return $ peerSize rpl + 1]
  gen <- arbitrary :: Gen StdGen
  let (peers, rpl') = evalRand (getNPeers count rpl) gen
  return (peers, rpl', count)

prop_get_correct_peers :: RandomPeerList -> Gen Bool
prop_get_correct_peers rpl = do
  (peers, _, count) <- getSomePeers rpl
  return $ length peers == min count (peerSize rpl)

prop_no_duplicate_peers :: RandomPeerList -> Gen Bool
prop_no_duplicate_peers rpl = do
  (peers, _, count) <- getSomePeers rpl
  return $ length peers == length (nub peers)

prop_get_peers_consistent :: RandomPeerList -> Gen Bool
prop_get_peers_consistent rpl = do
  (_, rpl', count) <- getSomePeers rpl
  return $ getAllPeerIds rpl == getAllPeerIds rpl'

getAllPeerIds rpl =
  let currSet = S.fromList $ map peerId $ M.elems (rplCurrent rpl)
      nextSet = S.fromList $ map peerId $ M.elems (rplNext rpl)
   in currSet `S.union` nextSet

prop_add_peer_has :: Peer -> RandomPeerList -> Gen Bool
prop_add_peer_has p rpl = do
  gen <- arbitrary :: Gen StdGen
  return $ hasPeer p (evalRand (addPeer p rpl) gen)

prop_remove_peer_nohas :: RandomPeerList -> Gen Bool
prop_remove_peer_nohas rpl =
  if peerSize rpl == 0
    then return True
    else do
      mp <- choosePeer rpl
      case mp of
        Nothing -> return False
        Just p  -> return $ not $ hasPeer p (removePeerId (peerId p) rpl)

prop_has_all_peers :: RandomPeerList -> Bool
prop_has_all_peers rpl =
  let currMap = rplCurrent rpl
      nextMap = rplNext rpl
   in all (`hasPeer` rpl) $ M.elems currMap ++ M.elems nextMap

instance Arbitrary Word160 where
  arbitrary = do
    [a, b, c, d, e] <- replicateM 5 arbitrary
    return $ Word160 a b c d e
  shrink (Word160 a b c d e) = do
    [a', b', c', d', e'] <- mapM shrink [a, b, c, d, e]
    return $ Word160 a b c d e

instance Arbitrary PortNumber where
  arbitrary = liftM (fromIntegral :: Word16 -> PortNumber) arbitrary
  shrink = return

instance Arbitrary SockAddr where
  arbitrary =
    oneof
      [ liftM2 SockAddrInet arbitrary arbitrary
      , liftM4 SockAddrInet6 arbitrary arbitrary arbitrary arbitrary
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
    return Peer {peerId = peerId', peerAddr = peerAddr'}

instance Arbitrary RandomPeerList where
  arbitrary = do
    count <-
      frequency
        [(1, return 0), (3, return 10), (3, return 100), (3, return 1000)]
    peers <- replicateM count arbitrary
    gen <- arbitrary :: Gen StdGen
    requestSize <- choose (0, count)
    let (rpl, gen') = runRand (foldM (flip addPeer) emptyPeerList peers) gen
        (_, rpl') = evalRand (getNPeers requestSize rpl) gen'
    return rpl'
  shrink rpl =
    let rSize = peerSize rpl
     in if rSize == 0
          then []
          else do
            x <- [0 .. rSize - 1]
            let (k, r) = M.elemAt x (rplReverse rpl)
            return
              RandomPeerList
                { rplCurrent = M.delete (r, k) (rplCurrent rpl)
                , rplNext = M.delete (r, k) (rplNext rpl)
                , rplReverse = M.delete k (rplReverse rpl)
                }

instance Arbitrary StdGen where
  arbitrary = liftM mkStdGen arbitrary
