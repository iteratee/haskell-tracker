module AnnounceServer where
import Announce
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.List (foldl')
import Data.Maybe
import Data.Time.Clock
import Data.Word
import Network.Socket
import PeerFinder
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as S

type ActivityRecord = M.Map PeerId UTCTime
type ActivityQueue = S.Set (UTCTime, Maybe PeerId)
data AnnounceState = AnnounceState {
    activeHashes :: MVar (M.Map InfoHash HashRecord)
  , peersLastSeen :: MVar (M.Map InfoHash (MVar ActivityRecord))
  , peerActivityQueue :: MVar (M.Map InfoHash (MVar ActivityQueue))
}

type Seconds = Word32
data AnnounceConfig = AnnounceConfig {
    ancInterval :: ! Seconds
  , ancMaxPeers :: ! Word32
  , ancDefaultPeers :: ! Word32
  , ancIdlePeerTimeout :: ! Seconds
}

defaultConfig :: AnnounceConfig
defaultConfig = AnnounceConfig {
    ancInterval = 120
  , ancMaxPeers = 50
  , ancDefaultPeers = 30
  , ancIdlePeerTimeout = 360
}

data AnnounceEnv = AnnounceEnv {
    anSt :: AnnounceState
  , anConf :: AnnounceConfig
}

type AnnounceT = ReaderT AnnounceEnv IO

getState :: AnnounceT AnnounceState
getState = asks anSt

getConf :: AnnounceT AnnounceConfig
getConf = asks anConf

pruneQueue :: AnnounceT ()
pruneQueue = do
  now <- liftIO $ getCurrentTime
  st <- getState
  hrMap <- liftIO $ takeMVar (peerActivityQueue st)
  lastSeen <- liftIO $ takeMVar (peersLastSeen st)
  queue <- liftIO $ takeMVar (peerActivityQueue st)
  liftIO $ putMVar (activHashes st) hrMap
  liftIO $ putMVar (peersLastSeen st) lastSeen
  liftIO $ putMVar (peerActivityQueue st) queue
  forM_ (M.assocs lastSeen) $ \(hash, hashActivityM) -> do
    let hashQueueM = queue M.! hash
    let hr = hrMap M.! hash
    pruneHashQueue now hashActivityM hashQueueM hr
  where
    pruneHashQueue :: UTCTime
                       -> MVar ActivityRecord
                       -> MVar ActivityQueue
                       -> MVar HashRecord
                       -> AnnounceT ()
    pruneHashQueue now hashActivityM hashQueueM hrM = do
      activity <- liftIO $ takeMVar hashActivityM
      queue <- liftIO $ takeMVar hashQueueM
      timeout <- liftM ancIdlePeerTimeout getConf
      let old_now = addUTCTime (fromIntegral $ negate timeout) now
          (old, queue') = S.split (old_now, Nothing) queue
          activity' = foldl' (flip (M.delete . fromJust . snd)) activity (S.elems old)
      liftIO $ putMVar hashActivityM activity'
      liftIO $ putMVar hashQueueM queue'
      forM [hrInet4 hr, hrInet6 hr] $ \phrM -> do
        phr <- takeMVar phrM
        putMVar phrM $ ProtocolHashRecord {
            phrSeeders = cleanUp old (phrSeeders phr)
          , phrLeechers = cleanUp old (phrLeechers phr)
        }
    cleanUp :: S.Set (UTCTime, Maybe PeerId) -> RandomPeerList -> RandomPeerList
    cleanUp old rpl =
      foldl' (flip (removePeerId . fromJust . snd)) rpl (S.elems old)

handleAnnounce :: Announce -> AnnounceT AnnounceResponse
handleAnnounce an = do
  let peer = anPeer an
      hash = anInfoHash an
  st <- getState
  hr <- liftIO $ getHashRecord st hash
  let phr = getProtocolHashRecord peer hr
      peerGetter =
        if (isSeeder an)
          then getLeechers
          else getPeers
  maxPeers <- liftM ancMaxPeers getConf
  defPeers <- liftM ancDefaultPeers getConf
  let peersWanted = fromMaybe defPeers (anWant an)
      peerCount = min maxPeers peersWanted
  peers <- liftIO $ peerGetter (fromIntegral peerCount) phr
  interval <- liftM ancInterval getConf
  addOrRemovePeer an
  return PeerList { plInterval = interval, plPeers = peers }

updateActivity :: AnnounceState -> InfoHash -> Peer -> IO ()
updateActivity st hash peer = do
  lastSeenMap <- takeMVar (peersLastSeen st)
  queueMap <- takeMVar (peerActivityQueue st)
  case M.lookup hash lastSeenMap of
    Nothing -> do
      newLastSeen <- newMVar M.empty
      newQueue <- newMVar S.empty
      putMVar (peersLastSeen st) $ M.insert hash newLastSeen lastSeenMap
      putMVar (peerActivityQueue st) $ M.insert hash newQueue queueMap
      updateHashActivity peer newLastSeen newQueue
    Just lastSeen -> do
      putMVar (peersLastSeen st) lastSeenMap
      putMVar (peerActivityQueue st) queueMap
      let activityQueue = queueMap M.! hash
      updateHashActivity peer lastSeen activityQueue
  where
    updateHashActivity peer lastSeenMV queueMV = do
      let pid = (peerId peer)
      now <- getCurrentTime
      lastSeen <- takeMVar lastSeenMV
      queue <- takeMVar queueMV
      case M.lookup pid lastSeen of
        Nothing -> do
          putMVar lastSeenMV $ M.insert pid now lastSeen
          putMVar queueMV $ S.insert (now, Just pid) queue
        Just old_now -> do
          putMVar lastSeenMV $ M.insert pid now lastSeen
          putMVar queueMV $
            S.insert (now, Just pid) $
            S.delete (old_now, Just pid) queue

getHashRecord :: AnnounceState -> InfoHash -> IO HashRecord
getHashRecord st hash = do
  hrMap <- takeMVar (activeHashes st)
  case M.lookup hash hrMap of
    Just hr -> do
      putMVar (activeHashes st) hrMap
      return hr
    Nothing -> do
      hr <- emptyHashRecord
      putMVar (activeHashes st) $ M.insert hash hr hrMap
      return hr

getProtocolHashRecord :: Peer -> HashRecord -> MVar ProtocolHashRecord
getProtocolHashRecord peer hr = do
  case peerAddr peer of
    SockAddrInet _ _ -> hrInet4 hr
    SockAddrInet6 _ _ _ _ -> hrInet6 hr
    _ -> error "Unix sockets not supported."
      -- TODO: Make this a reasonable exception

isSeeder :: Announce -> Bool
isSeeder an = (anLeft an) == 0

addOrRemovePeer :: Announce :: AnnounceT ()
addOrRemovePeer an = do
  let peer = anPeer an
      hash = anInfoHash an
  st <- getState
  case anEvent of
    -- No Event, just update
    Nothing -> liftIO $ updateActivity st hash peer
    Just Started -> addAnnounce an
    Just Completed -> shiftAnnounce an
    Just Stopped -> removeAnnounce an
  where
    addPeer :: Announce -> AnnounceT ()
    addPeer an = do
      
    shiftPeer :: Announce -> AnnounceT ()
    removePeer :: Announce -> AnnounceT ()
