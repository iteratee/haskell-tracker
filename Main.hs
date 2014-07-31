module Main where

import Announce
import AnnounceServer
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Reader
import Data.Monoid
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import SnapServer
import Snap.Http.Server.Config hiding (defaultConfig)
import Snap.Http.Server hiding (defaultConfig)
import UdpProtocol
import qualified Data.ByteString.Char8 as B8

oneMinuteMicros :: Int
oneMinuteMicros = 60 * 1000 * 1000

udpServerThread :: AnnounceEnv -> IO ()
udpServerThread anEnv = do
  putStrLn "Starting udp servers."
  env <- makeUdpEnv anEnv
  forM_ (ancAddrs $ anConf anEnv) $ \(addr, port) -> do
    putStrLn $ "Binding to " ++ addr ++ ":" ++ port
    addrinfos <- getAddrInfo Nothing (Just addr) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    when ((addrFamily serveraddr) == AF_INET6) $
      setSocketOption sock IPv6Only 1
    bindSocket sock (addrAddress serveraddr)
    forkIO $ acceptAndProcessRequests sock env
    forkIO $ cycleKeyThread env
  where
    acceptAndProcessRequests :: Socket -> UdpEnv -> IO ()
    acceptAndProcessRequests sock env = forever $ do
      (msg, addr) <- recvFrom sock 1024
      forkIO $ do
        runReaderT (handleUdpRequest sock addr msg) env
    cycleKeyThread env = forever $ do
      threadDelay (2 * oneMinuteMicros)
      runReaderT cycleKeys env

pruneInactiveThread :: AnnounceEnv -> IO ()
pruneInactiveThread anEnv = forever $ do
  threadDelay oneMinuteMicros
  runReaderT pruneQueue anEnv

snapServerThread :: AnnounceEnv -> IO ()
snapServerThread env = do
  putStrLn "Starting snap server."
  let baseConfig = setVerbose True mempty
  forM_ (ancAddrs $ anConf env) $ \(addr, port) -> do
    let config = setAccessLog (ConfigFileLog ("log/access" ++ addr ++ ".log")) $
          setErrorLog (ConfigFileLog ("log/error." ++ addr ++ ".log")) $
          setBind (B8.pack addr) $ setPort (read port) baseConfig
    forkIO $ httpServe config (completeSnap env)

main = do
  anSt <- emptyAnnounceState
  let anEnv = AnnounceEnv {
        anSt = anSt, anConf = defaultConfig }
  forkIO $ udpServerThread anEnv
  forkIO $ pruneInactiveThread anEnv
  forkIO $ snapServerThread anEnv
  forever $ threadDelay oneMinuteMicros
