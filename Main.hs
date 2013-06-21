module Main where

import Announce
import AnnounceServer
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Reader
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import UdpProtocol

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
        putStrLn $ "got packet from: " ++ (show addr)
        runReaderT (handleUdpRequest sock addr msg) env
    cycleKeyThread env = forever $ do
      threadDelay (2 * oneMinuteMicros)
      runReaderT cycleKeys env

pruneInactiveThread :: AnnounceEnv -> IO ()
pruneInactiveThread anEnv = forever $ do
  putStrLn "Starting inactive pruner."
  threadDelay oneMinuteMicros
  runReaderT pruneQueue anEnv

main = do
  anSt <- emptyAnnounceState
  let anEnv = AnnounceEnv {
        anSt = anSt, anConf = defaultConfig }
  forkIO $ udpServerThread anEnv
  forkIO $ pruneInactiveThread anEnv
  forever $ threadDelay oneMinuteMicros
