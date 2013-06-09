module Main where

import Data.Binary
import Data.Word
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

clientHints = defaultHints { addrFamily = AF_INET6 }
port = "6543"

clientPing = withSocketsDo $
  do
    addrinfos <- getAddrInfo (Just clientHints) (Just "localhost") (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    connect sock (addrAddress serveraddr)
    sendNum sock 42
    sendNum sock 1728
    sendNum sock 514229
  where
    sendNum :: Socket -> Word32 -> IO ()
    sendNum sock num = do
      let msg = BL.toStrict $ encode num
      sendAll sock msg 
      response <- recv sock 4
      let num' = decode $ BL.fromStrict response :: Word32
      putStrLn $ "received: " ++ (show num')

main = do
  putStrLn "Starting ping client."
  clientPing
