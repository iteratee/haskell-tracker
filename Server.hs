module Main where

import Control.Monad
import Data.Binary
import Data.Word
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

serverHints = defaultHints { addrFamily = AF_INET6 }
port = PortNum 6543

serverPong = withSocketsDo $
  do
    addrinfos <- getAddrInfo (Just serverHints) Nothing (Just "6543")
  
    let serveraddr = head addrinfos

    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    bindSocket sock (addrAddress serveraddr)

    pongMessages sock
  where
    pongMessages sock =
      do
        (msg, addr) <- recvFrom sock 1024
        unless ((B.length msg) /= 4) $ do
          let value = decode $ BL.fromStrict msg :: Word32
          let msg' = BL.toStrict $ encode (value + 1)
          sent <- sendTo sock msg' addr
          unless (sent /= (B.length msg')) $ pongMessages sock

main = do
  putStrLn "Starting pong server."
  serverPong
