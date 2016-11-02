{-# LANGUAGE OverloadedStrings #-}

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8

client' :: Int -> IO ()
client' = client "localhost"

client :: String -> Int -> IO ()
client host port = withSocketsDo $ do
                addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
                let serverAddr = head addrInfo
                sock <- socket (addrFamily serverAddr) Stream defaultProtocol
                connect sock (addrAddress serverAddr)
                msgSender sock
                sClose sock

msgSender :: Socket -> IO ()
msgSender sock = do
  message <- B8.getLine
  send sock message
  send sock "\n\r\n"
  rMsg <- recv sock 1024
  B8.putStrLn rMsg
  msgSender sock
