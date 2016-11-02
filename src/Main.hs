{-# LANGUAGE OverloadedStrings #-}

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8

client :: IO ()
client = withSocketsDo $ do
  localhost <- inet_addr "127.0.0.1"
  sock <- socket AF_INET Stream defaultProtocol
  connect sock (SockAddrInet 8000 localhost)
  sendUserMessage sock
  sClose sock

sendUserMessage :: Socket -> IO ()
sendUserMessage sock = do
  message <- B8.getLine
  send sock $ B8.concat [(B8.pack "GET /echo.php?message="), message, (B8.pack " HTTP/1.1\n\r\n")]
  send sock "Host: localhost\n\r\n"
  send sock "\n\r\n"
  receivedMessage <- recv sock 1024
  B8.putStrLn receivedMessage
  sendUserMessage sock
