import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
import Control.Concurrent

-- This is most likely really insecure with non-existant error handling

port = 8080
incomingBufferSize = 16384

main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet port iNADDR_ANY)
  listen sock sOMAXCONN
  mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
  (conn, _) <- accept sock
  forkIO (respond conn)
  mainLoop sock

respond :: Socket -> IO ()
respond conn = do
  incoming <- recv conn incomingBufferSize
  let requestedLocation = location incoming
  file <- readFile $ C.unpack requestedLocation
  send conn $ C.pack $ header ++ file
  close conn

header = "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\n"

-- Extremely dirty way of getting location
location httpData = C.tail $ C.split ' ' httpData !! 1

