import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Control.Concurrent
import Control.Exception
import qualified Data.Map.Strict as Map
import System.FilePath.Posix
import Text.Printf

-- This is most likely really insecure with non-existant error handling

port = 8080
incomingBufferSize = 16384
mimeTypes = Map.fromList [
    (".htm", "text/html"),
    (".html", "text/html"),
    (".js", "application/javascript"),
    (".css", "text/css"),
    (".png", "image/png"),
    (".jpg", "image/jpeg"),
    (".jpeg", "image/jpeg")
  ]
defaultMime = "application/octet-stream"

main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock $ SockAddrInet port iNADDR_ANY
  listen sock sOMAXCONN
  mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
  (conn, _) <- accept sock
  forkIO $ respond conn
  mainLoop sock

respond :: Socket -> IO ()
respond conn = do
  incoming <- recv conn incomingBufferSize
  let requestedLocation = location incoming
  file <- try $ B.readFile requestedLocation
  send conn $ response file $ takeExtension requestedLocation
  close conn

response :: Either SomeException B.ByteString -> String -> B.ByteString
response (Left _) extension = C.pack header404
response (Right fileContents) extension = C.pack headerWithMime `B.append` fileContents
  where headerWithMime = printf headerOkText $ Map.findWithDefault defaultMime extension mimeTypes

headerOkText = "HTTP/1.1 200 OK\r\nContent-Type: %s\r\n\r\n"
header404 = "HTTP/1.1 404\r\n\r\n"

-- Extremely dirty way of getting location, probably unsafe!
location httpData = C.unpack $ C.tail $ C.split ' ' httpData !! 1

