import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Control.Concurrent
import Control.Exception (SomeException, try)
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
headerOkText = "HTTP/1.1 200 OK\r\nContent-Type: %s\r\n\r\n"
header404 = "HTTP/1.1 404\r\n\r\n"

main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock $ SockAddrInet port iNADDR_ANY
  listen sock sOMAXCONN
  mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
  (conn, _) <- accept sock
  forkIO $ handle conn
  mainLoop sock

handle :: Socket -> IO ()
handle conn = do
  incoming <- recv conn incomingBufferSize
  responseText <- response $ location incoming
  send conn responseText 
  close conn
  where
    -- Extremely dirty way of getting location, probably unsafe!
    location = C.unpack . C.tail . head . tail . C.split ' '

response :: String -> IO (B.ByteString)
response requestedLocation = do
  file <- try $ B.readFile requestedLocation
  return (staticFileContents file $ takeExtension requestedLocation)

staticFileContents :: Either SomeException B.ByteString -> String -> B.ByteString
staticFileContents (Left _) extension = C.pack header404
staticFileContents (Right fileContents) extension = fullResponse extension fileContents

fullResponse extension contents = C.pack headerWithMime `B.append` contents
  where
    headerWithMime = printf headerOkText $ mimeForExtension extension
    mimeForExtension = flip (Map.findWithDefault defaultMime) mimeTypes
