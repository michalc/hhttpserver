import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.List
import Control.Concurrent
import Control.Exception (SomeException, try)
import qualified Data.Map.Strict as Map
import System.FilePath.Posix
import System.Directory (doesFileExist)
import Text.Printf

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
header500 = "HTTP/1.1 500\r\n\r\n"

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
  response <- responseForLocation $ extractPath incoming
  send conn response
  close conn
  where
    extractPath = C.unpack . C.tail . head . tail . C.split ' '

responseForLocation :: String -> IO (B.ByteString)
responseForLocation location = do
  if isSafeLocation location then
    do
      contents <- try $ getFileContents location
      return $ contentsOr500 contents
  else
    return $ C.pack header404
  where
    isSafeLocation location = not $ ".." `isInfixOf` location

getFileContents :: String -> IO (B.ByteString)
getFileContents requestedLocation = do
  exists <- doesFileExist requestedLocation
  if exists then
    do
      file <- B.readFile requestedLocation
      return $ fullResponse file $ takeExtension requestedLocation
    else
      return $ C.pack header404

contentsOr500 :: Either SomeException B.ByteString -> B.ByteString
contentsOr500 (Left _) = C.pack header500
contentsOr500 (Right contents) = contents

fullResponse :: B.ByteString -> String -> B.ByteString
fullResponse contents extension = C.pack headerWithMime `B.append` contents
  where
    headerWithMime = printf headerOkText $ mimeForExtension extension
    mimeForExtension = flip (Map.findWithDefault defaultMime) mimeTypes