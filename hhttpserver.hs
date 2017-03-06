import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.List
import Control.Concurrent
import Control.Exception (SomeException, try)
import Control.Monad
import qualified Data.Map.Strict as Map
import System.FilePath.Posix
import System.Directory (doesFileExist)
import Text.Printf
import Data.List.Split

port = 8080
incomingBufferSize = 16384
mimeTypes = Map.fromList [
    (".html", "text/html"),
    (".jpeg", "image/jpeg")
  ]
defaultMime = "application/octet-stream"
headerOkText = "HTTP/1.1 200 OK\r\nContent-Type: %s\r\n\r\n"
header404 = "HTTP/1.1 404\r\n\r\n"
header500 = "HTTP/1.1 500\r\n\r\n"

main :: IO ()
main =
  socket AF_INET Stream 0 >>= \sock ->
  setSocketOption sock ReuseAddr 1 >> 
  bind sock (SockAddrInet port iNADDR_ANY) >>
  listen sock sOMAXCONN >>
  mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = accept sock >>= forkIO . handle . fst >> mainLoop sock

handle :: Socket -> IO ()
handle conn =
  recv conn incomingBufferSize >>=
  responseForLocation . extractPath . C.unpack >>=
  send conn >>
  close conn

responseForLocation :: String -> IO (B.ByteString)
responseForLocation location =
  andM [return (isSafeLocation location), doesFileExist location] >>= \accessFile ->
  if accessFile then
    try (B.readFile location) >>= return . contentsOr500 
  else
    return $ C.pack header404

-- Non IO functions

contentsOr500 :: Either SomeException B.ByteString -> B.ByteString
contentsOr500 (Left _) = C.pack header500
contentsOr500 (Right contents) = contents

-- Overengineered for 2 cases? Can be simpler?
-- http://stackoverflow.com/a/27097421/1319998
andM = foldr (&&&) (return True)
  where ma &&& mb = ma >>= \p -> if p then mb else return p

fullResponse :: String -> B.ByteString -> B.ByteString
fullResponse extension = B.append $ C.pack headerWithMime
  where
    headerWithMime = printf headerOkText $ mimeForExtension extension

extractPath :: String -> String
extractPath = tail . head . tail . splitOn " "

mimeForExtension :: String -> String
mimeForExtension = flip (Map.findWithDefault defaultMime) mimeTypes

isSafeLocation :: String -> Bool
isSafeLocation = not . isInfixOf ".."
