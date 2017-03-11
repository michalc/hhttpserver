import Prelude hiding (readFile, log)

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Data.ByteString (ByteString, append, readFile)
import Data.ByteString.Char8 (pack, unpack)
import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Data.Map.Strict (findWithDefault, fromList)
import Data.Time.Clock (getCurrentTime)
import Network.Socket (
  Family(AF_INET), SockAddr(SockAddrInet), Socket, SocketOption(ReuseAddr), SocketType(Stream), 
  iNADDR_ANY, sOMAXCONN,
  accept, bind, close, getPeerName, listen, setSocketOption, socket
  )
import Network.Socket.ByteString (recv, send)
import System.Directory (doesFileExist)
import System.FilePath.Posix (takeExtension)
import Text.Printf (printf)

---------------
-- IO functions

main :: IO ()
main = socket AF_INET Stream 0 >>= \sock ->
       setSocketOption sock ReuseAddr 1 >> 
       bind sock (SockAddrInet port iNADDR_ANY) >>
       listen sock sOMAXCONN >>
       mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = forever $ accept sock >>= forkIO . handle

log :: (Show a) => String -> a -> IO a
log label val = getCurrentTime >>= \time ->
                putStrLn (printf logTemplate (show time) label (take maxLogLength $ show val)) >>
                return val

handle :: (Socket, SockAddr) -> IO ()
handle (conn, addr) =                                   log "address" addr >>
                      recv conn incomingBufferSize  >>= log "request"      >>=
                      return . extractPath . unpack >>= log "path"         >>=
                      response                      >>= log "response"     >>=
                      send conn                     >>
                      close conn

response :: String -> IO ByteString
response path = (isSafePath path) &&& (doesFileExist path) >>= responseForPath path

responseForPath :: String -> Bool -> IO ByteString
responseForPath _    False = return $ pack header404
responseForPath path True  = try (readFile path) >>= 
                             return . fullHttpResponseOr500 (mimeForPath path)

-- Short circuit && that accepts pure + IO action
(&&&) :: Bool -> IO Bool -> IO Bool
False &&& _         = return False
True  &&& bIOAction = bIOAction

-------------------
-- Non IO functions

fullHttpResponseOr500 :: String -> Either SomeException ByteString -> ByteString
fullHttpResponseOr500 _    (Left  _)        = pack header500
fullHttpResponseOr500 mime (Right contents) = fullHttpResponse mime contents

fullHttpResponse :: String -> ByteString -> ByteString
fullHttpResponse = append . pack . printf headerOk

extractPath :: String -> String
extractPath = tail . head . tail . splitOn " "

mimeForPath :: String -> String
mimeForPath path = findWithDefault defaultMime (takeExtension path) mimeTypes

isSafePath :: String -> Bool
isSafePath path = not (isInfixOf ".." path) && head path /= '/'

------------
-- Constants

port = 8080
incomingBufferSize = 16384
mimeTypes = fromList [
    (".html", "text/html"),
    (".jpeg", "image/jpeg")
  ]
defaultMime = "application/octet-stream"
headerOk = "HTTP/1.1 200 OK\r\nContent-Type: %s\r\n\r\n"
header404 = "HTTP/1.1 404\r\n\r\n"
header500 = "HTTP/1.1 500\r\n\r\n"
logTemplate = "[%s] [%s] %s"
maxLogLength = 1024
