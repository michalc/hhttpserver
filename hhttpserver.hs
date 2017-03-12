import Prelude hiding (readFile, log)

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Data.ByteString (ByteString, append, breakSubstring, empty, readFile)
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

log :: (Show a) => SockAddr -> String -> a -> IO a
log addr label val = getCurrentTime >>= \time ->
                     putStrLn (printf logTemplate (show time) (show addr) label (take maxLogLength $ show val)) >>
                     return val

handle :: (Socket, SockAddr) -> IO ()
handle (conn, addr) = recv conn incomingBufferSize  >>=
                      return . unpackHttpMessage    >>= log addr "request"  >>=
                      response . extractPath        >>= log addr "response" >>=
                      send conn . packHttpMessage   >>
                      close conn

response :: String -> IO HttpMessage
response path = (isSafePath path) &&& (doesFileExist path) >>= responseForPath path

responseForPath :: String -> Bool -> IO HttpMessage
responseForPath _    False = return http404
responseForPath path True  = try (readFile path) >>= 
                             return . fullHttpResponseOr500 (mimeForPath path)

-- Short circuit && that accepts pure + IO action
(&&&) :: Bool -> IO Bool -> IO Bool
False &&& _         = return False
True  &&& bIOAction = bIOAction

-------------------
-- Non IO functions

unpackHttpMessage :: ByteString -> HttpMessage
unpackHttpMessage byteString = HttpMessage {header = unpack header, contents = contents}
  where
  (header, contents) = breakSubstring httpHeaderEnd byteString

packHttpMessage :: HttpMessage -> ByteString
packHttpMessage HttpMessage {header = header, contents = contents} = pack header `append` hTTP_HEADER_END `append` contents 

fullHttpResponseOr500 :: String -> Either SomeException ByteString -> HttpMessage
fullHttpResponseOr500 _    (Left  _)        = http500
fullHttpResponseOr500 mime (Right contents) = HttpMessage {header = printf headerOk mime, contents = contents}

extractPath :: HttpMessage -> String
extractPath = tail . head . tail . splitOn " " . header

mimeForPath :: String -> String
mimeForPath path = findWithDefault defaultMime (takeExtension path) mimeTypes

isSafePath :: String -> Bool
isSafePath path = not (null path) && not (isInfixOf ".." path) && head path /= '/'

------------
-- Constants

port = 8080
incomingBufferSize = 16384

mimeTypes = fromList [
    (".html", "text/html"),
    (".jpeg", "image/jpeg")
  ]
defaultMime = "application/octet-stream"

headerOk = "HTTP/1.1 200 OK\r\nContent-Type: %s"
http404 = HttpMessage {header="HTTP/1.1 404 Not Found", contents=empty}
http500 = HttpMessage {header="HTTP/1.1 500 Internal Server Error", contents=empty}
httpHeaderEnd = pack "\r\n\r\n"

logTemplate = "[%s] [%s] [%s] %s"
maxLogLength = 1024

--------
-- Types

data HttpMessage = HttpMessage {
  header :: String,
  contents :: ByteString
} deriving (Show)
