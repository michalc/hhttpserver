{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (catch, readFile, log)

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch, try)
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

import qualified Data.Text as T
import Data.Text.Encoding as E

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

log :: (Show a) => SockAddr -> T.Text -> a -> IO a
log addr label val = getCurrentTime >>= \time ->
                     putStrLn (printf logTemplate (show time) (show addr) (T.unpack label) (take maxLogLength $ show val)) >>
                     return val

handle :: (Socket, SockAddr) -> IO ()
handle (conn, addr) = catch (sendResponse (conn, addr)) (send500 (conn, addr)) >>
                      close conn

sendResponse :: (Socket, SockAddr) -> IO ()
sendResponse (conn, addr) = recv conn incomingBufferSize  >>=
                            return . unpackHttpMessage    >>= log addr "request"  >>=
                            response . extractPath        >>= log addr "response" >>=
                            send conn . packHttpMessage   >> return ()

send500 :: (Socket, SockAddr) -> SomeException -> IO ()
send500 (conn, addr) e = log addr ("error") e >> send conn (packHttpMessage http500) >> return ()

response :: T.Text -> IO HttpMessage
response path = (isSafePath path) &&& (doesFileExist (T.unpack path)) >>= responseForPath path

responseForPath :: T.Text -> Bool -> IO HttpMessage
responseForPath _    False = return http404
responseForPath path True  = readFile (T.unpack path) >>=
                             return . wrapInHttpMessage (mimeForPath path)

-- Short circuit && that accepts pure + IO action
(&&&) :: Bool -> IO Bool -> IO Bool
False &&& _         = return False
True  &&& bIOAction = bIOAction

-------------------
-- Non IO functions

unpackHttpMessage :: ByteString -> HttpMessage
unpackHttpMessage byteString = HttpMessage {header = E.decodeUtf8 header, contents = contents}
  where (header, contents) = breakSubstring httpHeaderEnd byteString

packHttpMessage :: HttpMessage -> ByteString
packHttpMessage HttpMessage {header = header, contents = contents} = (E.encodeUtf8 header) `append` httpHeaderEnd `append` contents 

wrapInHttpMessage :: T.Text -> ByteString -> HttpMessage
wrapInHttpMessage mime contents = HttpMessage {header = T.pack $ printf (T.unpack headerOk) (T.unpack mime), contents = contents}

extractPath :: HttpMessage -> T.Text
extractPath = T.tail . head . tail . T.splitOn (" ") . header

mimeForPath :: T.Text -> T.Text
mimeForPath path = findWithDefault defaultMime (T.pack $ takeExtension $ T.unpack path) mimeTypes

isSafePath :: T.Text -> Bool
isSafePath path = not (T.null path) && not (T.isInfixOf ("..") path) && T.head path /= '/'

------------
-- Constants

port = 80
incomingBufferSize = 16384

mimeTypes = fromList [
    (".html", "text/html"),
    (".jpeg", "image/jpeg")
  ]
defaultMime = "application/octet-stream"

headerOk = "HTTP/1.1 200 OK\r\nContent-Type: %s"
http404 = HttpMessage {header="HTTP/1.1 404 Not Found", contents=empty}
http500 = HttpMessage {header="HTTP/1.1 500 Internal Server Error", contents=empty}
httpHeaderEnd = "\r\n\r\n"

logTemplate = "[%s] [%s] [%s] %s"
maxLogLength = 1024

--------
-- Types

data HttpMessage = HttpMessage {
  header :: T.Text,
  contents :: ByteString
} deriving (Show)
