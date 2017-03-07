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
main = socket AF_INET Stream 0 >>= \sock ->
       setSocketOption sock ReuseAddr 1 >> 
       bind sock (SockAddrInet port iNADDR_ANY) >>
       listen sock sOMAXCONN >>
       mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = accept sock >>= forkIO . handle . fst >> mainLoop sock

handle :: Socket -> IO ()
handle conn = recv conn incomingBufferSize >>=
              response . extractPath . C.unpack >>=
              send conn >>
              close conn

response :: String -> IO (B.ByteString)
response path = (isSafePath path) &&& (doesFileExist path) >>= responseForPath path

responseForPath :: String -> Bool -> IO (B.ByteString)
responseForPath _    False = return $ C.pack header404
responseForPath path True  = try (B.readFile path) >>= 
                             return . fullHttpResponseOr500 (mimeForPath path)

-- Short circuit && that accepts pure + IO action
(&&&) :: Bool -> IO (Bool) -> IO (Bool)
False &&& _         = return False
True  &&& bIOAction = bIOAction

-- Non IO functions

fullHttpResponseOr500 :: String -> Either SomeException B.ByteString -> B.ByteString
fullHttpResponseOr500 _    (Left  _)        = C.pack header500
fullHttpResponseOr500 mime (Right contents) = fullHttpResponse mime contents

fullHttpResponse :: String -> B.ByteString -> B.ByteString
fullHttpResponse = B.append . C.pack . printf headerOkText

extractPath :: String -> String
extractPath = tail . head . tail . splitOn " "

mimeForPath :: String -> String
mimeForPath path = Map.findWithDefault defaultMime (takeExtension path) mimeTypes

isSafePath :: String -> Bool
isSafePath = not . isInfixOf ".."
