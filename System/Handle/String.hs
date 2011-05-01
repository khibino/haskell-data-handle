
module System.Handle.String (
  connectTo,
  outputString,
  connectFrom
  ) where

import qualified System.Handle.Binary as B

import System.Posix.Handle (createPipe)
import System.IO (Handle, hPutStr, hClose)
import Data.ByteString.Lazy (ByteString)
import qualified Data.String.UTF8 as U8

import Control.Concurrent (forkIO)
import Control.Arrow (first)
import Control.Applicative ((<$>))

toIO :: a -> IO a
toIO =  return

b2s :: ByteString -> String
b2s =  U8.toString . U8.fromRep

connectTo :: IO (IO String, Handle)
connectTo =  first (b2s <$>) <$> B.connectTo

outputString :: (Handle -> IO ()) -> IO String
outputString wproc =
  b2s <$> B.outputByteString wproc
  
connectFrom :: String -> IO Handle
connectFrom s =
  do (rfh, wfh) <- createPipe
     _ <- forkIO (hPutStr wfh s >> hClose wfh)
     toIO rfh
