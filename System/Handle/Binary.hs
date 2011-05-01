
module System.Handle.Binary (
  connectTo,
  outputByteString,
  connectFrom
  ) where

import System.Posix.Handle (createPipe)
import System.IO (Handle, hIsEOF, hWaitForInput, hClose)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

import Control.Concurrent (forkIO)
import Control.Arrow (first)
import Control.Applicative ((<$>), (<*>))

toIO :: a -> IO a
toIO =  return

bufSize :: Int
bufSize =  0x10000

readAll :: Handle -> IO ByteString
readAll =  readLoop
  where readLoop fh =
          do eof <- hIsEOF fh
             if eof
               then toIO BS.empty
               else do ready <- hWaitForInput fh (-1)
                       if ready
                         then BS.append <$> BS.hGetNonBlocking fh bufSize <*> readLoop fh
                         else error "hWaitForInput bug?"

connectTo :: IO (IO ByteString, Handle)
connectTo =  first readAll <$> createPipe

outputByteString :: (Handle -> IO ()) -> IO ByteString
outputByteString wproc =
  do (act, wfh) <- connectTo
     _ <- forkIO (wproc wfh >> hClose wfh)
     act

connectFrom :: ByteString -> IO Handle
connectFrom s =
  do (rfh, wfh) <- createPipe
     _ <- forkIO (BS.hPut wfh s >> hClose wfh)
     toIO rfh
