
-----------------------------------------------------------------------------
-- |
-- Module      : System.Handle.String
-- Copyright   : 2011 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : non-portable (requires POSIX)
--
-- This package includes helper functions that converts data-stream written to
-- input-handle into ByteString data, or read from ByteString data to output-handle.
--
-----------------------------------------------------------------------------

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

{- | Get a byte-string and a writable handle.
     This byte-string is constructed by written data to this handle. -}
connectTo :: IO (IO ByteString, Handle)
connectTo =  first readAll <$> createPipe

{- | Get a byte-string from a function that writes data stream to handle. -}
outputByteString :: (Handle -> IO ()) -> IO ByteString
outputByteString wproc =
  do (act, wfh) <- connectTo
     _ <- forkIO (wproc wfh >> hClose wfh)
     act

{- | Get a handle of a byte-string. -}
connectFrom :: ByteString -> IO Handle
connectFrom s =
  do (rfh, wfh) <- createPipe
     _ <- forkIO (BS.hPut wfh s >> hClose wfh)
     toIO rfh
