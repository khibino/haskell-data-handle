
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
-- input-handle into String data, or read from String data to output-handle.
--
-----------------------------------------------------------------------------

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

{- | Get a string and a writable handle.
     This string is constructed by written data to this handle. -}
connectTo :: IO (IO String, Handle)
connectTo =  first (b2s <$>) <$> B.connectTo

{- | Get a string from a function that writes data stream to handle. -}
outputString :: (Handle -> IO ()) -> IO String
outputString wproc =
  b2s <$> B.outputByteString wproc
  
{- | Get a handle of a string. -}
connectFrom :: String -> IO Handle
connectFrom s =
  do (rfh, wfh) <- createPipe
     _ <- forkIO (hPutStr wfh s >> hClose wfh)
     toIO rfh
