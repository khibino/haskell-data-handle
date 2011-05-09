
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

module System.Handle.Enumerator (
  connectTo,
  outputEnumerator,
  connectFrom
  ) where

import System.Posix.Handle (createPipe)
import System.IO (Handle, hClose)
import Data.ByteString (ByteString)
import Data.Enumerator (Enumerator, concatEnums, ($$), run_)
import Data.Enumerator.Binary (enumHandle, iterHandle)

import Control.Monad.IO.Class (MonadIO)
import Control.Concurrent (forkIO)
import Control.Arrow (first)
import Control.Applicative ((<$>))

toIO :: a -> IO a
toIO =  return

chunkSize :: Integer
chunkSize =  0x1000

enumAll :: MonadIO m => Handle -> Enumerator ByteString m b
enumAll =  concatEnums . readLoop
  where readLoop :: MonadIO m => Handle -> [Enumerator ByteString m b]
        readLoop fh = enumHandle chunkSize fh : readLoop fh

{- | Get a binary-enumerator and a writable handle.
     This binary-enumerator is constructed by written data to this handle. -}
connectTo :: MonadIO m => IO (Enumerator ByteString m b, Handle)
connectTo =  first enumAll <$> createPipe

{- | Get a binary-enumerator from a function that writes data stream to handle. -}
outputEnumerator :: MonadIO m => (Handle -> IO ()) -> IO (Enumerator ByteString m b)
outputEnumerator wproc =
  do (enum, wfh) <- connectTo
     _ <- forkIO (wproc wfh >> hClose wfh)
     toIO enum

{- | Get a handle of a binary-enumerator. -}
connectFrom :: Enumerator ByteString IO () -> IO Handle
connectFrom e =
  do (rfh, wfh) <- createPipe
     _ <- forkIO (run_ (e $$ iterHandle wfh) >> hClose wfh)
     toIO rfh
