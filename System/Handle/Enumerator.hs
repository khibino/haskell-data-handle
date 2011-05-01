
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

connectTo :: MonadIO m => IO (Enumerator ByteString m b, Handle)
connectTo =  first enumAll <$> createPipe

outputEnumerator :: MonadIO m => (Handle -> IO ()) -> IO (Enumerator ByteString m b)
outputEnumerator wproc =
  do (enum, wfh) <- connectTo
     _ <- forkIO (wproc wfh >> hClose wfh)
     toIO enum

connectFrom :: Enumerator ByteString IO () -> IO Handle
connectFrom e =
  do (rfh, wfh) <- createPipe
     _ <- forkIO (run_ (e $$ iterHandle wfh) >> hClose wfh)
     toIO rfh
