module System.Posix.Handle (
  createPipe) where

import qualified System.Posix.IO as P
import System.IO (Handle)
import Control.Applicative ((<$>), (<*>))

createPipe :: IO (Handle, Handle)
createPipe =
  do (w, r) <- P.createPipe
     (,) <$> P.fdToHandle w <*> P.fdToHandle r
