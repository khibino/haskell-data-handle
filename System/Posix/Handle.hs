-----------------------------------------------------------------------------
-- |
-- Module      : System.Posix.Handle
-- Copyright   : 2011 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : non-portable (requires POSIX)
--
-- This package includes POSIX fd functions wrapped with handle.
--
-----------------------------------------------------------------------------

module System.Posix.Handle (
  createPipe,
  dup, dupTo,
  ) where

import qualified System.Posix.IO as P
import System.IO (Handle)
import Control.Applicative ((<$>), (<*>))

{- | Get a pipe wrapped by IO.Handle. -}
createPipe :: IO (Handle, Handle)
createPipe =
  do (w, r) <- P.createPipe
     (,) <$> P.fdToHandle w <*> P.fdToHandle r

{- | Duplicate handle like dup. Handle may be flushed. -}
dup :: Handle -> IO Handle
dup from =
  do fromFd <- P.handleToFd from
     toFd   <- P.dup fromFd
     P.fdToHandle toFd

{- | Duplicate handle like dup2. Handle may be flushed. -}
dupTo :: Handle -> Handle -> IO Handle
dupTo old new =
  do oldFd <- P.handleToFd old
     newFd <- P.handleToFd new
     resFd <- P.dupTo oldFd newFd
     P.fdToHandle resFd
