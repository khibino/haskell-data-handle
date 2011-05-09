{-# OPTIONS -fno-warn-unused-imports #-}

-----------------------------------------------------------------------------
-- |
-- Module      : System.Handle
-- Copyright   : 2011 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : non-portable (requires POSIX)
--
-- This package includes helper functions that converts data-stream written to
-- input-handle into string-like data, or read from string-like data to output-handle.
--
-----------------------------------------------------------------------------

module System.Handle where

import qualified System.Handle.Binary as Binary (connectTo, connectFrom)
import System.Handle.Binary hiding (connectTo, connectFrom)
import qualified System.Handle.String as String (connectTo, connectFrom)
import System.Handle.String hiding (connectTo, connectFrom)
--import qualified System.Handle.Enumerator as Enumerator (connectTo, connectFrom)
--import System.Handle.Enumerator hiding (connectTo, connectFrom)
