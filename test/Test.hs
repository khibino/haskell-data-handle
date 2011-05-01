
module Test where

import qualified System.Handle.Binary as B
import qualified System.Handle.String as S
import qualified System.Handle.Enumerator as E

import System.IO (Handle, hPutStr, hGetContents)
import Control.Monad.IO.Class (MonadIO)
import Data.String.UTF8 (UTF8Bytes)
import qualified Data.String.UTF8 as U8
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ZBS
import qualified Data.ByteString as BS
import Data.Enumerator (Enumerator, enumList)

times :: Int -> [a] -> [a]
times n = concat . replicate n

outBS :: Int -> IO ByteString
outBS n =
  B.outputByteString
  (`hPutStr` times n "Hello world? Hello world!\n")

outS :: Int -> IO String
outS n =
  S.outputString
  (`hPutStr` times n "Hello world? Hello world!\n")

outE :: (MonadIO m) => Int -> IO (Enumerator BS.ByteString m b)
outE n =
  E.outputEnumerator
  (`hPutStr` times n "Hello world? Hello world!\n")

chunkSize :: IO ByteString -> IO [Int]
chunkSize =  fmap (map BS.length . ZBS.toChunks)

s2u8 :: UTF8Bytes string int => String -> string
s2u8 =  U8.toRep . U8.fromString

btimes :: Int -> String -> ByteString
btimes n = ZBS.fromChunks . replicate n . s2u8

inBS' :: Int -> IO Handle
inBS' n =  B.connectFrom $ s2u8 $ times n "Hello world? Hello world!\n"

inBS :: Int -> IO Handle
inBS n =  B.connectFrom $ btimes n "Hello world? Hello world!\n"

inS :: Int -> IO Handle
inS n =  S.connectFrom $ times n "Hello world? Hello world!\n"

inEnum :: Int -> IO Handle
inEnum n =
  E.connectFrom $ enumList 100 $ replicate n $ s2u8 "Hello world? Hello world!\n"

runIn :: (Int -> IO Handle) -> Int -> IO String
runIn f n = f n >>= hGetContents
