{-# LANGUAGE PartialTypeSignatures, DeriveGeneric, StrictData #-}


module Parser.ByteStringReader where


import Base
import Config


import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.ByteString.Builder
import qualified Data.ByteString as BS
import Data.ByteString.Internal (w2c)
import Data.Word 
import Data.List
import Data.Functor ((<&>))
import Data.Int
import Data.Maybe
import Data.Char (showLitChar,isAscii,isHexDigit)


import Data.Serialize.LEB128.Lenient 
import Control.Monad.State.Strict
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Char8 as BSC (unpack)

import qualified Data.Serialize.Get as S
import qualified Data.Serialize as Cereal hiding (get,put)
import GHC.Generics


import Debug.Trace

type ByteStringAt = (BS.ByteString,Int)

consume :: Int -> State ByteStringAt BS.ByteString
consume n = do
  (bs,pos) <- get
  let (read,rest) = BS.splitAt n bs
  put (rest,pos+BS.length read)
  return read

read_bytes :: Int -> State ByteStringAt [Word8]
read_bytes n = BS.unpack <$> consume n

read_uint8 :: State ByteStringAt Word8
read_uint8 = consume 1 <&> runGet S.getWord8

read_uint16 :: State ByteStringAt Word16
read_uint16 = consume 2 <&> runGet S.getWord16le

read_uint32 :: State ByteStringAt Word32
read_uint32 = consume 4 <&> runGet S.getWord32le

read_uint64 :: State ByteStringAt Word64
read_uint64 = consume 8 <&> runGet S.getWord64le

read_sint32 :: State ByteStringAt Int32
read_sint32 = consume 4 <&> runGet S.getInt32le



read_uleb128 :: (Show a, LEB128 a) => State ByteStringAt a
read_uleb128 = do
  (bs,_) <- get
  let v = runGet getLEB128 bs
  let encoding = toLEB128 v
  let num_bytes = BS.length encoding
  if BS.take num_bytes bs == encoding then do
    _ <- consume num_bytes
    return v
  else
    error $ show (show $ BS.unpack $ BS.take 20 bs, v, encoding,num_bytes)

read_sleb128 :: (Show a, SLEB128 a) => State ByteStringAt a
read_sleb128 = do
  (bs,_) <- get
  let v = runGet getSLEB128 bs
  let encoding = toSLEB128 v
  let num_bytes = BS.length encoding
  if BS.take num_bytes bs == encoding then do
    _ <- consume num_bytes
    return v
  else
    error $ show (show $ BS.unpack $ BS.take 20 bs, v, encoding,num_bytes)

read_string :: State ByteStringAt String
read_string = do
  (bs,pos) <- get
  let (ret,rest) = BS.span ((/=) 0) bs
  put (BS.tail rest, pos+BS.length ret+1)
  return $ BSC.unpack ret
  


runGet :: S.Get a -> BS.ByteString -> a
runGet g bs =
  case S.runGet g bs of
    Left err -> error $ show err
    Right w  -> w



read_until_at :: Int -> State ByteStringAt a -> State ByteStringAt [a]
read_until_at pos1 m = do
  (bs,pos) <- get
  if pos >= pos1 then
    return []
  else do
    a <- m
    as <- read_until_at pos1 m
    return $ a:as


read_n_times :: Int -> State ByteStringAt a -> State ByteStringAt [a]
read_n_times 0 r = return []
read_n_times n m = do
  a <- m
  as <- read_n_times (n-1) m
  return $ a:as
  

read_repeat_until_empty :: Int -> (Int -> State ByteStringAt a) -> State ByteStringAt [a]
read_repeat_until_empty count m = do
  (bs,_) <- get
  if BS.null bs then
    return []
  else do
    a <- m count
    as <- read_repeat_until_empty (count+1) m
    return $ a:as


