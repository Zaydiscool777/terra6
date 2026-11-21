{-# LANGUAGE TemplateHaskell #-}
module Data_ where

import Data.Vector (Vector)
import Data.Word (Word8, Word16)
import Control.Lens
import System.Random

{-
CDEF
89AB
4567
0123
-}

ramlen :: Int
ramlen = 0x1000
wordlen :: Int
wordlen = 4

data Model = Model {
  _ram :: Vector Word8,
  _rv :: Vector Word8,
  _ri :: Word16,
  _td :: Int,
  _ts :: Int,
  _pc :: Word16,
  _sp :: Word16,
  _sec :: Int, -- mod 256 = ~4.3 sec
  _seed :: StdGen,
  _scr :: Vector Word8
}
makeLenses ''Model
makePrisms ''Model
