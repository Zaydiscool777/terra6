module Lib
    ( step
    ) where
-- https://en.wikipedia.org/wiki/CHIP-8

import Data_

-- import qualified Data.Vector as V
import Data.Vector ((//))
import Data.Word (Word8)
import Data.Bits
import Control.Lens
import Data.Function (on)
import System.Random
import Lower

step :: Model -> IO Model
step m =
  let
    pc_ = m ^. pc
    ram_ = m ^. ram
    rv_ = m ^. rv
    ri_ = fE (m ^. ri)
    rvg :: Enum a => a -> Word8
    rvg = (rv_ !-)
    rf = rv . ix 0xF
    byte0 = ram_ !- pc_
    byte1 = ram_ !- succ pc_
  in case
    ((byte0 `shiftR` 4) .&. 0xF,
    byte0 .&. 0xF,
    (byte1 `shiftR` 4) .&. 0xF,
    byte1 .&. 0xF) of
      (0x0, 0x0, 0xE, 0x0) -> clrscr m & bgstep 
      (0x0, 0x0, 0xE, 0xE) -> ret m & bgstep
      (0x1, a, b, c) -> m & pc .~ comb3 a b c & bgstep
      (0x2, a, b, c) -> call m (comb3 a b c) & bgstep
      (0x3, x, b, c) ->
        m & pc %~
        (+ if rvg x == comb2 b c
          then 2
          else 0)
        & bgstep
      (0x4, x, b, c) ->
        m & pc %~
        (+ if rvg x /= comb2 b c
          then 2
          else 0)
        & bgstep
      (0x5, x, y, 0x0) ->
        m & pc %~
        (+ if rvg x == rvg y
          then 2
          else 0)
        & bgstep
      (0x6, x, b, c) -> m & rv %~ (x //- comb2 b c) & bgstep 
      (0x7, x, b, c) ->
        m & rv %~ (x //- rvg x + comb2 b c) & bgstep 
      (0x8, x, y, 0x0) ->
        m & rv %~ (x //- rvg y) & bgstep 
      (0x8, x, y, 0x1) ->
        m & rv %~ (x //- (rvg x .|. rvg y)) & bgstep 
      (0x8, x, y, 0x2) ->
        m & rv %~ (x //- rvg x .&. rvg y) & bgstep 
      (0x8, x, y, 0x3) ->
        m & rv %~ (x //- rvg x `xor` rvg y) & bgstep 
      (0x8, x, y, 0x4) -> let
          a = ((+) `on` toInteger) (rvg x) (rvg y)
          b = if a <= toInteger (maxBound :: Word8) then 1 else 0
        in m & rv %~ x //- fromInteger a & rf .~ b & bgstep
      (0x8, x, y, 0x5) -> let
          a = ((-) `on` toInteger) (rvg x) (rvg y)
          b = if a <= toInteger (minBound :: Word8) then 1 else 0
        in m & rv %~ x //- fromInteger a & rf .~ b & bgstep
      (0x8, x, y, 0x6) -> let
          a' = rvg y `shiftR` 1
          a = a' .&. 0x7F
          b = if a' .&. 0x80 == 0 then 0 else 1
        in m & rv %~ x //- a & rf .~ b & bgstep
      (0x8, x, y, 0x7) -> let
          a = ((-) `on` toInteger) (rvg y) (rvg x)
          b = if a <= toInteger (minBound :: Word8) then 1 else 0
        in m & rv %~ x //- fromInteger a & rf .~ b & bgstep
      (0x8, x, y, 0xE) -> let
          a' = rvg y `shiftL` 1
          a = a' .&. 0xFE
          b = if a' .&. 0x01 == 0 then 0 else 1
        in m & rv %~ x //- a & rf .~ b & bgstep
      (0x9, x, y, 0x0) ->
        m & pc %~
        (+ if rvg x /= rvg y
          then 2
          else 0)
        & bgstep
      (0xA, a, b, c) -> m & ri .~ comb3 a b c & bgstep 
      (0xB, a, b, c) -> m & pc .~ comb3 a b c + comb2 0 (rvg 0) & bgstep
      (0xC, x, b, c) ->
        let (a, s) = random (m ^. seed) in
        m & seed .~ s & rv %~ x //- (a .&. comb2 b c) & bgstep
      (0xD, x, y, c) -> sprite m x y c & bgstep
      (0xE, x, 0x9, 0xE) -> pressed m x >>= bgstep
      (0xE, x, 0xA, 0x1) -> unpressed m x >>= bgstep
      (0xF, x, b', c') -> case comb2 b' c' of
        0x07 -> m & rv . ix (fromIntegral x) .~ fromIntegral (m ^. td) & bgstep
        0x0A -> key m x >>= bgstep
        0x15 -> m & td .~ fE (rvg x) & bgstep 
        0x18 -> m & ts .~ fE (rvg x) & bgstep 
        0x1E -> m & ri %~ (+ comb2 0 (rvg x)) & bgstep 
        0x29 -> m & ri .~ comb2 0 (x * 5) & bgstep -- TODO: put font at beginning of RAM
        0x33 -> m & ram %~ (// [
          (ri_, rvg x `div` 100),
          (ri_ + 1, (rvg x `mod` 100) `div` 10),
          (ri_ + 2, rvg x `mod` 10)]) & bgstep
        0x55 -> m & ram %~ (// [(ri_ + fE i, rvg i) | i <- [0..x]]) & bgstep
        0x65 -> m & rv %~ (// [(fE i, ram_ !- ri_ + i) | i <- [0..x]]) & bgstep
        _ -> do
          putStr "F:"
          print (x, b', c')
          m & bgstep
      x -> do
        print pc_
        print x
        m & bgstep
