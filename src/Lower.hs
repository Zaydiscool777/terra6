{-# LANGUAGE LexicalNegation, BlockArguments #-}
module Lower where

import Data_

import Control.Lens
import Data.Word
import Data.Vector ( Vector, (!), (//) )
import Data.Bits
import Data.Foldable.WithIndex
import Control.Monad

import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Util.Colors

comb3 :: (Integral a2, Integral a3, Integral a4, Num a5) => a2 -> a3 -> a4 -> a5
comb3 a b = comb2 a . comb2 b
comb2 :: (Integral a1, Integral a2, Num a3) => a1 -> a2 -> a3
comb2 b c = fromInteger (toInteger b * 256 + toInteger c)

infix 7 !-
(!-) :: Enum a1 => Vector a2 -> a1 -> a2
a !- b = a ! fromEnum b

infix 5 //-
(//-) :: Enum a1 => a1 -> a2 -> Vector a2 -> Vector a2
b //- c = (// [(fromEnum b, c)])

fE :: Enum a => a -> Int
fE = fromEnum

fI :: (Integral a, Num b) => a -> b
fI = fromIntegral

bgstep :: Model -> IO Model
bgstep m = do
  -- print (_ram m)
  beginDrawing
  iforM_ (m ^. scr) \ri' ->
    imapM_ \ci x -> do
      let k = pixelsz
      drawRectangle (ci * k) (ri' * k) k k if x then white else black
  endDrawing
  return $ m
    & pc %~ (+ 2)
    & td %~ (max 0 . pred)
    & ts %~ (max 0 . pred)

word8ToBits :: Word8 -> [Bool]
word8ToBits = (<$> [7,6..0]) . testBit

clrscr, ret :: Model -> Model
pressed, unpressed, key :: Model -> Word8 -> IO Model

clrscr m = m & scr %~ fmap (False <$)
sprite :: Model -> Word8 -> Word8 -> Word8 -> Model
sprite m'' x y h' =
  let
    (vf, m') = foldr (\h s ->
      foldr (\b' (vf', m) ->
        let
          row = (fE (m ^. rv !- y) + fE h) `mod` 32
          col = (fE (m ^. rv !- x) + fE b') `mod` 64
          bit' = testBit ((m ^. ram) ! (fE (m ^. ri) + fE h)) (7 - fE b')
          pix = (m ^. scr) ! row ! col
        in (vf' || pix && bit', m & scr . ix row . ix col .~ (pix /= bit'))
      ) s [0 :: Integer ..7]) (False, m'') [0..pred h']
  in m' & rv . ix 0xF .~ if vf then 1 else 0
-- set vf to 0
-- for each h' in [0..pred h]
  -- for each b' in [0..7]
    -- take the BIT p at (vi + h') * 8 + b'
    -- look at the pixel at (y * 64) + x
    -- set vf to vf on or (pixel on and bit on)
    -- set pixel to pixel on xor bit on

ret m = m & pc .~ m ^. sp & sp %~ (+ 2)
call :: Model -> Word16 -> Model
call m x = m & sp %~ (- 2)
  & ram . ix (fE (m ^. sp - 2)) .~ fI x
  & ram . ix (fE (m ^. sp - 1)) .~ fI (x `shiftR` 8)
  & pc .~ x

pressed m x = do
  b <- isKeyDown (review hexPrism $ toEnum $ fI $ x .&. 0xF)
  return $ m & pc +~ if b then 4 else 2
unpressed m x = do
  b <- isKeyUp (review hexPrism $ toEnum $ fI $ x .&. 0xF)
  return $ m & pc +~ if b then 4 else 2
key m x = do
  let
    gk m' = do
      k <- getKeyPressed
      m'' <- bgstep m'
      maybe (gk m'') (return . (,) m'') (preview hexPrism k)
  (m', k) <- gk m
  return $ m' & ram %~ (x //- toEnum (fE k))
