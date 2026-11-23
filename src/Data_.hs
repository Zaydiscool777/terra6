{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module Data_ where

import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import Data.Word (Word8, Word16)
import Control.Lens
import System.Random

import Raylib.Types.Core
import Data.Maybe (fromJust)
import GHC.Stack ( HasCallStack )

ramlen :: Int
ramlen = 0x1000
wordlen :: Int
wordlen = 4
pixelsz :: Int
pixelsz = 10

keymap :: [KeyboardKey]
keymap = [
  KeyX,
  KeyOne,
  KeyTwo,
  KeyThree,
  KeyQ,
  KeyW,
  KeyE,
  KeyA,
  KeyS,
  KeyD,
  KeyZ,
  KeyC,
  KeyFour,
  KeyR,
  KeyF,
  KeyV
  ]
data HexDigit =
  H0 | H1 | H2 | H3 |
  H4 | H5 | H6 | H7 |
  H8 | H9 | HA | HB |
  HC | HD | HE | HF deriving (Bounded, Enum, Eq)

enumPrism :: (Eq s, Bounded a, Enum a) => (a -> s) -> Prism' s a
enumPrism inject = prism' inject (`lookup` [(b, a) | (a, b) <- table])
  where -- prism' :: forall b s a. (b -> s) -> (s -> Maybe a) -> Prism s s a b
    table = [(b, inject b) | b <- [minBound..maxBound]]

hexPrism :: Prism' KeyboardKey HexDigit
hexPrism = enumPrism (\x -> fromJust (lookup x (zip [minBound..maxBound] keymap)))

data Model = Model {
  _ram :: Vector Word8,
  _rv :: Vector Word8,
  _ri :: Word16,
  _td :: Int,
  _ts :: Int,
  _pc :: Word16,
  _sp :: Word16,
  _seed :: StdGen,
  _scr :: Vector (Vector Bool)
}
makeLenses ''Model

init' :: Model
init' = Model {
  _ram = V.replicate 0x1000 0,
  _rv = V.replicate 16 0,
  _ri = 0,
  _td = 0,
  _ts = 0,
  _pc = 0x0200,
  _sp = 0,
  _seed = mkStdGen 0,
  _scr = V.replicate 32 (V.replicate 64 False)
}

loadRomBytes :: HasCallStack => Model -> BS.ByteString -> Model
loadRomBytes m bytes
  | pc_ > V.length ram_ = error "ROM start address is outside RAM"
  | BS.length bytes > V.length ram_ - pc_ = error "ROM does not fit in RAM"
  | otherwise = m & ram .~ (ram_ V.// zip [pc_..] (BS.unpack bytes))
  where
    ram_ = m ^. ram
    pc_ = fromIntegral (m ^. pc)

loadRom :: HasCallStack => FilePath -> IO Model
loadRom = (loadRomBytes init' <$>) . BS.readFile
