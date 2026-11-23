{-# LANGUAGE LexicalNegation, BlockArguments #-}
module Main (main) where

import Data_
import Lib
import Raylib.Core
import Raylib.Types
import Control.Monad (unless)

forever' :: (a -> IO a) -> a -> IO ()
forever' x i = do
  r <- x i
  b <- windowShouldClose
  unless b (forever' x r)

main :: IO ()
main = do
  _ <- initWindow (64 * pixelsz) (32 * pixelsz) "emulator"
  setTargetFPS 6 -- 60
  setExitKey KeyEscape
  m <- loadRom "file.ch8"
  forever' step m
