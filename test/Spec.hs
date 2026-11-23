import Control.Exception (bracket, onException)
import Control.Lens ((^.))
import Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Data_
import System.Directory (removeFile)
import System.IO (hClose, openBinaryTempFile)

assert :: String -> Bool -> IO ()
assert message condition = unless condition (fail message)

withRomFile :: BS.ByteString -> (FilePath -> IO a) -> IO a
withRomFile bytes = bracket createRom removeFile
  where
    createRom = do
      (path, handle) <- openBinaryTempFile "." "terra6-test.ch8"
      (BS.hPut handle bytes >> hClose handle >> pure path)
        `onException` (hClose handle >> removeFile path)

main :: IO ()
main = do
  let bytes = BS.pack [0x00, 0xE0, 0x60, 0x01]
  withRomFile bytes $ \path -> do
    model <- loadRom path
    assert "ROM bytes should begin at address 0x0200"
      (V.toList (V.slice 0x0200 (BS.length bytes) (model ^. ram)) == BS.unpack bytes)
    assert "Loading a ROM should leave the initial PC unchanged"
      (model ^. pc == 0x0200)
  assert "A ROM larger than available RAM should be rejected"
    (case loadRomBytes init' (BS.replicate (ramlen - 0x0200 + 1) 0) of
      Left _ -> True
      Right _ -> False)
