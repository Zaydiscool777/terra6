module Lower where

import Control.Lens
import Data.Word
import Data.Vector

import Data_

clrscr m = m & scr %~ (0 <$)
ret m = _
call m x = _
draw m = _
pressed m x = _
unpressed m x = _
key m x = _
