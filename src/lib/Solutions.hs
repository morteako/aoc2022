module Solutions where

import qualified Day.Day01

import Data.Map (Map)
import qualified Data.Map as Map
import DayVersion (DayVersion (NormalDay, SpecialVersion))
import Utils

solutions :: Map DayVersion (String -> IO ())
solutions =
  Map.fromList
    [ "01" =: Day.Day01.run
    ]
