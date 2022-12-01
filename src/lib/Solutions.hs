module Solutions where

import Day.Day01 qualified

import Data.Map (Map)
import Data.Map qualified as Map
import DayVersion (DayVersion (NormalDay, SpecialVersion))
import Utils

solutions :: Map DayVersion (String -> IO ())
solutions =
  Map.fromList
    [ "01" =: Day.Day01.run
    ]
