module Solutions where

import Day.Day01 qualified
import Day.Day02 qualified
import Day.Day03 qualified
import Day.Day03Monoid qualified
import Day.Day04 qualified
import Day.Day05 qualified
import Day.Day05StateLens qualified
import Day.Day06 qualified

import Data.Map (Map)
import Data.Map qualified as Map
import Day.Day07 qualified
import DayVersion (DayVersion (NormalDay, SpecialVersion))
import Utils

solutions :: Map DayVersion (String -> IO ())
solutions =
  Map.fromList
    [ "01" =: Day.Day01.run
    , "02" =: Day.Day02.run
    , "03" =: Day.Day03.run
    , "03Monoid" =: Day.Day03Monoid.run
    , "04" =: Day.Day04.run
    , "05" =: Day.Day05.run
    , "05StateLens" =: Day.Day05StateLens.run
    , "06" =: Day.Day06.run
    , "07" =: Day.Day07.run
    ]
