module Main (
  main,
) where

import Solutions (solutions)
import CmdArgs
import Control.Lens
import Control.Monad (join, void)
import Data.Bitraversable (Bitraversable (bitraverse))
import qualified Data.Map as Map
import qualified Data.Map.Strict as Map
import DayVersion
import Input (getInput)
import Options.Applicative (execParser)
import System.TimeIt
import Utils ((=:))

runner :: Options -> IO ()
runner o@Options{day, input} = do
  let lastDayNr :: DayVersion
      lastDayRunnner :: String -> IO ()
      (lastDayNr, lastDayRunnner) = Map.findMax solutions
  let func :: String -> IO ()
      func i = case day of
        LastDay ->
          lastDayRunnner i
        SpecificDay d ->
          case Map.lookup d solutions of
            Nothing -> do
              putStrLn $ show d <> " is not implemented."
              putStrLn $ "Currently implemented : " <> unwords (show <$> Map.keys solutions)
            Just dayRunner ->
              dayRunner i
  inputFile <- case input of
    StdIn -> do
      getContents
    File path -> do
      readFile path
    Test -> do
      let path = "inputs/" <> "/" <> show lastDayNr <> "test"
      readFile path
    DayInput -> do
      case day of
        LastDay -> getInput $ getDayNum lastDayNr
        SpecificDay d -> getInput $ getDayNum d
  putStr "> "
  print o
  timeIt $ func inputFile

log s = putStrLn $ "> " <> s

main :: IO ()
main = do
  let parser = cmdParser
  execParser parser >>= void . runner