{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (guard, when, (>=>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative
import SantaLib
import Solutions
import System.Exit (exitFailure, exitSuccess)
import Text.Pretty.Simple (pPrint)
import Text.Read (readMaybe)

data RunConfiguration = RunConfiguration
  { submit :: Bool,
    useExampleInput :: Bool,
    day :: Day,
    part :: Part
  }
  deriving (Show)

configParser :: Parser RunConfiguration
configParser =
  RunConfiguration
    <$> switch (long "submit" <> short 's' <> help "Submit answer instead of just printing.")
    <*> switch (long "example" <> short 'x' <> help "Use example input")
    <*> (argument dayReader (help "Day to run" <> metavar "day") <|> option dayReader (long "day" <> short 'd' <> help "Day to run" <> metavar "day"))
    <*> (argument partReader (help "Part to run" <> metavar "part" <> value Part1) <|> option partReader (long "part" <> short 'p' <> help "Part to run" <> metavar "part"))

dayReader :: ReadM Day
dayReader = maybeReader $ readMaybe >=> mkDay

partReader :: ReadM Part
partReader = maybeReader $ \str -> do
  part <- readMaybe str
  case part of
    1 -> Just Part1
    2 -> Just Part2
    _ -> Nothing

configInfo :: ParserInfo RunConfiguration
configInfo = info (configParser <**> helper) fullDesc

unsolvedMessage :: Day -> Part -> Text
unsolvedMessage day part = "Day " <> T.show (dayInt day) <> ", part " <> T.show (partInt part) <> " is unsolved"

main :: IO ()
main = do
  RunConfiguration {..} <- execParser configInfo
  aocOptions <- getOpts
  fetchDescription aocOptions day
  actualInput <- fetchInput aocOptions day
  exampleInput <- getExample day
  let input = if useExampleInput then exampleInput else actualInput
  let sol = solution day
  let partSol = partSolution sol part
  answer <- case partSol of
    Unsolved -> do
      TIO.putStrLn (unsolvedMessage day part)
      exitFailure
    Solved answer -> answer input
  TIO.putStrLn answer
  if submit
    then do
      (response, result) <- submitAnswer aocOptions day part answer
      TIO.putStrLn (T.pack (showSubmitRes result) <> "\n" <> response)
      case result of
        SubCorrect _ -> exitSuccess
        _ -> exitFailure
    else exitSuccess
