{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (guard, when, (>=>))
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Network.Wai.Handler.Warp (runEnv)
import Options.Applicative
import SantaLib
import SantaLib.Service
import SantaLib.Service (AocAPI)
import Servant
import Solutions
import System.Exit (exitFailure, exitSuccess)
import System.IO
import Text.Pretty.Simple (pPrint)
import Text.Read (readMaybe)

data InputSource = ExampleInput | AocUserInput | ExplicitInput Text
  deriving (Show, Eq)

inputSourceParser :: Parser InputSource
inputSourceParser =
  asum
    [ ExampleInput <$ flag' True (long "example" <> short 'x' <> help "Use example input"),
      ExplicitInput <$> option str (long "input" <> short 'i' <> help "Input for part" <> metavar "input"),
      AocUserInput <$ flag True True (long "userInput" <> help "Use user input (default)")
    ]

data RunConfiguration
  = Serve
      { port :: Int
      }
  | RunConfiguration
      { submit :: Bool,
        inputSource :: InputSource,
        day :: Day,
        part :: Part
      }
  deriving (Show)

cliConfigParser :: Parser RunConfiguration
cliConfigParser =
  RunConfiguration
    <$> switch (long "submit" <> short 's' <> help "Submit answer instead of just printing.")
    <*> inputSourceParser
    <*> (argument dayReader (help "Day to run" <> metavar "day") <|> option dayReader (long "day" <> short 'd' <> help "Day to run" <> metavar "day"))
    <*> (argument partReader (help "Part to run" <> metavar "part" <> value Part1) <|> option partReader (long "part" <> short 'p' <> help "Part to run" <> metavar "part"))

configParser :: Parser RunConfiguration
configParser =
  (Serve <$ flag' True (long "serve" <> help "Run as restful service"))
    <*> option auto (long "port" <> short 'p' <> help "port" <> metavar "port" <> value 8080)
      <|> cliConfigParser

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

runCli :: Bool -> InputSource -> Day -> Part -> IO ()
runCli submit inputSource day part = do
  aocOptions <- getOpts
  fetchDescription aocOptions day
  actualInput <- fetchInput aocOptions day
  exampleInput <- getExample day
  let input = case inputSource of
        ExampleInput -> exampleInput
        AocUserInput -> actualInput
        ExplicitInput i -> i
  let sol = solution day
  let partSol = partSolution sol part
  answer <- case partSol of
    Unsolved -> do
      TIO.putStrLn (unsolvedMessage day part)
      exitFailure
    Solved answer -> do
      putStrLn "Running solution"
      answer input
  TIO.putStrLn answer
  if submit
    then do
      (response, result) <- submitAnswer aocOptions day part answer
      TIO.putStrLn (T.pack (showSubmitRes result) <> "\n" <> response)
      case result of
        SubCorrect _ -> exitSuccess
        _ -> exitFailure
    else exitSuccess

runServe :: Int -> IO ()
runServe port = do
  putStrLn $ "Aoc up and running on port: " <> show port
  hFlush stdout
  runEnv port $ serve aocApi $ health :<|> handle
  where
    health :: Handler Text
    health = return "ok"
    handle :: AocSolutionRequest -> Handler AocSolutionResponse
    handle req = do
      let sol = solution req.day
      let partSol = partSolution sol req.part
      case partSol of
        Unsolved -> return $ AocSolutionResponse req Nothing
        Solved ans -> AocSolutionResponse req . Just <$> liftIO (ans req.input)

main :: IO ()
main =
  execParser configInfo >>= \case
    RunConfiguration {..} -> runCli submit inputSource day part
    Serve {..} -> runServe port
