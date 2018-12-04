module Day04a where

import Data.List
import Data.Time
import Text.Regex.PCRE
import qualified Data.Map.Strict as M

-- Time, action
type LogEntry = (UTCTime, Action)
data Action = Sleep | WakeUp | BeginShift Int deriving (Show)
data State = Awake | Asleep deriving (Eq)
type MinutesSlept = M.Map Int Int

main :: IO ()
main = do
  s <- readFile "input/Day04_test.txt"
  mapM_ print $ sortOnDate $ parseInput s

sampleEntry :: String
sampleEntry = "[1518-10-14 00:05] falls asleep"

addMinutesSlept :: Int -> Int -> MinutesSlept -> MinutesSlept
addMinutesSlept = M.insertWith (+)

step :: MinutesSlept -> (Int, State, UTCTime) -> LogEntry -> (MinutesSlept, (Int, State, UTCTime))
step ms (id, state, start) (end, action) =
  case action of
    Sleep -> (ms, (id, Asleep, end))
    WakeUp -> (addMinutesSlept id minutes ms, (id, Awake, end))
    BeginShift newId ->
      if state == Awake then (ms, (newId, Awake, end))
      else (addMinutesSlept id minutes ms, (newId, Awake, end))
  where
    minutes = round $ diffUTCTime end start

sortOnDate :: [LogEntry] -> [LogEntry]
sortOnDate = sortOn fst

-- Parsing
parseInput :: String -> [LogEntry]
parseInput = map parseLogEntry . lines

parseLogEntry :: String -> LogEntry
parseLogEntry s = (parseDateTime timePart, parseAction actionPart)
  where
    timePart = s =~ "(?<=\\[).*(?=\\])"
    actionPart = s =~ "(?<=\\] ).*"

parseDateTime :: String -> UTCTime
parseDateTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M"

parseAction :: String -> Action
parseAction "falls asleep" = Sleep
parseAction "wakes up" = WakeUp
parseAction s = BeginShift (read (extractID s))
  where
    extractID :: String -> String
    extractID action = action =~ "\\d+"
