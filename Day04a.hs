module Day04a where

import Data.List
import Data.Time
import Text.Regex.PCRE
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

-- Time, action
type LogEntry = (UTCTime, Action)
data Action = Sleep | WakeUp | BeginShift Int deriving (Show)
data State = Awake | Asleep deriving (Eq, Show)
-- Tuple consists of ID and state. Index can be used to retrieve the minute.
type StateAtMinute = V.Vector (Int, State)

main :: IO ()
main = do
  s <- readFile "input/Day04.txt"
  print $ solve s

sampleEntry :: String
sampleEntry = "[1518-10-14 00:05] falls asleep"

solve :: String -> Int
solve s = let entries = parseInput s
              sam = buildStateAtMinuteMap entries
              minutesSlept = getMinutesSlept sam M.empty
              longestSleeper = getLongestSleeper minutesSlept
              bestMinute = getKeyForMaxValue $ getTimesSleptAtMinuteForId sam longestSleeper
              in bestMinute

getKeyForMaxValue :: Ord a => M.Map b a -> b
getKeyForMaxValue m = k
  where
    (k,_) = last $ sortOn snd $ M.toList m

getTimesSleptAtMinuteForId :: StateAtMinute -> Int -> M.Map Int Int
getTimesSleptAtMinuteForId sam id =
  foldl updateMap M.empty indexed
  where
    indexed = zip [0..] sam
    updateMap c (i, (id', state)) = case state of
      Awake -> c
      Asleep -> if id' == id then M.insertWith (+) (i `mod` 60) 1 c
                else c

getLongestSleeper :: M.Map Int Int -> Int
getLongestSleeper = getKeyForMaxValue

getMinutesSlept :: StateAtMinute -> M.Map Int Int -> M.Map Int Int
getMinutesSlept sams ms
  | V.null sams = ms
  |
getMinutesSlept [] ms = ms
getMinutesSlept ((id,Awake):sams) ms = getMinutesSlept sams ms
getMinutesSlept ((id,Asleep):sams) ms = getMinutesSlept sams (M.insertWith (+) id 1 ms)

buildStateAtMinuteMap :: [LogEntry] -> StateAtMinute
buildStateAtMinuteMap entries =
  fst $ foldl (\(sam, prev) entry -> step sam prev entry) firstStateAtMinute (tail entries)
  where
    (firstTime, firstAction) = head entries
    firstId = case firstAction of BeginShift id -> id
    firstStateAtMinute = ([], (firstId, Awake, firstTime))

addState :: StateAtMinute -> Int -> State -> Int -> StateAtMinute
addState sam id s m = sam ++ replicate m (id, s)

step :: StateAtMinute -> (Int, State, UTCTime) -> LogEntry -> (StateAtMinute, (Int, State, UTCTime))
step sam (id, state, start) (end, action) =
  case action of
    Sleep -> (addState sam id Awake minutes, (id, Asleep, end))
    WakeUp -> (addState sam id Asleep minutes, (id, Awake, end))
    BeginShift newId ->
      if state == Awake then (addState sam id Awake minutes, (newId, Awake, end))
      else (addState sam id Asleep minutes, (newId, Awake, end))
  where
    minutes = (`div` 60) $ round $ diffUTCTime end start

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
