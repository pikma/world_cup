module WorldCupData where

import Control.Applicative
import Data.Char (isSpace)
import Data.Maybe
import GHC.IO.Encoding
import System.Directory
import System.IO
import Text.Regex.Posix

import Match

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

team_p = "(['a-zA-Z. ãéô/-]+)"
score_p = "([0-9]+)-([0-9]+)"
sep_p = "\\s+"

matchFromRegexResult :: [[String]] -> Maybe Match
matchFromRegexResult l = case l of
    [[_, t1, s1, s2, t2]] -> Just Match {
        first_team=trim t1, second_team=trim t2, first_team_goals=read s1,
        second_team_goals=read s2, penalty_winner=Nothing}
    _ -> Nothing

parseQualiMatch :: String -> Maybe Match
parseQualiMatch s =
  let date_p = "\\w{3}/[0-9]{1,2} 20[0-9]{2}" in
  let pattern = concat ["^\\s*", date_p, sep_p, team_p, sep_p, score_p, sep_p,
                        team_p, sep_p, "@"] in
  matchFromRegexResult (s =~ pattern)

parseCupMatch :: String -> Maybe Match
parseCupMatch s =
  let date_p = ".* [0-0]{1,2}:[0-9]{2}" in
  let pattern = concat [date_p, team_p, score_p, ".*", team_p, sep_p, "@"] in
  matchFromRegexResult (s =~ pattern)

parseMatchesFromContents :: (String -> Maybe Match) -> String -> [Match]
parseMatchesFromContents f = (mapMaybe f) . lines

qualiFiles :: IO [FilePath]
qualiFiles =
  let dir =  "data/world_cup/2014--brazil/quali/" in do
    all_files <- getDirectoryContents dir
    return $ map (dir ++) $ filter (\x -> x =~ "\\.txt$") all_files

cupFiles :: IO [String]
cupFiles =
  let dir = "data/world_cup/2014--brazil/" in do
    all_files <- getDirectoryContents dir
    return $ map (dir ++) $ filter (\x -> x =~ "^cup.*\\.txt$") all_files

readMatchesFromFiles :: [FilePath] -> (String -> Maybe Match) -> IO [Match]
readMatchesFromFiles files parse_fn = do
  file_contents <- mapM readFile files
  return $ concatMap (parseMatchesFromContents parse_fn) file_contents

readMatches :: IO [Match]
readMatches = do
  setLocaleEncoding utf8
  cup_files <- cupFiles
  cup_matches <- readMatchesFromFiles cup_files parseCupMatch
  quali_files <- qualiFiles
  quali_matches <- readMatchesFromFiles quali_files parseQualiMatch
  return $ cup_matches ++ quali_matches
