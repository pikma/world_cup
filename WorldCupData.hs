module WorldCupData where

import Data.Char (isSpace)
{- module WorldCupData (readMatches) where -}

import Data.Maybe
import Text.Regex.Posix

import Match

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

parseQualiMatch :: String -> Maybe Match
{- parseQualiMatch :: String -> [[String]] -}
parseQualiMatch s =
  let date_p = "\\w{3}/[0-9]{1,2} 20[0-9]{2}"
      team_p = "([a-zA-Z. ãé/-]+)"
      score_p = "([0-9]+)-([0-9]+)"
      sep_p = "\\s+" in
  let pattern = concat ["^\\s*", date_p, sep_p, team_p, sep_p, score_p, sep_p,
                        team_p, sep_p, "@"] in
  case s =~ pattern of
    [[_, t1, s1, s2, t2]] -> Just Match {
        first_team=trim t1, second_team=trim t2, first_team_goals=read s1,
        second_team_goals=read s2, penalty_winner=Nothing}
    _ -> Nothing

