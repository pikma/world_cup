module FriendlyData (readMatches) where

import Data.List
import Text.Regex.Posix

import Match

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p l = reverse $ map reverse $ foldl f [] l where
  f [] e = [[e]]
  f res@(x:xs) e = if p e then [e]:res else (e:x):xs

isLineDate :: String -> Bool
isLineDate s = s =~ "^[0-9]{2}\\.[0-9]{2}\\.\\s+[0-9]{2}:[0-9]{2}"

parseGoals :: String -> (Int, Int)
parseGoals s =
  let [[_, s1, s2]] = s =~ "([0-9]+)\\s*:\\s*([0-9]+)" in (read s1, read s2)

parseMatch :: [String] -> Match
parseMatch (l1:l2:l3:l4:xs) =
  let first_team = l2
      second_team = l3
      (first_team_goals, second_team_goals) = parseGoals l4
      penalty_winner = case xs of
        []  -> Nothing
        [s] -> let goals_str = filter (\x -> (x /= '(') && (x /= ')'))
                                      s in
               let (g1, g2) = parseGoals goals_str in
                     if g1 > g2 then Just first_team
                                else Just second_team
        _ -> error "Too many lines for match block"
  in
    Match {first_team=first_team, second_team=second_team,
           first_team_goals=first_team_goals,
           second_team_goals=second_team_goals, penalty_winner=penalty_winner}
parseMatch _ = error "Not enough lines in Match string representation"

isJunior :: Team -> Bool
isJunior team = team =~ "U[0-9]{2}"

isValid :: Match -> Bool
isValid m = not $ any isJunior [first_team m, second_team m]

readMatchesFromLines :: [String] -> [Match]
readMatchesFromLines all_lines =
  let match_str_blocks = splitWhen isLineDate all_lines in
  let all_matches = map parseMatch match_str_blocks in
    filter isValid all_matches

readMatches :: IO [Match]
readMatches =
  let years = [2014, 2013] in
  let files = map (\y -> "friendly_data/" ++ (show y) ++ ".txt") years in
  let contents = mapM readFile files in
  let all_lines = fmap (concat . map lines) contents in
    fmap readMatchesFromLines all_lines
