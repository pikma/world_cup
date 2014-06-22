import Data.List
import Match
import qualified Data.Map as Map

import FriendlyData
import Stats

mapToCsv :: (Show a) => (v -> [a]) -> [String] -> Map.Map String v -> [String]
mapToCsv vals headers m =
  (intercalate "," headers):(map f $ Map.toList m) where
    f (k, v) = intercalate "," $ k:(map show $ vals v)

main = do
  matches <- readMatches
  goals_per_team <- return $ avgGoals matches
  writeFile "data/goals_per_team.csv" $ unlines $
      mapToCsv (\(g1, g2) -> [g1, g2])
               ["team", "avg_goals_scored", "avg_goals_received"]
               goals_per_team
  writeFile "data/all_matches.csv" $ unlines $
      "first_team,second_team,first_team_goals,second_team_goals":
          (map toCsv matches)

