module Stats where

import qualified Data.Map as Map
import Data.Monoid
import Data.List

import Match
import FriendlyData

numTeams :: [Match] -> Int
numTeams matches = length $ nub $ ((map first_team) matches) ++
                                  ((map second_team) matches)

-- The function count returns a pair (c1, c2) where c1 is the count increment
-- for the first team, and c2 for the second team.
countPerTeam :: (Monoid c) => (Match -> (c, c)) -> [Match] -> Map.Map Team c
countPerTeam count = foldl f Map.empty where
  f map match =
    let (c1, c2) = count match in
    Map.insertWith mappend (first_team match) c1 $
    Map.insertWith mappend (second_team match) c2 map

countScoredGoals =
  countPerTeam  (\m -> (Sum $ first_team_goals m, Sum $ second_team_goals m))

countReceivedGoals =
  countPerTeam (\m -> (Sum $ second_team_goals m, Sum $ first_team_goals m))

countGoals = countPerTeam f where
  f m = let (g1, g2) = (Sum $ first_team_goals m, Sum $ second_team_goals m) in
        ((g1, g2), (g2, g1))

printGeneralStats = do
  matches <- readMatches
  putStrLn $ "Number of matches: " ++ show (length matches)
  putStrLn $ "Number of teams: " ++ show (numTeams matches)
  putStrLn "Number of matches: "
  putStrLn $ show $ reverse $ sort $ Map.elems $
    countPerTeam (return (Sum 1, Sum 1)) matches
  putStrLn "Number of goals scored: "
  putStrLn $ show $ reverse $ sort $ Map.elems $ countScoredGoals matches

printCsv :: (Show a, Show k) => (v -> [a]) -> Map.Map k v -> IO ()
printCsv vals = mapM_ f . Map.toList where
  f (k, v) = putStrLn $ intercalate ", " $ (show k):(map show $ vals v)

main = do
  matches <- readMatches
  goals_per_team <- return $ countGoals matches
  printCsv (\(g1, g2) -> map getSum [g1, g2]) goals_per_team


