module Match (Team, Match(..), toCsv) where

import qualified Data.List

type Team = String

data Match = Match {
  first_team :: Team,
  second_team :: Team,
  first_team_goals :: Int,
  second_team_goals :: Int,
  penalty_winner :: Maybe Team
} deriving (Show)

toCsv :: Match -> String
toCsv m = Data.List.intercalate "," [first_team m, second_team m,
                                     show $ first_team_goals m,
                                     show $ second_team_goals m]
