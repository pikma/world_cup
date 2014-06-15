module Match (Team, Match(..)) where

type Team = String

data Match = Match {
  first_team :: Team,
  second_team :: Team,
  first_team_goals :: Int,
  second_team_goals :: Int,
  penalty_winner :: Maybe Team
} deriving (Show)

