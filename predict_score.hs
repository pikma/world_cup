import Data.List
import Data.List.Split
import qualified Data.Map as Map
import GHC.Exts (sortWith)
import qualified Statistics.Distribution as D
import qualified Statistics.Distribution.Poisson as D.P

import Match

-- The parameters of one team in the Poisson model: (offense, defense).
type TeamPoissonParams = (Double, Double)

data PoissonModel = PoissonModel {
  min_lambda :: Double,
  team_params :: Map.Map Team TeamPoissonParams
} deriving (Show)

allTeams :: PoissonModel -> [Team]
allTeams = Map.keys . team_params

modelFromString :: String -> PoissonModel
modelFromString s =
  PoissonModel {
    team_params = foldl f Map.empty $ map teamParamsFromString $ lines s,
    min_lambda = 0.3
  }
  where f m (team, params) = Map.insert team params m

teamParamsFromString :: String -> (Team, TeamPoissonParams)
teamParamsFromString s =
  let [team_name, offense_str, defense_str] = splitOn "," s in
    (read team_name, (read offense_str, read defense_str))

type Score = (Int, Int)

scoreProbability :: PoissonModel -> Team -> Team -> Score -> Double
scoreProbability m t1 t2 (s1, s2) =
  (goalsProbability m t1 t2 s1) * (goalsProbability m t2 t1 s2)

goalsProbability :: PoissonModel -> Team -> Team -> Int -> Double
goalsProbability m t1 t2 s =
  let (offense, _) = (team_params m) Map.! t1
      (_, defense) = (team_params m) Map.! t2 in
  let l = lambda (min_lambda m) offense defense in
  let d = D.P.poisson l in D.probability d s

lambda :: Double -> Double -> Double -> Double
lambda min_lambda offense defense = max (offense - defense) min_lambda

data Outcome = FirstWins | SecondWins | Draw deriving Eq

outcome :: Score -> Outcome
outcome (g1, g2)
  | g1 > g2   = FirstWins
  | g1 < g2   = SecondWins
  | otherwise = Draw

-- The number of points given for predicting the first score when the actual
-- score is the second. We use Superbru's formula, see:
-- http://www.superbru.com/worldcup/how_to_play_scoring.php.
predictionPoints :: Score -> Score -> Double
predictionPoints predicted final =
  let winning_points = if outcome predicted == outcome final then 1.0 else 0.0 in
  let closeness (ge1, ge2) (gf1, gf2) =
         let gd = abs ((ge1 - ge2) - (gf1 - gf2))
             gt = abs ((ge1 + ge2) - (gf1 + gf2)) in
               (fromIntegral gd :: Double) + (fromIntegral gt :: Double) / 2  in
  let c = closeness predicted final in
  let margin_points = if c == 0.0      then 1.0
                      else if c <= 1.0 then 0.75
                      else if c <= 1.5 then 0.5
                      else if c <= 2.0 then 0.25
                      else 0.0
        in winning_points + margin_points

allScores :: Int -> [Score]
allScores max_num_goals =
  let possible_goals = [0..max_num_goals] in
    [(i, j) | i <- possible_goals, j <- possible_goals]

expectedPoints :: PoissonModel -> Team -> Team -> Score -> Double
expectedPoints m t1 t2 s =
  let all_scores = allScores 10 in
  let probs = map (scoreProbability m t1 t2) all_scores
      points = map (predictionPoints s) all_scores in
    sum $ zipWith (*) probs points

bestPredictions :: PoissonModel -> Team -> Team -> [(Score, Double)]
bestPredictions m t1 t2 =
  let all_scores_and_points =
         [(s, expectedPoints m t1 t2 s) | s <- (allScores 10)] in
    sortWith (negate . snd) all_scores_and_points

promptForTeam :: [Team] -> String -> IO Team
promptForTeam teams msg = do
  putStrLn msg
  team <- getLine
  if elem team teams then return team
                     else promptForTeam teams "Invalid team. Try again:"

main = do
  contents <- readFile "data/poisson_model.csv"
  m <- return $ modelFromString contents
  t1 <- promptForTeam (allTeams m) "First team?"
  t2 <- promptForTeam (filter (/= t1) $ allTeams m) "Second team?"
  putStrLn $ "\n" ++ t1 ++ " vs. " ++ t2
  best_predictions <- return $ take 6 $ bestPredictions m t1 t2
  mapM_ (putStrLn . f) best_predictions
    where f (score, points) =
            (show score) ++ " --> " ++ (show points) ++ " pts"
