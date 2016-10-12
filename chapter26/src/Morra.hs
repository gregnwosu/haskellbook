module Morra where
import qualified Data.Map as M
import Control.Monad.State.Lazy
import Control.Monad
import System.Random
import qualified Data.List.Split as SP
import Data.List
import qualified Text.Trifecta as P

data PlayerStats = PlayerStats {p1:: PlayerData, p2:: PlayerData}
data PlayerData = PlayerData {score :: Int, history :: RevHistory}
data Choice = Choice { fingers :: Int , total :: Int}
data Outcome = P1Wins | P2Wins | Draw
type RevHistory =  [Int]
winPoints = 1

instance Show PlayerStats where
    show (PlayerStats (PlayerData p1score (p1:_)) (PlayerData p2score (p2:_))) =
        "\n Player guessed total : " ++ show p1  ++ "\t Player2 guessed total: " ++ show p2 ++
        "\n Player1 score: " ++ show p1score  ++ "\t Player2 score: " ++ show p2score

instance Show Outcome where
    show P1Wins = "\nPlayer1 Wins!"
    show P2Wins = "\nPlayer2 Wins!"
    show Draw   = "\nDraw."

predict :: (PlayerStats -> PlayerData) -> StateT PlayerStats IO Choice
predict f = do
    fingers <-  lift  (randomRIO (0,5) :: IO Int)
    hist <- history . f <$> get
    let last2 = take 2 hist
        predictions = map last $ SP.split (SP.dropBlanks . SP.dropDelims $ SP.onSublist last2) hist
        numChoices = length predictions
        repeatedSequence =  (length last2 == 2) && last2 `isInfixOf` hist && numChoices > 1
    total <- if repeatedSequence then
        do
           idx <- lift $  randomRIO (0, numChoices - 1)
           let predicted = predictions !! idx
           lift $ putStrLn  "\nho ho, Ive seen this before I think Im going to win "
           return $ fingers + predicted
        else
           lift $ randomRIO (fingers,10)
    return (Choice fingers total)


_computerchoice :: StateT PlayerStats IO Choice
_computerchoice  = do
    histRev <-   history . p1 <$> get
    predict p1

_userchoice :: IO Choice
_userchoice = do
  putStr "\nhow many fingers?: "
  fingers' <- getLine
  let (P.Success fingers) = P.parseString P.integer  mempty fingers'
  putStr "\nwhats the total?: "
  totalString <- getLine
  let (P.Success total) = P.parseString P.integer  mempty totalString
  return $ Choice (fromInteger fingers) (fromInteger total)

outcome :: Ordering -> Outcome
outcome x
   | x == LT = P1Wins
   | x == GT = P2Wins
   | otherwise = Draw

updateStats :: Outcome -> PlayerStats -> PlayerStats
updateStats P1Wins (PlayerStats (PlayerData score hist) p2)
    =  PlayerStats (PlayerData (score + winPoints) hist) p2
updateStats P2Wins (PlayerStats p1 (PlayerData score hist))
    = PlayerStats p1 (PlayerData (score + winPoints) hist)
updateStats Draw k
    = k

updateHistory :: Choice -> Choice -> PlayerStats -> PlayerStats
updateHistory (Choice _ p1_total) (Choice _ p2_total) (PlayerStats (PlayerData p1_score p1_hist) (PlayerData p2_score p2_hist)) =
    PlayerStats (PlayerData p1_score (p1_total:p1_hist)) (PlayerData p2_score (p2_total: p2_hist))

morra :: StateT PlayerStats IO ()
morra =  do
  p2_choice <- _computerchoice
  p1_choice <- lift _userchoice
  lift $ putStrLn ("\nplayer fingers : " ++ show (fingers p1_choice ) ++ "\t player2 fingers:" ++ show (fingers p2_choice))
  lift $ putStrLn ("\ntotal is " ++ show (fingers p1_choice + fingers p2_choice))
  let winner = findWinner p1_choice p2_choice
  modify $ updateStats winner . updateHistory p1_choice p2_choice
  lift $ print winner
  stats <- get
  lift $ print stats

main = runStateT (forever morra)  (PlayerStats z z)
       where z = PlayerData 0 []

findWinner :: Choice -> Choice -> Outcome
findWinner (Choice userfingers usertotal) (Choice cpufingers cputotal) =
    let truetotal = userfingers + cpufingers
        userdiff  = abs (usertotal - truetotal)
        cpudiff   = abs (cputotal  - truetotal)
    in  outcome $ compare userdiff cpudiff
