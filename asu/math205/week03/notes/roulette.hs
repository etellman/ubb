import System.Random

main = do
  gen <- getStdGen 
  let infiniteRolls = randomRs (0, 37) gen :: [Int]
  let results = fixedNumGames (Wheel oddEven infiniteRolls) 50 20 

  -- let wheel = Wheel { bettingStrategy = oddEven, rolls = infiniteRolls }
  -- let (amount, newWheel) = spin wheel
  -- print amount

  -- let results = fixedAmount oneNumber 10 20 rolls
  putStrLn "delta.money"
  printResults results

  
type Roll = Int
type Rolls = [Roll]
type Money = Int
type Roulette = Roll -> Money 
data Wheel = Wheel { bettingStrategy :: Roulette, rolls :: [Int] } 
data Player = Player { cash :: Money, gamesPlayed :: Int } 

spin :: Wheel -> (Money, Wheel)
spin (Wheel bettingStrategy rolls) = 
  (bettingStrategy (head rolls), 
    Wheel { bettingStrategy = bettingStrategy, rolls = (tail rolls) })

fixedNumGames :: Wheel -> Int -> Int -> [Money]
fixedNumGames wheel gamesPerPlayer players 
  | players == 0 = []
  | otherwise = nextPlayerResult : 
                (fixedNumGames nextWheel gamesPerPlayer (players - 1)) 
      where (nextPlayerResult, nextWheel) = games wheel 50

-- play the specified number of games and return the amount of money remaining
games' :: Wheel -> Player -> (Wheel, Player)
games' wheel player 
  | gamesPlayed player == 50 = (wheel player)
  | otherwise = games' nextWheel nextPlayer
      where (result, nextWheel) = spin wheel
            nextPlayer = (Player (cash player + result) (gamesPlayed player + 1))

-- play the specified number of games and return the amount of money remaining
games :: Wheel -> Int -> (Money, Wheel)
games wheel numGames 
  | numGames == 0 = (0, wheel)
  | otherwise = (bet + otherBets, lastWheel)
      where (bet, nextWheel) = spin wheel
            (otherBets, lastWheel) = games nextWheel (numGames - 1)

-- play with some number of players, each with a fixed amount of money, returning the 
-- number of games it took them to loose all their money for each player
fixedAmount :: Roulette -> Int -> Money -> Rolls -> [Int]
fixedAmount roulette players cash rolls 
  | players == 0 = []
  | otherwise = result : (fixedAmount roulette (players - 1) cash (drop result rolls)) 
      where result = spendAll roulette cash rolls 

-- return the number of games it takes to spend all your money 
spendAll :: Roulette -> Money -> Rolls -> Int
spendAll roulette cash rolls 
  | cash == 0 = 0
  | otherwise = 1 + spendAll roulette (cash + roulette(rolls !! 0)) (tail rolls)  

-- play one game of roulette
roulette :: Int -> Money -> Roll -> Money
roulette waysToWin payout roll 
  | roll < waysToWin = payout
  | otherwise = -1

oddEven :: Roulette
oddEven = roulette 18 1

fourNumbers :: Roulette
fourNumbers = roulette 4 8

oneNumber :: Roulette
oneNumber = roulette 1 35

printResults :: [Int] -> IO()
printResults [] = return ()
printResults (x:xs) = do
  print x
  printResults xs

