import System.Random

main = do
  gen <- getStdGen 
  let rolls = randomRs (0, 37) gen :: [Int]
  -- let results = fixedNumGames oneNumber 50 20 rolls
  let results = fixedAmount oneNumber 10 20 rolls
  putStrLn "games"
  printResults results
  
type Roll = Int
type Rolls = [Roll]
type Money = Int
type Roulette = Roll -> Money
data Wheel = Wheel { bettingStrategy :: Roulette, rolls :: [Int] }

spin :: Wheel -> (Money, Wheel)
spin wheel(bettingStrategy, rolls) = (0, Wheel(bettingStrategy, tail rolls))

fixedNumGames :: Roulette -> Int -> Int -> Rolls -> [Int]
fixedNumGames roulette gamesPerPlayer players rolls 
  | players == 0 = []
  | otherwise = nextPlayerResult : 
                (fixedNumGames roulette gamesPerPlayer (players - 1) nextRolls) 
      where nextPlayerResult = games roulette rolls 50
            nextRolls = drop gamesPerPlayer rolls

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

-- play the specified number of games and return the amount of money remaining
games :: Roulette -> Rolls -> Int -> Money
games roulette rolls numGames 
  | numGames == 0 = 0
  | otherwise = roulette(rolls !! 0) + games roulette (tail rolls) (numGames - 1) 

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

