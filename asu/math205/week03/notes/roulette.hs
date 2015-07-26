import System.Random

main = do
  gen <- getStdGen 
  let infiniteRolls = randomRs (0, 37) gen :: [Int]
  let wheel = (Wheel oneNumber infiniteRolls)
  let numPlayers = 1000

  -- let startingCash = 0
  -- let results = simulation (nGames 50) startingCash wheel numPlayers 

  let startingCash = 50
  let results = simulation spendAll startingCash wheel numPlayers 

  -- let wheel = Wheel { bettingStrategy = oddEven, rolls = infiniteRolls }
  -- let (amount, newWheel) = spin wheel
  -- print amount
  -- let results = fixedAmount oneNumber 10 20 rolls
  putStrLn "cash,games.played"
  printResults results

  
type Roll = Int
type Rolls = [Roll]
type Money = Int
type DonePlaying = Player -> Bool
type Roulette = Roll -> Money 
data Wheel = Wheel { bettingStrategy :: Roulette, rolls :: [Int] } 
data Player = Player { cash :: Money, gamesPlayed :: Int } 

-- a single spin of the wheel, returning the result and the next wheel to spin
spin :: Wheel -> (Money, Wheel)
spin (Wheel bettingStrategy rolls) = 
  (bettingStrategy (head rolls), 
    Wheel { bettingStrategy = bettingStrategy, rolls = (tail rolls) })

-- simulates some number of players playing roulette
simulation :: DonePlaying -> Money -> Wheel -> Int -> [Player]
simulation donePlaying startingCash wheel players 
  | players == 0 = []
  | otherwise = nextPlayer : 
                (simulation donePlaying startingCash nextWheel (players - 1)) 
      where (nextWheel, nextPlayer) = games donePlaying wheel (Player startingCash 0)

nGames :: Int -> Player -> Bool
nGames numGames player = gamesPlayed player >= numGames 

spendAll :: Player -> Bool
spendAll player = cash player <= 0

-- play the specified number of games and return the amount of money remaining
games :: DonePlaying -> Wheel -> Player -> (Wheel, Player)
games donePlaying wheel player 
  | donePlaying player = (wheel, player)
  | otherwise = games donePlaying nextWheel nextPlayer
      where (result, nextWheel) = spin wheel
            nextPlayer = (Player (cash player + result) (gamesPlayed player + 1))

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

printResults :: [Player] -> IO()
printResults [] = return ()
printResults (player:players) = do
  let string = (show $ cash player) ++ "," ++ (show $ gamesPlayed player)
  putStrLn string
  printResults players

