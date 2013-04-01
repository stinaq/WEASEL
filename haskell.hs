import Control.Monad (liftM, replicateM)
import System.Random (newStdGen, randomRIO)
import Data.List (sortBy)
import Data.Function (on)

generationSize = 100
mutationRate = 0.04
goal = "METHINKS IT IS LIKE A WEASEL"
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ "

randomString :: IO String
randomString = replicateM (length goal) randomCharacter

randomCharacter :: IO Char
randomCharacter = newStdGen 
	>>  randomRIO (0, length alphabet - 1) 
	>>= return . (!!) alphabet

mutate :: Char -> IO Char
mutate c = newStdGen
	>> (randomRIO (0.0, 1.0) :: IO Float)
	>>= \r -> if r < mutationRate then randomCharacter else return c

procreate :: String -> IO [String]
procreate parent = mapM (mapM mutate) (replicate (length goal) parent)

findFittest :: [String] -> String
findFittest = fst . head . reverse . sortBy (compare `on` snd) . map fitness 

fitness :: String -> (String, Int)
fitness s = (,) s . length . filter (\(x,y) -> x == y) . zip goal $ s

evolve :: String -> IO String
evolve current
  | goal == current = return current
  | otherwise       = do
		progeny <- procreate current
		let fittest = findFittest progeny
		putStrLn fittest
		evolve fittest

main = do
	current <- randomString
	evolve current
