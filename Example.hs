import SpellCheck
import System.IO
import Data.Map (Map)
import qualified Data.Map as Map

countItems :: (Ord a) => [a] -> Map a Int
countItems xs = foldl step Map.empty xs
  where step map word = if Map.member word map
                        then Map.adjust (+1) word map
                        else Map.insert word 0 map

spellCheckLoop :: Map String Int -> IO ()
spellCheckLoop table = do
  line <- getLine
  if null line
    then putStrLn "Done"
    else do putStrLn $ show $ candidates line table
            spellCheckLoop table

main = do
  trainingWords <- fmap words $ withFile "big.txt" ReadMode hGetContents
  spellCheckLoop $ countItems trainingWords
  
