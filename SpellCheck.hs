import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

countItems :: (Ord a) => [a] -> Map a Int
countItems xs = foldl step Map.empty xs
  where step map word = if Map.member word map
                        then Map.adjust (+1) word map
                        else Map.insert word 0 map


splits :: [a] -> [([a], [a])]
splits xs = [(take n xs, drop n xs) | n <- [1..length xs]]


alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

deletes :: String -> Set String
deletes xs = Set.fromList [a ++ drop 1 b | (a, b) <- splits xs]

inserts :: String -> Set String
inserts xs = Set.fromList [a ++ [i] ++ b | (a, b) <- splits xs, i <- alphabet]

replaces :: String -> Set String
replaces xs = Set.fromList [a ++ [i] ++ drop 1 b | (a, b) <- splits xs, i <- alphabet]

--transposes :: String -> Set String
--transposes xs = Set.fromList [dropRight 1 a ++ take | (a, b) <- splits xs]
                                

editOnce :: String -> Set String
editOnce word = Set.unions edits
  where edits = [deletes word, replaces word, inserts word]

editRepeat :: String -> Int -> Set String
editRepeat word 0 = editOnce word
editRepeat word n = Set.unions $ map editOnce $ Set.toList $ editRepeat word (n - 1)


known :: Map String Int -> Set String -> Set String
known table words = Set.filter inTable words
  where inTable word = Map.member word table

candidates :: String -> Map String Int -> Set String
candidates word table = Set.unions [this, edit, editTwice]
  where lookup = known table
        this = lookup $ Set.singleton word
        edit = lookup $ editOnce word
        editTwice = lookup $ editRepeat word 2
