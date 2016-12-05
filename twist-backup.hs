import Language.Words (allStringWords)
import Data.Set (fromList, member, filter, Set)
import Data.List (nub, permutations, subsequences, groupBy, sortBy, all)
import Data.List.Split (chunksOf)
import Data.Char (toLower, isLower)
import Data.Ord (comparing)
import Data.Map (toList, fromList, Map, size, elems, member, update)
import Text.PrettyPrint.Boxes

safeWords :: Set String
safeWords = Data.Set.filter (notElem '\233')
          $ Data.Set.filter (notElem '\'')
          $ Data.Set.filter (all isLower) allWords
  where allWords = Data.Set.fromList allStringWords

getSubWords :: String -> [String]
getSubWords = nub . sortBy (comparing length) . subWords
  where
    subWords word = concatMap (`ofLength` word) [3..(length word)]
    ofLength n = Prelude.filter (`Data.Set.member` safeWords)
                . concatMap permutations
                . Prelude.filter (\x -> length x == n) . subsequences
                . Prelude.map toLower

startingState :: String -> [String] -> Map String Bool
startingState seed subWords = Data.Map.fromList
                            $ Prelude.map obscure subWords
  where obscure word
          | word == seed = (word, True)
          | otherwise    = (word, False)

showState :: Map String Bool -> IO ()
showState = printTable . chunksOf 4 . stateToArray
  where
    stateToArray = map (uncurry render) . sortBy keyLen . Data.Map.toList
    render k v -- (\(k,v) -> render k v)
      | v   = k
      | otherwise = replicate (length k) '_'
   -- | otherwise = k
    keyLen (k1, _) (k2, _) = compare (length k1) (length k2)
  --printTable :: [[String]] -> IO ()
    printTable rows = printBox $ hsep 2 left (map (vcat left . map text) rows)

play :: Map String Bool -> IO ()
play state
  | gameOver state = win
  | otherwise      = move state
  where
    move state = do
      putStrLn " - Please enter a guess:"
      guess <- getLine
      let (correct, newState) = guessWord guess state
      showState newState
      play newState
    win = putStrLn " - You win!"
  --gameOver :: Map String Bool -> Bool
    gameOver state = Data.List.all (==True) $ Data.Map.elems state
  --guessWord :: String -> Map String Bool -> (Bool, Map String Bool)
    guessWord guess state
      | Data.Map.member guess state = (True, newState guess state)
      | otherwise = (False, state)
    newState = Data.Map.update (\_ -> Just True)

main = do
  putStrLn " - T E X T   T W I S T"
  putStrLn " - Please enter a seed word:"
  seed <- getLine
  let subWords = getSubWords seed
  let state = startingState seed subWords
  showState state
  play state
