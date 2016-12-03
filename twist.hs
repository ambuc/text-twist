import Data.Char
import Data.List.Split
import Data.Ord
import Language.Words
import Text.PrettyPrint.Boxes
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

safeWords :: S.Set String
safeWords = S.filter (notElem '\233')
          $ S.filter (notElem '\'')
          $ S.filter (all isLower) allWords
  where allWords = S.fromList allStringWords

getSubWords :: String -> [String]
getSubWords = L.nub . L.sortBy (comparing length) . subWords
  where
    subWords word = concatMap (`ofLength` word) [3..(length word)]
    ofLength n = Prelude.filter (`S.member` safeWords)
                . concatMap L.permutations
                . Prelude.filter (\x -> length x == n) . L.subsequences
                . Prelude.map toLower

startingState :: String -> [String] -> M.Map String Bool
startingState seed subWords = M.fromList $ Prelude.map hide subWords
  where hide word
          | word == seed = (word, True)
          | otherwise    = (word, False)

showState :: M.Map String Bool -> IO ()
showState state = printMatrix state (matrix state)
  where
  --printMatrix :: [[String]] -> IO ()
    printMatrix state matrix = do
      printLine
      printBox $ hsep 2 left (map (vcat left . map text) (wrap matrix))
      printLine
      where printLine = putStrLn $ "+" ++ replicate n '-' ++ "+"
              where n = sum (map (maximum . map length) matrix) + 2 * (1 + length matrix)
            wrap matrix = col ++ matrix ++ col
              where col = [replicate h ['|']]
                    h = length $ head matrix
  --matrix :: M.Map String Bool -> [[String]]
    matrix = chunksOf 4 . map (uncurry render) . L.sortBy keyLen . M.toList
    render k v -- (\(k,v) -> render k v)
      | v         = k
      | otherwise = replicate (length k) '_'
  --keyLen :: (Foldable t1) =>. (t1 a, _) -> (t1 a, _) -> Ordering
    keyLen (k1, _) (k2, _) = compare (length k1) (length k2)

play :: M.Map String Bool -> IO ()
play state = if gameOver state
               then putStrLn " > You win!"
               else move state
  where
  --gameOver :: Map String Bool -> Bool
    gameOver state = L.all (==True) $ M.elems state
  --move :: Map String Bool -> IO ()
    move state = do
      putStrLn " > Please enter a guess:"
      guess <- getLine
      let (correct, newState) = guessWord guess state
      if correct
        then showState newState
        else displayIncorrectGuess guess
      play newState
  --guessWord :: String -> Map String Bool -> (Bool, Map String Bool)
    guessWord guess state
      | M.member guess state = (True, updateState guess state)
      | otherwise = (False, state)
          --updateState :: String -> Map String Bool -> Map String Bool
      where updateState = M.update (\_ -> Just True)
    displayIncorrectGuess guess
      | length guess <= 2         = putStrLn "Too short."
      | guess `notElem` safeWords = putStrLn "Not a word."
      | otherwise                 = putStrLn "Not a subword."

main = do
  putStrLn " > T E X T   T W I S T"
  putStrLn " > Please enter a seed word:"
  seed <- getLine
  if seed `elem` safeWords
    then do
      let subWords = getSubWords seed
      if length subWords > 1
        then do
          let state = startingState seed subWords
          showState state
          play state
        else error "Not enough subwords."
    else error "Use a real word as a seed."
