import Control.Monad
import Data.Char
import Data.List.Split
import Data.Ord
import Language.Words
import System.Exit
import System.Random
import Text.PrettyPrint.Boxes
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

safeWords :: S.Set String
safeWords = S.filter (\w -> all ($ w) conditions) allWords
  where allWords = S.fromList allStringWords
        conditions = [notElem '\233', notElem '\'', all isLower]

getSubWords :: String -> [String]
getSubWords word = L.nub . L.sortBy (comparing length)
                 $ concatMap (`ofLength` word) [3..(length word)]
  where ofLength n = filter (`S.member` safeWords)
                   . concatMap L.permutations
                   . filter (\x -> length x == n) . L.subsequences
                   . map toLower

showState :: [String] -> M.Map String Bool -> String -> IO ()
showState words state revealed = printMatrix state (matrix state)
  where printMatrix state matrix = do
          putStrLn $ "┏" ++ replicate n '━' ++ "┓"
          printBox $ hsep pad left $ map (vcat left . map text)
                                   $ col ++ matrix ++ col
          putStrLn $ "┗┯" ++ replicate (pred n) '━' ++ "┛"
            where n = pad * (1 + length matrix)
                    + (sum . map (maximum . map length) $ matrix)
                  col = [replicate (length $ head matrix) ['┃']]
                  pad = 1
        matrix = chunksOf sq . map (uncurry render) . L.sortBy keyLen . M.toList
          where sq = ceiling $ sqrt $ fromIntegral $ length words
                render k v = if v then k else map obscure k
                  where obscure l = if l `notElem` revealed then '~' else l
                keyLen (k1, _) (k2, _) = compare (length k1) (length k2)

playGame :: [String] ->  M.Map String Bool -> String -> IO ()
playGame words state revealed = do
  showState words state revealed
  when gameOver $ die " └ You win!"
  tInst "Please enter a guess:"
  guess <- getLine
  when (guess == "") $ playGame words state revealed
  when (head guess == '?') $ do
    tInst $ "Showing '" ++ [guess !! 1] ++ "'."
    playGame words state $ (guess !! 1) : shorten revealed
  let correct = M.member guess state
  unless correct $ displayIncorrectGuess guess
  playGame words (updateState correct guess state) ""
  where gameOver = L.all (==True) $ M.elems state
        updateState bool = M.update (\_ -> Just bool)
        displayIncorrectGuess guess
          | length guess <= 2         = tInst "Too short."
          | guess `notElem` safeWords = tInst "Not a word."
          | otherwise                 = tInst "Not a subword."
        shorten revealed = if (length revealed <= 1)
                             then revealed
                             else L.nub $ init revealed

tInst :: String -> IO ()
tInst text = putStrLn $ " ├ " ++ text

main = do
  putStrLn $ " ┌ " ++ "T E X T   T W I S T"
  gen <- getStdGen
  let seed = randomWord gen (between 5 8 safeWords)
  when (seed `notElem` safeWords) $ die " └ Use a real word as a seed."
  let subWords = getSubWords seed
  when (length subWords <= 2) $ die " └ Not enough subwords."
  tInst $ "Using '" ++ seed ++ "'. For a hint, "
  tInst "type ? and then a letter."
  playGame subWords (blankState seed subWords) ""
    where blankState seed = M.fromList . map (\word -> (word, word==seed))
          between min max = S.filter (\x -> min < length x && length x < max)
          randomWord gen set = S.elemAt (randomNum (length set) gen) set
          randomNum max = fst . randomR (1, max)
