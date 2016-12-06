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

blankState :: String -> [String] -> M.Map String Bool
blankState seed = M.fromList . map (\word -> (word, word==seed))

showState :: [String] -> M.Map String Bool -> IO ()
showState words state = printMatrix state (matrix state)
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
          where sq = (ceiling $ sqrt $ fromIntegral $ length words)
                render k v = if v then k else replicate (length k) '~'
                keyLen (k1, _) (k2, _) = compare (length k1) (length k2)

playGame :: [String] ->  M.Map String Bool -> IO ()
playGame words state = do
                         showState words state
                         when (gameOver) $ die " └ You win!"
                         tInst "Please enter a guess:"
                         guess <- getLine
                         let correct = M.member guess state
                         let newState = updateState correct guess state
                         unless correct $ displayIncorrectGuess guess
                         playGame words newState
  where gameOver = L.all (==True) $ M.elems state
        updateState bool = M.update (\_ -> Just bool)
        displayIncorrectGuess guess
          | length guess <= 2         = tInst "Too short."
          | guess `notElem` words     = tInst "Not a subword."
          | otherwise                 = tInst "Not a word."

tInst :: [Char] -> IO ()
tInst text = putStrLn $ " ├ " ++ text

randomWord set gen = S.elemAt (fst $ randomR ( 1, length set ) gen) set

main = do
  putStrLn $ " ┌ " ++ "T E X T   T W I S T"
  --tInst "Please enter a seed word:"
  gen <- getStdGen
  let seed = randomWord safeWords gen
  when (seed `notElem` safeWords) $ die " └ Use a real word as a seed."
  let subWords = getSubWords seed
  when (length subWords <= 2) $ die " └ Not enough subwords."
  playGame subWords $ blankState seed subWords
