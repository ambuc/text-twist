import Data.Random.Extras (choice)
import Data.Random (sample, MonadRandom)

startingWord :: MonadRandom m => Int -> m String
startingWord n = sample $ choice
               $ toList
               $ Data.Set.filter (\w -> length w == n) safeWords

main = do
  let word = startingWord 6
  putStrLn =<< word
