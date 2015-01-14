import qualified System.Directory as Directory
import qualified System.FilePath.Posix as Posix
import qualified Data.Map as DataMap

data TrainingWord = TrainingWord String deriving Show
data TrainingText = TrainingText [TrainingWord] deriving Show
data Chain = ChainEnd | Chain (DataMap.Map TrainingWord Chain) deriving Show

getNextChain :: TrainingText -> Int -> (Maybe Chain, TrainingText)
getNextChain tText chainLength = undefined

getTrainingText :: String -> TrainingText
getTrainingText = undefined
    where
        getTrainingWords :: String -> [TrainingWord]
        getTrainingWords = undefined

getTrainingFilenames = do
  fnames <- Directory.getDirectoryContents "training"
  return $ map ("training/" ++) $ filter ((== ".txt") . Posix.takeExtension) fnames

getTrainingStrings = getTrainingFilenames >>= mapM readFile

main = getTrainingStrings >>= (map getTrainingText >> mapM putStrLn)
