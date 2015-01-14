import qualified System.Directory as Directory
import qualified System.FilePath.Posix as Posix
import qualified Data.Map as DataMap
import qualified Data.Text as DataText
import qualified Data.Maybe as Maybe

data TrainingWord = TrainingWord String deriving (Show, Eq, Ord)
data TrainingText = TrainingText [TrainingWord] deriving Show

data Chain = Chain [TrainingWord] deriving Show
data ChainSet = ChainEnd | ChainSet (DataMap.Map TrainingWord ChainSet) deriving Show

addChain :: ChainSet -> Chain -> ChainSet
addChain chainSet (Chain []) = chainSet
addChain (ChainSet csMap) (Chain chainWords) = ChainSet newCsMap where
    newCsMap = (DataMap.insert chainHead newInnerChainSet csMap)
    chainHead = head chainWords
    chainRest = Chain $ tail chainWords
    oldInnerChainSet = Maybe.fromMaybe ChainEnd $ DataMap.lookup chainHead csMap
    newInnerChainSet = addChain oldInnerChainSet chainRest

-- Creates a chain if it's long enough
getChain :: TrainingText -> Int -> Maybe Chain
getChain (TrainingText str) desiredLength
    | actualLength == desiredLength = Just $ Chain chainWords
    | otherwise = Nothing
        where
            chainWords = take desiredLength str
            actualLength = length chainWords


addTrainingTextToChainSet :: Int -> ChainSet -> TrainingText -> ChainSet
addTrainingTextToChainSet chainLength chainSet trainingText =
    foldl getNextChainSet chainSet (getSubTrainingTexts trainingText)
        where
            getSubTrainingTexts :: TrainingText -> [TrainingText]
            getSubTrainingTexts (TrainingText []) = []
            getSubTrainingTexts (TrainingText words) =
                (TrainingText words):(getSubTrainingTexts (TrainingText (tail words)))

            -- Given a training text, which could be a subset of another training text,
            -- assemble the next chainset
            getNextChainSet :: ChainSet -> TrainingText -> ChainSet
            getNextChainSet chainSet tText =
                maybe chainSet (addChain chainSet) (getChain tText chainLength)

getTrainingText :: String -> TrainingText
getTrainingText trainingString = TrainingText $ getTrainingWords trainingString
    where
        getTrainingWords :: String -> [TrainingWord]
        getTrainingWords trainingString = map (TrainingWord . cleanWord . DataText.unpack)
            $ DataText.splitOn (DataText.pack " ")
            $ DataText.pack trainingString
        cleanWord :: String -> String
        cleanWord = id -- later we can remove commas, etc

getTrainingFilenames = do
  fnames <- Directory.getDirectoryContents "training"
  return $ map ("training/" ++) $ filter ((== ".txt") . Posix.takeExtension) fnames

getTrainingStrings = getTrainingFilenames >>= mapM readFile

main = do
    trainingStrings <- getTrainingStrings
    let trainingTexts = map getTrainingText trainingStrings
    let chainSet = foldl (addTrainingTextToChainSet 5) ChainEnd trainingTexts
    putStrLn $ show chainSet
    return ()
