import qualified System.Directory as Directory
import qualified System.FilePath.Posix as Posix
import qualified Data.Map as DataMap
import qualified Data.Text as DataText
import qualified Data.Maybe as Maybe
import qualified Text.Groom as Groom

data TrainingWord = TrainingWord String deriving (Show, Eq, Ord)
data TrainingText = TrainingText [TrainingWord] deriving Show

data State = State [TrainingWord] deriving Show
data Chain = ChainEnd | Chain (DataMap.Map TrainingWord Chain) deriving Show

addChain :: Chain -> State -> Chain
addChain chain (State []) = chain
addChain ChainEnd state = addChain (Chain (DataMap.fromList [])) state
addChain (Chain csMap) (State stateWords) = Chain newCsMap where
    newCsMap = (DataMap.insert stateHead newInnerChain csMap)
    stateHead = head stateWords
    stateRest = State $ tail stateWords
    oldInnerChain = Maybe.fromMaybe ChainEnd $ DataMap.lookup stateHead csMap
    newInnerChain = addChain oldInnerChain stateRest

-- Creates a state if it's long enough
getChain :: TrainingText -> Int -> Maybe State
getChain (TrainingText str) desiredLength
    | actualLength == desiredLength = Just $ State stateWords
    | otherwise = Nothing
        where
            stateWords = take desiredLength str
            actualLength = length stateWords


addTrainingTextToChain :: Int -> Chain -> TrainingText -> Chain
addTrainingTextToChain stateLength chain trainingText =
    foldl getNextChain chain (getSubTrainingTexts trainingText)
        where
            getSubTrainingTexts :: TrainingText -> [TrainingText]
            getSubTrainingTexts (TrainingText []) = []
            getSubTrainingTexts (TrainingText tWords) =
                (TrainingText tWords):(getSubTrainingTexts (TrainingText (tail tWords)))

            -- Given a training text, which could be a subset of another training text,
            -- assemble the next chain
            getNextChain :: Chain -> TrainingText -> Chain
            getNextChain chain' tText =
                maybe chain' (addChain chain') (getChain tText stateLength)

getTrainingText :: String -> TrainingText
getTrainingText trainingString = TrainingText $ getTrainingWords trainingString
    where
        getTrainingWords :: String -> [TrainingWord]
        getTrainingWords trainingString' = map (TrainingWord . cleanWord . DataText.unpack)
            $ DataText.splitOn (DataText.pack " ")
            $ DataText.pack trainingString'
        cleanWord :: String -> String
        cleanWord = id -- later we can remove commas, etc

getTrainingFilenames :: IO [String]
getTrainingFilenames = do
  fnames <- Directory.getDirectoryContents "training"
  return $ map ("training/" ++) $ filter ((== ".txt") . Posix.takeExtension) fnames

getTrainingStrings :: IO [String]
getTrainingStrings = getTrainingFilenames >>= mapM readFile

main :: IO ()
main = do
    trainingStrings <- getTrainingStrings
    let trainingTexts = map getTrainingText trainingStrings
    let chain = foldl (addTrainingTextToChain 5) ChainEnd trainingTexts
    putStrLn $ Groom.groom chain
    return ()
