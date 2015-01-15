import qualified System.Directory as Directory
import qualified System.FilePath.Posix as Posix
import qualified Data.Map as DataMap
import qualified Data.Text as DataText
import qualified Data.Maybe as Maybe
import qualified Text.Groom as Groom

data TrainingToken = TrainingToken String deriving (Show, Eq, Ord)
data TrainingText = TrainingText [TrainingToken] deriving Show

data State = State [TrainingToken] deriving Show
data Chain = StateEnd | Chain (DataMap.Map TrainingToken Chain) deriving Show

addChain :: Chain -> State -> Chain
addChain chain (State []) = chain
addChain StateEnd state = addChain (Chain (DataMap.fromList [])) state
addChain (Chain csMap) (State stateTokens) = Chain newCsMap where
    newCsMap = (DataMap.insert stateHead newInnerChain csMap)
    stateHead = head stateTokens
    stateRest = State $ tail stateTokens
    oldInnerChain = Maybe.fromMaybe StateEnd $ DataMap.lookup stateHead csMap
    newInnerChain = addChain oldInnerChain stateRest

-- Creates a state if it's long enough
getChain :: TrainingText -> Int -> Maybe State
getChain (TrainingText str) desiredLength
    | actualLength == desiredLength = Just $ State stateTokens
    | otherwise = Nothing
        where
            stateTokens = take desiredLength str
            actualLength = length stateTokens


addTrainingTextToChain :: Int -> Chain -> TrainingText -> Chain
addTrainingTextToChain stateLength chain trainingText =
    foldl getNextChain chain (getSubTrainingTexts trainingText)
        where
            getSubTrainingTexts :: TrainingText -> [TrainingText]
            getSubTrainingTexts (TrainingText []) = []
            getSubTrainingTexts (TrainingText tTokens) =
                (TrainingText tTokens):(getSubTrainingTexts (TrainingText (tail tTokens)))

            -- Given a training text, which could be a subset of another training text,
            -- assemble the next chain
            getNextChain :: Chain -> TrainingText -> Chain
            getNextChain chain' tText =
                maybe chain' (addChain chain') (getChain tText stateLength)

getTrainingText :: String -> TrainingText
getTrainingText trainingString = TrainingText $ getTrainingTokens trainingString
    where
        getTrainingTokens :: String -> [TrainingToken]
        getTrainingTokens trainingString' = map (TrainingToken . cleanToken . DataText.unpack)
            $ DataText.splitOn (DataText.pack " ")
            $ DataText.pack trainingString'
        cleanToken :: String -> String
        cleanToken = id -- later we can remove commas, remove caps

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
    let chain = foldl (addTrainingTextToChain 5) StateEnd trainingTexts
    putStrLn $ Groom.groom chain
    return ()
