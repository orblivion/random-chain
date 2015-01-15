import qualified System.Directory as Directory
import qualified System.FilePath.Posix as Posix
import qualified Data.Map as DataMap
import qualified Data.Text as DataText
import qualified Data.Maybe as Maybe
import qualified Text.Groom as Groom

data TrainingToken = TrainingToken String deriving (Show, Eq, Ord)
data TrainingText = TrainingText [TrainingToken] deriving Show

data State = State [TrainingToken] deriving Show

type StateTreeNode = DataMap.Map TrainingToken StateTree
data StateTree = StateTree StateTreeNode deriving Show
data Chain = Chain [(Chain, Int)] StateTree deriving Show

emptyStateTree :: StateTree
emptyStateTree = StateTree $ DataMap.fromList []

emptyChain :: Chain
emptyChain = Chain [] emptyStateTree

addState :: Chain -> State -> Chain
addState (Chain connections stateTree) (State stateTokens) =
    Chain connections (addState' stateTree stateTokens)

addState' :: StateTree -> [TrainingToken] -> StateTree
addState' stateTree [] = stateTree 
addState' (StateTree stateTreeNode) stateTokens = newStateTree where
    newStateTree :: StateTree
    newStateTree = StateTree (DataMap.insert stateHead newInnerStateTree stateTreeNode)

    stateHead :: TrainingToken
    stateHead = head stateTokens

    stateRest :: [TrainingToken]
    stateRest = tail stateTokens

    oldInnerStateTree :: StateTree
    oldInnerStateTree = Maybe.fromMaybe emptyStateTree $ DataMap.lookup stateHead stateTreeNode

    newInnerStateTree :: StateTree
    newInnerStateTree = addState' oldInnerStateTree stateRest

-- Creates a state if it's long enough
getState :: TrainingText -> Int -> Maybe State
getState (TrainingText str) desiredLength
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
                maybe chain' (addState chain') (getState tText stateLength)

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
    let chain = foldl (addTrainingTextToChain 5) emptyChain trainingTexts
    putStrLn $ Groom.groom chain
    return ()
