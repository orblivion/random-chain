import qualified System.Directory as Directory
import qualified System.FilePath.Posix as Posix
import qualified Data.Map as DataMap
import qualified Data.Text as DataText
import qualified Data.Maybe as Maybe
import qualified Text.Groom as Groom
-- import qualified Debug.Trace as Trace

data State = State [Token] deriving Show

type StateTreeNode = DataMap.Map Token StateTree
type NextTokenCounts = DataMap.Map Token Int
data StateTree = StateLeaf NextTokenCounts | StateBranch StateTreeNode deriving Show
data Chain = Chain StateTree deriving Show

------------------------
-- Getting Training Text
------------------------

data Token = Token String deriving (Show, Eq, Ord)
data TrainingText = TrainingText [Token] deriving Show

getTrainingText :: String -> TrainingText
getTrainingText trainingString = TrainingText $ getTokens trainingString
    where
        getTokens :: String -> [Token]
        getTokens trainingString' = map (Token . cleanToken . DataText.unpack)
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

---------------------
-- Creating the Chain
---------------------

emptyStateBranch :: StateTree
emptyStateBranch = StateBranch $ DataMap.fromList []
emptyStateLeaf :: StateTree
emptyStateLeaf = StateLeaf $ DataMap.fromList []

emptyChain :: Chain
emptyChain = Chain emptyStateBranch

addState :: Chain -> Maybe Token -> State -> Chain
addState (Chain stateTree) mbNextStateToken (State stateTokens) =
    Chain (addState' stateTree stateTokens mbNextStateToken)

addState' :: StateTree -> [Token] -> Maybe Token -> StateTree
addState' (StateLeaf nextTokenCounts) [] mbNextStateToken =
    StateLeaf (newNextTokenCounts mbNextStateToken)
        where
            newNextTokenCounts Nothing = nextTokenCounts
            newNextTokenCounts (Just nextStateToken) =
                DataMap.insert nextStateToken (oldCount + 1) nextTokenCounts
                    where
                        oldCount = Maybe.fromMaybe 0 $ DataMap.lookup nextStateToken nextTokenCounts

addState' (StateBranch stateTreeNode) stateTokens mbNextStateToken = newStateTree where
    newStateTree :: StateTree
    newStateTree = StateBranch (DataMap.insert stateHead newInnerStateTree stateTreeNode)

    stateHead :: Token
    stateHead = head stateTokens

    stateRest :: [Token]
    stateRest = tail stateTokens

    defaultInnerStateTree :: StateTree
    defaultInnerStateTree
        | stateRest == [] = emptyStateLeaf
        | otherwise       = emptyStateBranch

    oldInnerStateTree :: StateTree
    oldInnerStateTree =
        Maybe.fromMaybe defaultInnerStateTree $ DataMap.lookup stateHead stateTreeNode

    newInnerStateTree :: StateTree
    newInnerStateTree = addState' oldInnerStateTree stateRest mbNextStateToken

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
            getNextChain chain' tText = maybe chain' (addState chain' $ mbNextStateToken tText) state where
                mbNextStateToken (TrainingText tokens) =
                    Maybe.listToMaybe . (take 1) . (drop stateLength) $ tokens
                state = (getState tText stateLength)

------------------------
-- Main
------------------------


main :: IO ()
main = do
    trainingStrings <- getTrainingStrings
    let trainingTexts = map getTrainingText trainingStrings
    let chain = foldl (addTrainingTextToChain 5) emptyChain trainingTexts
    putStrLn $ Groom.groom chain
    return ()
