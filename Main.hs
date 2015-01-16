import qualified System.Directory as Directory
import qualified System.FilePath.Posix as Posix
import qualified Data.Map as DataMap
import qualified Data.Maybe as Maybe
import qualified System.Random as Random
-- import qualified Text.Groom as Groom
-- import qualified Debug.Trace as Trace

data State = State {getTokens :: [Token]} deriving Show

type StateTreeNode = DataMap.Map Token StateTree
type NextTokenCounts = DataMap.Map Token Int
data StateTree = StateLeaf NextTokenCounts | StateBranch StateTreeNode deriving (Show, Eq)
data Chain = Chain {getStateTree :: StateTree} deriving Show

------------------------
-- Getting Training Text
------------------------

data Token = TextStart | Word String deriving (Show, Eq, Ord)
data TrainingText = TrainingText [Token] deriving Show

getTrainingText :: String -> TrainingText
getTrainingText trainingString = TrainingText $ TextStart:(getTrainingTokens trainingString)
    where
        getTrainingTokens :: String -> [Token]
        getTrainingTokens trainingString' = map Word
            $ words
            $ trainingString'

isSufficientTrainingText :: Int -> TrainingText -> Bool
-- + 1 for the TextStart token
isSufficientTrainingText stateLength (TrainingText tokens) = length tokens >= stateLength + 1

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
addState' (StateLeaf _) _ _ = error $ "StateLeaf reached with extra tokens - "
    ++ " Make sure token lists are all of the same length"

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
-- Generating Random Stuff
------------------------

genText :: Chain -> Random.StdGen -> String
genText chain initStdGen = renderTokens $ genTokens initStdGen where
    genTokens stdGen = firstTokens ++ nextTokens ++ nextRunTokens where
        (firstStateStdGen, stdGen') = Random.split stdGen
        (nextTokensStdGen, nextRunStdGen) = Random.split stdGen'

        -- first state
        firstState = genFirstState chain firstStateStdGen
        State firstTokens = firstState

        -- subsequent states
        nextTokens = genNextTokens chain nextTokensStdGen firstState

        -- next run, in case we hit an end
        nextRunTokens = genTokens nextRunStdGen

-- Our first so many tokens can't be due to a transition from an old State,
-- since a State requires a full roster of tokens

isEndState :: StateTree -> State -> Bool
isEndState bStateTree (State tokens) = walkStateTree bStateTree tokens == emptyStateLeaf

genFirstState :: Chain -> Random.StdGen -> State
genFirstState (Chain baseSTree) initStdGen = State $ genFirstStateFull stdGens baseSTree where
    stdGens = genStdGens initStdGen

    genFirstStateRest :: [Random.StdGen] -> StateTree -> [Token]
    genFirstStateRest [] _ = error "genFirstStateRest: stdGen list should be infinite"
    genFirstStateRest _ (StateLeaf _) = []
    genFirstStateRest (stdGen:nextStdGens) stateTree = thisToken:nextTokens where
        thisToken = (genTokenFromTree stdGen stateTree)
        nextTokens = genFirstStateRest nextStdGens (walkStateTree stateTree [thisToken])

    -- Force it to start with a TextStart token, so generated text will make more sense.
    genFirstStateFull stdGens' stateTree = TextStart:nextTokens where
        nextTokens = genFirstStateRest stdGens' (walkStateTree stateTree [TextStart])

-- Given a chain, and a first state out of the chain, get tokens that
-- come *after* that state
genNextTokens :: Chain -> Random.StdGen -> State ->  [Token]
genNextTokens (Chain baseSTree) initStdGen firstState = nextTokens where
    stdGens = genStdGens initStdGen

    genNextStates :: [Random.StdGen] -> State -> [State]
    genNextStates [] _ = error "genNextStates: stdGen list should be infinite"
    genNextStates (stdGen:nextStdGens) thisState 
        | isEndState baseSTree nextState = [nextState]
        | otherwise = nextState:genNextStates nextStdGens nextState
            where
                nextState = State $ nextStateStartTokens ++ [nextStateLastToken]
                nextStateStartTokens = tail $ getTokens thisState
                lastStateBranch = walkStateTree baseSTree nextStateStartTokens
                nextStateLastToken = (genTokenFromTree stdGen lastStateBranch)

    nextTokens :: [Token]
    nextTokens = lastTokenEachState where
        states = genNextStates stdGens firstState
        lastTokenEachState = map (last . getTokens) states

-- Get an endless list of random sources based on a random source
genStdGens :: Random.StdGen -> [Random.StdGen]
genStdGens stdGen = stdGen1:stdGen2:(genStdGens stdGen3) where
    (stdGen1, stdGen') = Random.split stdGen
    (stdGen2, stdGen3) = Random.split stdGen'

walkStateTree :: StateTree -> [Token] -> StateTree
walkStateTree stateTree [] = stateTree -- We want this for either
walkStateTree (StateBranch stateTreeNode) (headToken:tailTokens) =
    walkStateTree (Maybe.fromJust (DataMap.lookup headToken stateTreeNode)) tailTokens
walkStateTree (StateLeaf _) _ = error "StateLeaf reached with extra tokens"

randomPick :: Random.StdGen -> [k] -> k
randomPick stdGen lst = lst !! (fst $ Random.randomR (0, (length lst) - 1) stdGen)

genTokenFromTree :: Random.StdGen -> StateTree -> Token
genTokenFromTree stdGen (StateBranch stateTreeNode) = randomKey stdGen stateTreeNode where
    randomKey :: Random.StdGen -> DataMap.Map k v -> k
    randomKey rkStdGen map' = randomPick rkStdGen $ DataMap.keys map'

genTokenFromTree stdGen (StateLeaf nextTokenCounts) = weightedRandomKey stdGen nextTokenCounts where
    weightedRandomKey :: Random.StdGen -> DataMap.Map k Int -> k
    weightedRandomKey krkStdGen map' = randomPick krkStdGen $ concatMap replicateTuple kvPairs where
        replicateTuple = (uncurry (flip replicate))
        kvPairs = (DataMap.assocs map')

renderTokens :: [Token] -> String
renderTokens tokens = concat $ map renderToken tokens

renderToken :: Token -> String
renderToken (Word str) = str ++ " "
renderToken TextStart = ""

------------------------
-- Main
------------------------

main :: IO ()
main = do
    trainingStrings <- getTrainingStrings
    let stateLength = 3
    let trainingTexts = filter (isSufficientTrainingText stateLength) $ map getTrainingText trainingStrings
    let chain = foldl (addTrainingTextToChain stateLength) emptyChain trainingTexts
    if getStateTree chain == emptyStateBranch
        then putStrLn "Insufficient training texts found"
        else do
            -- putStrLn $ Groom.groom chain
            stdGen <- Random.getStdGen
            putStrLn $ take 10000 $ genText chain stdGen
            return ()
