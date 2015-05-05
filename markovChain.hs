{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Monad.State.Lazy
import qualified Data.Map as Map
import qualified Numeric.LinearAlgebra.HMatrix as Mat
import Data.List.Ordered
import Data.List
import System.IO
import Data.Char
import Data.List.Split as Split
import Data.Maybe

-- TransitionMatrix comprised of a transition matrix and state labels
data TransitionMatrix t = 
                   TransitionMatrix { mat :: Mat.Matrix Double, states :: [t] }
instance (Show t) => Show (TransitionMatrix t) where 
    show (TransitionMatrix mat states) = "States: " ++ show states ++ "\n" 
                                        ++ Mat.disps 3 mat

-- Turn a list of states into a Map counting transitions between states
countTransitions :: (Eq t, Ord t) => [t] -> State (Map.Map (t, t) Int) [t]
countTransitions [x] = return []
countTransitions (x1:x2:xs) = do
    counts <- get
    put $ incr_count (x1,x2) counts
    r <- countTransitions (x2:xs)
    return (x1:r) -- only used if runState or evalState is called, not execState

-- Increment the count of item x in Map y, or set it to 0 if not available
incr_count :: (Ord t) => t -> Map.Map t Int -> Map.Map t Int
incr_count x y = 
    Map.insert x (currVal+1) y
    where currVal = Map.findWithDefault 0 x y

-- Turn a list of data (instances of states) into a transition matrix with the
-- i,j element representing the probability of transitioning i->j in a time step
makeTransitionMatrix :: (Eq t, Ord t) => [t] -> TransitionMatrix t 
makeTransitionMatrix dataList = TransitionMatrix {
                    mat = convertToProbs $ (size Mat.>< size) $ makeDataList count states 
                    , states = states}
    where count = execState (countTransitions dataList) Map.empty
          states = Data.List.nub dataList
          size = length states

-- Convert a Map of transition frequencies between state to a matrix with
-- element i,j representing count of i->j transitions
makeDataList :: (Eq t, Ord t) => (Map.Map (t, t) Int) -> [t] -> [Double]
makeDataList transCount states = map (\x -> fromIntegral $ Map.findWithDefault 0 x transCount) indices
    where indices = [(x, y) | x <- states, y <- states]

-- Convert a matrix representing transition frequencies to one representing
-- transition probabilities (row totals equal to 1)
convertToProbs :: Mat.Matrix Double -> Mat.Matrix Double
convertToProbs mat = 
    let noZeros xs x = if (x==0) then 1:xs else x:xs
        onesVec = Mat.fromList $ replicate (Mat.rows mat) 1
        rowTotals = mat Mat.#> onesVec 
        rowTotals_noZero = Mat.fromList . reverse . foldl noZeros [] $ Mat.toList rowTotals
        scaleMat = rowTotals_noZero `Mat.outer` onesVec
    in mat / scaleMat

-- Returns the probability ditribution p_n+k of being in each state after k time
-- steps, starting from probability distribution p_n
predict :: Mat.Matrix Double -> Mat.Matrix Double -> Int -> Mat.Matrix Double
predict transMat p_n 0 = p_n
predict transMat p_n k = do
    let p_nplusk = (predict transMat p_n (k-1)) Mat.<> transMat
        in normaliseRowVec p_nplusk

-- Turns a string into a list of tokens, removing punctuation such as parentheses 
tokenize :: String -> [String]
tokenize s = filter (\x -> x /= " " && x /= "") $
             Split.split rule $ map toLower s
        where rule = Split.dropDelims $ oneOf ":., \n[]();\"" -- get rid of delimeters
        --where rule = Split.whenElt (\x -> isSeparator x || isPunctuation x || x == '\n') -- keep

-- Returns a list of un-normalised probabilities of the associated states,
-- determined from the input Map inits
makeInitialProbs :: (Eq t, Ord t) => (Map.Map t Double) -> [t] -> [Double]
makeInitialProbs _ [] = []
makeInitialProbs emptyMap states | Map.null emptyMap = replicate (length states) 1.0 
makeInitialProbs inits (state:states) = 
            (Map.findWithDefault 0 state inits):(makeInitialProbs inits states)
       
-- Returns the a row vector with the sum of the [not all zero] elements equal to 1
normaliseRowVec :: Mat.Matrix Double -> Mat.Matrix Double
normaliseRowVec x = do
        let cols = Mat.cols x 
            sum = x Mat.<> (cols Mat.>< 1) (replicate cols 1)
            in x / sum

-- Sort the states list by the descending probabilities in the p row vector
mostProbableStates :: (Eq t, Ord t) => Mat.Matrix Double -> [t] -> [t]
mostProbableStates p states = reverse $ 
        sortOn (f ((Mat.toLists p)!!0) states) states
        where f p states state = p !! (fromJust $ elemIndex state states)

filename = "data.txt"                   -- file to analyse transitions from
k = 1                                   -- number of time steps ahead to predict
n = 3                                   -- top n most likely states will be displayed
initialProbs = [("be", 1),("or",1.5)]   -- initial distribution weights (will be normalised)

main = do
    dataset <- readFile filename
    let 
        transMat = makeTransitionMatrix $ tokenize dataset
        numStates = length (states transMat)
        p_0 = normaliseRowVec $ (1 Mat.>< numStates) $
              makeInitialProbs (Map.fromList initialProbs) (states transMat)
        p_k = predict (mat transMat) p_0 k
        s = mostProbableStates p_k (states transMat)
    print $ "State labels (" ++ show numStates ++ "):"
    print (states transMat)
    print "Initial state weights:"
    print initialProbs
    print $ "Probabilities after " ++ show k ++ " timestep(s):"
    Mat.dispDots 3 p_k 
    print $ take n s
