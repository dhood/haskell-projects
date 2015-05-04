import Control.Monad.State.Lazy
import qualified Data.Map as Map
import qualified Numeric.LinearAlgebra.HMatrix as Mat
import Data.List

data TransitionMatrix t = 
                   TransitionMatrix { mat :: Mat.Matrix Double, states :: [t] }
instance (Show t) => Show (TransitionMatrix t) where 
    show (TransitionMatrix mat states) = "States: " ++ show states ++ "\n" 
                                        ++ show mat

countTransitions :: (Eq t, Ord t) => [t] -> State (Map.Map (t, t) Int) [t]
countTransitions [x] = return []
countTransitions (x1:x2:xs) = do
    counts <- get
    put $ incr_count (x1,x2) counts
    r <- countTransitions (x2:xs)
    return (x1:r) -- used if runState or evalState is called, not execState

incr_count :: (Ord t) => t -> Map.Map t Int -> Map.Map t Int
incr_count x y = 
    Map.insert x (currVal+1) y
    where currVal = Map.findWithDefault 0 x y

makeTransitionMatrix :: (Eq t, Ord t) => [t] -> TransitionMatrix t 
makeTransitionMatrix dataList = TransitionMatrix {
                    mat = convertToProbs $ (size Mat.>< size) $ makeDataList count states 
                    , states = states}
    where count = execState (countTransitions dataList) Map.empty
          states = nub dataList
          size = length states

makeDataList :: (Eq t, Ord t) => (Map.Map (t, t) Int) -> [t] -> [Double]
makeDataList transCount states = map (\x -> fromIntegral $ Map.findWithDefault 0 x transCount) indices
    where indices = [(x, y) | x <- states, y <- states]

convertToProbs :: Mat.Matrix Double -> Mat.Matrix Double
convertToProbs mat = 
    let noZeros xs x = if (x==0) then 1:xs else x:xs
        onesVec = Mat.fromList $ replicate (Mat.rows mat) 1
        rowTotals = mat Mat.#> onesVec 
        rowTotals_noZero = Mat.fromList . reverse . foldl noZeros [] $ Mat.toList rowTotals
        scaleMat = rowTotals_noZero `Mat.outer` onesVec
    in mat / scaleMat

predict :: Mat.Matrix Double -> Mat.Matrix Double -> Int -> Mat.Matrix Double
predict transMat p_n 0 = p_n
predict transMat p_n k = do
    let p_nplusk = (predict transMat p_n (k-1)) Mat.<> transMat
        cols = Mat.cols p_nplusk
        sump = p_nplusk Mat.<> (cols Mat.>< 1) (replicate cols 1)
        in p_nplusk / sump

--d = [1,1,1,1,1,2,2,2,2,2,2,2,5,6,5,6,5,6,5,6,5,6,5,6,5,6,5,1]
d = words "rain shine rain shine clouds rain"
k = 30 
main = do 
    let transMat = makeTransitionMatrix d
        numStates = length (states transMat)
        p_0 = (1 Mat.>< numStates) (1.0:replicate (numStates-1) 0.0)
        p_k = predict (mat transMat) p_0 k
          in Mat.disp 3 p_k --print transMat

