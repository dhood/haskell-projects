import Control.Monad.State.Lazy
import qualified Data.Map as Map
import qualified Numeric.LinearAlgebra.HMatrix as Mat

countTransitions :: [Int] -> State (Map.Map (Int, Int) Int) [Int]
countTransitions [x] = return []
countTransitions (x1:x2:xs) = do
    counts <- get
    put $ incr_count (x1,x2) counts
    r <- countTransitions (x2:xs)
    return (x1:r) -- used if runState or evalState is called, not execState

incr_count x y = 
    Map.insert x (currVal+1) y
    where currVal = Map.findWithDefault 0 x y

makeTransitionMatrix :: (Map.Map (Int, Int) Int) -> (Mat.Matrix Double) 
makeTransitionMatrix m = convertToProbs $ (size Mat.>< size) $ makeDataList m 
    where size = fst $ fst $ Map.findMax m

makeDataList :: (Map.Map (Int, Int) Int) -> [Double]
makeDataList m = map (\x -> fromIntegral $ Map.findWithDefault 0 x m) indices
    where indices = (\n ->  [(x, y) | x <- [1..n], y <- [1..n]]) 
                        (fst $ fst $ Map.findMax m)

convertToProbs :: Mat.Matrix Double -> Mat.Matrix Double
convertToProbs mat = mat / scaleMat
    where scaleMat = (mat Mat.#> onesVec) `Mat.outer` onesVec
          onesVec = Mat.fromList (replicate (Mat.rows mat) 1)

d = [1,1,1,1,1,2,2,2,2,2,2,2,5,6,5,6,5,6,5,6,5,6,5,6,5,6,5,1]
main = do
     let count = execState (countTransitions d) Map.empty
         transMat = makeTransitionMatrix count
          in Mat.disp 3 $ transMat
