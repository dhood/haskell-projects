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

makeTransitionMatrix :: [Int] -> (Mat.Matrix Double) 
makeTransitionMatrix l = convertToProbs $ (size Mat.>< size) $ makeDataList count 
    where count = execState (countTransitions l) Map.empty
          size = fst . fst $ Map.findMax count 

makeDataList :: (Map.Map (Int, Int) Int) -> [Double]
makeDataList m = map (\x -> fromIntegral $ Map.findWithDefault 0 x m) indices
    where indices = (\n ->  [(x, y) | x <- [1..n], y <- [1..n]]) 
                        (fst . fst $ Map.findMax m)

convertToProbs :: Mat.Matrix Double -> Mat.Matrix Double
convertToProbs mat = 
    let noZeros xs x = if (x==0) then 1:xs else x:xs
        onesVec = Mat.fromList $ replicate (Mat.rows mat) 1
        rowTotals = mat Mat.#> onesVec 
        rowTotals_noZero = Mat.fromList . reverse . foldl noZeros [] $ Mat.toList rowTotals
        scaleMat = rowTotals_noZero `Mat.outer` onesVec
    in mat / scaleMat

d = [1,1,1,1,1,2,2,2,2,2,2,2,5,6,5,6,5,6,5,6,5,6,5,6,5,6,5,1]
main = do 
    let transMat = makeTransitionMatrix d
          in Mat.disp 3 $ transMat
