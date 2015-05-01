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

makeTransitionMatrix :: Int -> (Map.Map (Int, Int) Int) -> (Mat.Matrix Double) 
makeTransitionMatrix size m = (size Mat.>< size) dataList
     where dataList = makeDataList (makeIndices 6) m

makeIndices :: Int -> [(Int, Int)]
makeIndices maxVal = [(x, y) | x <- [1..maxVal], y <- [1..maxVal]]

makeDataList :: [(Int, Int)] -> (Map.Map (Int, Int) Int) -> [Double]
makeDataList indices m = map (\x -> fromIntegral $ Map.findWithDefault 0 x m) indices

main = do
     let count = execState (countTransitions [1,1,1,1,1,2,2,2,2,2,2,2,5,6,5,6,5,6,5,6,5,6,5,6,5,6,5,1]) Map.empty
         transMat = makeTransitionMatrix 6 count
         l = makeDataList (makeIndices 6) count
          in Mat.disp 3 transMat
