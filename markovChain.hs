import Control.Monad.State.Lazy
import qualified Data.Map as M

countTransitions :: [Int] -> State (M.Map (Int, Int) Int) [Int]
countTransitions [x] = return []
countTransitions (x1:x2:xs) = do
    counts <- get
    put $ incr_count (x1,x2) counts
    r <- countTransitions (x2:xs)
    return (x1:r) -- used if runState or evalState is called, not execState

incr_count x y = 
    M.insert x (currVal+1) y
    where currVal = M.findWithDefault 0 x y

main = do
     let count = execState (countTransitions [1,1,1,1,1,2,2,2,2,2,2,2,5,6,5,6,5,6,5,6,5,6,5,6,5,6,5,1]) M.empty 
          in print count
