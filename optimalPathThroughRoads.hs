-- Computation of optimal path through a directed road graph (Learn You a Haskell chapter 10)
main = print $ getOptimalPath [50,10,30,5,90,20,40,2,25,10,8]

data Label = A | B | C deriving Show -- A: north EW road, B: south EW road, C: NS road 
type Segment a = (Label, a)          -- road segment with the road's type and cost
type Path a = [Segment a]            -- a sequence of road segments
data (Ord a, Num a) => Section a =   -- section of a road graph comprised of three road segments
                       Section { getA :: Segment a, getB :: Segment a, getC :: Segment a } deriving Show
type RoadSystem a = [Section a]      -- road graph comprised of sections

-- format a list of numbers into a list of sections of three roads
groupIntoSections :: (Ord a, Num a) => [a] -> RoadSystem a
groupIntoSections []               = []
groupIntoSections (x : y : z : xs) = Section (A, x) (B, y) (C, z) : groupIntoSections xs
groupIntoSections (x : y : xs)     = [Section (A, x) (B, y) (C, 0)] -- only the last section can have 2

-- update path given the additional section of the road graph
processSection :: (Ord a, Num a) => (a, Path a) -> Section a -> (a, Path a)
processSection (switchCost, []) (Section a b c) = -- start in the state with minimum cost
                                                  if (snd b <= snd a) 
                                                  then ((snd c), [b]) 
                                                  else ((snd c), [a])
processSection (switchCost, p@((A, cost):path)) (Section a b c) = evalPath switchCost a b c p
processSection (switchCost, p@((B, cost):path)) (Section a b c) = evalPath switchCost b a c p

-- determine the appropriate path through a three-road segment section
evalPath :: (Ord a, Num a) => a -> Segment a -> Segment a -> Segment a -> Path a -> (a, Path a)
evalPath switchCost a b c p = if (switchCost + (snd b) <= (snd a)) 
                              then ((snd c), b:(C, switchCost):p)
                              else ((snd c), a:p)

-- determine the optimal path through a directed road graph
getOptimalPath :: (Ord a, Num a) => [a] -> Path a
getOptimalPath xs = reverse $ snd $ foldl processSection (0, []) (groupIntoSections xs) 
