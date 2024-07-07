module Graph where

import Set (Set)
import Set qualified as Set
import Data.List (sort, group)

class Graph g where
  empty   :: g a
  vertex  :: a -> g a
  union   :: g a -> g a -> g a
  connect :: g a -> g a -> g a

data Relation a = Relation {domain :: Set a, relation :: Set (a, a)}
  deriving (Eq, Show)

instance Graph Relation where
  empty = Relation Set.empty Set.empty
  vertex x = Relation (Set.singleton x) Set.empty
  union (Relation d1 r1) (Relation d2 r2) = Relation (Set.union d1 d2) (Set.union r1 r2)
  connect (Relation d1 r1) (Relation d2 r2) = Relation d r
    where
      d = Set.union d1 d2
      r = Set.union r1 (Set.union r2 (cartesianProduct' d1 d2))
      cartesianProduct' :: Set a -> Set a -> Set (a, a)
      cartesianProduct' set1 set2 = Set.fromList [(x, y) | x <- Set.toList set1, y <- Set.toList set2]

instance (Ord a, Num a) => Num (Relation a) where
  fromInteger = vertex . fromInteger
  (+)         = union
  (*)         = connect
  signum      = const empty
  abs         = id
  negate      = id

data Basic a
  = Empty
  | Vertex a
  | Union (Basic a) (Basic a)
  | Connect (Basic a) (Basic a)

instance Graph Basic where
  empty   = Empty
  vertex  = Vertex
  union   = Union
  connect = Connect

instance (Ord a) => Eq (Basic a) where
  (==) g1 g2 = toStandardForm g1 == toStandardForm g2
  
instance (Ord a, Num a) => Num (Basic a) where
  fromInteger = vertex . fromInteger
  (+)         = union
  (*)         = connect
  signum      = const empty
  abs         = id
  negate      = id


instance Semigroup (Basic a) where
  (<>) = union

instance Monoid (Basic a) where
  mempty = Empty

fromBasic :: (Graph g) => Basic a -> g a
fromBasic Empty           = empty
fromBasic (Vertex x)      = vertex x
fromBasic (Union g1 g2)   = union (fromBasic g1) (fromBasic g2)
fromBasic (Connect g1 g2) = connect (fromBasic g1) (fromBasic g2)

removeDuplicates::(Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort


toStandardForm::(Ord a) => Basic a -> ([a], [(a, a)])
toStandardForm g = clean $ helper g ([], [])
  where 
    helper::(Ord a) => Basic a -> ([a], [(a, a)]) -> ([a], [(a, a)])
    helper Empty (vertices, edges) = (vertices, edges)
    helper (Vertex a) (vertices, edges) = (a:vertices, edges)
    helper (Union l r) (vertices, edges) = helper r (helper l (vertices, edges))

    helper (Connect l r) (vertices, edges)
      | null verticesLeft && null edgesLeft = (vertices ++ verticesRight, edges ++ edgesRight)
      | null verticesRight && null edgesRight = (vertices ++ verticesLeft, edges ++ edgesLeft)
      | otherwise = (removeDuplicates vertices, removeDuplicates $ allEdges ++ edges ++ edgesLeft ++ edgesRight)
      where
        (verticesLeft, edgesLeft) = helper l ([], [])
        (verticesRight, edgesRight) = helper r ([], [])
        allEdges = [(x, y) |
          x <- removeDuplicates $ verticesLeft ++ tupleToList edgesLeft,
          y <- removeDuplicates $ verticesRight ++ tupleToList edgesRight]
  
    clean::(Ord a) => ([a], [(a, a)]) -> ([a], [(a, a)])
    clean (vertices, edges) = (help (removeDuplicates $ tupleToList cleanEdges) cleanvertices, cleanEdges)   
      where 
        cleanEdges = removeDuplicates edges
        cleanvertices = removeDuplicates vertices

    help [] ys = ys
    help _ [] = []
    help (x:xs) (y:ys)
      | x < y     = help xs (y:ys)
      | x > y     = y : help (x:xs) ys
      | otherwise = help xs ys

    
    tupleToList::[(a,a)] -> [a]
    tupleToList ((a,b):xs) = a : b : tupleToList xs
    tupleToList _          = []



instance (Ord a, Show a) => Show (Basic a) where
  show g = "edges " ++ show edges ++ " + vertices " ++ show vertices
    where 
      (vertices, edges) = toStandardForm g


-- | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]
example34 :: Basic Int
example34 = 1 * 2 + 2 * (3 + 4) + (3 + 4) * 5 + 17

todot :: (Ord a, Show a) => Basic a -> String
todot g = "digraph {\n" ++ edgesStr ++ verticesStr ++ "}"
  where
    (vertices, edges) = toStandardForm g
    verticesStr = unlines $ map (\v -> show v ++ ";") vertices
    edgesStr = unlines $ map (\(v1, v2) -> show v1 ++ " -> " ++ show v2 ++ ";") edges

instance Functor Basic where
  fmap _ Empty            = Empty
  fmap f (Vertex x)       = Vertex (f x)
  fmap f (Union g1 g2)    = Union (fmap f g1) (fmap f g2)
  fmap f (Connect g1 g2)  = Connect (fmap f g1) (fmap f g2)

-- | Merge vertices
-- >>> mergeV 3 4 34 example34
-- edges [(1,2),(2,34),(34,5)] + vertices [17]
mergeV :: (Eq a) => a -> a -> a -> Basic a -> Basic a
mergeV x y z = fmap replace
  where
    replace v
      | v == x || v == y = z
      | otherwise = v

instance Applicative Basic where
  pure = Vertex
  (<*>) Empty _           = Empty
  (<*>) _ Empty           = Empty
  (<*>) (Vertex f) g      = f <$> g
  (<*>) (Union g1 g2) x   = Union (g1 <*> x) (g2 <*> x)
  (<*>) (Connect g1 g2) x = Connect (g1 <*> x) (g2 <*> x)

instance Monad Basic where
  (>>=) Empty _           = Empty
  (>>=) (Vertex x) f      = f x
  (>>=) (Union g1 g2) f   = Union (g1 >>= f) (g2 >>= f)
  (>>=) (Connect g1 g2) f = Connect (g1 >>= f) (g2 >>= f)

-- | Split Vertex
-- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
splitV a b c = (>>= \x -> if x == a then Union (Vertex b) (Vertex c) else Vertex x)