module Graph where
import Set(Set)
import qualified Set as Set
class Graph g where
  empty   :: g a
  vertex  :: a -> g a
  union   :: g a -> g a -> g a
  connect :: g a -> g a -> g a

data Relation a = Relation { domain :: Set a, relation :: Set (a, a) }
    deriving (Eq, Show)

data Basic a = Empty
             | Vertex a
             | Union (Basic a) (Basic a)
             | Connect (Basic a) (Basic a)

allPairs :: Set a -> Set a -> Set (a, a)
allPairs v1 v2 = Set.fromList [(x1, x2) | x1 <- Set.toList v1, x2 <- Set.toList v2]

allPairsUnique :: Ord a => Set a -> Set a -> Set (a, a)
allPairsUnique v1 v2 = Set.fromList [(x1, x2) | x1 <- Set.toAscList v1, x2 <- Set.toAscList v2]

instance Graph Relation where
    empty         = Relation Set.empty Set.empty
    vertex x      = Relation (Set.singleton x) Set.empty
    union r1 r2   = Relation (Set.union (domain r1) (domain r2)) (Set.union (relation r1) (relation r2))
    connect r1 r2 = Relation (Set.union (domain r1) (domain r2)) (Set.union (Set.union (relation r1) (relation r2)) (allPairs (domain r1) (domain r2)))

instance (Ord a, Num a) => Num (Relation a) where
    (+) = undefined
    (-) = undefined
    (*) = undefined
    negate = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined

-- instance Graph Basic where

-- instance Ord a => Eq (Basic a) where

-- instance (Ord a, Num a) => Num (Basic a) where
--     fromInteger = vertex . fromInteger
--     (+)         = union
--     (*)         = connect
--     signum      = const empty
--     abs         = id
--     negate      = id

-- instance Semigroup (Basic a) where
--   (<>) = union

-- instance Monoid (Basic a) where
--   mempty = Empty

-- fromBasic :: Graph g => Basic a -> g a

-- instance (Ord a, Show a) => Show (Basic a) where

-- -- | Example graph
-- -- >>> example34
-- -- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

-- example34 :: Basic Int
-- example34 = 1*2 + 2*(3+4) + (3+4)*5 + 17

-- todot :: (Ord a, Show a) => Basic a -> String
-- todot = undefined

-- instance Functor Basic where

-- -- | Merge vertices
-- -- >>> mergeV 3 4 34 example34
-- -- edges [(1,2),(2,34),(34,5)] + vertices [17]

-- mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
-- mergeV = undefined

-- instance Applicative Basic where

-- instance Monad Basic where

-- -- | Split Vertex
-- -- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- -- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

-- splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
-- splitV = undefined

