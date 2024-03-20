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

cartesianProduct :: [a] -> [a] -> [(a, a)]
cartesianProduct l1 l2 = [(x1, x2) | x1 <- l1, x2 <- l2]

instance Graph Relation where
    empty :: Relation a
    empty = Relation Set.empty Set.empty

    vertex :: a -> Relation a
    vertex x = Relation (Set.singleton x) Set.empty

    union :: Relation a -> Relation a -> Relation a
    union r1 r2 =
      let
        vertices = Set.union (domain r1)   (domain r2)
        edges    = Set.union (relation r1) (relation r2)
      in
        Relation vertices edges
    
    connect :: Relation a -> Relation a -> Relation a
    connect r1 r2 =
      let
        vertices = Set.union (domain r1) (domain r2)
        oldEdges = Set.union (relation r1) (relation r2)
        newEdges = Set.fromList (cartesianProduct (Set.toList $ domain r1) (Set.toList $ domain r2))
        edges    = Set.union oldEdges newEdges
      in
        Relation vertices edges

instance (Ord a, Num a) => Num (Relation a) where
    fromInteger = vertex . fromInteger
    (+)         = union
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Graph Basic where
    empty         = Empty
    vertex x      = Vertex x
    union g1 g2   = Union g1 g2
    connect g1 g2 = Connect g1 g2

instance Ord a => Eq (Basic a) where
    g1 == g2 =
      let
        graph1 = fromBasic g1
        graph2 = fromBasic g2
      in
        domain graph1 == domain graph2 && relation graph1 == relation graph2

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

fromBasic :: Graph g => Basic a -> g a
fromBasic Empty           = empty
fromBasic (Vertex x)      = vertex x
fromBasic (Union g1 g2)   = union (fromBasic g1) (fromBasic g2)
fromBasic (Connect g1 g2) = connect (fromBasic g1) (fromBasic g2)

instance (Ord a, Show a) => Show (Basic a) where
    show g =
      let
        graph = fromBasic g
        vertices = Set.toAscList $ domain graph
        edges = Set.toAscList $ relation graph
      in
        "vertices: " ++ show vertices ++ "; edges: " ++ show edges

-- | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

example34 :: Basic Int
example34 = 1*2 + 2*(3+4) + (3+4)*5 + 17

todot :: (Ord a, Show a) => Basic a -> String
todot g =
  let
    graph = fromBasic g
    vertices = Set.toAscList $ domain graph
    edges = Set.toAscList $ relation graph
  in
    "digraph {\n" ++
    concat [show x1 ++ " -> " ++ show x2 | (x1, x2) <- edges] ++
    concat [show x ++ ";\n" | x <- vertices] ++
    "}\n"

instance Functor Basic where
    fmap f Empty           = Empty
    fmap f (Vertex x)      = Vertex $ f x
    fmap f (Union g1 g2)   = Union (fmap f g1) (fmap f g2)
    fmap f (Connect g1 g2) = Connect (fmap f g1) (fmap f g2)

-- | Merge vertices
-- >>> mergeV 3 4 34 example34
-- edges [(1,2),(2,34),(34,5)] + vertices [17]

mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
mergeV x1 x2 y Empty           = Empty
mergeV x1 x2 y (Vertex x)      = if x1 == x || x2 == x then Vertex y else Vertex x
mergeV x1 x2 y (Union g1 g2)   = Union (mergeV x1 x2 y g1) (mergeV x1 x2 y g2)
mergeV x1 x2 y (Connect g1 g2) = Connect (mergeV x1 x2 y g1) (mergeV x1 x2 y g2)

-- instance Applicative Basic where

-- instance Monad Basic where

-- -- | Split Vertex
-- -- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- -- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

-- splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
-- splitV = undefined

