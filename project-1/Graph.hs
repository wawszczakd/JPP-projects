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
  g1 + g2     =
    let
      v1 = Set.fromList $ Set.toAscList $ domain g1
      e1 = Set.fromList $ Set.toAscList $ relation g1
      v2 = Set.fromList $ Set.toAscList $ domain g2
      e2 = Set.fromList $ Set.toAscList $ relation g2
    in
      union (Relation v1 e1) (Relation v2 e2)
  g1 * g2     =
    let
      v1 = Set.fromList $ Set.toAscList $ domain g1
      e1 = Set.fromList $ Set.toAscList $ relation g1
      v2 = Set.fromList $ Set.toAscList $ domain g2
      e2 = Set.fromList $ Set.toAscList $ relation g2
    in
      connect (Relation v1 e1) (Relation v2 e2)
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
  g1 + g2     =
    let
      v1 = Set.fromList $ Set.toAscList $ domain g1
      e1 = Set.fromList $ Set.toAscList $ relation g1
      v2 = Set.fromList $ Set.toAscList $ domain g2
      e2 = Set.fromList $ Set.toAscList $ relation g2
    in
      union (Relation v1 e1) (Relation v2 e2)
  g1 * g2     =
    let
      v1 = Set.fromList $ Set.toAscList $ domain g1
      e1 = Set.fromList $ Set.toAscList $ relation g1
      v2 = Set.fromList $ Set.toAscList $ domain g2
      e2 = Set.fromList $ Set.toAscList $ relation g2
    in
      connect (Relation v1 e1) (Relation v2 e2)
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

getVertices :: (Ord a) => Basic a -> [a]
getVertices g =
  let
    graph = fromBasic g
    vertices = Set.toAscList $ domain graph
    edges = Set.toAscList $ relation graph
    used = Set.toAscList $ Set.fromList $ [x1 | (x1, x2) <- edges] ++ [x2 | (x1, x2) <- edges]
    
    go :: Ord a => [a] -> [a] -> [a] -> [a]
    go []        used'     acc = acc
    go (h1 : t1) []        acc = go t1 [] (h1 : acc)
    go (h1 : t1) (h2 : t2) acc
      | h1 == h2  = go t1        t2        acc
      | h1 < h2   = go (h1 : t1) t2        acc
      | otherwise = go t1        (h2 : t2) (h1 : acc)
  in
    go (reverse vertices) (reverse used) []

instance (Ord a, Show a) => Show (Basic a) where
  show g =
    let
      vertices = getVertices g
      edges = Set.toAscList $ relation $ fromBasic g
    in
      "edges " ++ show edges ++ " + vertices " ++ show vertices

-- | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

example34 :: Basic Int
example34 = 1*2 + 2*(3+4) + (3+4)*5 + 17

todot :: (Ord a, Show a) => Basic a -> String
todot g =
  let
    vertices = getVertices g
    edges = Set.toAscList $ relation $ fromBasic g
    
    addStrings :: String -> String -> String
    addStrings s t = foldr (\c -> \acc -> c : acc) t s
    
    addManyStrings :: [String] -> String -> String
    addManyStrings l t = foldr (\s -> \acc -> addStrings s acc) t l
    
    edgesString :: (Ord a, Show a) => [(a, a)] -> String -> String
    edgesString []             acc = acc
    edgesString ((x1, x2) : t) acc = edgesString t $ addManyStrings [show x1, " -> ", show x2, ";\n"] acc
    
    verticesString :: (Ord a, Show a) => [a] -> String -> String
    verticesString []      acc = acc
    verticesString (h : t) acc = verticesString t $ addManyStrings [show h, ";\n"] acc
  in
    addManyStrings ["digraph {\n", edgesString (reverse edges) "", verticesString (reverse vertices) "", "}"] ""

instance Functor Basic where
  fmap f Empty           = Empty
  fmap f (Vertex x)      = Vertex $ f x
  fmap f (Union g1 g2)   = Union (fmap f g1) (fmap f g2)
  fmap f (Connect g1 g2) = Connect (fmap f g1) (fmap f g2)

-- | Merge vertices
-- >>> mergeV 3 4 34 example34
-- edges [(1,2),(2,34),(34,5)] + vertices [17]

mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
mergeV a b c Empty           = Empty
mergeV a b c (Vertex x)      = if a == x || b == x then Vertex c else Vertex x
mergeV a b c (Union g1 g2)   = Union (mergeV a b c g1) (mergeV a b c g2)
mergeV a b c (Connect g1 g2) = Connect (mergeV a b c g1) (mergeV a b c g2)

instance Applicative Basic where
  Empty         <*> f = Empty
  Vertex x      <*> f = fmap x f
  Connect g1 g2 <*> f = connect (g1 <*> f) (g2 <*> f)
  Union g1 g2   <*> f = union (g1 <*> f) (g2 <*> f)
  pure                = Vertex

instance Monad Basic where
  Empty         >>= f = Empty
  Vertex x      >>= f = f x
  Union g1 g2   >>= f = union (g1 >>= f) (g2 >>= f)
  Connect g1 g2 >>= f = connect (g1 >>= f) (g2 >>= f)

-- | Split Vertex
-- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
splitV a b c g = g >>= (\x -> if x == a then Union (Vertex b) (Vertex c) else return x)