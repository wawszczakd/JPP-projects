module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems
              ) where
import Prelude hiding(null)

data Set a = Empty
           | Singleton a
           | Union (Set a) (Set a)

empty :: Set a
empty = Empty

null :: Set a -> Bool
null Empty         = True
null (Singleton x) = False
null (Union s1 s2) = null s1 && null s2

member :: Eq a => a -> Set a -> Bool
member y Empty         = False
member y (Singleton x) = x == y
member y (Union s1 s2) = member y s1 || member y s2

singleton :: a -> Set a
singleton x = Singleton x

fromList :: [a] -> Set a
fromList l = foldl (\s -> \x -> insert x s) Empty l

toList :: Set a -> [a]
toList s =
  let
    helper :: Set a -> [a] -> [a]
    helper Empty         acc = acc
    helper (Singleton x) acc = x : acc
    helper (Union s1 s2) acc = helper s1 (helper s2 acc)
  in
    helper s []

toAscList :: Ord a => Set a -> [a]
toAscList s =
  let
    unique :: Ord a => [a] -> [a]
    unique []      = []
    unique (h : t) =
      let
        helper :: Ord a => a -> [a] -> [a] -> [a]
        helper prev []        acc = acc
        helper prev (h' : t') acc
          | h' == prev = helper h' t' acc
          | otherwise  = helper h' t' (h' : acc)
      in
        reverse $ helper h t [h]
    
    mergeSort :: Ord a => [a] -> [a]
    mergeSort []  = []
    mergeSort [x] = [x]
    mergeSort l   =
      let
        split :: [a] -> ([a], [a]) -> Bool -> ([a], [a])
        split []      (acc1, acc2) _     = (acc1, acc2)
        split (h : t) (acc1, acc2) True  = split t (h : acc1, acc2) False
        split (h : t) (acc1, acc2) False = split t (acc1, h : acc2) True
        
        merge :: Ord a => [a] -> [a] -> [a] -> [a]
        merge []        []        acc = reverse acc
        merge (h1 : t1) []        acc = merge t1 [] (h1 : acc)
        merge []        (h2 : t2) acc = merge [] t2 (h2 : acc)
        merge (h1 : t1) (h2 : t2) acc
          | h1 <= h2  = merge t1 (h2 : t2) (h1 : acc)
          | otherwise = merge (h1 : t1) t2 (h2 : acc)
        
        (one, two) = split l ([], []) True
      in
        merge (mergeSort one) (mergeSort two) []
  in
    unique (mergeSort (toList s))

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
union s1 s2 = Union s1 s2

insert :: a -> Set a -> Set a
insert x s = Union s (Singleton x)

instance Ord a => Eq (Set a) where
    s1 == s2 = toAscList s1 == toAscList s2

instance Semigroup (Set a) where
    s1 <> s2 = Union s1 s2

instance Monoid (Set a) where
    mempty  = Empty

instance Show a => Show (Set a) where
    show s = show $ toList s

instance Functor Set where
    fmap f Empty         = Empty
    fmap f (Singleton x) = Singleton (f x)
    fmap f (Union s1 s2)   = Union (fmap f s1) (fmap f s2)
