module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems
              ) where
import Prelude hiding(null)
import Data.List (sort)

data Set a = Empty
           | Singleton a
           | Union (Set a) (Set a)

empty :: Set a
empty = Empty

null :: Set a -> Bool
null Empty         = True
null (Singleton x) = False
null (Union s1 s2) = (null s1) && (null s2)

member :: Eq a => a -> Set a -> Bool
member y Empty         = False
member y (Singleton x) = x == y
member y (Union s1 s2) = (member y s1) || (member y s2)

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
        helper prev []      acc = acc
        helper prev (h : t) acc
          | h == prev = helper h t acc
          | otherwise = helper h t (h : acc)
      in
        reverse (helper h t [h])
  in
    unique (sort (toList s))

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
union s1 s2 = Union s1 s2

insert :: a -> Set a -> Set a
insert x s = Union (Singleton x) s

instance Ord a => Eq (Set a) where
    s1 == s2 = toAscList s1 == toAscList s2

instance Semigroup (Set a) where
    s1 <> s2 = Union s1 s2

instance Monoid (Set a) where
    mempty  = Empty
    mappend = (<>)

instance Show a => Show (Set a) where
    show Empty         = "_"
    show (Singleton x) = show x
    show (Union s1 s2)   = "(" ++ (show s1) ++ ", " ++ (show s2) ++ ")"

instance Functor Set where
    fmap f Empty         = Empty
    fmap f (Singleton x) = Singleton (f x)
    fmap f (Union s1 s2)   = Union (fmap f s1) (fmap f s2)
