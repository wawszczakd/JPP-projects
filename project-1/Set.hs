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
null (Union s t)   = (null s) && (null t)

member :: Eq a => a -> Set a -> Bool
member y Empty         = False
member y (Singleton x) = x == y
member y (Union s t)   = (member y s) || (member y t)

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
    helper (Union s t)   acc = helper s (helper t acc)
  in
    helper s []

toAscList :: Ord a => Set a -> [a]
toAscList s = sort (toList s)

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
union s t = Union s t

insert :: a -> Set a -> Set a
insert x s = Union (Singleton x) s

instance Ord a => Eq (Set a) where
  s == t =
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
      unique (toAscList s) == unique (toAscList t)

instance Semigroup (Set a) where
  s <> t = Union s t

instance Monoid (Set a) where
  mempty  = Empty
  mappend = (<>)

instance Show a => Show (Set a) where
  show Empty         = "_"
  show (Singleton x) = show x
  show (Union s t)   = "(" ++ (show s) ++ ", " ++ (show t) ++ ")"

instance Functor Set where
  fmap f Empty         = Empty
  fmap f (Singleton x) = Singleton (f x)
  fmap f (Union s t)   = Union (fmap f s) (fmap f t)
