module Set
  ( Set (..),
    empty,
    null,
    singleton,
    union,
    fromList,
    member,
    toList,
    toAscList,
    elems,
  )
where

import Data.List (sort, group)
import Prelude hiding (null)

data Set a
  = Empty
  | Singleton a
  | Union (Set a) (Set a)

empty :: Set a
empty = Empty

null :: Set a -> Bool
null Empty = True
null _ = False

member :: (Eq a) => a -> Set a -> Bool
member _ Empty = False
member x (Singleton a) = x == a
member x (Union a b) = member x a || member x b

singleton :: a -> Set a
singleton = Singleton

fromList :: [a] -> Set a
fromList = foldr insert Empty


toList :: Set a -> [a]
toList s = go s []
  where
    go Empty acc = acc
    go (Singleton a) acc = a : acc
    go (Union s1 s2) acc = go s1 (go s2 acc)

toAscList :: (Ord a) => Set a -> [a]
toAscList = map head . group . sort . toList

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
union a Empty = a
union Empty b = b
union a b = Union a b

insert :: a -> Set a -> Set a
insert x = Union (Singleton x)

instance (Ord a) => Eq (Set a) where
  s1 == s2 = toAscList s1 == toAscList s2

instance Semigroup (Set a) where
  (<>) = union

instance Monoid (Set a) where
  mempty = empty

instance (Show a) => Show (Set a) where
  show s = show (toList s)

instance Functor Set where
  fmap _ Empty = Empty
  fmap f (Singleton a) = Singleton (f a)
  fmap f (Union a b) = Union (fmap f a) (fmap f b)

