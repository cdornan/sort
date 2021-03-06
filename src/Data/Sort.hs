
module Data.Sort
  (
  -- * The Vanilla Sorts
    L.sort
  , L.sortBy
  , L.sortOn
  -- * Sorting Associations
  , monoidSortAssocs
  , monoidSortAssocsBy
  , groupSortAssocs
  , groupSortAssocsBy
  -- * Sorting with Monoids
  , monoidSort
  , monoidSortOn
  , monoidSortBy
  -- * Unique Sorts
  , uniqueSort
  , uniqueSortOn
  , uniqueSortBy
  -- * Group Sorting
  , groupSort
  , groupSortOn
  , groupSortBy
  ) where

import qualified Data.List                as L
import           Data.Monoid
import           Data.Ord


-- | Sort the list of associations, aggregating duplicates with the
-- monoid.
monoidSortAssocs :: (Monoid a,Ord k)
                 => [(k,a)]
                 -> [(k,a)]
monoidSortAssocs = monoidSortAssocsBy compare

-- | Sort the list of associations, aggregating duplicates with the
-- monoid and ordering the keys with the argument compare function.
monoidSortAssocsBy :: (Monoid a)
                   => (k->k->Ordering)
                   -> [(k,a)]
                   -> [(k,a)]
monoidSortAssocsBy cmp = groupSortAssocsBy cmp $ const monoid_group

-- | Sort the list of associations, aggregating duplicates with the
-- supplied function.
groupSortAssocs :: Ord k
                => (k->a->[a]->b)
                -> [(k,a)]
                -> [(k,b)]
groupSortAssocs = groupSortAssocsBy compare

-- | Sort the list of associations, aggregating duplicates with the
-- supplied function and ordering the keys with the argument
-- compare function.
groupSortAssocsBy :: (k->k->Ordering)
                  -> (k->a->[a]->b)
                  -> [(k,a)]
                  -> [(k,b)]
groupSortAssocsBy cmp0 grp0 = groupSortBy cmp grp
  where
    cmp (k,_) (k',_) = cmp0 k k'

    grp (k,y) ps     = (,) k $ grp0 k y $ map snd ps


-- | Sort the list, agregating duplicates with the monoid.
monoidSort :: (Monoid a,Ord a) => [a] -> [a]
monoidSort = monoidSortBy compare

-- | Sort the list, agregating duplicates with the monoid and
-- ordering the elements by the items generated by the
-- argument function.
monoidSortOn :: (Monoid a,Ord k) => (a->k) -> [a] -> [a]
monoidSortOn chg = groupSortOn chg $ const monoid_group

-- | Sort the list, agregating duplicates with the monoid
-- and ordering the keys with the argument compare function.
monoidSortBy :: Monoid a => (a->a->Ordering) -> [a] -> [a]
monoidSortBy cmp = groupSortBy cmp monoid_group


-- | Sort the list, discarding duplicates.
uniqueSort :: Ord a => [a] -> [a]
uniqueSort = uniqueSortBy compare

-- | Sort the list, discarding duplicates and
-- ordering the elements by the items generated by the
-- argument function.
uniqueSortOn :: Ord k => (a->k) -> [a] -> [a]
uniqueSortOn chg = groupSortOn chg $ const const

-- | Sort the list, discarding duplicates and ordering the keys with
-- the argument compare function.
uniqueSortBy :: (a->a->Ordering) -> [a] -> [a]
uniqueSortBy cmp = groupSortBy cmp const


-- | Sort a list of elements with a stable sort, grouping together the
-- equal elements with the argument grouping function
groupSort :: (Ord a) => (a->[a]->b) -> [a] -> [b]
groupSort = groupSortBy compare

-- | Sort a list of elements with a stable sort, using the argument
-- @compare@ function determine the ordering, grouping together the
-- equal elements with the grouping function
groupSortOn :: Ord k
            => (a->k)
            -> (k->a->[a]->b)
            -> [a]
            -> [b]
groupSortOn chg grp = groupSortBy (comparing fst) grp_val . map inj
  where
    grp_val a as = grp k (snd a) $ map snd as
      where
        k = fst a

    inj x = k `seq` (k,x)
      where
        k = chg x


-- | Sort a list of elements with a stable sort, grouping together the
-- equal elements with the argument grouping function.
groupSortBy :: (a->a->Ordering)
            -> (a->[a]->b)
            -> [a]
            -> [b]
groupSortBy cmp grp = aggregate . L.sortBy cmp
  where
    aggregate []    = []
    aggregate (h:t) = seq g $ g : aggregate rst
      where
        g         = grp h eqs
        (eqs,rst) = span is_le t

        is_le x   = case cmp x h of
          LT -> True
          EQ -> True
          GT -> False


-- the monoid_group helper

monoid_group :: Monoid a => a -> [a] -> a
monoid_group x xs = x <> mconcat xs
