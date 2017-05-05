module Data.Sort
  ( groupSort
  , groupSortWith
  ) where

import           Data.List


-- | sort a list of elements with a stable sort, grouping together the
-- equal elements with the argument grouping function
groupSort :: Ord a => (a->[a]->b) -> [a] -> [b]
groupSort = groupSortWith compare


-- | sort a list of elements with a stable sort, using the argument
-- @compare@ function determine the ordering, grouping together the
-- equal elements with the grouping function
groupSortWith :: (a->a->Ordering) -> (a->[a]->b) -> [a] -> [b]
groupSortWith cmp grp = aggregate . sortBy cmp
  where
    aggregate []    = []
    aggregate (h:t) = grp h eqs : aggregate rst
      where
        (eqs,rst) = span is_le t

        is_le x   = case cmp x h of
          LT -> True
          EQ -> True
          GT -> False
