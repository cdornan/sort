{-# LANGUAGE CPP #-}

module SortBenchmarks (sortBenchmarks) where

import           Criterion.Main
import           Criterion.Types
import           Data.List
import           Data.Monoid
import           Data.Sort
import qualified Data.Vector          as V
import           System.Random.MWC


-- | Run the Criterion benchmarks with the given Criterion
-- configuration.
sortBenchmarks :: Config -> IO ()
sortBenchmarks cfg = do

  -- | generate 1000 psuedo-random numbers in r1k
  gen <- initialize $ V.singleton 42
  r1k <- V.toList `fmap` uniformVector gen 1000 :: IO [Int]

  defaultMainWith cfg
    [ bgroup "sort"
      [ bench "r1k"  $ nf sort                       r1k
      , bench "a1k"  $ nf sort                       a1k
      , bench "d1k"  $ nf sort                       d1k
      ]
    , bgroup "sortOn"
      [ bench "r1k"  $ nf (sortOn id)                r1k
      , bench "a1k"  $ nf (sortOn id)                a1k
      ]
    , bgroup "monoidSortAssocs"
      [ bench "r1k"  $ nf monoidSortAssocs $ map ass r1k
      , bench "a1k"  $ nf monoidSortAssocs $ map ass a1k
      ]
    , bgroup "monoidSort"
      [ bench "r1k"  $ nf monoidSort       $ map Sum r1k
      , bench "a1k"  $ nf monoidSort       $ map Sum a1k
      ]
    , bgroup "groupSortOn"
      [ bench "r1k"  $ nf (groupSortOn id grp')      r1k
      , bench "a1k"  $ nf (groupSortOn id grp')      a1k
      ]
    , bgroup "groupSort"
      [ bench "r1k"  $ nf (groupSort grp)            r1k
      , bench "a1k"  $ nf (groupSort grp)            a1k
      ]
    ]

-- | 1000 ascending integers
a1k :: [Int]
a1k = [1..1000]

-- | 1000 descending integers
d1k :: [Int]
d1k = [1000,999..1]

-- | a variant of grp for use with groupSortOn that takes the key in
-- its first argument
grp' :: Int -> Int -> [Int] -> (Int,Int)
grp' = const grp

-- | a grouping function that associates each number with the number of
-- times the element was duplicated
grp :: Int -> [Int] -> (Int,Int)
grp x l = (x,1+length l)

-- | simply associate each ietm with a unit value
ass :: Int -> (Int,())
ass = flip (,) ()
