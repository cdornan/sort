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

  gen <- initialize $ V.singleton 42
  r1k <- V.toList `fmap` uniformVector gen 1000 :: IO [Int]

  defaultMainWith cfg
    [ bgroup "sort"
      [ bench "sort.r1k"              $ whnf sort                       r1k
      , bench "sort.a1k"              $ whnf sort                       a1k
      , bench "sort.d1k"              $ whnf sort                       d1k
      , bench "sortON.r1k"            $ whnf (sortON id)                r1k
      , bench "sortON.a1k"            $ whnf (sortON id)                a1k
      , bench "groupSort.r1k"         $ whnf (groupSort grp)            r1k
      , bench "groupSort.a1k"         $ whnf (groupSort grp)            a1k
      , bench "monoidSort.r1k"        $ whnf monoidSort       $ map Sum r1k
      , bench "monoidSort.a1k"        $ whnf monoidSort       $ map Sum a1k
      , bench "monoidSortAssocs.r1k"  $ whnf monoidSortAssocs $ map ass r1k
      , bench "monoidSortAssocs.a1k"  $ whnf monoidSortAssocs $ map ass a1k
      ]
    ]

-- | 1000 ascending integers
a1k :: [Int]
a1k = [1..1000]

-- | 1000 descending integers
d1k :: [Int]
d1k = [1000,999..1]

-- | a grouping function that associates each number with the number of
-- times the element was duplicated
grp :: Int -> [Int] -> (Int,Int)
grp x l = (x,1+length l)

-- | simply associate each ietm with a unit value
ass :: Int -> (Int,())
ass = flip (,) ()
