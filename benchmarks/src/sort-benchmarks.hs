module Main where

import           Criterion.Main
import           SortBenchmarks


-- | Run the bunchmarks with a printing simple text report on stdout.
main :: IO ()
main = sortBenchmarks defaultConfig
