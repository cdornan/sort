
import           Criterion.Main
import           Criterion.Types
import           SortBenchmarks


-- | This script runs the benchmarks, updating the HTML report on the
-- website.
main :: IO ()
main = sortBenchmarks
  defaultConfig
    { reportFile = Just "../../regex-uk/sort-benchmarks.html"
    }
