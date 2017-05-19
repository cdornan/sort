
import           Data.List
import           Data.Ord
import           Data.Sort
import qualified Data.Vector          as V
import           System.Random.MWC
import           Test.Tasty
import           Test.Tasty.HUnit

-- | A simple test suite to test the functions are behaving.
main :: IO ()
main = do
  r1c <- gen_r1c
  defaultMain $
    testGroup "Sort Tests"
      [ testCase "groupSort" $ do
          assertEqual "counts"    (counts  r1c) (counts'   r1c)
      , testCase "monoidSortAssocs" $ do
          assertEqual "strings"  (strings  r1c) (strings'  r1c)
      , testCase "groupSortOn" $ do
          assertEqual "ostrings" (ostrings r1c) (ostrings' r1c)
      , testCase "uniqueSort" $ do
          assertEqual "dups"     (dups     unq) (sort      unq)
      ]

-- | groupsort to associate unique ints with repetition counts
counts :: [Int] -> [(Int,Int)]
counts = groupSort $ \i l -> (i,1+length l)

-- | sort+nub to to associate unique ints with repetition counts
counts' :: [Int] -> [(Int,Int)]
counts' xs = [ (i,length $ filter (i==) xs) | i<-sort $ nub xs ]

-- | monoidSort to associate each unique Int with its Show string, repeated as
-- many times as it apped in the argument list
strings :: [Int] -> [(Int,String)]
strings l = monoidSortAssocs $ [ (i,show i) | i<-l ]

-- | counts'/sort+nub to associate each unique Int with its Show string,
-- repeated as many times as it apped in the argument list
strings' :: [Int] -> [(Int,String)]
strings' l = [ (i,concat $ replicate n $ show i) | (i,n)<-counts l ]

-- | groupSortOn to sort the argument list into 'show' order,
-- associating each show string s with (i,s), repeated as many times as
-- i appears in the argument  list
ostrings :: [Int] -> [(String,[(Int,String)])]
ostrings = groupSortOn snd (\k x xs->(k,x:xs)) . strings

-- | groupSortBy to sort the argument list into 'show' order,
-- associating each show string s with (i,s), repeated as many times as
-- i appears in the argument  list
ostrings' :: [Int] -> [(String,[(Int,String)])]
ostrings' = groupSortBy (comparing snd) (\x@(_,k) xs->(k,x:xs)) . strings

-- | uniqueSort to duplicate the argument list and sort it, discarding
-- the duplicates
dups :: [Int] -> [Int]
dups xs = uniqueSort $ xs ++ reverse xs

-- | a list of integers with no repetitions
unq :: [Int]
unq = [10,9..1]

-- geberate 100 random integers distributed evenly between 1 and 12
gen_r1c :: IO [Int]
gen_r1c = do
    gen <- initialize $ V.singleton 42
    sequence $ replicate 100 (uniformR (1,12) gen)
