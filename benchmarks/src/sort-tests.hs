
import           Data.List
import           Data.Ord
import           Data.Sort
import qualified Data.Vector          as V
import           System.Random.MWC
import           Test.Tasty
import           Test.Tasty.HUnit


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

counts :: [Int] -> [(Int,Int)]
counts = groupSort $ \i l -> (i,1+length l)

counts' :: [Int] -> [(Int,Int)]
counts' xs = [ (i,length $ filter (i==) xs) | i<-sort $ nub xs ]

strings :: [Int] -> [(Int,String)]
strings l = monoidSortAssocs $ [ (i,show i) | i<-l ]

strings' :: [Int] -> [(Int,String)]
strings' l = [ (i,concat $ replicate n $ show i) | (i,n)<-counts l ]

ostrings :: [Int] -> [(String,[(Int,String)])]
ostrings = groupSortOn snd (\k x xs->(k,x:xs)) . strings

ostrings' :: [Int] -> [(String,[(Int,String)])]
ostrings' = groupSortBy (comparing snd) (\x@(_,k) xs->(k,x:xs)) . strings

dups :: [Int] -> [Int]
dups xs = uniqueSort $ xs ++ reverse xs

unq :: [Int]
unq = [10,9..1]

gen_r1c :: IO [Int]
gen_r1c = do
    gen <- initialize $ V.singleton 42
    sequence $ replicate 100 (uniformR (1,12) gen)
