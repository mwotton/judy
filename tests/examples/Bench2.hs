import Control.Monad (forM_)
import Criterion.Config
import Criterion.Main
import qualified Data.IntMap as I
import qualified Data.Judy as J
import qualified Data.Map as M
import Data.List (foldl')

-- Work around the fact that the GC won't run finalizers aggressively
-- enough for us.
myConfig = defaultConfig { cfgPerformGC = ljust True }

main = defaultMainWith myConfig [
{-
        bgroup "judy" [
                     bench "insert 1M"   (testit 1000000)
                   , bench "insert 10M"  (testit 10000000)
                   , bench "insert 100M" (testit 100000000)
                   ],
-}
        bgroup "intmap" [
                    bench "insert 10M" (testmap 10000000)
                    ]
    ]

testit n = do
   j <- J.new :: IO (J.JudyL Int)
   forM_ [1..n] $ \n -> J.insert n (fromIntegral n :: Int) j
   v <- J.lookup 100 j
   v `seq` return ()

testmap :: Int -> Int -> I.IntMap Int
testmap n i =
    foldl' (\m k -> I.insert k k m) I.empty [0..(n+i-i)]

{-
testmap :: Int -> Int -> M.Map Int Int
testmap n i =
    foldl' (\m k -> M.insert k k m) M.empty [0..(n+i-i)]
    -}
