import Control.Monad (forM_)
import Criterion.Config
import Criterion.Main
import qualified Data.IntMap as I
import qualified Data.Judy as J
import qualified Data.Map as M
import Data.List (foldl')
import Data.Word

import System.IO.Unsafe
import System.Random.Mersenne

-- Work around the fact that the GC won't run finalizers aggressively
-- enough for us.
myConfig = defaultConfig { cfgPerformGC = ljust True }

main = do
    print "Setting up..."
--    judy100k `seq` judy1M `seq` judy10M `seq` return ()
--  judy10k `seq` return ()
--    judy100k `seq` return ()
--    judy1M `seq` return ()
    judy10M `seq` return ()
    print "done"

    defaultMainWith myConfig [
--        bench "delete 10k"  (testit 10000)
--          bench "delete 100k" (testit 100000)
--      bench "delete 1M"   (testit 1000000)
        bench "findMax 10M"  (testit 10000000)
        ]

testit :: Int -> IO (Maybe (J.Key, Int))
testit n = do
    J.findMax (h n)
  where
     --   h 10000 = judy10k
     --   h 100000 = judy100k
     --   h 1000000 = judy1M
        h 10000000 = judy10M

------------------------------------------------------------------------
-- construction.

{-
judy10k :: J.JudyL Int
judy10k  = unsafePerformIO $ do
   g  <- getStdGen
   rs <- randoms g
   j <- J.new :: IO (J.JudyL Int)
   forM_ (take n rs) $ \n -> J.insert n (fromIntegral n :: Int) j
   return j
 where
   n = truncate 1e5
-}

{-
judy100k :: J.JudyL Int
judy100k  = unsafePerformIO $ do
   g  <- getStdGen
   rs <- randoms g
   j <- J.new :: IO (J.JudyL Int)
   forM_ (take n rs) $ \n -> J.insert n (fromIntegral n :: Int) j
   return j
 where
   n = truncate 1e6
-}

{-
judy1M :: J.JudyL Int
judy1M    = unsafePerformIO $ do
   g  <- getStdGen
   rs <- randoms g
   j <- J.new :: IO (J.JudyL Int)
   forM_ (take n rs) $ \n -> J.insert n (fromIntegral n :: Int) j
   return j
 where
   n = truncate 1e7
   -}

judy10M :: J.JudyL Int
judy10M   = unsafePerformIO $ do
   g  <- getStdGen
   rs <- randoms g
   j <- J.new :: IO (J.JudyL Int)
   forM_ (take n rs) $ \n -> J.insert n (fromIntegral n :: Int) j
   return j
 where
   n = truncate 1e8

------------------------------------------------------------------------

{-
testit n = do
   j <- J.new :: IO (J.JudyL Int)
   forM_ [1..n] $ \n -> J.insert n (fromIntegral n :: Int) j
   v <- J.lookup 100 j
   v `seq` return ()
-}

testmap :: Int -> Int -> I.IntMap Int
testmap n i =
    foldl' (\m k -> I.insert k k m) I.empty [0..(n+i-i)]

{-
testmap :: Int -> Int -> M.Map Int Int
testmap n i =
    foldl' (\m k -> M.insert k k m) M.empty [0..(n+i-i)]
    -}
