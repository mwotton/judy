{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-full-laziness #-}

-- cabal install judy.

import Criterion.Main

import Control.Monad
import qualified Data.Judy as J
import qualified Data.IntMap as I
import qualified Data.HashTable as H
import Data.List

import Control.Parallel.Strategies

import Control.Monad
import qualified Data.Judy as J

main = defaultMain [
  --     bench "insert 100"  (testit 100)

--        bench "insert 1k"   (testit     1000)
--       ,bench "insert 100k" (testit   100000)
--       ,bench "insert 10M"  (testit 10000000)

--        bench "insert hash 1k"   (testit     1000)
--       ,bench "insert hash 100k" (testit   100000)
       bench "findMax IntMap 10M"  (\(_::Int) -> testit 10000000)

       {-
       ,bench "insert 10k"  (testit 10000)
       ,bench "insert 100k" (testit 100000)
       ,bench "insert 1M"   (testit 1000000)
       ,bench "insert 10M"  (testit 10000000)
       -}
    ]

testit n = foldl' (\b a -> I.insert a (fromIntegral a) b) I.empty [1..n] :: I.IntMap Int

{-
main = do
    let h = foldl' (\b a -> I.insert a (fromIntegral a) b) I.empty [1..10000000] :: I.IntMap Int
    print (I.size h)
    print (I.lookup 100 h)
-}


{-
testit n = do
   j <- H.new (==) H.hashInt -- :: IO (J.JudyL Int)
   forM_ [1..n] $ \n -> H.insert j n (fromIntegral n :: Int)
   k <- H.lookup j 100
   k `seq` return ()
-}

{-
testit n = do
   j <- J.new :: IO (J.JudyL Int)
   forM_ [1..n] $ \n -> J.insert n (fromIntegral n :: Int) j
   k <- J.lookup 100 j
   k `seq` return ()
   -}

instance NFData (IO ()) where rnf x = x `seq` ()

