{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
#include "MachDeps.h"
module Data.JudySpec where

import           Control.Arrow   ((***))
import qualified Data.ByteString as S
import           Data.Int        (Int16, Int32, Int64, Int8)
import qualified Data.Judy       as J
import           Data.List       (groupBy, nub, partition, sort, sortBy)
import           Data.Ord        (comparing)
import           Data.Word       (Word16, Word32, Word64, Word8)
import           System.Mem      (performGC)
import           Test.Hspec      (Spec, describe, it, shouldBe, shouldReturn,
                                  shouldSatisfy)
import           Test.QuickCheck (property, (==>))

spec :: Spec
spec = describe "Data.Judy" $ do
  it "should be set to the correct value after setting" $
    property $ \(k, v::Int) -> do
      j <- J.new :: IO (J.JudyL Int)
      before <- J.lookup k j
      J.insert k v j
      result <- J.lookup k j
      (before,result) `shouldBe` (Nothing, Just v)

#if MIN_VERSION_base(4,10,0)
  it "should be set to the correct value after setting (Float)" $
    property $ \(k, v::Float) -> do
      j <- J.new :: IO (J.JudyL Float)
      before <- J.lookup k j
      J.insert k v j
      result <- J.lookup k j
      (before,result) `shouldBe` (Nothing, Just v)

  it "should be set to the correct value after setting (Double/Word64)" $
    property $ \(k, v::Double) -> do
      j <- J.new :: IO (J.JudyL Double)
      before <- J.lookup k j
      J.insert k v j
      result <- J.lookup k j
      (before,result) `shouldBe` (Nothing, Just v)
#endif

  it "should respect the last val set" $
    property $ \(k, v::Int) -> do
      j <- J.new :: IO (J.JudyL Int)
      J.insert k 0 j
      J.insert k v j
      result <- J.lookup k j

      result `shouldBe` Just v

  it "should report membership" $
    property $ \(k, other, v::Int) ->
      k /= other ==>
        do
          j <- J.new :: IO (J.JudyL Int)
          J.member k j `shouldReturn` False
          J.insert k v j
          J.member k j `shouldReturn` True
          J.member other j `shouldReturn` False

  it "should delete values" $
    property $ \(k, v::Int) -> do
      j <- J.new :: IO (J.JudyL Int)
      J.delete k j
      J.insert k v j
      J.delete k j
      J.lookup k j `shouldReturn` Nothing

  it "should adjust existing values only" $
    property $ \(k, other, v::Int) ->
      k /= other ==>
        do
          j <- J.new :: IO (J.JudyL Int)
          J.adjust (+ 1) k j
          J.lookup k j `shouldReturn` Nothing
          J.insert k v j
          J.adjust (+ 1) k j
          J.adjust (+ 1) other j
          J.lookup k j `shouldReturn` Just (v + 1)
          J.lookup other j `shouldReturn` Nothing

  it "should report whether the array is empty" $ do
    j <- J.new :: IO (J.JudyL Int)
    J.null j `shouldReturn` True
    J.insert 1 1 j
    J.null j `shouldReturn` False

  it "freezing should be idempotent" $
    property $ \(values'::[(J.Key, Int)]) -> do
      let values = uniqueByKey $ sortBy (comparing fst) values'
      j <- J.new
      mapM_ (\(k,v) -> J.insert k v j) values
      newj <- J.unsafeFreeze j
      performGC
      J.toList newj `shouldReturn` values

  (`mapM_` [("unsafefreeze",J.unsafeFreeze)
           ,("safeFreeze", J.freeze)
           ])
    $ \(name,method) ->
    it ("should fetch keys & vals in the right order using " <> name) $
      property $ \(al :: [(J.Key,Int)]) ->
        length al == length (nub $ map fst al) ==>
        do
          let sortedL = sortBy (comparing fst) al
          j <- J.new :: IO (J.JudyL Int)
          mapM_ (\(k,v) -> J.insert k v j) al
        -- this is necessary to avoid running out of memory:
        -- memory pressure on the C side will have no effect on the
        -- ghc collector.
          performGC

          frozen <- method j
          J.keys frozen `shouldReturn` map fst sortedL
          J.elems frozen `shouldReturn` map snd sortedL
          J.toList frozen `shouldReturn` sortedL

  it "insertWith should be correct" $
    -- bit ugly, but we don't have a Maybe instance for JE yet
    let combine _ _ = (-1) in

    property $ \(values'::[(J.Key, Int)]) -> do
      -- want lots of repeats, so we take the modulo of the key.
      -- as noted above, because of the lack of a Maybe instance we
      -- denote a collision with a negative number: therefore, all
      -- values coming in must be positive.
      let values = map (\(a,b) -> (a `mod` 20,abs b)) values'
      j <- J.new :: IO (J.JudyL Int)
      mapM_ (\(k,v) -> J.insertWith combine k v j) values
      -- at this point, all repeated keys should have Nothing values
      let (repeats, noRepeats) = (groupKeys *** groupKeys)
                                 $ partition (\x -> length x > 1)
                                 $ groupBy (\a b -> fst a == fst b)
                                 $ sortBy (comparing fst) values
      repeatResults <- (`mapM` repeats) $ \k -> J.lookup k j
      norepeatResults <- (`mapM` noRepeats) (`J.lookup` j)
      performGC
      length repeatResults `shouldBe` length repeats
      length norepeatResults `shouldBe` length noRepeats
      repeatResults `shouldSatisfy` all (== Just (-1))
      norepeatResults `shouldSatisfy` all (maybe False (>= 0))

  it "insertWith should combine new and old values" $ do
    j <- J.new :: IO (J.JudyL Int)
    J.insert 1 10 j
    J.insertWith (+) 1 5 j
    J.lookup 1 j `shouldReturn` Just 15

  it "should return key-value pairs from the array state at the point `toList` was called" $
    property $ \(k1, k2, v1::Int, v2::Int) -> do
      j <- J.new :: IO (J.JudyL Int)
      J.insert k1 v1 j

      l <- J.toList =<< J.freeze j
      J.insert k2 v2 j

      l == [(k1, v1)] `shouldBe` True

  it "should return the correct size" $
    property $ \(ls :: [J.Key]) -> do
      j <- J.new :: IO (J.JudyL ())
      let ordered = uniqueSorted $ sort ls
      mapM_ (\k -> J.insert k () j) ordered
      J.size j `shouldReturn` length ordered

  it "findMax should find the max" $
    property $ \(ls :: [J.Key]) -> do
      j <- J.new :: IO (J.JudyL ())
      let ordered = uniqueSorted $ sort ls
      mapM_ (\k -> J.insert k () j) ordered
      let res = case reverse ordered of [] -> Nothing; x:_ -> Just (x,())
      J.findMax j `shouldReturn` res

  it "findMin should find the min" $
    property $ \(ls :: [J.Key]) -> do
      j <- J.new :: IO (J.JudyL ())
      let ordered = uniqueSorted $ sort ls
      mapM_ (\k -> J.insert k () j) ordered
      let res = case ordered of [] -> Nothing; x:_ -> Just (x,())
      J.findMin j `shouldReturn` res

  it "should round-trip storable element representations" $ do
    roundTrip ()
    roundTrip True
    roundTrip False
    roundTrip LT
    roundTrip EQ
    roundTrip GT
    roundTrip (123 :: Word)
    roundTrip (-123 :: Int)
    roundTrip (-12 :: Int8)
    roundTrip (-1234 :: Int16)
    roundTrip (-123456 :: Int32)
#if (WORD_SIZE_IN_BITS == 64)
    roundTrip (-123456789 :: Int64)
#endif
    roundTrip (12 :: Word8)
    roundTrip (1234 :: Word16)
    roundTrip (123456 :: Word32)
#if (WORD_SIZE_IN_BITS == 64)
    roundTrip (123456789 :: Word64)
#endif
    roundTrip 'J'
    roundTrip (S.pack [0, 1, 2, 255])

roundTrip :: (Eq a, Show a, J.JE a) => a -> IO ()
roundTrip value = do
  encoded <- J.toWord value
  decoded <- J.fromWord encoded
  decoded `shouldBe` value

uniqueSorted :: Eq a => [a] -> [a]
uniqueSorted [] = []
uniqueSorted (x:xs) = x : uniqueSorted (dropWhile (== x) xs)

uniqueByKey :: Eq k => [(k, a)] -> [(k, a)]
uniqueByKey [] = []
uniqueByKey (x:xs) = x : uniqueByKey (dropWhile ((== fst x) . fst) xs)

groupKeys :: [[(k, a)]] -> [k]
groupKeys = foldr addKey []
  where
    addKey [] acc = acc
    addKey ((k,_):_) acc = k : acc
