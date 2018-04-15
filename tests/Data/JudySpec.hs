{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Data.JudySpec where

import           Control.Arrow   ((***))
import qualified Data.Judy       as J
import           Data.List       (group, groupBy, nub, partition, sort, sortBy)
import           Data.Monoid     ((<>))
import           Data.Ord        (comparing)
import           Data.Word       (Word)
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

  it "should respect the last val set" $
    property $ \(k, v::Int) -> do
      j <- J.new :: IO (J.JudyL Int)
      J.insert k 0 j
      J.insert k v j
      result <- J.lookup k j

      result `shouldBe` Just v

  it "freezing should be idempotent" $
    property $ \(values'::[(Word, Int)]) -> do
      let values = map head
                   $ groupBy (\a b -> fst a == fst b)
                   $ sortBy (comparing fst) values'
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
      property $ \(al :: [(Word,Int)]) ->
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

    property $ \(values'::[(Word, Int)]) -> do
      -- want lots of repeats, so we take the modulo of the key.
      -- as noted above, because of the lack of a Maybe instance we
      -- denote a collision with a negative number: therefore, all
      -- values coming in must be positive.
      let values = map (\(a,b) -> (a `mod` 20,abs b)) values'
      j <- J.new :: IO (J.JudyL Int)
      mapM_ (\(k,v) -> J.insertWith combine k v j) values
      -- at this point, all repeated keys should have Nothing values
      let (repeats, noRepeats) = (map (fst.head) *** map (fst.head))
                                 $ partition (\x -> length x > 1)
                                 $ groupBy (\a b -> fst a == fst b)
                                 $ sortBy (comparing fst) values
      repeatResults <- (`mapM` repeats) $ \k -> J.lookup k j
      norepeatResults <- (`mapM` noRepeats) (`J.lookup` j)
      performGC
      length repeatResults `shouldBe` length repeats
      length norepeatResults `shouldBe` length noRepeats
      repeatResults `shouldSatisfy` all (== Just (-1))
      norepeatResults `shouldSatisfy` all (\(Just a) -> a >= 0)

  it "should return key-value pairs from the array state at the point `toList` was called" $
    property $ \(k1, k2, v1::Int, v2::Int) -> do
      j <- J.new :: IO (J.JudyL Int)
      J.insert k1 v1 j

      l <- J.toList =<< J.freeze j
      J.insert k2 v2 j

      l == [(k1, v1)] `shouldBe` True

  it "should return the correct size" $
    property $ \(ls :: [Word]) -> do
      j <- J.new :: IO (J.JudyL ())
      let ordered = map head . group $ sort ls
      mapM_ (\k -> J.insert k () j) ordered
      J.size j `shouldReturn` length ordered

  it "findMax should find the max" $
    property $ \(ls :: [Word]) -> do
      j <- J.new :: IO (J.JudyL ())
      let ordered = map head . group $ sort ls
      mapM_ (\k -> J.insert k () j) ordered
      let res = case ordered of [] -> Nothing; xs -> Just (last xs,())
      J.findMax j `shouldReturn` res
