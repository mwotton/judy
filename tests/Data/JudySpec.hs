{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Data.JudySpec where

import           Control.Arrow   ((&&&))
import           Control.Monad   (guard)
import qualified Data.Judy       as J
import           Data.List       (nub, sortBy)
import           Data.List       (groupBy, partition)
import           Data.Maybe      (isJust)
import           Data.Monoid     ((<>))
import           Data.Ord        (comparing)
import           Data.Word       (Word)
import           System.Mem      (performGC)
import           Test.Hspec
import           Test.QuickCheck


spec = describe "Data.Judy" $ do
  it "should be set to the correct value after setting" $
    property $ \(k, v::Int) -> do
      j <- J.new :: IO (J.JudyL Int)
      before <- J.lookup k j
      J.insert k v j
      result <- J.lookup k j
      (before,result) `shouldBe` (Nothing, Just v)

  it "should respect the last val set" $ do
    property $ \(k, v::Int) -> do
      j <- J.new :: IO (J.JudyL Int)
      J.insert k 0 j
      J.insert k v j
      result <- J.lookup k j

      result `shouldBe` Just v

  it "freezing should be idempotent" $ do
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
    $ \(name,method) -> do
    it ("should fetch keys & vals in the right order using " <> name) $ do
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

  it "insertWith should be correct" $ do
    -- bit ugly, but we don't have a Maybe instance for JE yet
    let combine _ _ = (-1)

    property $ \(values'::[(Word, Int)]) -> do
      -- want lots of repeats, so we take the modulo of the key.
      -- as noted above, because of the lack of a Maybe instance we
      -- denote a collision with a negative number: therefore, all
      -- values coming in must be positive.
      let values = map (\(a,b) -> (a `mod` 20,abs b)) values'
      j <- J.new :: IO (J.JudyL Int)
      mapM_ ((\(k,v) -> J.insertWith combine k v j)) values
      -- at this point, all repeated keys should have Nothing values
      let (repeats, noRepeats) = (\(x,y) -> (map (fst . head) x,
                                             map (fst . head) y))
                                 $ partition (\x -> length x > 1)
                                 $ groupBy (\a b -> fst a == fst b)
                                 $ sortBy (comparing fst) values
      repeatResults <- (`mapM` repeats) $ \k -> J.lookup k j
      norepeatResults <- (`mapM` noRepeats) $ \k -> do
        J.lookup k j
      performGC
      length repeatResults `shouldBe` length repeats
      length norepeatResults `shouldBe` length noRepeats
      repeatResults `shouldSatisfy` all (== Just (-1))
      norepeatResults `shouldSatisfy` all (\(Just a) -> a >= 0)
