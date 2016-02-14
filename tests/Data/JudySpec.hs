{-# LANGUAGE ScopedTypeVariables #-}
module Data.JudySpec where

import           Control.Monad   (guard)
import qualified Data.Judy       as J
import           Test.Hspec
import           Test.QuickCheck
import Data.List(nub, sortBy)
import Data.Ord(comparing)
import System.Mem(performGC)
import Data.Word(Word)

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

  it "should fetch keys & vals in the right order" $ do
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
        J.keys j `shouldReturn` map fst sortedL

        J.elems j `shouldReturn` map snd sortedL
        J.toList j `shouldReturn` sortedL
