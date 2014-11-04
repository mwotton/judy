{-# LANGUAGE ScopedTypeVariables #-}
module Data.JudySpec where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Judy as J
import Control.Monad (guard)

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
