module Main where

import qualified Data.JudySpec
import           Test.Hspec   (hspec)

main :: IO ()
main = hspec Data.JudySpec.spec
