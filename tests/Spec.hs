module Main where

import qualified Data.JudySpec
import           Test.Hspec   (hspec)

main :: IO ()
-- Keep this explicit list in sync with any future *Spec modules.
main = hspec Data.JudySpec.spec
