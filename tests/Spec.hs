module Main where

import qualified Data.JudySpec
import           Test.Hspec   (hspec)

main :: IO ()
-- Data.JudySpec is currently the only *Spec module under tests/.
main = hspec Data.JudySpec.spec
