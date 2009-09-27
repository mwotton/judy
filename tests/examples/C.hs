import Control.Monad
import qualified Data.Judy as J
import qualified Data.IntMap as I
import qualified Data.Map as M
import Data.List

import Control.Monad
import qualified Data.Judy as J

import qualified Data.ByteString.Char8 as S

{-
main = do
   s <- S.readFile "x"

   j <- J.new :: IO (J.JudyL S.ByteString)
--   J.insert 1 (S.pack "foo") j

   sequence_
        [ J.insert i x j
        | (x,i) <- zip (S.lines s) [1..]
        ]

   v <- J.lookup 52840 j
   print v
   -}


{-
main = do
    j <- J.new :: IO (J.JudyL Bool)
    forM_ [1..10000000] $ \n -> J.insert n True j
--    forM_ [1..10000000] $ \n -> J.delete n j
    J.insert 100 False j
    v <- J.lookup 100 j
    print v
-}

-- 7.164s total
main = do
    let h = foldl' (\b a -> M.insert a (fromIntegral a) b) M.empty [1..100000000] :: M.Map Int Int
    print (M.size h)
    print (M.lookup 100 h)
