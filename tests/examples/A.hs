import Control.Monad
import qualified Data.Judy as J
import qualified Data.IntMap as I
import Data.List

import Control.Monad
import qualified Data.Judy as J

main = do
   j <- J.new :: IO (J.JudyL Int)
   forM_ [1..10000000] $ \n -> J.insert n (fromIntegral n :: Int) j
   v <- J.lookup 100 j
   print v


{-
main = do
    j <- J.new :: IO (J.JudyL Bool)
    forM_ [1..10000000] $ \n -> J.insert n True j
--    forM_ [1..10000000] $ \n -> J.delete n j
    J.insert 100 False j
    v <- J.lookup 100 j
    print v
-}

{-
-- 7.164s total
main = do
    let h = foldl' (\b a -> I.insert a (fromIntegral a) b) I.empty [1..10000000] :: I.IntMap Int

    print (I.size h)

    print (I.lookup 100 h)
-}
