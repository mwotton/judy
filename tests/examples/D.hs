import Control.Monad
import qualified Data.IntMap as I
import Data.List

import qualified Data.ByteString.Char8 as S

main = do
   s <- S.readFile "x"

   let ls = S.lines s


   let m = foldl' (\m (v,k) ->  I.insert k v m)
                I.empty
                (zip (S.lines s) [1..])

   print (I.size m)

   print $ I.lookup 52840 m


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
