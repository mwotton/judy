--
-- Generate 1 million random integers.
-- Report the largest one we see.
--

import System.Random.Mersenne
import qualified Data.Judy as J
import Control.Monad

main = do
    g  <- getStdGen
    rs <- randoms g
    j  <- J.new :: IO (J.JudyL Int)
    forM_ (take 1000000 rs) $ \n ->
        J.insert n 1 j
    v  <- J.findMax j
    case v of
         Nothing    -> print "Done."
         Just (k,_) -> print k
