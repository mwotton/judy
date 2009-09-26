import Control.Monad
import qualified Data.Judy as J

main = do
    j <- J.new
    forM_ [1..10000000] $ \n -> J.insert n n j
    J.delete 100 j
    v <- J.lookup 100 j
    print v
