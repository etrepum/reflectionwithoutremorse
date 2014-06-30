import Control.Monad
import Control.Monad.Logic.Class
import System.Environment (getArgs)

-- A micro-benchmark of LogicT measuring repeated reflection

-- the three implementations of Logic:

--import BeforeFix.Logic -- direct style implementation
import Control.Monad.Logic -- two continuation implementation
--import Fixed.Logic -- direct style implementation with our solution applied to it





choose :: MonadPlus m => [a] -> m a
choose l = foldr mplus mzero $ map return l


bench :: MonadLogic m => Int -> m Int
bench n = choose [1..n] `interleave` choose [n,n-1..1] 

main :: IO ()
main = do args <- getArgs 
          let n = read (head args)
          x <- observeAllT $ bench n
          print (show $ length x)

