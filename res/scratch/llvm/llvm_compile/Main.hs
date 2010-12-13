import Data.Maybe
import Control.Monad
import LLVM.Core
import LLVM.ExecutionEngine
import UExp
import TExp
import Compile

main = do
    initializeNativeTarget
    let f :: Double -> Double
        Just f = compile "\\ (x::Double) -> if x == 0 then 0 else 1/(x*x)"
    print (f 2, f 3, f 0)
    --let g :: Double -> Double -> Double
    let g' = compile "\\ (x::Double) (y::Double) -> if x == y then 1 else 0"
    maybe (return ()) (\g -> print (g 0 0, g 1 1, g 0 1)) g'
        
compile :: (Translate a) => String -> Maybe a
compile = fmap translate . toTFun <=< mParseUFun


