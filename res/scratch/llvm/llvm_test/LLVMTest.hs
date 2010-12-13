module Main (
main
) where

import Data.Int
import Data.Word
import LLVM.Core
import LLVM.ExecutionEngine
import LLVM.Util.Arithmetic


-- tests llvm haskell bindings via both static and jit compilation
main = do
    -- jit function
    initializeNativeTarget
    greet <- simpleFunction bldGreet
    greet
    addMul <- simpleFunction mAddMul
    let addMul' = unsafePurify addMul
    print (addMul' 2 3 4)
    let fib = unsafeGenerateFunction mFib
    let fib1 :: Int32 -> Double
        fib1 = unsafeGenerateFunction mFib1
    print (fib 22, fib1 22)

    let lNormCDF = unsafeGenerateFunction mNormCDF
    print ("normcdf 0.01 - " ++ show (lNormCDF 0.01))

    -- write out function as bitcode in module
    mod <- newNamedModule "testMod"
    defineModule mod mAddMul
    defineModule mod mFib
    defineModule mod bldGreet
    defineModule mod mNormCDF
    writeBitcodeToFile "test.bc" mod    
    
    
-- | llvm test functions
mAddMul :: CodeGenModule (Function (Int32 -> Int32 -> Int32 -> IO Int32))
mAddMul = 
    createFunction ExternalLinkage $ \ x y z ->  
        do  t <- add x y
            r <-  mul t z
            ret r


-- | llvm test functions - linking to libc
bldGreet :: CodeGenModule (Function (IO ()))
bldGreet = do
    puts <- newNamedFunction ExternalLinkage "puts" :: TFunction (Ptr Word8 -> IO Word32)
    greetz <- createStringNul "Hello, World!"
    func <- createFunction ExternalLinkage $ do
      tmp <- getElementPtr greetz (0::Word32, (0::Word32, ()))
      call puts tmp -- Throw away return value.
      ret ()
    return func

-- | llvm arithmetic functions (uses instance of Num typeclass)
mFib :: CodeGenModule (Function (Int32 -> IO Int32))
mFib = recursiveFunction $ \ rfib n ->
    n %< 2 ? (1, rfib (n-1) + rfib (n-2))


-- | type generalisation, polymoprhic inputs, can't generate directly
-- mFib1 :: (Num a, Cmp a, IsConst a, Num b, Cmp b, IsConst b, FunctionRet b) => CodeGenModule (Function (a -> IO b))
mFib1 = recursiveFunction $ \ rfib n ->
    n %< 2 ? (1, rfib (n-1) + rfib (n-2))

-- | more complicated math funcs, look at mix of haskell and llvm jit code
mNormCDF :: CodeGenModule (Function (Float -> IO Float))
mNormCDF = createFunction ExternalLinkage ((arithFunction  normcdf))

-- is valid haskell, llvm scalar and llvm vector code!
normcdf x = x %< 0 ?? (1 - w, w)
  where w = 1.0 - 1.0 / sqrt (2.0 * pi) * exp(-l*l / 2.0) * poly k
        k = 1.0 / (1.0 + 0.2316419 * l)
        l = abs x
        poly = horner coeff 
        coeff = [0.0,0.31938153,-0.356563782,1.781477937,-1.821255978,1.330274429] 

horner coeff base = foldr1 multAdd coeff
  where multAdd x y = y*base + x

blackscholes iscall s x t r v = iscall ? (call, put)
  where call = s * normcdf d1 - x*exp (-r*t) * normcdf d2
        put  = x * exp (-r*t) * normcdf (-d2) - s * normcdf (-d1)
        d1 = (log(s/x) + (r+v*v/2)*t) / (v*sqrt t)
        d2 = d1 - v*sqrt t
