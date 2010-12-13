{-# LANGUAGE TypeFamilies, FlexibleContexts, ExistentialQuantification, FlexibleInstances, UndecidableInstances, GADTs #-}

-- test code for multiple parameters and return values
import Data.Int
import System.IO.Unsafe(unsafePerformIO)
import LLVM.Core
import LLVM.ExecutionEngine

-- our mini GADT syntax tree for functions
-- to expand into MRVs
type Id = String

data TExp a where
    TDbl   :: Double -> TExp Double

data TFun a where
    TBody :: TExp a                 -> TFun a
    TLam  :: Id -> TTyp a -> TFun b -> TFun (a->b)

data TTyp a where
    TTDbl ::                     TTyp Double
    TTArr :: TTyp a -> TTyp b -> TTyp (a->b)

class Type a where
    theType :: TTyp a
instance Type Double where
    theType = TTDbl
instance (Type a, Type b) => Type (a->b) where
    theType = TTArr theType theType

-- | Compile a TFun into the corresponding LLVM code.
compileFunction :: (Translate a) =>
                   TFun a -> CodeGenModule (Function (RetIO a))
compileFunction = createFunction ExternalLinkage . compileFun


class Compile a where
    type CG a
    type RetIO a
    type Returns a
    compileFun :: TFun a -> CG a

instance Compile Double where
    type CG Double = CodeGenFunction Double ()
    type RetIO Double = IO Double
    type Returns Double = Double
    compileFun (TBody e) = compileExp >>= ret
    -- TLam is not well typed

instance (Type a, Compile b) => Compile (a -> b) where
    type CG (a->b) = Value a -> CG b
    type RetIO (a->b) = a -> RetIO b
    type Returns (a->b) = Returns b
    -- TBody is not well typed
    -- compileFun (TLam i t e) = \ x -> compileFun ((i, AValue x t):r) e
    compileFun (TLam i t e) = \ x -> compileFun e


-- body of a func
compileExp :: CodeGenFunction r (Value Double)
compileExp = return $ valueOf (25.435::Double)

-- run a (pure) jit function
translate :: (Translate a) => TFun a -> a
translate = unsafePerformIO . fmap unsafePurify . simpleFunction . compileFunction

class    (Type a,
          Unsafe (RetIO a) a,
          FunctionArgs (RetIO a) (CG a) (CodeGenFunction (Returns a) ()),
          IsFunction (RetIO a),
          Compile a,
          Translatable (RetIO a)) =>
    Translate a
instance (Type a,
          Unsafe (RetIO a) a,
          FunctionArgs (RetIO a) (CG a) (CodeGenFunction (Returns a) ()),
          IsFunction (RetIO a),
          Compile a,
          Translatable (RetIO a)) =>
    Translate a


{-
compileFunc :: [Double] -> CodeGenModule (Function (IO Int32))
compileFunc args = createFunction ExternalLinkage . buildFunc args


-- recursively builds a func one arg at a time
buildFunc :: [Double] -> CodeGenFunction Double ()

buildFunc :: [Double] -> Value a ->  ()

-- build the args
buildFunc (x:xs) = \ y -> buildFunc xs

-- build body and return
buildFunc [] = do
    let b = valueOf (2::Int32)
    ret b
-}

main = do
    initializeNativeTarget
    -- cast and run the function
    let f :: Double
        f = translate (TBody (TDbl 3))
    print (f)

    -- a fun with mult args
    let g :: Double -> Double
        g = translate (TLam "test" TTDbl (TBody (TDbl 3)))
    print (g 2.0)

    -- create the module
    mod <-  newNamedModule "testModule"
    -- add module definition/body to mod - destructive !!
    mainFunc <-  defineModule mod buildMod
    -- write it to file so we can test it
    writeBitcodeToFile "output.bc" mod

    return ()

{-
    let f :: Double -> Double
        Just f = compile "\\ (x::Double) -> if x == 0 then 0 else 1/(x*x)"
    print (f 2, f 3, f 0)
    let g :: Double -> Double -> Double
        Just g = compile "\\ (x::Double) (y::Double) -> if x == y then 1 else 0"
    print (g 0 0, g 1 1, g 0 1)
-}


-- | build general module containing a single entry point for now
buildMod :: CodeGenModule (Function (IO Int32))
buildMod = do

    -- fun with no args
    f <- compileFunction (TBody (TDbl 3))
    -- fun with one arg
    g <- compileFunction (TLam "test" TTDbl (TBody (TDbl 3)))
    -- fun with mult args
    h <- compileFunction (TLam "test" TTDbl (TLam "test1" TTDbl (TLam "test2" TTDbl (TBody (TDbl 3)))))

    -- build the main function
    main <-  createNamedFunction ExternalLinkage "main" $ do
        ret (valueOf (2::Int32))
    return main


