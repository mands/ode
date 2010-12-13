-----------------------------------------------------------------------------
--
-- Module      :  Compile
-- Copyright   :  Copyright (C) 2010 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts, ExistentialQuantification, FlexibleInstances, UndecidableInstances, GADTs #-}

module Compile (
Translate(..), translate, codegen,
compileFunction, Compile(..), ATValue(..), Env, compileExp

) where

import Data.Maybe
import Data.Int
import System.IO.Unsafe(unsafePerformIO)
--import LLVM.Core
--import LLVM.ExecutionEngine
import AST
import Utilities
import TypedAST

-- our run-time variable environment
type Env = [(Id, ATValue)]

-- existientiall container for dynamically typed values
data ATValue = forall a . ATValue (Value a) (TTyp a)

-- extract a value of the correct type from it's typing container
castAValue :: TTyp a -> ATValue -> Maybe (Value a)
castAValue t (ATValue v s) = do
    Eq <- test t s
    return v

-- | recurislvy compiles an expression in monadic form
compileExp :: (Type a, IsFirstClass a) => Env -> TExp a -> CodeGenFunction r (Value Double)
--compileExp :: (Type a, IsFirstClass a) => Env -> TExp a -> CodeGenFunction r (Value a)
--compileExp _ (TDbl d) = return $ valueOf d
compileExp _ _ = return $ valueOf (2.0 :: Double)


-- | attempts to extract typed fun, TFun a, from existential container and pass to compileFun
-- extractCompile :: (Translate a) => ATFun -> CodeGenModule (Function (RetIO a))
--extractCompile :: ATFun -> CodeGenModule (Function (RetIO Double))
--extractCompile (ATFun f (TTDbl)) = compileFunction f
-- extractCompile (ATFun f (TTArr TTDbl TTDbl)) = compileFunction f
{-
extractCompile (ATFun f t) = tryComp (test t TTDbl) f
  where
    tryComp :: Maybe (Equal t Double) -> (TFun t) -> CodeGenModule (Function (RetIO t))
    tryComp (Just Eq) t = compileFunction t
-}

extractComp :: ATFun -> CodeGenModule (Function (IO Int32))
extractComp (ATFun f t) = do
    test <- extractComp' f t

    -- we create a dummy function to get around the typing errors, hack again
    main <- createNamedFunction InternalLinkage "dummy" $ do
        ret (valueOf (2::Int32))

    return main
  where
--    llvm_fun =
    extractComp' :: TFun a -> TTyp a -> CodeGenModule (Function (RetIO a))
    extractComp' f TTDbl = compileFunction f
    -- this works, just about. ginaormous hack to preserve type-safety, can use TH to automate,
    -- or figure out how to type-safely generate the TArr's at, perhaps type functions/families
    extractComp' f (TTArr TTDbl TTDbl) = compileFunction f
    extractComp' f (TTArr TTDbl (TTArr TTDbl TTDbl)) = compileFunction f
    extractComp' f (TTArr TTDbl (TTArr TTDbl (TTArr TTDbl TTDbl))) = compileFunction f
    extractComp' f (TTArr TTDbl (TTArr TTDbl (TTArr TTDbl (TTArr TTDbl TTDbl)))) = compileFunction f
    extractComp' f (TTArr TTDbl (TTArr TTDbl (TTArr TTDbl (TTArr TTDbl (TTArr TTDbl TTDbl))))) = compileFunction f
    -- do we crach, maybe check for it earlier
    -- extractComp' f _ =

-- | Compile a TFun into the corresponding LLVM code.
compileFunction :: (Translate a) => TFun a -> CodeGenModule (Function (RetIO a))
compileFunction = createFunction ExternalLinkage . compileFun []

-- Serious type trickery, shamelessly stolen from LLVM tutorial posts,
-- Uses type families and type functions to guide type-checker
class Compile a where
    type CG a
    type RetIO a
    type Returns a
    compileFun :: Env -> TFun a -> CG a

instance Compile Double where
    type CG Double = CodeGenFunction Double ()
    type RetIO Double = IO Double
    type Returns Double = Double
    compileFun r (TBody e) = compileExp r e >>= ret
    -- TLam is not well typed

instance (Type a, Compile b) => Compile (a -> b) where
    type CG (a-> b) = Value a -> CG b
    type RetIO (a-> b) = a -> RetIO b
    type Returns (a-> b) = Returns b
    -- TBody is not well typed
    compileFun r (TLam i t e) = \ x -> compileFun ((i, ATValue x t):r) e

-- our typed contraints/interface to the llvm module
class    (Type a, Unsafe (RetIO a) a,
          FunctionArgs (RetIO a) (CG a) (CodeGenFunction (Returns a) ()),
          IsFunction (RetIO a),
          Compile a,
          Translatable (RetIO a)) => Translate a
instance (Type a, Unsafe (RetIO a) a,
          FunctionArgs (RetIO a) (CG a) (CodeGenFunction (Returns a) ()),
          IsFunction (RetIO a),
          Compile a,
          Translatable (RetIO a)) => Translate a

-- compiler interface
-- translates/jits a single funcction to a haskell native
translate :: (Translate a) => TFun a -> a
translate = unsafePerformIO . fmap unsafePurify . simpleFunction . compileFunction

-- | builds a general purpose module with a main funcition that is fully typed inn haskell,
-- | we can jit execute within IO or dump to disk
buildMod :: [ATFun] -> CodeGenModule (Function (IO Int32))
buildMod funs = do
    -- test fun
    f <- compileFunction (TLam "test" TTDbl (TLam "test1" TTDbl (TBody (TDbl 42.0))))

--    fun <- extractCompile (head funs)

    -- try to build the functions
    funs <- mapM extractComp funs

    main <- createNamedFunction ExternalLinkage "main" $ do
        ret (valueOf (2::Int32))

    return main


codegen :: [ATFun] -> MExcept (IO ())
codegen funs = return $ do
    initializeNativeTarget

    let g :: Double -> Double
        g = translate (TLam "test" TTDbl (TBody (TDbl 2)))
    print (g 1.0)

    -- create the module
    mod <- newNamedModule "odecModule"
    -- add module body - destructive
    mainFunc <- defineModule mod (buildMod funs)
    -- write to file to inspect it
    writeBitcodeToFile "res/output.bc" mod
