-----------------------------------------------------------------------------
--
-- Module      :  Scratch
-- Copyright   :  Copyright (C) 2010 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Test and Sample code that relies on other modules
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs, ExistentialQuantification, PatternGuards, KindSignatures, ScopedTypeVariables,
FlexibleInstances, TypeFamilies, UndecidableInstances, FlexibleContexts  #-}

module Scratch (
runner, funrunner
) where

import LLVM.Core
import LLVM.ExecutionEngine

import Utilities
import AST
import TypedAST
import qualified ExecutableAST as E
import qualified Compile as C
-- a couple of test expressions ASTs
--e1 :: TExp Double
e1 = TDbl 3
e2 = TDblOp (TDbl 3) Add (TDbl 3)

--t_e1 :: ATExp
t_e1 = ATExp e1 TTDbl
t_e2 = ATExp e2 TTDbl

-- huh? shouldn't be allowed!, but this occurs when we try to create existential
e4 :: TExp Int
e4 = TVar "test"
-- t_e4 = ATExp e4 TTDbl

-- now some untyped expressions
-- from a string? unneeded
u_e3 = E.Number 5.0
--t_e3 :: Maybe ATExp
t_e3 = typeCheckExp [] u_e3

-- lets just see if we can extract our AST and do stuff with it...
runner :: [String]
runner = [myshow (eval e1), show (sin $ eval e2), maybe "nothing" extractExp2 t_e3]
--    [show e1, show f1, extractExp t_e1, extractExp t_e2, ]

-- tries to extract an expression and show it, doesn't work for eval
-- e isn't a real type
--extractExp :: ATExp -> String
--extractExp (ATExp e t) = show (sin $ eval e) --show e

-- works if we cast/pattern match the type tag during extraction
extractExp2 :: ATExp -> String
extractExp2 (ATExp e TTDbl) = show (sin $ eval e) --show e
extractExp2 (ATExp e _) = "nothing"

-- how about if we try to Eq the value before eval
extractExp3 :: ATExp -> String
extractExp3 (ATExp e t) = tryshow (test t TTDbl) (eval e)
  where
    tryshow :: (Maybe (Equal t Double)) -> t -> String
    tryshow (Just Eq) e = show (sin e)

-- and again in monadic form, similar to Exp3 above
extractExp4 :: ATExp -> Maybe String
extractExp4 (ATExp e t) = do
    Eq <- test t TTDbl
    return (show $ sin (eval e))

-- testing the dyn type, wraps any value/type up again
data Dynamic where
    Dyn :: TTyp a -> a -> Dynamic
-- is similar to extractExp2
extractExp5 :: ATExp -> Maybe Dynamic
extractExp5 (ATExp e t) = do
    return (Dyn t (eval e))

testExp5 :: Dynamic -> String
testExp5 (Dyn TTDbl x) = show x

-- do we need a type class or something to create the (TTyp a) for matching?
-- or chain the maybe for each static type together until correct match found

-- try create a typeclass variant and trigger it by callin a (TExp Double) only func
class MyType a where
    mytype :: TTyp a
instance MyType Double where
    mytype = TTDbl
instance (MyType a, MyType b) => MyType (a-> b) where
    mytype = TTArr mytype mytype


runnerExp6 :: ATExp -> String
runnerExp6 e = maybe "nothing" testFunc6 (extractExp6 e)

extractExp6 :: (MyType a) => ATExp -> Maybe (TExp a)
extractExp6 = extractExp6' mytype

extractExp6' :: TTyp a -> ATExp -> Maybe (TExp a)
--extractExp6' :: TTyp Double -> ATExp -> Maybe String
extractExp6' s (ATExp e t) = do
    Eq <- test t s
    return $ e

testFunc6 :: TExp Double -> String
testFunc6 e =  myshow (eval e)

-- try to simplfy Exp6
runnerExp7 :: ATExp -> String
runnerExp7 e = maybe "nothing" testFunc6 (extractExp7 mytype e)

extractExp7 :: (MyType a) => TTyp a -> ATExp -> Maybe (TExp a)
extractExp7 s (ATExp e t) = do
    Eq <- test t s
    return $ e

{- ideally what we want, the mytest call is self-contained, doesn't seem possible for now
extractExp7 :: (MyType a) => ATExp -> Maybe (TExp a)
extractExp7 (ATExp e t) = do
    Eq <- test t (mytype :: (MyType a) => TTyp a)
    return $ e
-}

-- try create another typeclass that has instances of double, e.g. show double
class MyShow a where
    myshow :: a -> String
instance MyShow Double where
    myshow x = "Double - " ++ show x
instance MyShow Int where
    myshow x = "Int - " ++ show x

-- can ensure evaultion can never get stuck due to incorrecte expressions
-- like in untyped ADTs due to typechecking
eval :: TExp a -> a
eval (TDbl x) = x
eval (TDblOp a op b) = (eval a) + (eval b)

{-
--extractExp1 :: TExp a -> String
--extractExp1 e =

runTExp :: TExp a -> String
runTExp e = show e

runTExp2 :: TExp a -> String
runTExp2 e = show e

--runTExp3 :: (Show a) => TExp a -> String
--runTExp3 e = show (eval e)

runTExpDbl :: TExp a -> String
runTExpDbl (TDbl x) = show (dblFun x)
runTExpDbl _ = "Nothing"

dblFun :: Double -> Double
dblFun x = sin x
-}
-- others?


-- And now for functions, can we get this right then plug into LLVM codegen
f1 :: TFun (Double)
f1 = TBody (TDbl 3)
t_f1 :: ATFun
t_f1 = ATFun f1 TTDbl

-- f2 incorrectly typed
f2 :: TFun (Int)
f2 = TBody (TVar "hello")

-- basic function
f3 :: TFun (Double -> Double)
f3 = TLam "test" TTDbl (TBody (TDbl 2))
t_f3 :: ATFun
t_f3 = ATFun f3 (TTArr TTDbl TTDbl)

-- lets just see if we can extract our AST and do stuff with it...
funrunner :: [String]
funrunner = [show f1, feval f1, extractF t_f1, testType5 t_f3]

-- can we extract the function body and eval it? it's just a (TExp a)
-- however the 'a' var could be anything, as underlying TExp can have anytype for some instances, therefore
-- unification no complete and extytra typing hints needed

-- explicit types to restict
feval :: TFun Double -> String
feval (TBody b) = "got func body" ++ (myshow $ eval b)
--feval (TLam id t b) = feval b

--feval1 :: TFun a -> String
--feval1 (TBody (b::TExp a)) = "got func body" ++ (myshow $ eval b)
--feval1 (TLam id t b) = feval1 b

-- excpicit typed - pattern matching in sub-terms
feval2 :: TFun a -> String
feval2 (TBody b@(TDbl _)) = myshow $ eval b
feval2 (TLam id t b) = feval2 b

-- restriction thru type tags, similar to dyn manner
feval3 :: TTyp a -> TFun a -> String
feval3 TTDbl (TBody e) = myshow $ eval e
feval3 (TTArr at bt) (TLam id t b) = feval3 bt b

-- could do automatically via mytype typeclass constraints
feval3' :: (MyType a) => TTyp a -> TFun a -> String
feval3' TTDbl (TBody e) = myshow $ eval e

--feval4 :: TFun a -> String
--feval4 (TBody ) = myshow $ eval e

-- can we extract from an ATFun existential container?
-- direct pattern-matching

extractF :: ATFun -> String
extractF (ATFun f t) = feval3 t f


extractF1 :: ATFun -> Maybe String
extractF1 (ATFun f t) = do
    Eq <- test t TTDbl
    return $ feval2 f

-- lets create a dummy compile function code here and see if we can run it
--extractComp :: ATFun -> CodeGenModule (Function (C.RetIO a))
--extractComp (ATFun f t) = extractComp' f t

extractComp' :: TFun a -> TTyp a -> CodeGenModule (Function (C.RetIO a))
extractComp' f TTDbl = C.compileFunction f
-- this works
extractComp' f (TTArr TTDbl TTDbl) = C.compileFunction f
extractComp' f (TTArr TTDbl (TTArr TTDbl TTDbl)) = C.compileFunction f


{-
we need to recursivly generate the TTArr value such that we create a TTyp at the type level that accuratly
reflects the function type, this was done using the haskell types and the Type typeclass to recursevily
convert from one to the other - can we do this, perhaps using recusive Double-> Double funcs, if isn't possible
then try using the value level and creating maunally from pattern matching and TTArr TTDbl constructors
-- we may need to use the Equal type to test for equality, either shallow or recursively deep, then create
bigger tests, perhaps use the Tyupe typeclass or new ones, such that eventually type-checker is convinced that the
TTyp is a function of doubles that it knows how to convert
-- structure like oleg/lennart checkers, usng Equal instead of pattern-matching.
-- do we need type-families/type-functions ?

plan
    * try create at type level
    * try pattern-maching ?
    * use Equal to create recusive checker
    * try create at value level
    * try both shallow and deep levels
    * try typeclasses and typefamilies
-}

--extractComp1 :: TFun a -> TTyp a -> CodeGenModule (Function (C.RetIO a))
--extractComp1 f TTDbl = C.compileFunction f
--extractComp1 f (TTArr a b) = C.compileFunction f

{-
extractComp1 :: TFun a -> TTyp a -> Maybe (CodeGenModule (Function (C.RetIO a)))
extractComp1 f t = do
    Eq <- test t (getType f)
    return $ C.compileFunction f

testType5 :: ATFun -> Maybe ()
testType5 (ATFun f t) = do
    Eq <- test t (getType f)
    let _ = C.compileFunction f
    return ()
-}

-- direct pattern matching
testType :: ATFun -> IO ()
testType (ATFun f TTDbl) = do
    let _ = C.compileFunction f
    return ()
testType (ATFun f (TTArr TTDbl TTDbl)) = do
    let _ = C.compileFunction f
    return ()

-- monadic typechecking
testType1 :: ATFun -> Maybe ()
testType1 (ATFun f t) = do
    Eq <- test t TTDbl
    let _ = C.compileFunction f
    return ()

-- oleg style
{-
testType2 :: ATFun -> Maybe ()
testType2 (ATFun f t) = trycompile (test t TTDbl) (C.compileFunction f)
  where
    trycompile :: (Maybe (Equal t Double)) -> CodeGenModule (Function (C.RetIO t)) -> Maybe ()
    trycompile (Just Eq) _ = Just ()
-}

-- and again
testType3 :: ATFun -> Maybe ()
testType3 (ATFun f t) = trycompile (test t TTDbl) f
  where
    trycompile :: (Maybe (Equal t Double)) -> TFun t -> Maybe ()
    trycompile (Just Eq) f = let _ = C.compileFunction f in Just ()

-- and again
{-
testType4 :: ATFun -> Maybe ()
testType4 (ATFun f t) = trycompile (test t TTDbl) (test t (TTArr a b)) f
  where
    trycompile :: (Maybe (Equal t Double)) -> (Maybe (Equal t (Double -> Double))) -> TFun t -> Maybe ()
    trycompile (Just Eq) Nothing f = let _ = C.compileFunction f in Just ()
    trycompile Nothing (Just Eq) f = let _ = C.compileFunction f in Just ()
-}


testType5 :: ATFun -> String
--testType5 (ATFun (TBody b) TTDbl) = "got double"
--testType5 (ATFun (TLam i t body) (TTArr a b)) = "got func" ++ testType5' body b
testType5 (ATFun f t) = "\\ " ++ testType5' f t

testType5' :: TFun a -> TTyp a -> String
testType5' f TTDbl = "Double"
testType5' (TLam i TTDbl body) (TTArr TTDbl b) = "Double -> "  ++ testType5' body b



testType6 :: ATFun -> Maybe ()
--testType5 (ATFun (TBody b) TTDbl) = "got double"
--testType5 (ATFun (TLam i t body) (TTArr a b)) = "got func" ++ testType5' body b
testType6 (ATFun f TTDbl) = do
    let t' = testType6' f TTDbl
    Eq <- test TTDbl t'
    let _ = C.compileFunction f
    return ()
testType6 (ATFun f t@(TTArr a b)) = do
    let t' = testType6' f t
    Eq <- test t t'
    --let _ = C.compileFunction f
    return ()


testType6' :: TFun a -> TTyp a -> TTyp a
testType6' f TTDbl = TTDbl
testType6' (TLam i TTDbl body) (TTArr TTDbl b) = TTArr TTDbl (testType6' body b)


testType7'' :: ATFun -> Maybe ()
testType7'' (ATFun f t) = do
    f <- testType7 f t
    return  ()

testType7 :: TFun a -> TTyp a ->  Maybe (TFun a)
testType7 f t =  do
    s <- testType7' t
    Eq <- test t s
    -- let _ = C.compileFunction f
    return f

-- keep recusivly building type and checking along the way
testType7' :: TTyp a -> Maybe (TTyp a)
testType7' t@TTDbl = do
    Eq <- test t TTDbl
    return TTDbl

testType7' t@(TTArr a b) = do
    Eq <- test a TTDbl
    b' <- testType7' b
    return $ TTArr TTDbl b'



testType8 :: ATFun -> Maybe ()
testType8 (ATFun f t) = do
    s <- testType8'' f t
    Eq <- test t s
    --let _ = C.compileFunction f
    return  ()
  where
{-    testType8' :: TFun a -> TTyp a ->  Maybe (TFun a)
    testType8' f' t' =  do
        s <- testType8'' f' t'
        Eq <- test t' s
        return f'
-}
    -- keep recusivly building type and checking along the way
    testType8'' :: TFun a -> TTyp a -> Maybe (TTyp a)
    testType8'' f''@(TBody e) t''@TTDbl = do
        Eq <- test t'' TTDbl
        return t''
    testType8'' (TLam i it body) t''@(TTArr a b) = do
        Eq <- test a TTDbl
        Eq <- test a it
        b' <- testType8'' body b
        return $ TTArr a b'

{-
exFun :: (Type a) => ATFun -> Maybe (TFun a)
exFun (ATFun f t) = do
    let s = (testType6' f t)
    Eq <- test s t
    ex s f t

ex :: TTyp a -> TFun a -> TTyp a -> Maybe (TFun a)
ex s f t = do
    Eq <- test t s
    return f
-}

-- this shoudl be a type function!!
{- class TestType a where
    testtype :: (TTyp a)
instance TestType (TTyp Double) where
    testtype = TTDbl
instance (TestType a, TestType b) => TestType (TTyp (a-> b)) where
    testtype = TTArr testtype testtype
-}

-- try create a type function, from Typ a to Haskell types
type family F a :: *
type instance F (TTyp Double) = Double
type instance F (TTyp (a-> b)) = (Double-> F b)
-- how can we use this?
{-
class TypeTest a where
    type Maintype :: *
    convert :: TTyp a -> Maintype

instance TypeTest (TTyp Double)
    type Maintype = Double
    convert
-}
--getType :: (C.Translate a) => TFun a -> TTyp a
--getType (TBody b) = TTDbl
--getType (TLam i t b) = TTArr t (getType b)


-- test local compiling
{-
testCompile :: ATFun -> Maybe ()
testCompile (ATFun f t) = do
    let _ = createFunction ExternalLinkage . compileFun [] t f
    return ()
-}
-- | A local compiile function
--compFun :: (C.Translate a) => TTyp a -> TFun a -> CodeGenModule (Function (C.RetIO a))
--compFun t f = createFunction ExternalLinkage . compileFun [] t f


-- trying to extract values from environment during eval a let binding
-- impossible it seems due to projecting existneitals into universals

castATValue :: TTyp a -> ATValue -> Maybe (a)
castATValue t (ATValue v s) = do
    Eq <- test t s
    return v

castATValue' :: ATValue -> Maybe (Double)
castATValue' (ATValue v s) = do
    Eq <- test TTDbl s
    return v


test' :: ATValue -> a
test' (ATValue a TTDbl) = a
--test' (ATValue a TTDbl) = test'' a TTDbl

test'' :: a -> TTyp a -> a
test'' a TTDbl = a

ext :: ATValue -> a
ext (ATValue v t) = tryExt (test t TTDbl) v


tryExt :: Maybe (Equal t Double) -> t -> t
tryExt (Just Eq) val = val
-- tryExt Nothing _ = what goes here??, this is a partial function, i.e. for existentials



