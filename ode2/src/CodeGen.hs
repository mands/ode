-----------------------------------------------------------------------------
--
-- Module      :  CodeGen
-- Copyright   :  Copyright (C) 2010 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Code generation module via LLVM
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, UndecidableInstances, GADTs, ExistentialQuantification #-}

module CodeGen (
codeGen

) where

import System.Log.Logger
import Data.Int
import Data.Word
import Data.Maybe
import qualified Data.Map as Map
import Data.Foldable
{-
import LLVM.Core
import LLVM.ExecutionEngine
import LLVM.Util.Optimize
-}
import qualified ExecutableAST as E

-- | Code generation entry function, takes an exe ast and generates llvm bitcode
-- | all runs in IO monad due to LLVM bindings
-- | atm we gerenate a module per model that is both output to bc and jit function
-- | jit maybe better long term as can interface easier with haskell code, and
-- | create a haskell vm for modelling
codeGen :: E.Model -> IO ()
codeGen model =
{-
    do  infoM "ODEc.codeGen" "Starting codegen"
        -- setup the jit
        initializeNativeTarget
        -- create the module
        mod <- newNamedModule "testModule"

        -- add module definition/body to mod - destructive !!
        mainFunc <- defineModule mod $ buildMod model

        -- write it to file so we can test it
        writeBitcodeToFile "res/output.bc" mod
        --_ <- optimizeModule 3 mod
        --writeBitcodeToFile "res/output-opt.bc" mod

        -- jit compile the module and obtain the main func
        jitMainFunc <- runEngineAccess $ do
            addModule mod
            generateFunction $ mainFunc

        -- execute in IO monad as is not pure
        jitMainFunc >>= print
-}
        infoM "ode2.codeGen" "Done codegen"

{-
-- | data wrapped and exportable in a codegen monad - single func for now
type Mod = Function (IO Int32)

-- | build general module containing a single entry point for now
buildMod :: E.Model -> CodeGenModule Mod
buildMod model = do
    -- call the test functions here, as in monad they are executed once when you lift them
    -- within the IO monad
    fGreet <- greet
    fAddMul <- addMul

    let modelMain = fromJust $ Map.lookup "main" model

    -- messy, clean up in executable stage
    let entryFuncsIds = map (\(E.ValueDef _ vValue)  -> case vValue of (E.IntFuncCall id _) -> id; _ -> undefined) (E.fBody modelMain)
    let entryFuncs = map (\id -> fromJust $ Map.lookup id model) entryFuncsIds

    -- print entryFuncs
    -- build up list of functions from a traveral of the components graph
    --let env = return Map.empty -- :: Map.Map E.Identifier (Function f))

    -- get the functions into the main module
    -- funcs <- foldlM buildFuncs (Map.empty :: Map.Map E.Identifier (Function f)) entryFuncs

    --env <- t1 entryFuncs
    let funcs = map buildFunc' entryFuncs
    f <- sequence funcs


    -- build the main function
    main <- createNamedFunction ExternalLinkage "main" $ do
        -- call a few test functions
        call fGreet
        res <- call fAddMul (valueOf 1) (valueOf 2) (valueOf 3)
        -- do main func work here


        -- get the entry points from the simulate stmts



        -- for each simulate statement generate a "for loop" around the entry component
        -- eventually to be removed in favour of a (hakell?)-repl - shellac


        -- return error code
        ret res
    return main
-}

{-
-- tail recursive call to build up functions as they are encountered within the model
-- TODO - should move into executable model gen ?
buildFuncs :: Map.Map E.Identifier (Function f) -> E.Function -> CodeGenModule (Map.Map E.Identifier (Function f))
buildFuncs modEnv (E.Function name ins outs body) = do
    -- TODO - setup inputs/outputs/etc - void for now
    func <- createNamedFunction InternalLinkage name $ do
        ret ()
    return (Map.insert name func modEnv)
-}

{-

buildFunc' :: (IsFunction f, FunctionArgs f g (CodeGenFunction r ()))  => E.Function -> CodeGenModule (Function f)
--buildFunc' :: IsFunction f => E.Function -> CodeGenModule (Function f)
--buildFunc' :: E.Function -> CodeGenModule (Function (IO Int32))
--buildFunc' :: forall a . (FunctionRet a) => E.Function -> CodeGenModule (Function (IO a))
buildFunc' (E.Function name ins outs body) = createNamedFunction ExternalLinkage name $ do
    let b = valueOf (2::Int32)
    ret (b :: Value r)

-}

{-
t1 :: [E.Function] -> CodeGenModule (Map.Map E.Identifier (Function f))
t1 funcs = do
    foldlM buildFuncs Map.empty funcs
  where
    buildFuncs env (E.Function name ins outs body) = env
-}

{-

    -- fold over the func body, adding each expression to the local enviornment
    foldl (buildFuncExpr) (modEnv, funcEnv) $ E.fBody func
  where
    funcEnv = Map.empty
    buildFuncExpr (modEnv, funcEnv) (E.FuncStmt id expr) =
        -- store as a value, indexed by id
        (modEnv, Map.insert id val funcEnv)
      where
        val = buildExpr modEnv funcEnv expr
-}


{-
-- take cur module env (and func env?)
buildExpr modEnv funcEnv (E.IntFuncCall id exprs) =
    -- code gen the args
    map buildFuncExpr exprs

    -- check if func exists
    let func = case (Map.lookup id env) of
        Nothing -> buildFuncs env (fromJust $ Map.lookup id model)
        Just f -> f

    -- call the func
-}
-- TODO, other expressions
-- buildFuncExpr env _ = undefined

{-
-- | standalone llvm test function
addMul :: TFunction (Int32 -> Int32 -> Int32 -> IO Int32)
addMul = createNamedFunction ExternalLinkage "mAddMul" $ \ x y z ->
        do
            t <- add x y
            r <- mul t z
            ret r

-- | standalone llvm test function
greet :: TFunction (IO ())
greet = do
    puts <- (newNamedFunction ExternalLinkage "puts" :: TFunction (Ptr Word8 -> IO Word32))
    greetz <- createStringNul "Hello, World!"
    func <- createNamedFunction ExternalLinkage "bldGreet" $ do
        tmp <- getElementPtr greetz (0::Word32, (0::Word32, ()))
        call puts tmp -- Throw away return value.
        ret ()
    return func
-}
