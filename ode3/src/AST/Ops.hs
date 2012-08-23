-----------------------------------------------------------------------------
--
-- Module      :  Lang.Common.Ops
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | This file holds the list of all built-in operators in the language including,
--
-- * basic numerical ops - add, mult, etc.
-- * logical ops - and, or, etc.
-- * mathematical ops - trig, exp, etc.
--
-- it is basically a hardcoded stdlib that wraps/interfaces to many GNU C functions
-- we manually include the types here, it's basically the Ode FFI
-- TODO - maybe add a general FFI ??
-----------------------------------------------------------------------------

module AST.Ops (
Op(..), BasicOp(..), MathOp(..), OtherOp(..) -- , opReservedNames
) where


import Control.Applicative
import Control.Monad.Identity

-- Op Definitions -------------------------------------------------------------------------------------------

-- Currently suported operations
data Op = BasicOp BasicOp | MathOp MathOp | OtherOp OtherOp deriving (Show, Eq, Ord)

-- basic operators implemented as CPU ops
data BasicOp    =
                -- Basic Ops
                  Add           -- (f, f) -> f
                | Sub           -- (f, f) -> f
                | Mul           -- (f, f) -> f
                | Div           -- (f, f) -> f
                | Mod           -- (f, f) -> f
                -- Relational Ops
                | LT            -- (f, f) -> b
                | LE            -- (f, f) -> b
                | GT            -- (f, f) -> b
                | GE            -- (f, f) -> b
                | EQ            -- (f, f) -> b
                | NEQ           -- (f, f) -> b
                -- logical Ops
                | And           -- (b, b) -> b
                | Or            -- (b, b) -> b
                | Not           -- b -> b
                | Neg           -- Only used within frontend
                deriving (Show, Eq, Ord)

-- these ops copied direct from GNU Math library (libm)
data MathOp     =
                -- basic trig
                  Sin           -- f -> f
                | Cos           -- f -> f
                | Tan           -- f -> f
                | SinCos        -- f -> (f,f)
                -- inverse trig
                | ASin          -- f -> f
                | ACos          -- f -> f
                | ATan          -- f -> f
                | ATan2         -- (f,f) -> f
                -- exponentials
                | Exp           -- f -> f
                | Exp2          -- f -> f
                | Exp10         -- f -> f
                | Pow10         -- f -> f
                -- logs
                | Log           -- f -> f
                | Log2          -- f -> f
                | Log10         -- f -> f
                | LogB          -- f -> f
                -- powers
                | Pow           -- (f,f) -> f
                | Sqrt          -- f -> f
                | Cbrt          -- f -> f
                -- specialised, composite, funcs
                | Hypot         -- (f,f) -> f
                | ExpM1         -- f -> f
                | Log1P         -- f -> f
                -- hyperbolics
                | SinH          -- f -> f
                | CosH          -- f -> f
                | TanH          -- f -> f
                | ASinH         -- f -> f
                | ACosH         -- f -> f
                | ATanH         -- f -> f
                -- special funcs (not including Bessel funcs.)
                | Erf           -- f -> f
                | ErfC          -- f -> f
                | LGamma        -- f -> f
                | Gamma         -- f -> f
                | TGamma        -- f -> f
                deriving (Show, Eq, Ord)

-- custom operations
data OtherOp    =
                -- random numbers (assume automatic seeding)
                Rand          -- None -> f (0 < f < 1)
                deriving (Show, Eq, Ord)

-- Parser -------------------------------------------------------------------------------------------

-- used by parser
-- moved

-- Types -------------------------------------------------------------------------------------------

