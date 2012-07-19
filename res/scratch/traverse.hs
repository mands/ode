{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures, DataKinds #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

import Data.Foldable
import Data.Traversable

-- genric traversable data strcuture
data Type ::  * where
    TVar ::  Int -> Type
    TFloat ::  Type
    TArr ::  Type -> Type -> Type
    TTuple ::  [Type] -> Type -- don't want to allow tuples of tuples
    deriving (Show, Eq, Ord)

-- can we automate this
mapType :: (Type -> Type) -> Type -> Type
mapType f (TArr t1 t2) = TArr (mapType f t1) (mapType f t2)
mapType f (TTuple ts) = TTuple $ map (mapType f) ts
mapType f t = f t

testF :: Type -> Type
testF (TVar _) = TFloat
testF t = t

testT = TTuple [TFloat, TVar 1, TArr (TTuple [TVar 2, TFloat]) (TVar 3)]

