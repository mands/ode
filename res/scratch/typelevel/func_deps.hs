{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, EmptyDataDecls, FlexibleContexts, FunctionalDependencies, UndecidableInstances #-}

data Z
data S n
class Plus m n r | m n -> r


instance Plus Z n n
instance (Plus m n r) => Plus (S m) n (S r)

