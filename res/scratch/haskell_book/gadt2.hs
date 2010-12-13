{-#LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}
-- here we add the KindSignatures pragma,
-- which makes the GADT declaration a bit more elegant.

data NotSafe
data Safe


data MarkedList             ::  * -> * -> * where
  Nil                       ::  MarkedList t NotSafe
  Cons                      ::  a -> MarkedList a b -> MarkedList a c


safeHead                    ::  MarkedList a Safe -> a
safeHead (Cons x _)          =  x


silly 0                      =  Nil
silly 1                      =  Cons () Nil
silly n                      =  Cons () $ silly (n-1)

