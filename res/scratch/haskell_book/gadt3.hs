{-#LANGUAGE GADTs, EmptyDataDecls, KindSignatures, ExistentialQuantification #-}

-- gadt's vs existential types
 data TE a = forall b. MkTE b (b->a)
 data TG a where { MkTG :: b -> (b->a) -> TG a }

 data TE2 = forall b. Show b => MkTE2 [b]
 data TG2 where
   MkTG2 :: Show b => [b] -> TG2

