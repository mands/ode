{-
data T = TI Int | TS String
plus :: T -> T -> T
plus (TI a) (TI b) = TI (a+b)

concat :: T -> T -> T
concat (TS a) (TS b) = TS (a++b)
-}

data T a = TI Int | TS String deriving Show
plus :: T Int -> T Int -> T Int
plus (TI a) (TI b) = TI (a+b)
plus _ _ = error "wrong type"

concat :: T String -> T String -> T String
concat (TS a) (TS b) = TS (a++b)
concat _ _ = error "wrong type"

{- usage
plus (TI 3 :: T Int) (TI 4 :: T Int)
plus (TS "324" :: T String) (TI 4 :: T Int)

concat (TS "324" :: T String) (TS "-hello" :: T String)
concat (TI 3 :: T Int) (TI 4 :: T Int)
-}

