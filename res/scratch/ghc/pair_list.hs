
-- standard list
data L a = N | L a (L a) deriving Show

--let a = L 1 (L 3 N)

-- pair - but allows mult types
data P a b = P a (P a b) | E deriving Show

-- single type pair
data P1 a = P1 a a

-- 2-elem list - but need to add a single elements in order to cons
data L2 a = L2 a a (L21 a) deriving Show
data L21 a = L21 a (L21 a) | N2 deriving Show

-- basically same as above but uses lsit for second
data Tuple a = Tuple a a [a] deriving Show

-- is not worth it, easier just to use Pair-consing or lists with a single-elem check

