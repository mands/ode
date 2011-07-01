
data L a = N | L a (L a) deriving Show

--let a = L 1 (L 3 N)

data P a b = P a (P a b) | E deriving Show

