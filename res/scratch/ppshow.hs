import Text.Show.Pretty

data TestData = TestData String Integer deriving Show

testData1 = TestData "Hello" 0
testData2 = TestData "World" 1

data TestRec = TestRec { str :: String, int :: [Integer] } deriving Show
testRec1 = TestRec "Hello" [0,1,2,3]
testRec2 = TestRec "World" [3,2,1,0]

data TestRecA = TestRecA { _str :: String, _int :: [Integer] } deriving Show
testRecA1 = TestRecA "Hello" [0,1,2,3]
testRecA2 = TestRecA "World" [3,2,1,0]


