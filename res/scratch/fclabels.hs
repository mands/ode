{-# LANGUAGE TemplateHaskell, TypeOperators #-}
import Control.Category
import Data.Label
import Prelude hiding ((.), id)
import Control.Applicative

data Person = Person
  { _name   :: String
  , _age    :: Int
  , _isMale :: Bool
  , _place  :: Place
  } deriving Show

data Place = Place
  { _city
  , _country
  , _continent :: String
  } deriving Show

$(mkLabels [''Person, ''Place])

-- examples
jan :: Person
jan = Person "Jan" 71 True (Place "Utrecht" "The Netherlands" "Europe")

moveToAmsterdam :: Person -> Person
moveToAmsterdam = set (city . place) "Amsterdam"

ageAndCity :: Person :-> (Int, String)
ageAndCity = Lens $ (,) <$> fst `for` age <*> snd `for` city . place

moveToAmsterdamOverTwoYears :: Person -> Person
moveToAmsterdamOverTwoYears = modify ageAndCity (\(a, _) -> (a+2, "Amsterdam"))

