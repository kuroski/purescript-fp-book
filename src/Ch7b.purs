module Ch7b where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype CSV
  = CSV String

derive instance newTypeCSV :: Newtype CSV _

derive newtype instance eqCSV :: Eq CSV

derive newtype instance showCSV :: Show CSV

class ToCSV a where
  toCSV :: a -> CSV

newtype FullName
  = FullName String

instance showFullName :: Show FullName where
  show (FullName name) = name

newtype Age
  = Age Int

-- Same thing as fullName
derive instance newtypeAge :: Newtype Age _

derive newtype instance showAge :: Show Age

data Occupation
  = Doctor
  | Dentist
  | Developer
  | Unemployed

derive instance genericOccupation :: Generic Occupation _

instance showOccupation :: Show Occupation where
  show = genericShow

data Person
  = Person
    { name :: FullName
    , age :: Age
    , occupation :: Occupation
    }

instance toCSVPerson :: ToCSV Person where
  toCSV (Person { name, age, occupation }) = CSV $ show name <> "," <> show age <> "," <> show occupation
