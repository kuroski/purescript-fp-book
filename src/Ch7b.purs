module Ch7b where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)

newtype CSV
  = CSV String

derive instance newTypeCSV :: Newtype CSV _

derive newtype instance eqCSV :: Eq CSV

derive newtype instance showCSV :: Show CSV

class ToCSV a where
  toCSV :: a -> CSV

-- From CSV
class FromCSV a where
  fromCSV :: CSV -> Maybe a

-- Implementation Example
newtype FullName
  = FullName String

instance showFullName :: Show FullName where
  show (FullName name) = name

derive newtype instance eqFullName :: Eq FullName

newtype Age
  = Age Int

-- Same thing as fullName
derive instance newtypeAge :: Newtype Age _

derive newtype instance showAge :: Show Age

derive newtype instance eqAge :: Eq Age

data Occupation
  = Doctor
  | Dentist
  | Developer
  | Unemployed

derive instance eqOccupation :: Eq Occupation

derive instance genericOccupation :: Generic Occupation _

instance showOccupation :: Show Occupation where
  show = genericShow

toOccupation :: String -> Maybe Occupation
toOccupation = case _ of
  "Doctor" -> Just Doctor
  "Dentist" -> Just Dentist
  "Developer" -> Just Developer
  "Unemployed" -> Just Unemployed
  _ -> Nothing

data Person
  = Person
    { name :: FullName
    , age :: Age
    , occupation :: Occupation
    }

derive instance eqPerson :: Eq Person

derive instance genericPerson :: Generic Person _

instance showPerson :: Show Person where
  show = genericShow

instance toCSVPerson :: ToCSV Person where
  toCSV (Person { name, age, occupation }) = CSV $ show name <> "," <> show age <> "," <> show occupation

-- This will convert the person to CSV, but I will learn better
-- about monads in future chapters to refactor this code.
instance fromCSVPerson :: FromCSV Person where
  fromCSV (CSV str) = case split (Pattern ",") str of
    [ name, age, occupation ] -> case fromString age of
      Just age' -> case toOccupation occupation of
        Just occupation' ->
          Just
            $ Person
                { name: FullName name
                , age: Age age'
                , occupation: occupation'
                }
        Nothing -> Nothing
      Nothing -> Nothing
    _ -> Nothing
