module Ch6 where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)

-- Custom Typeclasses
class HasAddress a where
  getAddress :: a -> Address

-- Application
data Place
  = First
  | Second
  | Third

instance eqPlace :: Eq Place where
  eq First First = true
  eq Second Second = true
  eq Third Third = true
  eq _ _ = false

instance ordPlace :: Ord Place where
  compare First First = EQ
  compare First _ = LT
  compare Second Third = LT
  compare Second Second = EQ
  compare Second First = GT
  compare Third Third = EQ
  compare Third _ = GT

instance showPlace :: Show Place where
  show First = "First"
  show Second = "Second"
  show Third = "Third"

data Address
  = Address
    { street1 :: String
    , street2 :: String
    , city :: String
    , state :: String
    , zip :: String
    }

instance eqAddress :: Eq Address where
  eq (Address a1) (Address a2) = a1 == a2

type Directions
  = String

data Person
  = Person
    { name :: String
    , age :: Int
    , address :: Address
    }

instance hasAddressPerson :: HasAddress Person where
  getAddress (Person p) = p.address

instance eqPerson :: Eq Person where
  eq (Person p1) (Person p2) = p1.name == p2.name && p1.age == p2.age && p1.address == p2.address

data Company
  = Company
    { name :: String
    , address :: Address
    }

instance hasAddressCompany :: HasAddress Company where
  getAddress (Company c) = c.address

data Residence
  = Home Address
  | Facility Address

instance hasAddressResidence :: HasAddress Residence where
  getAddress (Home address) = address
  getAddress (Facility address) = address

data EmptyLot
  = EmptyLot
    { daysEmpty :: Int
    , price :: Int
    , address :: Address
    }

instance hasAddressEmptyLot :: HasAddress EmptyLot where
  getAddress (EmptyLot e) = e.address

person :: Person
person =
  Person
    { name: "Joe Mama"
    , age: 22
    , address:
        Address
          { street1: "123 Main Street"
          , street2: "Apt 152"
          , city: "Jamestown"
          , state: "CA"
          , zip: "95327"
          }
    }

company :: Company
company =
  Company
    { name: "Acme"
    , address:
        Address
          { street1: "Business Street"
          , street2: "Suite 101"
          , city: "Irvine"
          , state: "CA"
          , zip: "92602"
          }
    }

home :: Residence
home =
  Home
    ( Address
        { street1: "1 1st Street"
        , street2: "Apt 1"
        , city: "Buford"
        , state: "WY"
        , zip: "82052"
        }
    )

facility :: Residence
facility =
  Facility
    ( Address
        { street1: "54321 Contdown Ave"
        , street2: ""
        , city: "Hutsville"
        , state: "AL"
        , zip: "35805"
        }
    )

getDirections :: ∀ a. HasAddress a => a -> Directions
getDirections hasAddress =
  let
    (Address address) = getAddress hasAddress
  in
    address.street1 <> " " <> address.street2 <> " " <> address.city <> " " <> address.state <> " " <> address.zip

test :: Effect Unit
test = do
  log $ show $ getDirections person
  log $ show $ getDirections company
  log $ show $ getDirections home
  log $ show $ getDirections facility
