module Ch6 where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)

type Address
  = { street1 :: String
    , street2 :: String
    , city :: String
    , state :: String
    , zip :: String
    }

type Directions
  = String

class HasAddress a where
  getAddress :: a -> Address

data Person
  = Person
    { name :: String
    , age :: Int
    , address :: Address
    }

instance hasAddressPerson :: HasAddress Person where
  getAddress (Person p) = p.address

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
    { street1: "1 1st Street"
    , street2: "Apt 1"
    , city: "Buford"
    , state: "WY"
    , zip: "82052"
    }

facility :: Residence
facility =
  Facility
    { street1: "54321 Contdown Ave"
    , street2: ""
    , city: "Hutsville"
    , state: "AL"
    , zip: "35805"
    }

getDirections :: ∀ a. HasAddress a => a -> Directions
getDirections hasAddress =
  let
    address = getAddress hasAddress
  in
    address.street1 <> " " <> address.street2 <> " " <> address.city <> " " <> address.state <> " " <> address.zip

-- newtype Miles
--   = Miles Int
-- getMiles :: Address -> Miles
-- getMiles address = Miles (String.length address.street1)
-- getMilesTo :: HasAddress -> Miles
-- getMilesTo =
--   getMiles
--     <<< case _ of
--         PersonAddress (Person { address }) -> address
--         CompanyAddress (Company { address }) -> address
--         ResidenceAddress (Home address) -> address
--         ResidenceAddress (Facility address) -> address
--         EmptyLotAddress (EmptyLot { address }) -> address
test :: Effect Unit
test = do
  log $ show $ getDirections person
  log $ show $ getDirections company
  log $ show $ getDirections home
  log $ show $ getDirections facility
