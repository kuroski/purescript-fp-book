module Ch6 where

import Prelude
import Data.String as String
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

data Person
  = Person
    { name :: String
    , age :: Int
    , address :: Address
    }

data Company
  = Company
    { name :: String
    , address :: Address
    }

data Residence
  = Home Address
  | Facility Address

data EmptyLot
  = EmptyLot
    { daysEmpty :: Int
    , price :: Int
    , address :: Address
    }

data HasAddress
  = PersonAddress Person
  | CompanyAddress Company
  | ResidenceAddress Residence
  | EmptyLotAddress EmptyLot

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

getDirections :: Address -> Directions
getDirections address = address.street1 <> " " <> address.street2 <> " " <> address.city <> " " <> address.state <> " " <> address.zip

getPersonDirections :: Person -> Directions
getPersonDirections (Person p) = getDirections p.address

getCompanyDirections :: Company -> Directions
getCompanyDirections (Company c) = getDirections c.address

getResidenceDirections :: Residence -> Directions
getResidenceDirections =
  getDirections
    <<< case _ of
        Home address -> address
        Facility address -> address

getEmptyLotDirections :: EmptyLot -> Directions
getEmptyLotDirections (EmptyLot l) = getDirections l.address

getDirectionsTo :: HasAddress -> Directions
getDirectionsTo = case _ of
  PersonAddress p -> getPersonDirections p
  CompanyAddress c -> getCompanyDirections c
  ResidenceAddress residence -> getResidenceDirections residence
  EmptyLotAddress l -> getEmptyLotDirections l

-- 
newtype Miles
  = Miles Int

getMiles :: Address -> Miles
getMiles address = Miles (String.length address.street1)

getMilesTo :: HasAddress -> Miles
getMilesTo =
  getMiles
    <<< case _ of
        PersonAddress (Person { address }) -> address
        CompanyAddress (Company { address }) -> address
        ResidenceAddress (Home address) -> address
        ResidenceAddress (Facility address) -> address
        EmptyLotAddress (EmptyLot { address }) -> address

--
test :: Effect Unit
test = do
  log $ show $ getDirectionsTo (PersonAddress person)
  log $ show $ getDirectionsTo (CompanyAddress company)
  log $ show $ getDirectionsTo (ResidenceAddress home)
  log $ show $ getDirectionsTo (ResidenceAddress facility)
