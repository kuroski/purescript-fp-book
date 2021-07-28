module Ch6 where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as String
import Effect (Effect)
import Effect.Class.Console (log)

-- Custom Typeclasses
class HasAddress a where
  getAddress :: a -> Address

-- Application
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

newtype Person
  = Person
  { name :: String
  , age :: Int
  , address :: Address
  }

instance hasAddressPerson :: HasAddress Person where
  getAddress (Person p) = p.address

instance eqPerson :: Eq Person where
  eq (Person p1) (Person p2) = p1.name == p2.name && p1.age == p2.age && p1.address == p2.address

newtype Ceo
  = Ceo Person

derive instance newtypeCeo :: Newtype Ceo _

derive newtype instance hasAddressCeo :: HasAddress Ceo

newtype Janitor
  = Janitor Person

derive instance newtypeJanitor :: Newtype Janitor _

derive newtype instance hasAddressJanitor :: HasAddress Janitor

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

-- Direved Instances
-- manual work
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

-- derive work
data SomeType
  = This
  | That
  | TheOther
  | AndYetAnother

-- Here is all the boiler plate the that the code abstract
-- instance eqSomeType :: Eq SomeType where
--   eq This This = true
--   eq That That = true
--   eq TheOther TheOther = true
--   eq AndYetAnother AndYetAnother = true
--   eq _ _ = false
-- instance ordSomeType :: Ord SomeType where
--   compare This This = EQ
--   compare This _ = LT
--   compare That TheOther = LT
--   compare That AndYetAnother = LT
--   compare That That = EQ
--   compare That This = GT
--   compare TheOther AndYetAnother = LT
--   compare TheOther TheOther = EQ
--   compare TheOther This = GT
--   compare TheOther That = GT
--   compare AndYetAnother AndYetAnother = EQ
--   compare AndYetAnother _ = GT
-- instance showSomeType :: Show SomeType where
--   show This = "This"
--   show That = "That"
--   show TheOther = "TheOther"
--   show AndYetAnother = "AndYetAnother"
derive instance eqSomeType :: Eq SomeType

derive instance ordSomeType :: Ord SomeType

derive instance genericSomeType :: Generic SomeType _

instance showSomeType :: Show SomeType where
  show = genericShow

------------
-- New type
------------
newtype FirstName
  = FirstName String

derive instance newTypeFirstName :: Newtype FirstName _

derive instance eqFirstName :: Eq FirstName

newtype LastName
  = LastName String

derive instance newTypeLastName :: Newtype LastName _

fullName :: FirstName -> LastName -> String
fullName (FirstName first) (LastName last) = first <> " " <> last

fullName' :: FirstName -> LastName -> String
fullName' first last = unwrap first <> " " <> unwrap last

-- generic magic 🦄
glueNames ::
  ∀ a b.
  Newtype a String =>
  Newtype b String =>
  String ->
  a ->
  b ->
  String
glueNames between n1 n2 = unwrap n1 <> between <> unwrap n2

lastNameFirst :: LastName -> FirstName -> String
lastNameFirst = glueNames ", "

fullName'' :: FirstName -> LastName -> String
fullName'' = glueNames " "

--------
-- Overlapping instances
--------
class Combine a where
  combine :: a -> a -> a

newtype AddInt
  = AddInt Int

newtype MultInt
  = MultInt Int

instance combineAddInt :: Combine AddInt where
  combine (AddInt x) (AddInt y) = AddInt (x + y)

instance combineMultInt :: Combine MultInt where
  combine (MultInt x) (MultInt y) = MultInt (x * y)

--
class IsRecord a where
  isRecord :: a -> Boolean

instance isRecordRecord :: IsRecord (Record a) where
  isRecord _ = true
else instance isRecordOther :: IsRecord a where
  isRecord _ = false

---------
-- Multi parametric typeclasses
---------
class Decapitate collection element where
  decapitate :: collection -> Maybe { head :: element, tail :: collection }

instance decapitateList :: Decapitate (List a) a where
  decapitate = List.uncons

instance decapitateString :: Decapitate String Char where
  decapitate = String.uncons

test :: Effect Unit
test = do
  log $ show $ getDirections person
  log $ show $ getDirections company
  log $ show $ getDirections home
  log $ show $ getDirections facility
  log $ show $ fullName (FirstName "Daniel") (LastName "Kuroski")
  log $ show $ fullName' (FirstName "Daniel") (LastName "Kuroski")
  log $ show $ lastNameFirst (LastName "Kuroski") (FirstName "Daniel")
  log $ show $ fullName'' (FirstName "Daniel") (LastName "Kuroski")
