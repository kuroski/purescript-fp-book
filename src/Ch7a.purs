module Ch7a where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Prelude (class Ord, class Show, Ordering(..), compare, (==), (||))

greaterThanOrEq :: ∀ a. Ord a => a -> a -> Boolean
greaterThanOrEq a1 a2 = cmp == GT || cmp == EQ
  where
  cmp = compare a1 a2

infixl 4 greaterThanOrEq as >=

data Maybe a
  = Nothing
  | Just a

-- instance eqMaybe :: Eq a => Eq (Maybe a) where
--   eq Nothing Nothing = true
--   eq (Just a1) (Just a2) = a1 == a2
--   eq _ _ = false
derive instance eqMaybe :: Eq a => Eq (Maybe a)

-- instance ordMaybe :: Ord a => Ord (Maybe a) where
--   compare Nothing Nothing = EQ
--   compare Nothing _ = LT
--   compare _ Nothing = GT
--   compare (Just a1) (Just a2) = compare a1 a2
derive instance ordMaybe :: Ord a => Ord (Maybe a)

-- instance showMaybe :: Show a => Show (Maybe a) where
--   show Nothing = "Nothing"
--   show (Just a) = "(Just " <> show a <> ")"
derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow
