module Ch7a where

import Data.Eq (class Eq)
import Prelude (class Ord, class Show, Ordering(..), compare, show, (<>), (==), (||))

greaterThanOrEq :: ∀ a. Ord a => a -> a -> Boolean
greaterThanOrEq a1 a2 = cmp == GT || cmp == EQ
  where
  cmp = compare a1 a2

infixl 4 greaterThanOrEq as >=

data Maybe a
  = Nothing
  | Just a

instance eqMaybe :: Eq a => Eq (Maybe a) where
  eq Nothing Nothing = true
  eq (Just a1) (Just a2) = a1 == a2
  eq _ _ = false

instance ordMaybe :: Ord a => Ord (Maybe a) where
  compare Nothing Nothing = EQ
  compare Nothing _ = LT
  compare _ Nothing = GT
  compare (Just a1) (Just a2) = compare a1 a2

instance showMaybe :: Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just a) = "(Just " <> show a <> ")"
