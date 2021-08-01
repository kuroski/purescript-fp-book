module Ch7a where

import Data.Eq (class Eq)
import Prelude (class Ord, compare, Ordering(..), (==))

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
