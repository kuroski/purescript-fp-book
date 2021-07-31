module Ch7a where

import Prelude (Unit, discard, show, (==), ($))
import Data.Eq (class Eq)
import Effect (Effect)
import Effect.Console (log)

data Maybe a
  = Nothing
  | Just a

instance eqMaybe :: Eq a => Eq (Maybe a) where
  eq Nothing Nothing = true
  eq (Just a1) (Just a2) = a1 == a2
  eq _ _ = false
