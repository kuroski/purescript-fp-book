module Ch5 where

import Data.List (List(..), (:))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show)

flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x

const :: ∀ a b. a -> b -> a
const x _ = x

apply :: ∀ a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped = flip apply

infixl 1 applyFlipped as #

singleton :: ∀ a. a -> List a
singleton x = x : Nil

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  flip const 1 2 # show # log