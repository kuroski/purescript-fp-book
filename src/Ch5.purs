module Ch5 where

import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, show)

flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x

const :: ∀ a b. a -> b -> a
const x _ = x

apply :: ∀ a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

test :: Effect Unit
test = do
  log $ show $ flip const 1 2