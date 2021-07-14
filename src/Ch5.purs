module Ch5 where

import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, const, show)

flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x

test :: Effect Unit
test = do
  log (show (flip const 1 2))