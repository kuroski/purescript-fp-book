module Test.Main where

import Ch5 (flip)
import Effect (Effect)
import Prelude (Unit, const, (==))
import Test.Assert (assert)

main :: Effect Unit
main = do
  assert (flip const 1 2 == 2)