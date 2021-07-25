module Main where

import Prelude
import Ch5 as Ch5
import Ch6 as Ch6
import Effect (Effect)

main :: Effect Unit
main = do
  Ch5.test
  Ch6.test
