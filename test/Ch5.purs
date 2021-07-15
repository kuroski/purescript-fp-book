module Test.Main where

import Prelude

import Ch5 as Ch5
import Data.Array (length, tail, (..))
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Ch5" do
    it "flip" do
      Ch5.flip Ch5.const 1 2 `shouldEqual` 2
    it "const" do
      Ch5.const 1 "hello" `shouldEqual` 1
    it "apply" do
      Ch5.apply (\f -> f + 1) 2 `shouldEqual` 3
      (\f -> f + 1) `Ch5.apply` 4 `shouldEqual` 5
    it "$" do
      (length Ch5.$ fromMaybe [] Ch5.$ tail (1 .. 5)) `shouldEqual` 4