module Test.Main where

import Ch5 (const, flip)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Prelude (Unit, discard, ($))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Ch5" do
    it "flip" do
      flip const 1 2 `shouldEqual` 2
    it "const" do
      const 1 "hello" `shouldEqual` 1