module Test.Main where

import Prelude
import Ch5 as Ch5
import Data.Array (length, tail, (..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
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
          it "applyFlipped" do
            Ch5.applyFlipped 2 (\f -> f + 1) `shouldEqual` 3
            4 `Ch5.applyFlipped` (\f -> f + 1) `shouldEqual` 5
          it "#" do
            (tail (1 .. 5) Ch5.# fromMaybe [] Ch5.# length) `shouldEqual` 4
          describe "List" do
            it "List.singleton" do
              Ch5.singleton "xyz" `shouldEqual` ("xyz" : Nil)
              Ch5.singleton 2 `shouldEqual` (2 : Nil)
            it "List.null" do
              Ch5.null Nil `shouldEqual` true
              Ch5.null ("xyz" : Nil) `shouldEqual` false
            it "List.snoc" do
              Ch5.snoc (1 : 2 : Nil) 3 `shouldEqual` (1 : 2 : 3 : Nil)
            it "List.length" do
              Ch5.length (1 : 2 : 3 : Nil) `shouldEqual` 3
            it "List.head" do
              Ch5.head ("abc" : "123" : Nil) `shouldEqual` Just "abc"
              Ch5.head (Nil :: List Unit) `shouldEqual` Nothing
            it "List.tail" do
              Ch5.tail ("abc" : "123" : "456" : Nil) `shouldEqual` Just ("123" : "456" : Nil)
              Ch5.tail (Nil :: List Unit) `shouldEqual` Nothing
