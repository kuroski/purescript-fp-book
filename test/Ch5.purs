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
            it "List.last" do
              Ch5.last (Nil :: List Unit) `shouldEqual` Nothing
              Ch5.last (1 : Nil) `shouldEqual` Just 1
              Ch5.last (1 : 2 : 3 : Nil) `shouldEqual` Just 3
            it "List.init" do
              Ch5.init (1 : 2 : 3 : Nil) `shouldEqual` Just (1 : 2 : Nil)
              Ch5.init (1 : Nil) `shouldEqual` Just Nil
              Ch5.init (Nil :: List Unit) `shouldEqual` Nothing
            it "List.uncons" do
              Ch5.uncons (1 : 2 : 3 : Nil) `shouldEqual` Just { head: 1, tail: (2 : 3 : Nil) }
            it "List.index" do
              Ch5.index (1 : Nil) 4 `shouldEqual` Nothing
              Ch5.index (1 : 2 : 3 : Nil) 1 `shouldEqual` Just 2
              Ch5.index (Nil :: List Unit) 0 `shouldEqual` Nothing
            it "!!" do
              ((1 : Nil) Ch5.!! 4) `shouldEqual` Nothing
              ((1 : 2 : 3 : Nil) Ch5.!! 1) `shouldEqual` Just 2
              ((Nil :: List Unit) Ch5.!! 0) `shouldEqual` Nothing
            it "List.findIndex" do
              Ch5.findIndex (_ >= 2) (1 : 2 : 3 : Nil) `shouldEqual` Just 1
              Ch5.findIndex (_ >= 99) (1 : 2 : 3 : Nil) `shouldEqual` Nothing
              Ch5.findIndex (10 /= _) (Nil :: List Int) `shouldEqual` Nothing
            it "List.findLastIndex" do
              Ch5.findLastIndex (_ == 10) (Nil :: List Int) `shouldEqual` Nothing
              Ch5.findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil) `shouldEqual` Just 5
              Ch5.findLastIndex (_ == 10) (11 : 12 : Nil) `shouldEqual` Nothing
            it "List.reverse" do
              Ch5.reverse (1 : 2 : 3 : Nil) `shouldEqual` (3 : 2 : 1 : Nil)
            it "List.concat" do
              Ch5.concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil) `shouldEqual` (1 : 2 : 3 : 4 : 5 : 6 : Nil)
            it "List.filter" do
              Ch5.filter (4 > _) (1 : 2 : 3 : 4 : 5 : 6 : Nil) `shouldEqual` (1 : 2 : 3 : Nil)
            it "List.catMaybes" do
              Ch5.catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 3 : Nil) `shouldEqual` (1 : 2 : 3 : Nil)
            it "List.range" do
              Ch5.range 1 5 `shouldEqual` (1 : 2 : 3 : 4 : 5 : Nil)
              Ch5.range 3 (-3) `shouldEqual` (3 : 2 : 1 : 0 : -1 : -2 : -3 : Nil)
            it "List.take" do
              Ch5.take (-1) (12 : 13 : 14 : Nil) `shouldEqual` (Nil)
              Ch5.take 5 (12 : 13 : 14 : Nil) `shouldEqual` (12 : 13 : 14 : Nil)
              Ch5.take 5 (1 : 2 : 3 : 4 : 5 : 6 : Nil) `shouldEqual` (1 : 2 : 3 : 4 : 5 : Nil)
            it "List.drop" do
              Ch5.drop (-1) (12 : 13 : 14 : Nil) `shouldEqual` (Nil)
              Ch5.drop 2 (1 : 2 : 3 : 4 : 5 : Nil) `shouldEqual` (3 : 4 : 5 : Nil)
              Ch5.drop 5 (1 : 2 : Nil) `shouldEqual` (Nil)
            it "List.takeWhile" do
              Ch5.takeWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil) `shouldEqual` (5 : 4 : Nil)
              Ch5.takeWhile (_ == -17) (1 : 2 : 3 : Nil) `shouldEqual` (Nil)
            it "List.dropWhile" do
              Ch5.dropWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil) `shouldEqual` (3 : 99 : 101 : Nil)
              Ch5.dropWhile (_ == -17) (1 : 2 : 3 : Nil) `shouldEqual` (1 : 2 : 3 : Nil)
            it "List.takeEnd" do
              Ch5.takeEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil) `shouldEqual` (4 : 5 : 6 : Nil)
              Ch5.takeEnd 10 (1 : Nil) `shouldEqual` (1 : Nil)
            it "List.dropEnd" do
              Ch5.dropEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil) `shouldEqual` (1 : 2 : 3 : Nil)
              Ch5.dropEnd 10 (1 : Nil) `shouldEqual` (Nil)
