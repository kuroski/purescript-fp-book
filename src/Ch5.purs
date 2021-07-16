module Ch5 where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, show, (+))

-- | Flips the order of the arguments to a function of two arguments.
-- |
-- | ```purescript
-- | flip const 1 2 = const 2 1 = 2
-- | ```
flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x

-- | Returns its first argument and ignores its second.
-- |
-- | ```purescript
-- | const 1 "hello" = 1
-- | ```
const :: ∀ a b. a -> b -> a
const x _ = x

-- | Applies a function to an argument.
-- |
-- | ```purescript
-- | apply (\f -> f + 1) 2 = 3
-- | ```
apply :: ∀ a b. (a -> b) -> a -> b
apply f x = f x

-- | Applies a function to an argument: the reverse of `(#)`.
-- |
-- | ```purescript
-- | length $ fromMaybe [] $ tail (1 .. 5) = 4
-- | ```
infixr 0 apply as $

-- | Applies an argument to a function.
-- |
-- | ```purescript
-- | applyFlipped 2 (\f -> f + 1) = 3
-- | ```
applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped = flip apply

-- | Applies an argument to a function: the reverse of `($)`.
-- |
-- | ```purescript
-- | tail (1 .. 5) # fromMaybe [] # length = 4
-- | ```
infixl 1 applyFlipped as #

-- List
-- | Create a list with a single element.
-- |
-- | ```purescript
-- | singleton "xyz" = ("xyz" : Nil)
-- | singleton 2 = (2 : Nil)
-- | ```
singleton :: ∀ a. a -> List a
singleton x = x : Nil

-- | Test whether a list is empty.
-- |
-- | ```purescript
-- | null Nil = true
-- | null ("xyz" : Nil) = false
-- | ```
null :: ∀ a. List a -> Boolean
null Nil = true

null _ = false

-- | Append an element to the end of a list, creating a new list.
-- |
-- | ```purescript
-- | snoc (1 : 2 : Nil) 3 = (1 : 2 : 3 : Nil)
-- | ```
snoc :: ∀ a. List a -> a -> List a
snoc Nil x = singleton x

snoc (h : t) x = h : snoc t x

-- | Get the length of a list
-- |
-- | ```purescript
-- | length (1 : 2 : 3 : Nil) = 3
-- | ```
length :: ∀ a. List a -> Int
length l = go 0 l
  where
  go :: Int -> List a -> Int
  go acc Nil = acc

  go acc (_ : t) = go (acc + 1) t

-- | Get the first element in a list, or Nothing if the list is empty.
-- |
-- | ```purescript
-- | head (1 : 2 : 3 : Nil) = 1
-- | ```
head :: List ~> Maybe
head Nil = Nothing

head (h : _) = Just h

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  flip const 1 2 # show # log
