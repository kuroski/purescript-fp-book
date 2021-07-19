module Ch5 where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, otherwise, show, negate, max, (+), (-), (<), (<<<), (==))

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

-- | Get all but the first element of a list, or Nothing if the list is empty.
-- |
-- | ```purescript
-- | tail (1 : 2 : 3 : Nil) = (2 : 3 : Nil)
-- | ```
tail :: ∀ a. List a -> Maybe (List a)
tail Nil = Nothing

tail (_ : t) = Just t

-- | Get the last element in a list, or Nothing if the list is empty.
-- |
-- | ```purescript
-- | last (1 : 2 : 3 : Nil) = Just 3
-- | last (Nil) = Nothing
-- | ```
last :: List ~> Maybe
last Nil = Nothing

last (h : Nil) = Just h

last (_ : t) = last t

-- | Get all but the last element of a list, or Nothing if the list is empty.
-- |
-- | ```purescript
-- | init (1 : 2 : 3 : Nil) = Just (1 : 2 : Nil)
-- | init (Nil) = Nothing
-- | ```
init :: ∀ a. List a -> Maybe (List a)
init Nil = Nothing

init l = Just $ go l
  where
  go Nil = Nil

  go (_ : Nil) = Nil

  go (h : t) = h : go t

-- | Break a list into its first element, and the remaining elements, or Nothing if the list is empty.
-- |
-- | ```purescript
-- | uncons (1 : 2 : 3 : Nil) = Just { head: 1, tail: (2 : 3 : Nil)}
-- | ```
uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing

uncons (h : t) = Just { head: h, tail: t }

-- | Get the element at the specified index, or Nothing if the index is out-of-bounds.
-- |
-- | ```purescript
-- | index (1 : Nil) 4 = Nothing
-- | index (1 : 2 : 3 : Nil) 1 = Just 2
-- | index (Nil :: List Unit) 0 = Nothing
-- | ```
index :: ∀ a. List a -> Int -> Maybe a
index Nil _ = Nothing

index _ i
  | i < 0 = Nothing

index (h : _) 0 = Just h

index (_ : t) i = index t (i - 1)

-- | Operator alias for Data.List.index (left-associative / precedence 8)
-- |
-- | ```purescript
-- | (1 : Nil) !! 4 = Nothing
-- | (1 : 2 : 3 : Nil) !! 1 = Just 2
-- | (Nil :: List Unit) !! 0 = Nothing
-- | ```
infixl 8 index as !!

-- | Find the first index for which a predicate holds.
-- |
-- | ```purescript
-- | findIndex (_ >= 2) (1 : 2 : 3 : Nil) = Just 1
-- | findIndex (_ >= 99) (1 : 2 : 3 : Nil) = Nothing
-- | findIndex (10 /= _) (Nil :: List Int) = Nothing
-- | ```
findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex pred = go 0
  where
  go :: Int -> List a -> Maybe Int
  go _ Nil = Nothing

  go i (x : xs) = if pred x then Just i else go (i + 1) xs

-- | Find the last index for which a predicate holds.
-- |
-- | ```purescript
-- | findIndex (_ == 10) (Nil :: List Int) = Nothing
-- | findIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil) = Just 5
-- | findIndex (_ == 10) (11 : 12 : Nil) = Nothing
-- | ```
findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex pred = go Nothing 0
  where
  go :: Maybe Int -> Int -> List a -> Maybe Int
  go fi _ Nil = fi

  go fi i (x : xs) = go (if pred x then Just i else fi) (i + 1) xs

-- | Reverse a list.
-- |
-- | ```purescript
-- | reverse (1 : 2 : 3 : Nil) = (3 : 2 : 1 : Nil)
-- | ```
reverse :: List ~> List
reverse = go Nil
  where
  go rl Nil = rl

  go rl (x : xs) = go (x : rl) xs

-- | Flatten a list of lists.
-- |
-- | ```purescript
-- | concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil) = (1 : 2 : 3 : 4 : 5 : 6 : Nil)
-- | ```
concat :: ∀ a. List (List a) -> List a
concat Nil = Nil

concat (Nil : xss) = concat xss

concat ((x : xs) : xss) = x : concat (xs : xss)

-- | Filter a list, keeping the elements which satisfy a predicate function.
-- |
-- | ```purescript
-- | filter (4 > _) (1 : 2 : 3 : 4 : 5 : 6 : Nil) = (1 : 2 : 3 : Nil)
-- | ```
filter :: ∀ a. (a -> Boolean) -> List a -> List a
filter pred = reverse <<< go Nil
  where
  go nl Nil = nl

  go nl (x : xs)
    | pred x = go (x : nl) xs
    | otherwise = go nl xs

-- | Filter a list of optional values, keeping only the elements which contain a value.
-- |
-- | ```purescript
-- | catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 3 : Nil) = (1 : 2 : 3 : Nil)
-- | ```
catMaybes :: ∀ a. List (Maybe a) -> List a
catMaybes Nil = Nil

catMaybes (Nothing : xs) = catMaybes xs

catMaybes (Just x : xs) = x : catMaybes xs

-- | Create a list containing a range of integers, including both endpoints.
-- |
-- | ```purescript
-- | range 1 5 = (1 : 2 : 3 : 4 : 5 : Nil)
-- | range 3 (-3) = (3 : 2 : 1 : 0 : -1 : -2 : -3 : Nil)
-- | ```
range :: Int -> Int -> List Int
range start end = go Nil end start
  where
  go rl start' end'
    | start' == end' = start' : rl
    | otherwise = go (start' : rl) (start' + step) end'

  step = if start < end then (-1) else 1

-- | Take the specified number of elements from the front of a list.
-- |
-- | ```purescript
-- | take 5 (12 : 13 : 14 : Nil) = (12 : 13 : 14 : Nil)
-- | take 5 (1 : 2 : 3 : 4 : 5 : 6 : Nil) = (1 : 2 : 3 : 4 : 5 : Nil)
-- | ```
take :: ∀ a. Int -> List a -> List a
take n = reverse <<< go Nil (max 0 n)
  where
  go nl _ Nil = nl

  go nl 0 _ = nl

  go nl n' (x : xs) = go (x : nl) (n' - 1) xs

-- | Drop the specified number of elements from the front of a list.
-- |
-- | ```purescript
-- | drop 2 (1 : 2 : 3 : 4 : 5 : Nil) = (3 : 4 : 5 : Nil)
-- | ```
drop :: ∀ a. Int -> List a -> List a
drop _ Nil = Nil

drop 0 l = l

drop n (_ : xs) = drop (n - 1) xs

-- | Take those elements from the front of a list which match a predicate.
-- |
-- | ```purescript
-- | takeWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil) = (5 : 4 : Nil)
-- | takeWhile (_ == -17) (1 : 2 : 3 : Nil) = (Nil)
-- | ```
takeWhile :: ∀ a. (a -> Boolean) -> List a -> List a
takeWhile _ Nil = Nil

takeWhile pred (x : xs) = if pred x then x : takeWhile pred xs else Nil

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  flip const 1 2 # show # log
