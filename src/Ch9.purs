module Ch9 where

import Prelude (Unit)

-- MAGMA
--
-- A Magma is a Set that is Closed under a Binary Operator (+, -, *, /, &&, ||)
-- Being "Closed" means that by applying a Binary operator to every combination
-- on a Set, the result will always be a value of that Set
--  E.g:
--    M = { true, false } and • = &&
--    The set "M" is the set of all "Booleans" and the Binary operator •
--    is the "Logical And" operation "&&"
--      true && true = true 
--      true && false = false 
--      false && true = false
--      false && false = false
--
--
-- Formal Definition of a Magma
-- ∀ a, b ∈ M ⇒ a • b ∈ M
--   ∀ a, b ∈ M   (For all a, and b that is an Element of M)
--   ⇒            (It follows that)
--   a • b ∈ M    (the result of a • b is an Element of M)
--
--
-- SEMIGROUP
--
-- A Semigroup is a Magma where the Binary Operator is Associative
-- A Binary operator • is "Associative" if the following is true
-- a • (b • c) == (a • b) • c
--
-- E.g:
--  S = { "a", "b", ..., "z", "aa", "ab", ...} and • = <>
--  S is the Set of NON-empty String
--  <> is the Concatenation (or Append) operator
--    ("ab" <> "cd") <> "ef" == "ab" <> ("cd" <> "ef")
--
-- Formal definition of a Semigroup
-- ∀ a, b, c ∈ S ⇒ a • (b • c) = (a • b) • c 
--   ∀ a, b, c ∈ S               (For all a, b and c that is an Element of S)
--   ⇒                           (It follows that)
--   a • (b • c) = (a • b) • c   (the result of applying a to
--                                the result of b • c is equal to
--                                the result of applying a • b to
--                                c)
class Semigroup a where
  append :: a -> a -> a

infixr 5 append as <>
