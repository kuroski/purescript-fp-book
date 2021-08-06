module Ch9 where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Prelude (class Show, class Eq)

-- | #### MAGMA
-- |
-- | A Magma is a Set that is Closed under a Binary Operator `(+, -, *, /, &&, ||)`
-- | 
-- | > Being "Closed" means that by applying a Binary operator to every combination on a Set, the result will always be a value of that Set
-- |
-- |  E.g:
-- |    `M = { true, false } and • = &&`
-- |    
-- |    The set "M" is the set of all "Booleans" and the Binary operator • is the "Logical And" operation "&&"
-- |    
-- |   ```  
-- |   true && true = true 
-- |   true && false = false 
-- |   false && true = false
-- |   false && false = false
-- |   ```
-- |
-- | Formal Definition of a Magma
-- | `∀ a, b ∈ M ⇒ a • b ∈ M`
-- |  ```
-- |   ∀ a, b ∈ M   (For all a, and b that is an Element of M)
-- |   ⇒            (It follows that)
-- |   a • b ∈ M    (the result of a • b is an Element of M)
-- |  ```
-- |
-- |
-- | #### SEMIGROUP
-- |
-- | A Semigroup is a Magma where the Binary Operator is Associative
-- |
-- | A Binary operator • is "Associative" if the following is true
-- | 
-- | `a • (b • c) == (a • b) • c`
-- |
-- | E.g:
-- |  `S = { "a", "b", ..., "z", "aa", "ab", ...} and • = <>`
-- |  
-- |  * S is the Set of NON-empty String
-- |  * <> is the Concatenation (or Append) operator
-- |  
-- |  `("ab" <> "cd") <> "ef" == "ab" <> ("cd" <> "ef")`
-- |
-- | Formal definition of a Semigroup
-- | `∀ a, b, c ∈ S ⇒ a • (b • c) = (a • b) • c`
-- |   ```
-- |   ∀ a, b, c ∈ S               (For all a, b and c that is an Element of S)
-- |   ⇒                           (It follows that)
-- |   a • (b • c) = (a • b) • c   (the result of applying a to
-- |                                the result of b • c is equal to
-- |                                the result of applying a • b to
-- |                                c)
-- |   ```
class Semigroup a where
  append :: a -> a -> a

infixr 5 append as <>

-- | #### MONOID
-- |
-- | A Monoid is a Semigroup where the Set has a Neutral Element, e.
-- | 
-- | A formal definition of a Monoid
-- | `e ∈ M, ∀ a ∈ M ⇒ a • e = e • a = a` 
-- | 
-- |  Where M is the Monoid Set
-- |   ```
-- |   e ∈ M                (e is an Element of M)
-- |   ∀ a ∈ M              (and for all a that is an Element of M)
-- |   ⇒                    (it follows that)
-- |   a • e = e • a = a    (applying a to e is equal to
-- |                         applying e to a which is equal to ai)
-- |   ```
-- |
-- | The Neutral Elemnet (Identity) can be applied to any Element of the Set
-- | Including itself, and have no impact on that Element.
-- | 
-- | E.g:
-- |  ```
-- |  M = { 0, 1, 2, 3, ...} and • = +, e = 0 
-- |  ```
-- |  - M is first a Magma: `1 + 3 = 4 --> (Closed)`
-- |  - And then a Semigroup: `1 + (2 + 3) = (1 + 2) + 3 --> (Associative)`
-- |  - And then a Monoid: `0 + 1 = 1 + 0 = 0 --> (Identity)`
-- | 
-- | E.g2:
-- |  ```
-- |  M = { true, false } and • = ||, e = false
-- |  ```
-- |  M is first a Magma then a Semigroup and then a Monoit.
-- |  ```
-- |  true || false = true                                 (Closed)
-- |  true || (true || false) = (true || true) || false    (Associative)
-- |  false || true = true || false = false                (Identity)
-- |  ```
class
  Semigroup a <= Monoid a where
  mempt :: a

-- Semigroup for AndBool

data AndBool = AFalse | ATrue
derive instance eqAndBool :: Eq AndBool
derive instance genericAndBool :: Generic AndBool _

instance showAndBool :: Show AndBool where
  show = genericShow

instance semigroupAndBool :: Semigroup AndBool where
  append ATrue ATrue = ATrue
  append _ _ = AFalse