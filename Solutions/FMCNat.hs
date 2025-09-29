{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!

import Distribution.Simple.Utils (xargs)
import Prelude
  ( Bool (..),
    Eq (..),
    Integral (..),
    Num (..),
    Ord (..),
    Show (..),
    fst,
    snd,
    error,
    not,
    otherwise,
    undefined,
    ($),
    (&&),
    (++),
    (.),
    (||),
  )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where
  -- zero  should be shown as O
  -- three should be shown as SSSO
  show O = "O"
  show (S n) = "S" ++ show n -- S (S (S O)) = "S" ++ S (S O) = "SS" ++ S O = "SSS" ++ "O" = "SSSO"

instance Eq Nat where
  O == O = True
  (S _) == O = False
  O == (S _) = False
  (S n) == (S m) = n == m

instance Ord Nat where
  O <= _ = True
  S _ <= O = False
  S n <= S m = n <= m

  -- Ord does not REQUIRE defining min and max.
  -- Howevener, you should define them WITHOUT using (<=).
  -- Both are binary functions: max m n = ..., etc.

  min O _ = O
  min _ O = O
  min (S n) (S m) = S (min n m)

  max O n = n
  max n O = n
  max (S n) (S m) = S (max n m) -- max one two = max SO SSO = S(max O SO)

----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

zero, one, two, three, four, five, six, seven, eight :: Nat
zero = O
one = S zero
two = S one
three = S two
four = S three
five = S four
six = S five
seven = S six
eight = S seven

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero _ = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

even :: Nat -> Bool
even O = True
even (S n) = odd n

odd :: Nat -> Bool
odd O = False
odd (S n) = even n

----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> O = n
n <+> S m = S (n + m)

infixl 6 <+>

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus = (<->)

(<->) :: Nat -> Nat -> Nat
O <-> n = O
n <-> O = n
S n <-> S m = n <-> m

infixl 6 <->

-- multiplication
times :: Nat -> Nat -> Nat
times = (<*>)

(<*>) :: Nat -> Nat -> Nat
_ <*> O = O
n <*> S m = n <*> m + n

infixl 7 <*>

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow = (<^>)

exp :: Nat -> Nat -> Nat
exp = (<^>)

(<^>) :: Nat -> Nat -> Nat
_ <^> O = S O
n <^> (S m) = n <^> m <*> n

infixl 8 <^>

-- quotient

(</>) :: Nat -> Nat -> Nat
n </> m = fst (eucdiv (n, m))

infixl 7 </>

-- remainder
(<%>) :: Nat -> Nat -> Nat
n <%> m = snd (eucdiv (n, m))

infixl 7 <%>

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv (_, O) = undefined
eucdiv (n, m) =
  if n < m
    then (O, n)
    else
      let (q, r) = eucdiv (n <-> m, m)
       in (S q, r)

-- divides
(<|>) :: Nat -> Nat -> Bool
_ <|> O = True
O <|> (S _) = False
n <|> m =
  isZero (m <%> n)

divides = (<|>)

-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist = (|-|)

(|-|) :: Nat -> Nat -> Nat
n |-| m = (n <-> m) + (m <-> n)

factorial :: Nat -> Nat
factorial O = S O
factorial (S n) = factorial n * S n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg = undefined

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O _ = undefined
lo _ O = undefined
lo (S O) _ = undefined
lo n m =
  case m </> n of
    O -> O
    _ -> S (lo n (m </> n))

----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: (Integral a) => a -> Nat
toNat x
  | x < 0 = undefined
  | x == 0 = O
  | otherwise = S (toNat (x - 1))

fromNat :: (Integral a) => Nat -> a
fromNat O = 0
fromNat (S n) = 1 + fromNat n

-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where
  (+) = (<+>)
  (*) = (<*>)
  (-) = (<->)
  abs n = n
  signum = sg
  fromInteger x
    | x < 0 = undefined
    | x == 0 = O
    | otherwise = S (fromInteger (x - 1))
