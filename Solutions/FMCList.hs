{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use elem" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use infix" #-}

module FMCList where

import Data.Char qualified as C
import Data.List qualified as L
import Data.Traversable (for)
import Prelude
  ( Bool (..),
    Char,
    Double,
    Enum (..),
    Eq (..),
    Float,
    Int,
    Integer,
    Integral (..),
    Num (..),
    Ord (..),
    String,
    curry,
    error,
    flip,
    not,
    otherwise,
    uncurry,
    undefined,
    ($),
    (&&),
    (.),
    (||),
  )
import Prelude qualified as P
import Data.Binary.Get (Decoder(Fail))

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}

{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head [] = undefined
head (x : _) = x

tail :: [a] -> [a]
tail [] = undefined
tail (x : xs) = xs

null :: [a] -> Bool
null [] = True
null _ = False

length :: (Integral i) => [a] -> i
length [] = 0
length (_ : xs) = length xs + 1

sum :: (Num a) => [a] -> a
sum [] = 0
sum (x : xs) = x + sum xs

product :: (Num a) => [a] -> a
product [] = 1
product (x : xs) = x * product xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

(++) :: [a] -> [a] -> [a]
[] ++ xs = xs
(x : xs) ++ ys = x : (xs ++ ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc y (x : xs) = x : snoc y xs

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ [] = xs
xs +++ [y] = xs <: y
xs +++ (y : ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

minimum :: (Ord a) => [a] -> a
minimum [] = undefined
minimum [x] = x
minimum (x : xs) = min x (minimum xs)

maximum :: (Ord a) => [a] -> a
maximum [] = undefined
maximum [x] = x
maximum (x : xs) = max x (maximum xs)

take :: (Integral i) => i -> [a] -> [a]
take i _ | i <= 0 = []
take _ [] = []
take i (x : xs) = x : take (i - 1) xs

drop :: (Integral i) => i -> [a] -> [a]
drop i xs | i <= 0 = xs
drop _ [] = []
drop i (_ : xs) = drop (i - 1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x : xs) =
  if p x
    then x : takeWhile p xs
    else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x : xs) =
  if p x
    then dropWhile p xs
    else xs

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x : xs) = (x : xs) : tails xs

init :: [a] -> [a]
init [] = undefined
init [x] = []
init (x : xs) = x : init xs

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x : xs) = [] : [x : y | y <- inits xs]

subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x : xs) =
  let subs = subsequences xs
   in subs ++ [x : s | s <- subs]

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x : xs) = p x || any p xs

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all p (x : xs) = p x && all p xs

and :: [Bool] -> Bool
and [] = True
and (x : xs) = x && and xs

or :: [Bool] -> Bool
or [] = False
or (x : xs) = x || or xs

concat :: [[a]] -> [a]
concat [] = []
concat (x : xs) = x ++ concat xs

-- elem using the funciton 'any' above

elem :: (Eq a) => a -> [a] -> Bool
elem x ys = any (== x) ys

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y : ys) = (x == y) || elem' x ys

(!!) :: (Integral i) => [a] -> i -> a
[] !! _ = undefined
(x : xs) !! 0 = x
(x : xs) !! i = xs !! (i - 1)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x : xs) =
  if p x
    then x : filter p xs
    else filter p xs

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

cycle :: [a] -> [a]
cycle [] = undefined
cycle xs = xs ++ cycle xs

repeat :: a -> [a]
repeat x = x : repeat x

replicate :: (Integral i) => i -> a -> [a]
replicate i _ | i <= 0 = []
replicate i x = x : replicate (i - 1) x 

isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ []  = False
isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

isInfixOf :: (Eq a) => [a] -> [a] -> Bool
isInfixOf [] _ = True
isInfixOf _ [] = False
isInfixOf xs (y : ys) = isPrefixOf xs ys || isInfixOf xs ys

isSuffixOf :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf xs ys = isPrefixOf (reverse xs) (reverse ys)

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}
