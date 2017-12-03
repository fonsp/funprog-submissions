Fons van der Plas
s4576586
f.vanderplas@student.ru.nl

> {-# LANGUAGE UnicodeSyntax #-}
> module Char
> where
> import Unicode
> import Data.Char

> equal      ∷ String → String → Bool
> equal [] [] = True
> equal [] (x:xs) = False
> equal (x:xs) [] = False
> equal (a:as) (b:bs) = (toLower a == toLower b) && equal as bs

Two nested functions: the inner map function maps each character in the input
string to `True` or `False`. The outer and function returns True iff every bool
is `True`.

> isNumeral  ∷ String → Bool
> isNumeral text = and (map isDigit text)

> isBlank    ∷ String → Bool
> isBlank text = and (map isSpace text)

These functions are built-in:
(https://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Char.html)

 > fromDigit  ∷ Char → Int
 > fromDigit = digitToInt

 > toDigit    ∷ Int → Char
 > toDigit = intToDigit

But that would be boring...
The following function map characters to and from the ASCII encoding, where
they all follow eachother in the order 0,1,2,...,9.

> fromDigit  ∷ Char → Int
> fromDigit c = ord c - ord '0'

> toDigit    ∷ Int → Char
> toDigit n = chr (ord '0' + n)

We will simply shift within the ASCII index, where A through Z are in
alphabetical order. We need to substract, and add, the ASCII index of
'A' to make set the index of 'A' at 0.
The seperate definition for ' ' is added for future usage.

> shift      ∷ Int → Char → Char
> shift n ' ' = ' '
> shift n c = chr (ord 'A' + (ord c - ord 'A' + n) `mod` 26)

> msg  ∷  String
> msg  =  "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ \
>         \JHLJBZ KPJABT HYJUBT LZA ULBAYVU"

> decypher ∷ Int → String → String
> decypher key message = map (shift key) message

> allPossibilities ∷ [(Int, String)]
> allPossibilities = map (\n → (n, decypher n msg)) [1..26]

From this sequence, it can be deduced (by reading) that 19 is the correct cypher.

> decypheredMessage ∷ String
> decypheredMessage = decypher 19 msg
