Fons van der Plas
s4576586
f.vanderplas@student.ru.nl

> {-# LANGUAGE UnicodeSyntax #-}
> module WordList
> where
> import Prelude hiding (Word)
> import Unicode
> import Data.List
> import Data.Char

> allTrue ∷ [Bool] → Bool
> allTrue [] = True
> allTrue (x:xs) = x && allTrue xs

The only change is the addition of 'not' before 'x'

> allFalse ∷ [Bool] → Bool
> allFalse [] = True
> allFalse (x:xs) = not x && allTrue xs

> member ∷ (Eq a) => a → [a] → Bool
> member z [] = False
> member z (x:xs) = (z == x) || member z xs

This function is not defined for empty sets.
For non empty sets, the recursion is over the first two elements.
We keep 'removing' the biggest of the two, until a singleton remains.

> smallest ∷ [Int] → Int
> smallest [x] = x
> smallest (x:y:xs)
>         | x < y     = smallest (x:xs)
>         | otherwise = smallest (y:xs)

To mix things up: negate, take the smallest value, negate

> largest ∷ [Int] → Int
> largest xs = 0-(smallest $ map (0-) xs)
