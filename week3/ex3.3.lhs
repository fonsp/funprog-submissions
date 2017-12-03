Fons van der Plas
s4576586
f.vanderplas@student.ru.nl

> {-# LANGUAGE UnicodeSyntax #-}
> module Runs
> where
> import Prelude hiding (Word)
> import Unicode
> import Data.List
> import Data.Char

> runs ∷ (Ord a) ⇒ [a] → [[a]]
> runs [] = []
> runs [x] = [[x]]

If x > y, then the sequence [x,y] is decreasing, which is not allowed.
Therefore, the sequence must end at x.

> runs (x:y:xs)
>     | x > y     = [x] : runs (y:xs)

If x <= y, then x and y are in the same sequence (which might continue further).
The result should be:
[x,y,...], seq2, ...
   seq1  , seq2, ...

seq1 is the first sequence in (y:xs), with x added to the start
seq2, ... is 'runs (y:xs)', but without the first sequence.

[y,seq1] is the first sequence in y:xs, without

>     | otherwise = (x: (head $ runs (y:xs))) : (tail $ runs (y:xs))

This might be useful for a sorting method that zips these runs together, while
mainting order within the zipees.
