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

> runs (x:y:xs)
>     | x > y     = [x] : runs (y:xs)
>     | otherwise = ((x): (head $ runs (y:xs))) : (tail $ runs (y:xs))


 > runs ∷ (Ord a) ⇒ [(b,a)] → [[b]]
 > runs [] = []
 > runs [(y,x)] = [[y]]

 > runs (x:y:xs)
 >     | snd x > snd y     = [fst x] : runs (y:xs)
 >     | otherwise = ((fst x): (head $ runs (y:xs))) : (tail $ runs (y:xs))
