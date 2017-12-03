Fons van der Plas
s4576586
f.vanderplas@student.ru.nl

> {-# LANGUAGE UnicodeSyntax #-}
> module Format
> where
> import Prelude hiding (Word)
> import Unicode
> import WordList (Word, lorem)
> import Runs



 > format :: Int -> [Word] -> [[Word]]

> format2 lineWidth text = formatHelper lineWidth text 0
>   where
>     formatHelper l []     n = [[]]
>     formatHelper l (t:ts) n
>       | n + length t > l = [t] : formatHelper l ts 0
>       | otherwise        = (t : head next) : tail next
>         where
>           next = formatHelper l ts (1 + n + length t)

 > format lineWidth text =
 >   where
 >     wordTaker lineWidth []   lineRemainder = []
 >     wordTaker lineWidth text lineRemainder
 >       | lineRemainder > length $ head text
 >          = (head text : )

> format lineWidth text = applyGroupingToText (calcGrouping lineWidth text) text

> applyGroupingToText []     text = []
> applyGroupingToText (g:gs) (t:ts)
>   | g <= 1    = [t] : applyGroupingToText gs ts
>   | otherwise = (t : head (applyGroupingToText ((g-1):gs) ts)) : (tail (applyGroupingToText ((g-1):gs) ts))
>   where
>     next = applyGroupingToText ((g-1):gs) ts

Calculate the number of words per line. This is done by mapping each word in
'text' to its length + 1 (to account for the space). This list is accumulated
(which gives the list of offsets from the text start, for each word). This list
is taken modulo (lineWidth + 1), which gives the offset from line start for
each word. The function 'run' is now precisely the function needed to split
the text into lines.

> calcGrouping :: Int -> [Word] -> [Int]
> calcGrouping lineWidth text = map length $ runs $ map (\n -> n `mod` (lineWidth+1)) $ accumulate $ map (\s -> 1 + length s) text


A helper function that accumulates a list of integers, i.e.:

[1,7,7,15] -> [1,8,15,30]

The recursive step is: the first element is unchanged, the remaining
elements are incremented by x, and then this list om remainders is accumulated.

> accumulate :: [Int] -> [Int]
> accumulate [] = []
> accumulate [x] = [x]
> accumulate (x:xs) = x : (map (+x) (accumulate xs))
