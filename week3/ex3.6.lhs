Fons van der Plas
s4576586
f.vanderplas@student.ru.nl

> {-# LANGUAGE UnicodeSyntax #-}
> module Format
> where
> import Prelude hiding (Word)
> import Unicode
> import WordList (Word, lorem)

My implementation uses a helper function:

> format ∷ Int → [Word] → [[Word]]
> format lineWidth text = formatHelper lineWidth text 0
>   where

formatHelper keeps taking words from 'text', but has an accumulator 'n' that
counts the number of characters in the current line.
If 1 + n + 'the length of the next word' is greater than the line width, then
a new line must be started.
If not, t must be added to the first line of the recursive call.

>     formatHelper l []     n = [[]]
>     formatHelper l (t:ts) n
>       | n + length t > l = [t] : formatHelper l ts 0
>       | otherwise        = (t : head recurse) : tail recurse
>         where
>           recurse = formatHelper l ts (1 + n + length t)
