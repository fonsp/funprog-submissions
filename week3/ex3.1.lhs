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

> type Word  =  String

> lorem ∷ String
> lorem
>   = "Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam \
>     \nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam \
>     \erat, sed diam voluptua. At vero eos et accusam et justo duo dolores \
>     \et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est \
>     \Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur \
>     \sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et \
>     \dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam \
>     \et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea \
>     \takimata sanctus est Lorem ipsum dolor sit amet."

> wordList ∷ String → [(Word, Int)]


Read the following description from the bottom up.
==================================================


Sort the pairs (word, count) by count:

> wordList text =    sortBy (\(x,nx) (y,ny) → compare nx ny) $

Map each list of identical words to ("the word", "number of occurances"):

>                    map (\x → (head x, length x)) $

Group identical words (which are next to each other in the list):

>                    group $

Split by whitespace and sort alphabetically:

>                    sort $
>                    words $

Remove punctuation and de-capitalize:

>                    map toLower $
>                    filter (\c → c /= '.') text


    ^
    |
    |
Start here.
===========
