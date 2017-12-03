Fons van der Plas
s4576586
f.vanderplas@student.ru.nl

> {-# LANGUAGE UnicodeSyntax #-}
> module TwoPointFour
> where
> import Unicode
> import Data.Char


Old type:

 > swap ∷ (Int, Int) → (Int, Int)

New type:

> swap ∷ (a, b) → (b, a)
> swap (x,y) = (y,x)

Two other functions with the same (original) type:

> complexConjugate ∷ (Int, Int) → (Int, Int)
> complexConjugate (x,y) = (x,-y)

> nextFibonacciNumbers ∷ (Int, Int) → (Int, Int)
> nextFibonacciNumbers (x,y) = (y,x+y)

Which can be used for:

> goldenRatio ∷ Double
> goldenRatio = ratio $ swap ((iterate nextFibonacciNumbers (1,1)) !! 50)
>   where
>       ratio (a,b) = fromIntegral a / fromIntegral b

which equals 1.61803⋯



Changing the type of the swap function to
    (a,b) → (b,a)
makes it more general. For example, the expression
    swap (True, "asfd")
now returns
    ("asdf", True)

Changing the type in the same manner for the next two functions results in an
error, since these functions use integer operators (- and +).

The difference between (Int, (Char, Bool)) and (Int, Char, Bool) seems obvious.

> convert12to3 ∷ (Int, (Char, Bool)) → (Int, Char, Bool)
> convert12to3 (n, (c, b)) = (n, c, b)

> convert3to12 ∷  (Int, Char, Bool) → (Int, (Char, Bool))
> convert3to12 (n, c, b) = (n, (c, b))
