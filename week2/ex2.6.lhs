Fons van der Plas
s4576586
f.vanderplas@student.ru.nl

> {-# LANGUAGE UnicodeSyntax #-}
> module TwoPointFour
> where
> import Unicode
> import Data.Char

1.

> lastDigit ∷ Int → Int
> lastDigit n = n `mod` 10

> identity ∷ a → a
> identity x = x

> mygcd ∷ (Int, Int) → Int
> mygcd (a,0) = a
> mygcd (a,b) = mygcd (b, a `mod` b)

> second ∷ (a,a) → a
> second (_,x) = x

> first ∷ (a,b) → a
> first (x,_) = x

The are (2^64)^(2^64) functions of the type Int → Int
There is only one function of the type a → a

2.

> doubleFirst ∷ (a,a) → (a,a)
> doubleFirst (x,y) = (x,x)

> swap ∷ (a,b) → (b,a)
> swap (x,y) = (y,x)

Since (a → b) is a function from a to b:

> apply ∷ (a → b) → a → b
> apply f x = f(x)

> anotherFirst ∷ (a,x) → a
> anotherFirst (n,m) = n

> applyChainedFun ∷ (x → a → b, a, x) → b
> applyChainedFun (f, n, m) = f m n

> applyBoth ∷ (a → b, x → a, x) → b
> applyBoth (f, g, n) = (f ∘ g) n

> combineFun ∷ (x → a → b, x → a, x) → b
> combineFun (f,g,n) = (f n) (g n)

3.

> createAdder ∷ Int → (Int → Int)
> createAdder n = (+n)

> valueAtZero ∷ (Int → Int) → Int
> valueAtZero f = f 0

Outputs a function that maps every input to x

> createTrivialMapper ∷ a → (a → a)
> createTrivialMapper x = \y → x

There are #input ^ #outputs = ((2^64)^(2^64))^(2^64) functions of the type
(Int → Int) → Int.
There are no functions of the type (a → a) → a.
