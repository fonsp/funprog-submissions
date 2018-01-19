Fons van der Plas
Robin de Ruiter

> {-# LANGUAGE UnicodeSyntax #-}
> import Prelude hiding (Monoid)
> import Unicode
> import Data.List

> class Monoid a where
>   ε    ∷  a
>   (•)  ∷  a → a → a

> reduce  ∷  (Monoid m) ⇒ [m] → m
> reduce  =  foldr (•) ε

|| 7.6.2

> data Range = Range Integer Integer Integer Integer deriving (Show)

> getMaxSegmentSum :: Range -> Integer
> getMaxSegmentSum (Range a b c d) = a

We construct a monoid which stores the four properties given in the Hint.
In this definition we have:
(a) maximum segment sum;
(b) maximum preﬁx sum;
(c) maximum sufﬁx sum;
(d) overall sum.

The implementation is pretty straightforward.

> instance Monoid Range where
>   ε = Range 0 0 0 0
>   (Range xa xb xc xd) • (Range ya yb yc yd)
>     = Range (maximum [xa, ya, xc+yb]) (max xb (xd+yb)) (max yc (yd+xc)) (xd+yd)

> maximumSegmentSum :: [Integer] -> Integer
> maximumSegmentSum xs = getMaxSegmentSum $ reduce $ map (\i -> Range i i i i) xs

|| 7.6.1

We tried making the types used in 7.6.2 more general: replace Integer by (Monoid a)
and initialize the ε value of Range to Range ε ε ε ε (the ε value of Monoid a).
It did not work :'(

|| 7.6.3

A solution closer to the problem: take the maximal profit (i.e. sell value - buy value)
of any non-empty subsequence.

> maximumProfit :: [Integer] -> Integer
> maximumProfit stocks = maximum $ map (\l -> (last l) - (head l)) $ sequences stocks
>   where
>     sequences = filter (not ∘ null) ∘ concat ∘ (map tails) ∘ inits

|| 7.6.4

From exercise 3.3:

> runs :: (Ord a) => [a] -> [[a]]
> runs [] = []
> runs (x:xs) = derp [x] xs
>   where
>     derp xs []         = [xs]
>     derp (x:xs) (y:ys)
>       | x > y     = (reverse (x:xs)) : (runs (y:ys))
>       | otherwise = derp (y:(x:xs)) ys

If we allow multiple, non-overlapping transactions, the most lucrative strategy
is to possess stocks iff the value is rising. Thus:

> maximumProfitMultipleTransactions :: [Integer] -> Integer
> maximumProfitMultipleTransactions stocks = sum $ map (\l -> (last l) - (head l)) $ runs stocks

Another solution is to sum all postive differences in value.
