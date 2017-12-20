> {-# LANGUAGE UnicodeSyntax #-}
> module Numeral
> where
> import Unicode

> type Base   =  Integer
> type Digit  =  Integer

> msdf, lsdf :: Base -> [Digit] -> Integer
> msdf b l = foldl (\x y -> b*x + y) 0 l
> lsdf b l = foldr (\x y -> b*y + x) 0 l


The only difference between 'mdsf base' and 'lsdf base' is the reading order of
the digits. This means that:

lsdf base = msdf base ∘ reverse
  and
msdf base = lsdf base ∘ reverse


In the case of foldl and folr, notice that not only the order of application is
reversed, but also the inputs of the fold function.

If we define:

> swapInputs :: (a -> b -> c) -> (b -> a -> c)
> swapInputs f = \y x -> (f x y)

The we have the relation:

foldl f e = foldr (swapInputs f) e ∘ reverse
  and
foldr f e = foldl (swapInputs f) e ∘ reverse
