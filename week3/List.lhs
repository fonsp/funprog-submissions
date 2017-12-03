> {-# LANGUAGE UnicodeSyntax #-}
> module List (filter)
> where
> import Prelude hiding (filter)
> import Unicode

> filter ∷ (a → Maybe b) → ([a] → [b])
> filter _f []  =  []
> filter f (a : as)  =  case f a of
>   Nothing  →      filter f as
>   Just b   →  b : filter f as
