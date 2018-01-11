> {-# LANGUAGE UnicodeSyntax #-}
> module MapReduce
> where
> import Prelude hiding (Monoid)
> import Unicode
> import Hardware

> class Monoid a where
>   ε    ∷  a
>   (•)  ∷  a → a → a

> reduce  ∷  (Monoid m) ⇒ [m] → m
> reduce  =  foldr (•) ε

instance Monoid Bool where

> newtype OrdList elem = Ord [elem]

instance (Ord elem) ⇒ Monoid (OrdList elem) where

foldm ∷ (a → a → a) → a → ([a] → a)

> kpg ∷ (Bit, Bit) → (Carry → Carry)
> kpg (O,  O  )  =  \ _c  → O  -- kill
> kpg (O,  I  )  =  \ c   → c  -- propagate
> kpg (I,  O  )  =  \ c   → c  -- propagate
> kpg (I,  I  )  =  \ _c  → I  -- generate

> data KPG  =  K | P | G deriving (Show)
> instance Monoid KPG where
>   ε   = P
>   K • _ = K
>   G • _ = G
>   P • x = x

> apply :: KPG -> (Carry -> Carry)
> apply K = \c -> O
> apply P = \c -> c
> apply G = \c -> I

> newtype KPGbinary = KPGbinary (Bit,Bit) deriving (Show)

A trivial definition is as follows:
The first bit I represents propagation
The first bit O represents ignoring the carry and returning the second bit.

> instance Monoid KPGbinary where
>   ε = KPGbinary (I,O)
>   (KPGbinary (O,b)) • _ = KPGbinary (O,b)
>   (KPGbinary (I,b)) • x = x
