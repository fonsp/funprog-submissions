> {-# LANGUAGE UnicodeSyntax #-}
>
> module Unicode
> where

> infixr 2 ∨
> (∨)  ∷  Bool → Bool → Bool
> a ∨ b  =  a || b

> infixr 3 ∧
> (∧)  ∷  Bool → Bool → Bool
> a ∧ b  =  a && b
               
> infix 4 ≤, ≥
> (≤), (≥)  ∷  (Ord a) ⇒ a → a → Bool
> a ≤ b  =  a <= b
> a ≥ b  =  a >= b

> infixr 5 ↓, ↑
> (↓), (↑)  ∷  (Ord a) ⇒ a → a → a
> a ↓ b  =  a `min` b
> a ↑ b  =  a `max` b

> infixr 9 ∘
> (∘) ∷ (b → c) → (a → b) → (a → c)
> (∘)  =  (.)

∷ ⇒ ∀ → ← ⋯ ∨ ∧ ≤ ≥ ↓ ↑ ∘
