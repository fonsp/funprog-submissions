> {-# LANGUAGE UnicodeSyntax #-}
> module SKI
> where
> import Unicode
> import Lambda
> import Parser

> data SKI var
>   =  Free var                 -- free/unbound variable
>   |  S
>   |  K
>   |  I'
>   |  App (SKI var) (SKI var)  -- application

> i ∷ env → env
> i arg = arg
> k ∷ a → (env → a)
> k x _arg = x
> s ∷ (env → a → b) → (env → a) → (env → b)
> s x y arg = (x arg) (y arg)

abstr   ∷ (Eq var) ⇒ var → SKI var → SKI var
compile ∷ (Eq var) ⇒ Lambda var → SKI var
reduce  ∷ SKI var → [SKI var] → SKI var

twice = parse expr "λf.λx.f(fx)"
twice
compile twice
reduce it [Free 's', Free 'z']

compile (twice :@ twice)
reduce it [Free 's', Free 'z']

parse expr "(λx.xx)(λx.xx)"
compile it
reduce it []
parse expr "λf.(λx.f(xx))(λx.f(xx))"
compile it
reduce it [Free 's', Free 'z']
