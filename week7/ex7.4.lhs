Fons van der Plas
Robin de Ruiter


|| 7.4.1

We have the fusion law:
> f(a ▷ b) = a ► f b

And:
> map f = foldr (\x ys -> (f x):ys) []

So that

> foldr (▷) e ∘ map f =
> foldr (▷) e ∘ (foldr (\x ys -> (f x):ys) []) =

Now, instead of first creating a new list and then folding, we fold right away.
This means that instead of appending (f x) to ys, we apply ▷ to (f x) and the
already folded ys (ysFolded).

> foldr (\x ysFolded -> (f x) ▷ ysFolded) e =
> foldr (\a b        -> (f a) ▷ b       ) e


|| 7.4.2

Let's work from right to left.

> map f ∘ map g

We can rewrite map f as an application of foldr:

> map f = foldr (\x ys -> (f x):ys) []
>       = foldr (▷)                 e

We have defined:

> (▷) ≡ \x ys -> (f x):ys
> e ≡ []

The right-hand equation then becomes:

> foldr (▷) e ∘ map g =
> foldr (\a b -> (g a) ▷ b ) e =    -- by the foldr-map rule
> foldr (\a b -> (f (g a)):b ) [] =  -- substituting our definitions
> foldr (\a b -> ((f ∘ g) a):b) [] = -- rewrite
> foldr (\a b -> (h a) ◎ b ) ε =    -- we define h ≡ f ∘ g,	ε ≡ [] and ◎ ≡ :
> foldr (◎) ε ∘ map (f ∘ g) =        -- by the foldr-map law
> foldr (:) [] ∘ map (f ∘ g) =       -- substitution
> map (f ∘ g)

Since 'foldr (:) []' is the identity function.


|| 7.4.3

We have

> concat = foldr (++) []
> reduce = foldr (•) ε

So that the right hand side equals

> reduce ∘ map reduce =
> foldr (•) ε ∘ map reduce =
> foldr (\a b -> (reduce a) • b) ε

Since • is associative, there is no difference between
(a1 • a2 • .. • an) • (b1 • .. bm) • ..
and
a1 • a2 • .. • an • b1 • .. bm • ..

The step shown here is concatenation. Thus,

> reduce ∘ map reduce = reduce ∘ concat
