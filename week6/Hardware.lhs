> {-# LANGUAGE UnicodeSyntax #-}
> module Hardware
> where
> import Unicode hiding ((∨),(∧))

> data Bit  =  O | I
>   deriving (Eq, Ord, Show)

> infixr 3 ∧
> (∧) ∷ Bit → Bit → Bit
> O ∧ _b  =  O
> I ∧ b   =  b

> infixr 2 ∨
> (∨) ∷ Bit → Bit → Bit
> O ∨ b   =  b
> I ∨ _b  =  I

> infixr 4 ⊕
> (⊕) ∷ Bit → Bit → Bit
> O ⊕ O  =  O
> O ⊕ I  =  I
> I ⊕ O  =  I
> I ⊕ I  =  O

xs needs to be reversed, since list recursion is from left to right.

> mapr ∷ ((a, state) → (b, state)) → (([a], state) → ([b], state))
> mapr f = \(xs, s) -> (iterator (reverse xs) s)
>   where
>     iterator []     s  = ([], s)

We apply f to the first element (which is actually the rightmost element) and
pass on the state to the recursive call. 

>     iterator (x:xs) s  = let (y,s') = f (x,s)
>                              prev   = iterator xs s'
>                          in ((fst prev) ++ [y], (snd prev))


> test :: (Int, Int) -> (Int, Int)
> test (x,y) = (x*x, y*10+x)

mapr test ([1,2,3],0) == ([1,4,9],321)


> type Carry  =  Bit

> halfAdder ∷ (Bit, Bit) → (Bit, Carry)
> halfAdder (x,y) = (x ⊕ y, x ∧ y)

From https://en.wikipedia.org/wiki/Adder_(electronics)#Full_adder

> fullAdder ∷ ((Bit, Bit), Carry) → (Bit, Carry)
> fullAdder ((x,y),cin) = firstPrelim (halfAdder (x,y)) cin
>   where
>     firstPrelim  (s,c2) cin = secondPrelim (halfAdder (s,cin)) c2
>     secondPrelim (s,c3) c2  = (s, c2 ∨ c3)

Now that we have a full adder, the rippleCarry implementation is easy.
pair is the 'transpose' of the two input lists.

> rippleCarry :: [Bit] -> [Bit] -> [Bit]
> rippleCarry as bs = fst $ mapr fullAdder (pair as bs, O)
>   where
>     pair []     []     = []
>     pair (x:xs) (y:ys) = (x,y) : pair xs ys
