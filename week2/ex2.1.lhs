Fons van der Plas
s4576586
f.vanderplas@student.ru.nl

> {-# LANGUAGE UnicodeSyntax #-}

Excercise 2.1
1. There are 4 possible functions. Each possible input must be assigned to one
of two possible outputs: False or True

> makeFalse ∷ Bool → Bool
> makeFalse False = False
> makeFalse True  = False

> makeTrue ∷ Bool → Bool
> makeTrue False = True
> makeTrue True  = True

> identity ∷ Bool → Bool
> identity False = False
> identity True  = True

> negate ∷ Bool → Bool
> negate False = True
> negate True  = False



2. There are 8 possible functions. There are 4 possible inputs, and each can be
assigned to two possible outputs.

> and ∷ (Bool, Bool) → Bool
> and (x,y) = x && y

> or ∷ (Bool, Bool) → Bool
> or (x,y)
>   | x == True = True
>   | otherwise = y

> nor ∷ (Bool, Bool) → Bool
> nor (False,False) = True
> nor (x,y) = False

> xor ∷ (Bool, Bool) → Bool
> xor (x,y) =
>   if x == True
>       then nor (False, y)
>       else y

3. These are almost exactly the same functions, but with `x y` instead of `(x,y)`
