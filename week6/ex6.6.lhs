> {-# LANGUAGE UnicodeSyntax #-}
> module DigitalSorting
> where
> import Unicode
> import Data.List (groupBy, sortBy)
> import Data.Either (isLeft, isRight)

> class Rank key where
>   sort  ∷  [(key, val)] → [val]
>   rank  ∷  [(key, val)] → [[val]]
>   sort  =  concat ∘ rank

> genericSort ∷ (Ord key) ⇒ [(key, val)] → [val]
> genericSort kvs  =  map snd (sortBy (\ kv1 kv2 → compare (fst kv1) (fst kv2)) kvs)

The following implementation is based on that of 'genericSort', with the addition
of grouping. map snd needs to be replaced by map (map snd).

> genericRank :: (Ord key) => [(key, val)] -> [[val]]
> genericRank kvs = map (map snd) (groupBy (\x y -> compare (fst x) (fst y) == EQ) (sortBy (\x y -> compare (fst x) (fst y)) kvs))

> instance Rank () where
>   sort kvs   =  map snd kvs
>   rank kvs   =  [ map snd kvs | not (null kvs) ]

Ranking the list [(x,LT),(y,GT)] has three possible outputs:
1. [[LT],[GT]]  -> when x < y   -> output LT
2. [[GT],[LT]]  -> when x > y   -> output GT
3. [[LT,GT]]    -> when x == y  -> output EQ

This leads to the following definition:

> genericCompare :: (Rank key) => key -> key -> Ordering
> genericCompare x y = evaluate $ rank [(x,LT),(y,GT)]
>   where
>     evaluate [p]    = EQ
>     evaluate (p:ps) = head p

For ranking, we first rank the list [(key1, (key2, v))]. This creates a list
of lists, of the form [[(key2, v)]]. By then ranking each of these lists, we
get the desired output.
Sort is similar.

> instance (Rank key1, Rank key2) ⇒ Rank (key1, key2) where
>   rank kvs = concat $ map rank $ rank [(key1, (key2,v)) | ((key1,key2),v) <- kvs]
>   sort kvs = concat $ map sort $ rank [(key1, (key2,v)) | ((key1,key2),v) <- kvs]

> instance Rank Bool where
>   rank = genericRank

These built-in functions were not available in my compiler (GHCi 7.10.3):

> fromLeft  :: a -> Either a b -> a
> fromLeft  _ (Left a)  = a
> fromLeft  a _         = a
> fromRight :: b -> Either a b -> b
> fromRight _ (Right b) = b
> fromRight b _         = b

We first rank the list by the property 'isRight'. This produces a list with
at most 2 elements, each being a list of key-value pairs of the same type.
We then do some trickery to change the key types from 'Either key1 key2' to
'key1' or 'key2', depending on the type of the first key in that rank.

> instance (Rank key1, Rank key2) ⇒ Rank (Either key1 key2) where
>   rank kvs = concat $ map specialrank $ rank [(isRight k, (k,v)) | (k,v) <- kvs]
>     where
>       specialrank (x:xs) = derp x (x:xs)
>       derp (Left  firstkey, firstval) l = rank $ map (\(k,v) -> (fromLeft  firstkey k,v)) l
>       derp (Right firstkey, firstval) l = rank $ map (\(k,v) -> (fromRight firstkey k,v)) l

> type List elem  =  Either () (elem, [elem])
>
> toList ∷ [elem] → List elem
> toList []        =  Left ()
> toList (a : as)  =  Right (a, as)

If we map the key arrays to Lists, all desired properties follow from the
definitions of List, Rank (Either k1 k2) and Rank (k1,k2).

> instance (Rank key) ⇒ Rank [key] where
>   rank kvs = rank [(toList k,v) | (k,v) <- kvs]


repeatedSegments ∷ (Rank key) ⇒ Int → [key] → [[Integer]]

instance Rank Base where
