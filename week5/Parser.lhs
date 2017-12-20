> {-# LANGUAGE UnicodeSyntax #-}
> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE LambdaCase #-}

> module Parser (
>   Parser,
>   fail, (.|),
>   symbol,
>   parse, parseAll,
>   alt, many, many1,
>   digit, lower, upper )
> where
> import Prelude hiding (fail)
> import Unicode ()
> import Control.Monad (ap)

Implementation technique: ``list of successes''.

> newtype Parser val
>   =  Parser { apply ∷ String → [(val, String)] }
>      deriving (Functor)

Sequencing.

> instance Applicative Parser where
>   pure   =  return
>   (<*>)  =  ap
>
> instance Monad Parser where
>   return a  =  Parser (\ inp → [(a, inp)])
>   p >>= q   =  Parser (\ inp → [ res | (a, inp') ← apply p inp, res ← apply (q a) inp' ])

Choice.

> fail  ∷  Parser val
> fail  =  Parser (\ _inp → [])

> infixr 6 .|
> (.|) ∷ Parser val → Parser val → Parser val
> p .| q  =  Parser (\ inp → apply p inp ++ apply q inp)

Selection.

> symbol ∷ Char → Parser Char
> symbol c  =  Parser (\case
>                      ""       → []
>                      a : inp' → if c == a then [(a, inp')] else [])

Applying a parser.

> parse ∷ Parser val → (String → val)
> parse p inp  =  case [ v | (v,  "") <- apply p inp ] of
>                 []  -> error "parse: parse failed"
>                 [v] -> v
>                 _   -> error "parse: ambiguous parse"

> parseAll ∷ Parser val → (String → [val])
> parseAll p inp  =  case [ v | (v,  "") <- apply p inp ] of
>                 []  -> error "parse: parse failed"
>                 l   -> l


Derived operations.

> alt  ∷  [Parser val] → Parser val
> alt  =  foldr (.|) fail

> many, many1 ∷ Parser val → Parser [val]
> many   p  =  many1 p .| return []
> many1  p  =  do a ← p; as ← many p; return (a : as)

> digit, lower, upper ∷ Parser Char
> digit  =  alt [ symbol c | c ← ['0' .. '9'] ]
> lower  =  alt [ symbol c | c ← ['a' .. 'z'] ]
> upper  =  alt [ symbol c | c ← ['A' .. 'Z'] ]
