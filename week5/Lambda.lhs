> {-# LANGUAGE UnicodeSyntax #-}
> module Lambda
> where
> import Prelude hiding (fail)
> import Unicode ()
> import Parser

> infixl 9 :@
>
> data Lambda var
>   =  Var var                   -- variable
>   |  Fun var (Lambda var)      -- abstraction/λ-expression
>   |  Lambda var :@ Lambda var  -- application
>   deriving (Show)

Context-free grammar:

E -> V
V -> a..z
E -> EE
E -> FE
F -> λV.E
E -> (E)

In the following way, application binds to the left, and abstraction extends
maximally to the right.

expr, jenesaisquoi, term and variable are expressions of decreasing complexity,
with one exception: the first three can contain an expr within parentheses,
which creates a kind of cycle of complexity:


                                 enclosed in
                                 parentheses
                  /-----------------------------------------\
                  |                                         |
                  ↓                                         |
  variable  --> term ----------> jenesaisquoi -----------> expr
                     can contain              can contain
                     application              abstraction


> expr, jenesaisquoi, term, variable, abstraction, application :: Parser (Lambda Char)
> expr = jenesaisquoi
>     .| abstraction
>     .| (jenesaisquoi >>= \t -> abstraction >>= \a -> return (t :@ a))
> abstraction = (symbol 'λ' >> lower >>= \l -> symbol '.' >> expr >>= \e -> return (Fun l e))

The third case for expr handles cases like aλx.x and abλx.x.
A jenesaisquoi is an expression without abstraction, or an expression contained
within parentheses.

> jenesaisquoi = term
>     .| application
> application = (term >>= \e -> many1 jenesaisquoi >>= \js -> return (foldl (:@) e js))

A term is either a single lowercase letter (variable) or an expression contained
within parentheses.

> term = variable
>     .| (symbol '(' >> expr >>= \i -> symbol ')' >> return i)

> variable = (lower >>= \l -> return (Var l))
