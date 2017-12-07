> {-# LANGUAGE UnicodeSyntax #-}
> module BinaryTree
> where
> import Unicode

> data Tree elem = Empty | Node (Tree elem) elem (Tree elem)
>   deriving (Show)

> instance Functor Tree where
>   fmap _f Empty         =  Empty
>   fmap f  (Node l a r)  =  Node (fmap f l) (f a) (fmap f r)

> ex1  ∷  Tree Integer
> ex1  =  Node Empty 4711 (Node Empty 0815 (Node Empty 42 Empty))
> ex2  ∷  Tree String
> ex2  =  Node (Node (Node Empty "Frits" Empty) "Peter" Empty) "Ralf" Empty
> ex3  ∷  Tree Char
> ex3  =  Node (Node Empty 'a' Empty) 'k' (Node Empty 'z' Empty)

> size ∷ Tree elem → Int
> size Empty = 0
> size (Node left node right) = 1 + (size left) + (size right)


> fig1 :: Tree Char
> fig1 = Node a 'c' f
>   where
>     a = Node (Empty) 'a' (Node Empty 'b' Empty)
>     f = Node (Node Empty 'd' Empty) 'f' (Node Empty 'g' Empty)


For drawing the tree, observe that:
- Empty nodes are not drawn.
- Every non-empty node is on its own line, and every line contains a node.
- The number of spaces of a node is four times its height in the tree.
- The lines are in the correct order if they are added using tree recursion,
  in the form:
  [left branch]
  [node]
  [right branch]

The following definition uses a helper function, 'draw' which has two additional
parameters: 'spacing' and 'edgeChar'.
'spacing' represents the height: 4 spaces are added whenever the height
increases.
'edgeChar' is either /, \ or -. Its value is set by the caller.

> layout ∷ (Show elem) => Tree elem → String
> layout tree = draw "" "-" tree
>   where
>     draw spacing edgeChar Empty = ""
>     draw spacing edgeChar (Node left node right)
>       = (draw (spacing++"    ") "/" left)
>         ++ spacing ++ edgeChar ++ " " ++ (show node) ++ "\n" ++
>         (draw (spacing++"    ") "\\" right)

minHeight, maxHeight ∷ Tree elem → Int
member ∷ (Eq elem) ⇒ elem → Tree elem → Bool
preorder, inorder, postorder ∷ Tree elem → [elem]
build ∷ [elem] → Tree elem
balanced ∷ [elem] → Tree elem
create ∷ Int → Tree ()
