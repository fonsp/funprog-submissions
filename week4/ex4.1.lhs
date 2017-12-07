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

> fig1 :: Tree Char
> fig1 = Node a 'c' f
>   where
>     a = Node (Empty) 'a' (Node Empty 'b' Empty)
>     f = Node (Node Empty 'd' Empty) 'f' (Node Empty 'g' Empty)


ex1:

              4711
            /     \
           Ɛ     0815
                /     \
               Ɛ      42
                    /   \
                   Ɛ     Ɛ

ex2:

                Ralf
              /     \
           Peter     Ɛ
         /      \
      Frits      Ɛ
     /     \
    Ɛ       Ɛ

ex3:

           k
         /   \
        a     z
       / \   / \
      Ɛ  Ɛ   Ɛ  Ɛ


A recursive definition for 'size': empty nodes have 0 size, regular nodes
contribute 1 to the total size.

> size ∷ Tree elem → Int
> size Empty = 0
> size (Node left node right) = 1 + (size left) + (size right)


> minHeight, maxHeight ∷ Tree elem → Int
> minHeight Empty = 0
> minHeight (Node left node right) = 1 + min (minHeight left) (minHeight right)

> maxHeight Empty = 0
> maxHeight (Node left node right) = 1 + max (maxHeight left) (maxHeight right)


We have found the following relationship:

2^(minHeight) - 1 + (maxHeight-minHeight)
  <=
size
  <=
2^(maxHeight) - 1 - (2^(maxHeight-minHeight) - 1)
= 2^(maxHeight)   -  2^(maxHeight-minHeight)

Note: by a 'complete n-tree' we mean a binary tree where every leaf is at height n.

The first inequality describes the case where every branch in tree has height
'minHeight', and there is one long branch of height 'maxHeight'.
The size of this tree is (size of complete minHeight-tree) + (addition length of
the single long branch).
In this case, the inequality is strict.

The second inequality descibes the case where the tree is a maxHeight-tree, but
a complete (maxHeight-minHeight)-subtree is removed to expose a leaf at height
minHeight.
In this case, the inequality is strict.

A simple recursive definition:

> member ∷ (Eq elem) ⇒ elem → Tree elem → Bool
> member x Empty = False
> member x (Node left node right)
>   | x == node   = True
>   | otherwise   = (member x left) || (member x right)



preorder, inorder, postorder ∷ Tree elem → [elem]
layout ∷ (Show elem) => Tree elem → String
build ∷ [elem] → Tree elem
balanced ∷ [elem] → Tree elem
create ∷ Int → Tree ()
