Fons van der Plas
Robin de Ruiter


inorderCat t xs = inorder t ++ xs =


inorderCat Empty xs =
definition inorder
[] ++ xs

inorderCat (Node l a r) xs = inorder l ++ [a] ++ inorder r ++ xs =
induction hypothesis tells us the equivalence holds for tree l and r
inorderCat l [a] ++ inorderCat r xs

> inorderCat :: Tree elem -> [elem] -> [elem]
> inorderCat Empty xs = xs
> inorderCat (Node l a r) xs = inorderCat l [a] ++ inorderCat r xs

since inorder t++[] = inorder t

> inorder :: Tree elem -> [elem]
> inorder t = inorderCat t []


preorderCat t xs = xs ++ preorder t
definition preorder
xs++[a]++preorder l++preorder r =
associativity of (++)
xs++([a]++preorder l)++preorder r =
induction hypothesis
xs++(preorderCat l [a])++preorder r =
associativity of (++)
xs++((preorderCat l [a])++preorder r) =
induction hypothesis
xs ++ preorderCat r (preorderCat l [a])

> preorderCat :: Tree elem -> [elem] -> [elem]
> preorderCat Empty xs = xs
> preorderCat (Node l a r) xs = xs ++ preorderCat r (preorderCat l [a])

> preorder :: Tree elem -> [elem]
> preorder t = preorderCat t []

> postorderCat :: Tree elem -> [elem] -> [elem]
> postorderCat Empty xs = xs
> postorderCat (Node l a r) xs = preorderCat l (preorderCat r [a]) ++ xs

> postorder :: Tree elem -> [elem]
> postorder t = preorderCat t []

7.2.3
??

7.2.4
??
