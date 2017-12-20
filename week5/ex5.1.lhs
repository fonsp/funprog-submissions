> {-# LANGUAGE UnicodeSyntax #-}

> allTrue :: [Bool] -> Bool
> allTrue = foldl (&&) True

> allFalse :: [Bool] -> Bool
> allFalse = not . (foldl (||) False)

> member :: (Eq a) => a -> [a] -> Bool
> member z l = foldl (\x y -> x || (z==y)) False l

> smallest :: [Int] -> Int
> smallest (x:xs) = foldl min x xs

> largest :: [Int] -> Int
> largest (x:xs) = foldl max x xs
