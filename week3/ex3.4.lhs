Fons van der Plas
s4576586
f.vanderplas@student.ru.nl

> {-# LANGUAGE UnicodeSyntax #-}
> module DNA
> where
> import Prelude hiding (filter)
> import Unicode
> import List

Nucleobases or DNA-bases are the basic building blocks of
deoxyribonucleic acid (DNA).

> data Base  =  A | C | G | T
>   deriving (Eq, Ord)

Adenin (A), Cytosin (C), Guanin (G) und Thymin (T).

> instance Show Base where
>   showsPrec _ A  =  showChar 'A'
>   showsPrec _ C  =  showChar 'C'
>   showsPrec _ G  =  showChar 'G'
>   showsPrec _ T  =  showChar 'T'
>
>   showList  =  foldr (.) id . map shows

> base ∷ Char → Maybe Base
> base 'A'  =  Just A
> base 'C'  =  Just C
> base 'G'  =  Just G
> base 'T'  =  Just T
> base _    =  Nothing

> type DNA      =  [Base]
> type Segment  =  [Base]

> dna  ∷  DNA
> dna  =  [A, T, G, T, A, A, A, G, G, G, T, C, C, A, A, T, G, A]

> mm  ∷  DNA
> mm  =  filter base
>    "ATGTAAAGGGTCCAATGACTGGAATTACTTCACAGCCCTGACACTGTGGAGAGATGGATA\
>    \CCAGTTTAATTATGGACAGCTGAGAATAATCTGGGAGTCACCTGCTCTGGATATTTATCA\
>    \TCATTATGAACTTCTCACCAGGCTGTGGCCCGAACACTTTCATAACGTCCCATTTGTGTT\
>    \GGGCAGACATTATGATCTATACAGAACCTGCCTTGTCTGTCACCTTTGAATTTATGGATA\
>    \GACAATTTAATAT\
>    \GTGTTCCTGGCAGCAAAGATAATCATGGAGAGTGGAGAGAAACTAACCTTACCATTGATA\
>    \GGGAAACTCTTGAAGTGTCAACTTCTCCATATTAAATCCAAGGACCAGCAGAGACGAGAA\
>    \AATGAAAAGAAGATGGTAGAAGAAAGAACAAAATCAGAAAAAGACAAAGGAAAAGGGAAG\
>    \TCTCCAAAGGAGAAGAAAGTTGCCAGTGCCAAGCCTGGGAAGGGAAAGAGCAAGGACCAG"

> readDNA ∷ FilePath → IO [Base]
> readDNA path
>   =  do  x ← readFile path
>          return (filter base x)

The 'contains' function will use a helper function: segmentPositions. If its
output is a non-empty set, then the DNA strands contains the segment.

> contains            ∷ Segment → DNA → Bool
> contains seg dna = (length $ segmentPositions seg dna) > 0

Returns all valid starting positions for the segment. To check validity, the
function isSegmentStart counts down from n, skipping DNA positions, and then
starts comparing the segment to the remaining DNA strand.
Note that this function will also find overlapping segments.


> segmentPositions ∷ Segment → DNA → [Int]
> segmentPositions seg dna = [n | n ← [1..(1+length dna - length seg)], isSegmentStart seg dna n]
>   where
>     isSegmentStart []     dna     1 = True
>     isSegmentStart (s:ss) (d:ds)  1 = (s == d) && isSegmentStart ss ds 1
>     isSegmentStart seg    (d:ds)  n = isSegmentStart seg ds (n-1)

I implemented my own 'group' function (below), which I used to find all sequences
of As. This can easily be used for the next two assigments.

> longestOnlyAs       ∷ DNA → Integer
> longestOnlyAs dna = fromIntegral $ largest [n | (b,n) ← group dna, head b == A]

> longestAtMostTenAs  ∷ DNA → Integer
> longestAtMostTenAs dna = fromIntegral $ largest [n | (b,n) ← group dna, head b == A, n <= 10]

The group function is based on a 'grouper' function, which clumps together
equal neighbours. e.g.:

[1,2,2,3] → [[1],[2,2],[3]]

This is then mapped to

[([1],1), ([2,2],2), ([3],1)]

The implementation of 'grouper' is very similar to that of 'runs', from
Exercise 3.3. The only change is to the condition of the first guard.

> group ∷ (Eq a) ⇒ [a] → [([a],Int)]
> group xs = map (\g → (g, length g)) (grouper xs)
>   where
>     grouper [] = []
>     grouper [x] = [[x]]
>     grouper (x:y:xs)
>         | x /= y    = [x] : grouper (y:xs)
>         | otherwise = (x: (head $ grouper (y:xs))) : (tail $ grouper (y:xs))

Copied from Exercise 3.2:

> smallest ∷ [Int] → Int
> smallest [x] = x
> smallest (x:y:xs)
>         | x < y     = smallest (x:xs)
>         | otherwise = smallest (y:xs)

To mix things up: negate, take the smallest value, negate

> largest ∷ [Int] → Int
> largest xs = 0-(smallest $ map (0-) xs)
