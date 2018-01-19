Fons van der Plas
Robin de Ruiter

P(xs) :=
ordered xs => ordered (insert x xs)

base case
P([]) =
ordered xs =>  ordered (insert x [])
definition insert
ordered xs =>  ordered ([x])
P([]) holds

inductive hypothesis
P(xs)

P(x:xs)
ordered (x:xs) => ordered(insert a (x:xs))
definition insert
case a<=x:
ordered(x:xs), and a<=x, therefore ordered(a:x:xs), P holds for case
case a>x:
induction hypothesis tells us ordered(insert a xs), and x<a && x<=head(xs)
therefore ordered(x:insert a xs), and thus P holds for case
P(x:xs) proven

P proven

Q(xs) := ordered(insertionSort xs)

base case
Q([]) =
ordered(insertionSort []) =
definition insertionSort
ordered([]) holds
therefore Q([]) holds

Q(x:xs) =
According to the induction hypothesis ordered(xs) holds, and the first part of the proof has shown that
ordered xs => ordered (insert x xs)
therefore Q(x:xs) holds.
