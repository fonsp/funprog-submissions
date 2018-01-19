Fons van der Plas
Robin de Ruiter


7.2.1


Base case: Empty

Induction hypothesis: the property holds for subtrees l and r

2.
Base case P(Empty):
Amount of inner nodes: 0
Amount of outer nodes: 1
P(Empty) holds

Case Node l a r
Amount of inner nodes: li + ri + 1
Amount of outer nodes (li+1)+(ri+1)
P(Node l a r) holds

3.
Base case P(Empty):
minHeight = 1
size = 1
maxHeight = 1
2^1-1 <= 1 <= 2^1-1
P(Empty) holds

Case Node l a r
minHeight = max(minHeightl, minHeightr)+1
size = sizel+sizer+1
maxHeight = max(maxHeightl, maxHeightr)+1

Assume 2^minHeightl-1 = sizel = 2^maxHeightl-1
and 2^minHeightl-1 = sizel = 2^maxHeightl-1
This assumption is the strictest case from the induction hypothesis
in this case sizel = sizer:
sizel = sizer = x
2^minHeight-1 = 2^(minHeightl+1)-1 = 2^(minHeightl)*2  = 2^(x + 1)
size = x + x + 1
maxHeight = 2^(x + 1)
