---- MODULE Utils ----
\****
\* A set of definitions for general operations that are used in the ReActor execution model

EXTENDS Naturals, Bags

----
\****
\* A function with no domain (and as a result no codomain)
EmptyFunction == [x \in {} |-> {}]

\****
\* Extends the domain of a given function f by the given k and maps f[k] to v.
ExtendFunction(f,k,v) ==
   [x \in (DOMAIN f \cup {k})
    |-> IF x \in DOMAIN f
           THEN f[x]
           ELSE v ]

\****
\* Combines given functions f and g to one function.
\* If function f and g have any domain elements in common, the mapping of f is choosen.
CombineFunctions(f,g) ==
   [x \in (DOMAIN f \cup DOMAIN g)
    |-> IF x \in DOMAIN f
           THEN f[x]
           ELSE g[x] ]

\****
\* Reduces the domain of a given function by k.
ReduceFunction(f,k) ==
   [x \in (DOMAIN f \ k)
    |-> f[x] ]

\****
\* True if Function f is a partial function of g.
IsPartialFunctionOf(f,g) ==
   /\ (DOMAIN f) \subseteq (DOMAIN g)
   /\ \A x \in (DOMAIN f) : f[x] = g[x]

----

\****
\* Takes one Element e from the Bag B.
TakeFromBag(e, B) ==
   IF CopiesIn(e,B) = 1
   THEN ReduceFunction(B,{e})
   ELSE [x \in (DOMAIN B) |-> IF x /= e
                              THEN B[x]
                              ELSE (B[x] - 1) ]

\****
\* Adds n times the element e to the bag B.
AddToBag(e,n,B) ==
   IF CopiesIn(e,B) > 0
   THEN [x \in (DOMAIN B) |-> IF x /= e 
                              THEN B[x]
                              ELSE (B[x] + n)]
   ELSE [x \in (DOMAIN B \cup {e}) |-> IF x \in DOMAIN B
                                       THEN B[x]
                                       ELSE n]

----

\****
\* Merges two Functions that are mapping to Bags to a combined Function with Bags put together for same Domain elements.
MergeFunctionsWithBags(f, g) ==
   [x \in (DOMAIN f \cup DOMAIN g)
    |-> IF x \notin DOMAIN f
           THEN g[x]
           ELSE IF x \notin DOMAIN g
                THEN f[x]
                ELSE g[x] (+) f[x] ]


====

