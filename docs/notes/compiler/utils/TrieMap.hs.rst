`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/utils/TrieMap.hs>`_

compiler/utils/TrieMap.hs
=========================


Note [foldTM determinism]
~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/utils/TrieMap.hs#L138>`__

We want foldTM to be deterministic, which is why we have an instance of
TrieMap for UniqDFM, but not for UniqFM. Here's an example of some things that
go wrong if foldTM is nondeterministic. Consider:

::

  f a b = return (a <> b)

..

Depending on the order that the typechecker generates constraints you
get either:

::

  f :: (Monad m, Monoid a) => a -> a -> m a

..

or:

::

  f :: (Monoid a, Monad m) => a -> a -> m a

..

The generated code will be different after desugaring as the dictionaries
will be bound in different orders, leading to potential ABI incompatibility.

One way to solve this would be to notice that the typeclasses could be
sorted alphabetically.

Unfortunately that doesn't quite work with this example:

::

  f a b = let x = a <> a; y = b <> b in x

..

where you infer:

::

  f :: (Monoid m, Monoid m1) => m1 -> m -> m1

..

or:

::

  f :: (Monoid m1, Monoid m) => m1 -> m -> m1

..

Here you could decide to take the order of the type variables in the type
according to depth first traversal and use it to order the constraints.

The real trouble starts when the user enables incoherent instances and
the compiler has to make an arbitrary choice. Consider:

::

  class T a b where
    go :: a -> b -> String

..

::

  instance (Show b) => T Int b where
    go a b = show a ++ show b

..

::

  instance (Show a) => T a Bool where
    go a b = show a ++ show b

..

::

  f = go 10 True

..

GHC is free to choose either dictionary to implement f, but for the sake of
determinism we'd like it to be consistent when compiling the same sources
with the same flags.

inert_dicts :: DictMap is implemented with a TrieMap. In getUnsolvedInerts it
gets converted to a bag of (Wanted) Cts using a fold. Then in
solve_simple_wanteds it's merged with other WantedConstraints. We want the
conversion to a bag to be deterministic. For that purpose we use UniqDFM
instead of UniqFM to implement the TrieMap.

See Note [Deterministic UniqFM] in UniqDFM for more details on how it's made
deterministic.



Note [Compressed TrieMap]
~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/utils/TrieMap.hs#L314>`__

The GenMap constructor augments TrieMaps with leaf compression.  This helps
solve the performance problem detailed in #9960: suppose we have a handful
H of entries in a TrieMap, each with a very large key, size K. If you fold over
such a TrieMap you'd expect time O(H). That would certainly be true of an
association list! But with TrieMap we actually have to navigate down a long
singleton structure to get to the elements, so it takes time O(K*H).  This
can really hurt on many type-level computation benchmarks:
see for example T9872d.

The point of a TrieMap is that you need to navigate to the point where only one
key remains, and then things should be fast.  So the point of a SingletonMap
is that, once we are down to a single (key,value) pair, we stop and
just use SingletonMap.

'EmptyMap' provides an even more basic (but essential) optimization: if there is
nothing in the map, don't bother building out the (possibly infinite) recursive
TrieMap structure!

Compressed triemaps are heavily used by CoreMap. So we have to mark some things
as INLINEABLE to permit specialization.

