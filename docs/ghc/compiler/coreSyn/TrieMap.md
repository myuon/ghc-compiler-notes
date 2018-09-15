[[src]](https://github.com/ghc/ghc/tree/master/compiler/coreSyn/TrieMap.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998



This module implements TrieMaps, which are finite mappings
whose key is a structured value like a CoreExpr or Type.

The code is very regular and boilerplate-like, but there is
some neat handling of *binders*.  In effect they are deBruijn
numbered on the fly.

The regular pattern for handling TrieMaps on data structures was first
described (to my knowledge) in Connelly and Morris's 1995 paper "A
generalization of the Trie Data Structure"; there is also an accessible
description of the idea in Okasaki's book "Purely Functional Data
Structures", Section 10.3.2

# The TrieMap class


# IntMaps


### Note: foldTM determinism

We want foldTM to be deterministic, which is why we have an instance of
TrieMap for UniqDFM, but not for UniqFM. Here's an example of some things that
go wrong if foldTM is nondeterministic. Consider:

  f a b = return (a <> b)

Depending on the order that the typechecker generates constraints you
get either:

  f :: (Monad m, Monoid a) => a -> a -> m a

or:

  f :: (Monoid a, Monad m) => a -> a -> m a

The generated code will be different after desugaring as the dictionaries
will be bound in different orders, leading to potential ABI incompatibility.

One way to solve this would be to notice that the typeclasses could be
sorted alphabetically.

Unfortunately that doesn't quite work with this example:

  f a b = let x = a <> a; y = b <> b in x

where you infer:

  f :: (Monoid m, Monoid m1) => m1 -> m -> m1

or:

  f :: (Monoid m1, Monoid m) => m1 -> m -> m1

Here you could decide to take the order of the type variables in the type
according to depth first traversal and use it to order the constraints.

The real trouble starts when the user enables incoherent instances and
the compiler has to make an arbitrary choice. Consider:

  class T a b where
    go :: a -> b -> String

  instance (Show b) => T Int b where
    go a b = show a ++ show b

  instance (Show a) => T a Bool where
    go a b = show a ++ show b

  f = go 10 True

GHC is free to choose either dictionary to implement f, but for the sake of
determinism we'd like it to be consistent when compiling the same sources
with the same flags.

inert_dicts :: DictMap is implemented with a TrieMap. In getUnsolvedInerts it
gets converted to a bag of (Wanted) Cts using a fold. Then in
solve_simple_wanteds it's merged with other WantedConstraints. We want the
conversion to a bag to be deterministic. For that purpose we use UniqDFM
instead of UniqFM to implement the TrieMap.

### Note: Deterministic UniqFM

# Maybes


If              m is a map from k -> val
then (MaybeMap m) is a map from (Maybe k) -> val


# Lists


# Basic maps


# GenMap


### Note: Compressed TrieMap


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


# CoreMap


### Note: Binders

 * In general we check binders as late as possible because types are
   less likely to differ than expression structure.  That's why
      cm_lam :: CoreMapG (TypeMapG a)
   rather than
      cm_lam :: TypeMapG (CoreMapG a)

 * We don't need to look at the type of some binders, notably
     - the case binder in (Case _ b _ _)
     - the binders in an alternative
   because they are totally fixed by the context

### Note: Empty case alternatives

* For a key (Case e b ty (alt:alts))  we don't need to look the return type
  'ty', because every alternative has that type.

* For a key (Case e b ty []) we MUST look at the return type 'ty', because
  otherwise (Case (error () "urk") _ Int  []) would compare equal to
            (Case (error () "urk") _ Bool [])
  which is utterly wrong (Trac #6097)

We could compare the return type regardless, but the wildly common case
is that it's unnecessary, so we have two fields (cm_case and cm_ecase)
for the two possibilities.  Only cm_ecase looks at the type.

### Note: Empty case alternatives

# Coercions


# Types


# Variables
