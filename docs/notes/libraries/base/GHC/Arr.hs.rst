`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/GHC/Arr.hs>`_

libraries/base/GHC/Arr.hs
=========================


Note [Inlining index]
~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/GHC/Arr.hs#L124>`__

We inline the 'index' operation,

 * Partly because it generates much faster code
   (although bigger); see #1216

 * Partly because it exposes the bounds checks to the simplifier which
   might help a big.

If you make a per-instance index method, you may consider inlining it.



Note [Double bounds-checking of index values]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/GHC/Arr.hs#L136>`__

When you index an array, a!x, there are two possible bounds checks we might make:

::

  (A) Check that (inRange (bounds a) x) holds.

::

      (A) is checked in the method for 'index'

::

  (B) Check that (index (bounds a) x) lies in the range 0..n,
      where n is the size of the underlying array

::

      (B) is checked in the top-level function (!), in safeIndex.

Of course it *should* be the case that (A) holds iff (B) holds, but that
is a property of the particular instances of index, bounds, and inRange,
so GHC cannot guarantee it.

 * If you do (A) and not (B), then you might get a seg-fault,
   by indexing at some bizarre location.  #1610

 * If you do (B) but not (A), you may get no complaint when you index
   an array out of its semantic bounds.  #2120

At various times we have had (A) and not (B), or (B) and not (A); both
led to complaints.  So now we implement *both* checks (#2669).

For 1-d, 2-d, and 3-d arrays of Int we have specialised instances to avoid this.



Note [Out-of-bounds error messages]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/GHC/Arr.hs#L164>`__

The default method for 'index' generates hoplelessIndexError, because
Ix doesn't have Show as a superclass.  For particular base types we
can do better, so we override the default method for index.
Abstract these errors from the relevant index functions so that
the guts of the function will be small enough to inline.



Note [amap]
~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/GHC/Arr.hs#L775>`__

amap was originally defined like this:

::

 amap f arr@(Array l u n _) =
     unsafeArray' (l,u) n [(i, f (unsafeAt arr i)) | i <- [0 .. n - 1]]

There are two problems:

1. The enumFromTo implementation produces (spurious) code for the impossible
   case of n<0 that ends up duplicating the array freezing code.

2. This implementation relies on list fusion for efficiency. In order
   to implement the "amap/coerce" rule, we need to delay inlining amap
   until simplifier phase 1, which is when the eftIntList rule kicks
   in and makes that impossible.  (c.f. #8767)
See Breitner, Eisenberg, Peyton Jones, and Weirich, "Safe Zero-cost
Coercions for Haskell", section 6.5:
  http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/coercible.pdf

