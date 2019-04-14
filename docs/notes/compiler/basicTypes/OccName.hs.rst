`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/OccName.hs>`_

compiler/basicTypes/OccName.hs
==============================


Note [Suppressing uniques in OccNames]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/OccName.hs#L292>`__

This is a hack to de-wobblify the OccNames that contain uniques from
Template Haskell that have been turned into a string in the OccName.
See Note [Unique OccNames from Template Haskell] in Convert.hs



Note [The Unique of an OccName]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/OccName.hs#L375>`__

They are efficient, because FastStrings have unique Int# keys.  We assume
this key is less than 2^24, and indeed FastStrings are allocated keys
sequentially starting at 0.

So we can make a Unique using
        mkUnique ns key  :: Unique
where 'ns' is a Char representing the name space.  This in turn makes it
easy to build an OccEnv.



Note [TidyOccEnv]
~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/OccName.hs#L771>`__

type TidyOccEnv = UniqFM Int

* Domain = The OccName's FastString. These FastStrings are "taken";
           make sure that we don't re-use

* Int, n = A plausible starting point for new guesses
           There is no guarantee that "FSn" is available;
           you must look that up in the TidyOccEnv.  But
           it's a good place to start looking.

* When looking for a renaming for "foo2" we strip off the "2" and start
  with "foo".  Otherwise if we tidy twice we get silly names like foo23.

  However, if it started with digits at the end, we always make a name
  with digits at the end, rather than shortening "foo2" to just "foo",
  even if "foo" is unused.  Reasons:
     - Plain "foo" might be used later
     - We use trailing digits to subtly indicate a unification variable
       in typechecker error message; see TypeRep.tidyTyVarBndr

We have to take care though! Consider a machine-generated module (#10370)
  module Foo where
     a1 = e1
     a2 = e2
     ...
     a2000 = e2000
Then "a1", "a2" etc are all marked taken.  But now if we come across "a7" again,
we have to do a linear search to find a free one, "a2001".  That might just be
acceptable once.  But if we now come across "a8" again, we don't want to repeat
that search.

So we use the TidyOccEnv mapping for "a" (not "a7" or "a8") as our base for
starting the search; and we make sure to update the starting point for "a"
after we allocate a new one.



Note [Tidying multiple names at once]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/OccName.hs#L809>`__

Consider

::

    > :t (id,id,id)

..

Every id contributes a type variable to the type signature, and all of them are
"a". If we tidy them one by one, we get

::

    (id,id,id) :: (a2 -> a2, a1 -> a1, a -> a)

..

which is a bit unfortunate, as it unfairly renames only one of them. What we
would like to see is

::

    (id,id,id) :: (a3 -> a3, a2 -> a2, a1 -> a1)

..

To achieve this, the function avoidClashesOccEnv can be used to prepare the
TidyEnv, by “blocking” every name that occurs twice in the map. This way, none
of the "a"s will get the privilege of keeping this name, and all of them will
get a suitable number by tidyOccName.

This prepared TidyEnv can then be used with tidyOccName. See tidyTyCoVarBndrs
for an example where this is used.

This is #12382.

