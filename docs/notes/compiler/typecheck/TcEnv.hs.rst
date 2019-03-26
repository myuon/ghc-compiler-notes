`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcEnv.hs>`_

compiler/typecheck/TcEnv.hs
===========================


Note [AFamDataCon: not promoting data family constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcEnv.hs#L743>`__

Consider
  data family T a
  data instance T Int = MkT
  data Proxy (a :: k)
  data S = MkS (Proxy 'MkT)

Is it ok to use the promoted data family instance constructor 'MkT' in
the data declaration for S (where both declarations live in the same module)?
No, we don't allow this. It *might* make sense, but at least it would mean that
we'd have to interleave typechecking instances and data types, whereas at
present we do data types *then* instances.

So to check for this we put in the TcLclEnv a binding for all the family
constructors, bound to AFamDataCon, so that if we trip over 'MkT' when
type checking 'S' we'll produce a decent error message.

#12088 describes this limitation. Of course, when MkT and S live in
different modules then all is well.



Note [Don't promote pattern synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcEnv.hs#L764>`__

We never promote pattern synonyms.

Consider this (#11265):
  pattern A = True
  instance Eq A
We want a civilised error message from the occurrence of 'A'
in the instance, yet 'A' really has not yet been type checked.

Similarly (#9161)
  {-# LANGUAGE PatternSynonyms, DataKinds #-}
  pattern A = ()
  b :: A
  b = undefined
Here, the type signature for b mentions A.  But A is a pattern
synonym, which is typechecked as part of a group of bindings (for very
good reasons; a view pattern in the RHS may mention a value binding).
It is entirely reasonable to reject this, but to do so we need A to be
in the kind environment when kind-checking the signature for B.

Hence tcAddPatSynPlaceholers adds a binding
    A -> APromotionErr PatSynPE
to the environment. Then TcHsType.tcTyVar will find A in the kind
environment, and will give a 'wrongThingErr' as a result.  But the
lookup of A won't fail.



Note [Extended defaults]
~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcEnv.hs#L906>`__

In interative mode (or with -XExtendedDefaultRules) we add () as the first type we
try when defaulting.  This has very little real impact, except in the following case.
Consider:
        Text.Printf.printf "hello"
This has type (forall a. IO a); it prints "hello", and returns 'undefined'.  We don't
want the GHCi repl loop to try to print that 'undefined'.  The neatest thing is to
default the 'a' to (), rather than to Integer (which is what would otherwise happen;
and then GHCi doesn't attempt to print the ().  So in interactive mode, we add
() to the list of defaulting types.  See #1200.

Additionally, the list type [] is added as a default specialization for
Traversable and Foldable. As such the default default list now has types of
varying kinds, e.g. ([] :: * -> *)  and (Integer :: *).



Note [Out of scope might be a staging error]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcEnv.hs#L1136>`__

Consider
  x = 3
  data T = MkT $(foo x)

where 'foo' is imported from somewhere.

This is really a staging error, because we can't run code involving 'x'.
But in fact the type checker processes types first, so 'x' won't even be
in the type envt when we look for it in $(foo x).  So inside splices we
report something missing from the type env as a staging error.
See #5752 and #5795.

