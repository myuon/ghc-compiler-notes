`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/ghc-prim/GHC/Types.hs>`_

====================
libraries/ghc-prim/GHC/Types.hs.rst
====================

Note [Kind-changing of (~) and Coercible]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(~) and Coercible are tricky to define. To the user, they must appear as
constraints, but we cannot define them as such in Haskell. But we also cannot
just define them only in GHC.Prim (like (->)), because we need a real module
for them, e.g. to compile the constructor's info table.

Furthermore the type of MkCoercible cannot be written in Haskell
(no syntax for ~#R).

So we define them as regular data types in GHC.Types, and do magic in TysWiredIn,
inside GHC, to change the kind and type.


Note [Optimizing isTrue#]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Current definition of isTrue# is a temporary workaround. We would like to
have functions isTrue# and isFalse# defined like this:

.. code-block:: haskell

    isTrue# :: Int# -> Bool
    isTrue# 1# = True
    isTrue# _  = False

.. code-block:: haskell

    isFalse# :: Int# -> Bool
    isFalse# 0# = True
    isFalse# _  = False

These functions would allow us to safely check if a tag can represent True
or False. Using isTrue# and isFalse# as defined above will not introduce
additional case into the code. When we scrutinize return value of isTrue#
or isFalse#, either explicitly in a case expression or implicitly in a guard,
the result will always be a single case expression (given that optimizations
are turned on). This results from case-of-case transformation. Consider this
code (this is both valid Haskell and Core):

case isTrue# (a ># b) of
    True  -> e1
    False -> e2

Inlining isTrue# gives:

case (case (a ># b) of { 1# -> True; _ -> False } ) of
    True  -> e1
    False -> e2

Case-of-case transforms that to:

case (a ># b) of
  1# -> case True of
          True  -> e1
          False -> e2
  _  -> case False of
          True  -> e1
          False -> e2

Which is then simplified by case-of-known-constructor:

case (a ># b) of
  1# -> e1
  _  -> e2

While we get good Core here, the code generator will generate very bad Cmm
if e1 or e2 do allocation. It will push heap checks into case alternatives
which results in about 2.5% increase in code size. Until this is improved we
just make isTrue# an alias to tagToEnum#. This is a temporary solution (if
you're reading this in 2023 then things went wrong). See #8326.


Note [Runtime representation of modules and tycons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We generate a binding for M.$modName and M.$tcT for every module M and
data type T.  Things to think about

  - We want them to be economical on space; ideally pure data with no thunks.

  - We do this for every module (except this module GHC.Types), so we can't
    depend on anything else (eg string unpacking code)

That's why we have these terribly low-level representations.  The TrName
type lets us use the TrNameS constructor when allocating static data;
but we also need TrNameD for the case where we are deserialising a TyCon
or Module (for example when deserialising a TypeRep), in which case we
can't conveniently come up with an Addr#.

