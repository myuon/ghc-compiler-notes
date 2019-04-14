`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/hsSyn/Convert.hs>`_

compiler/hsSyn/Convert.hs
=========================


Note [Dropping constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/hsSyn/Convert.hs#L994>`__

When we drop constructors from the input (for instance, when we encounter @TupE [e]@)
we must insert parentheses around the argument. Otherwise, @UInfix@ constructors in @e@
could meet @UInfix@ constructors containing the @TupE [e]@. For example:

::

  UInfixE x * (TupE [UInfixE y + z])

..

If we drop the singleton tuple but don't insert parentheses, the @UInfixE@s would meet
and the above expression would be reassociated to

::

  OpApp (OpApp x * y) + z

..

which we don't want.



Note [Converting UInfix]
~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/hsSyn/Convert.hs#L1032>`__

When converting @UInfixE@, @UInfixP@, and @UInfixT@ values, we want to readjust
the trees to reflect the fixities of the underlying operators:

::

  UInfixE x * (UInfixE y + z) ---> (x * y) + z

..

This is done by the renamer (see @mkOppAppRn@, @mkConOppPatRn@, and
@mkHsOpTyRn@ in RnTypes), which expects that the input will be completely
right-biased for types and left-biased for everything else. So we left-bias the
trees of @UInfixP@ and @UInfixE@ and right-bias the trees of @UInfixT@.

Sample input:

  UInfixE
   (UInfixE x op1 y)
   op2
   (UInfixE z op3 w)

Sample output:

  OpApp
    (OpApp
      (OpApp x op1 y)
      op2
      z)
    op3
    w

The functions @cvtOpApp@, @cvtOpAppP@, and @cvtOpAppT@ are responsible for this
biasing.



Note [Converting strings]
~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/hsSyn/Convert.hs#L1159>`__

If we get (ListE [CharL 'x', CharL 'y']) we'd like to convert to
a string literal for "xy".  Of course, we might hope to get
(LitE (StringL "xy")), but not always, and allCharLs fails quickly
if it isn't a literal string



Note [Binders in Template Haskell]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/hsSyn/Convert.hs#L1885>`__

Consider this TH term construction:
  do { x1 <- TH.newName "x"   -- newName :: String -> Q TH.Name
     ; x2 <- TH.newName "x"   -- Builds a NameU
     ; x3 <- TH.newName "x"

     ; let x = mkName "x"     -- mkName :: String -> TH.Name
                              -- Builds a NameS

::

     ; return (LamE (..pattern [x1,x2]..) $
               LamE (VarPat x3) $
               ..tuple (x1,x2,x3,x)) }

..

It represents the term   \[x1,x2]. \x3. (x1,x2,x3,x)

a) We don't want to complain about "x" being bound twice in
   the pattern [x1,x2]
b) We don't want x3 to shadow the x1,x2
c) We *do* want 'x' (dynamically bound with mkName) to bind
   to the innermost binding of "x", namely x3.
d) When pretty printing, we want to print a unique with x1,x2
   etc, else they'll all print as "x" which isn't very helpful

When we convert all this to HsSyn, the TH.Names are converted with
thRdrName.  To achieve (b) we want the binders to be Exact RdrNames.
Achieving (a) is a bit awkward, because
   - We must check for duplicate and shadowed names on Names,
     not RdrNames, *after* renaming.
     See Note [Collect binders only after renaming] in HsUtils

   - But to achieve (a) we must distinguish between the Exact
     RdrNames arising from TH and the Unqual RdrNames that would
     come from a user writing \[x,x] -> blah

So in Convert.thRdrName we translate
   TH Name                          RdrName
   --------------------------------------------------------
   NameU (arising from newName) --> Exact (Name{ System })
   NameS (arising from mkName)  --> Unqual

Notice that the NameUs generate *System* Names.  Then, when
figuring out shadowing and duplicates, we can filter out
System Names.

This use of System Names fits with other uses of System Names, eg for
temporary variables "a". Since there are lots of things called "a" we
usually want to print the name with the unique, and that is indeed
the way System Names are printed.

There's a small complication of course; see Note [Looking up Exact
RdrNames] in RnEnv.



Note [Pattern synonym type signatures and Template Haskell]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/hsSyn/Convert.hs#L1940>`__

In general, the type signature of a pattern synonym

::

  pattern P x1 x2 .. xn = <some-pattern>

..

is of the form

::

   forall univs. reqs => forall exis. provs => t1 -> t2 -> ... -> tn -> t

..

with the following parts:

   1) the (possibly empty lists of) universally quantified type
      variables `univs` and required constraints `reqs` on them.
   2) the (possibly empty lists of) existentially quantified type
      variables `exis` and the provided constraints `provs` on them.
   3) the types `t1`, `t2`, .., `tn` of the pattern synonym's arguments x1,
      x2, .., xn, respectively
   4) the type `t` of <some-pattern>, mentioning only universals from `univs`.

Due to the two forall quantifiers and constraint contexts (either of
which might be empty), pattern synonym type signatures are treated
specially in `deSugar/DsMeta.hs`, `hsSyn/Convert.hs`, and
`typecheck/TcSplice.hs`:

::

   (a) When desugaring a pattern synonym from HsSyn to TH.Dec in
       `deSugar/DsMeta.hs`, we represent its *full* type signature in TH, i.e.:

..

::

           ForallT univs reqs (ForallT exis provs ty)
              (where ty is the AST representation of t1 -> t2 -> ... -> tn -> t)

..

::

   (b) When converting pattern synonyms from TH.Dec to HsSyn in
       `hsSyn/Convert.hs`, we convert their TH type signatures back to an
       appropriate Haskell pattern synonym type of the form

..

::

         forall univs. reqs => forall exis. provs => t1 -> t2 -> ... -> tn -> t

..

::

       where initial empty `univs` type variables or an empty `reqs`
       constraint context are represented *explicitly* as `() =>`.

..

::

   (c) When reifying a pattern synonym in `typecheck/TcSplice.hs`, we always
       return its *full* type, i.e.:

..

::

           ForallT univs reqs (ForallT exis provs ty)
              (where ty is the AST representation of t1 -> t2 -> ... -> tn -> t)

..

The key point is to always represent a pattern synonym's *full* type
in cases (a) and (c) to make it clear which of the two forall
quantifiers and/or constraint contexts are specified, and which are
not. See GHC's user's guide on pattern synonyms for more information
about pattern synonym type signatures.

