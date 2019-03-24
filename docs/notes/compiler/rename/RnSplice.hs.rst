`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnSplice.hs>`_

Note [Free variables of typed splices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider renaming this:
        f = ...
        h = ...$(thing "f")...

where the splice is a *typed* splice.  The splice can expand into
literally anything, so when we do dependency analysis we must assume
that it might mention 'f'.  So we simply treat all locally-defined
names as mentioned by any splice.  This is terribly brutal, but I
don't see what else to do.  For example, it'll mean that every
locally-defined thing will appear to be used, so no unused-binding
warnings.  But if we miss the dependency, then we might typecheck 'h'
before 'f', and that will crash the type checker because 'f' isn't in
scope.

Currently, I'm not treating a splice as also mentioning every import,
which is a bit inconsistent -- but there are a lot of them.  We might
thereby get some bogus unused-import warnings, but we won't crash the
type checker.  Not very satisfactory really.



Note [Renamer errors]
~~~~~~~~~~~~~~~~~~~~~
It's important to wrap renamer calls in checkNoErrs, because the
renamer does not fail for out of scope variables etc. Instead it
returns a bogus term/type, so that it can report more than one error.
We don't want the type checker to see these bogus unbound variables.


Note [Running splices in the Renamer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Splices used to be run in the typechecker, which led to (#4364). Since the
renamer must decide which expressions depend on which others, and it cannot
reliably do this for arbitrary splices, we used to conservatively say that
splices depend on all other expressions in scope. Unfortunately, this led to
the problem of cyclic type declarations seen in (#4364). Instead, by
running splices in the renamer, we side-step the problem of determining
dependencies: by the time the dependency analysis happens, any splices have
already been run, and expression dependencies can be determined as usual.

However, see (#9813), for an example where we would like to run splices
*after* performing dependency analysis (that is, after renaming). It would be
desirable to typecheck "non-splicy" expressions (those expressions that do not
contain splices directly or via dependence on an expression that does) before
"splicy" expressions, such that types/expressions within the same declaration
group would be available to `reify` calls, for example consider the following:

> module M where
>   data D = C
>   f = 1
>   g = $(mapM reify ['f, 'D, ''C] ...)

Compilation of this example fails since D/C/f are not in the type environment
and thus cannot be reified as they have not been typechecked by the time the
splice is renamed and thus run.

These requirements are at odds: we do not want to run splices in the renamer as
we wish to first determine dependencies and typecheck certain expressions,
making them available to reify, but cannot accurately determine dependencies
without running splices in the renamer!

Indeed, the conclusion of (#9813) was that it is not worth the complexity
to try and
 a) implement and maintain the code for renaming/typechecking non-splicy
    expressions before splicy expressions,
 b) explain to TH users which expressions are/not available to reify at any
    given point.



Note [Delaying modFinalizers in untyped splices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When splices run in the renamer, 'reify' does not have access to the local
type environment (#11832, [1]).

For instance, in

> let x = e in $(reify (mkName "x") >>= runIO . print >> [| return () |])

'reify' cannot find @x@, because the local type environment is not yet
populated. To address this, we allow 'reify' execution to be deferred with
'addModFinalizer'.

> let x = e in $(do addModFinalizer (reify (mkName "x") >>= runIO . print)
                    [| return () |]
                )

The finalizer is run with the local type environment when type checking is
complete.

Since the local type environment is not available in the renamer, we annotate
the tree at the splice point [2] with @HsSpliceE (HsSpliced finalizers e)@ where
@e@ is the result of splicing and @finalizers@ are the finalizers that have been
collected during evaluation of the splice [3]. In our example,

> HsLet
>   (x = e)
>   (HsSpliceE $ HsSpliced [reify (mkName "x") >>= runIO . print]
>                          (HsSplicedExpr $ return ())
>   )

When the typechecker finds the annotation, it inserts the finalizers in the
global environment and exposes the current local environment to them [4, 5, 6].

> addModFinalizersWithLclEnv [reify (mkName "x") >>= runIO . print]

References:

[1] https://ghc.haskell.org/trac/ghc/wiki/TemplateHaskell/Reify
[2] 'rnSpliceExpr'
[3] 'TcSplice.qAddModFinalizer'
[4] 'TcExpr.tcExpr' ('HsSpliceE' ('HsSpliced' ...))
[5] 'TcHsType.tc_hs_type' ('HsSpliceTy' ('HsSpliced' ...))
[6] 'TcPat.tc_pat' ('SplicePat' ('HsSpliced' ...))

--------------------


Note [Partial Type Splices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Partial Type Signatures are partially supported in TH type splices: only
anonymous wild cards are allowed.

.. code-block:: haskell

  -- ToDo: SLPJ says: I don't understand all this

Normally, named wild cards are collected before renaming a (partial) type
signature. However, TH type splices are run during renaming, i.e. after the
initial traversal, leading to out of scope errors for named wild cards. We
can't just extend the initial traversal to collect the named wild cards in TH
type splices, as we'd need to expand them, which is supposed to happen only
once, during renaming.

Similarly, the extra-constraints wild card is handled right before renaming
too, and is therefore also not supported in a TH type splice. Another reason
to forbid extra-constraints wild cards in TH type splices is that a single
signature can contain many TH type splices, whereas it mustn't contain more
than one extra-constraints wild card. Enforcing would this be hard the way
things are currently organised.

Anonymous wild cards pose no problem, because they start out without names and
are given names during renaming. These names are collected right after
renaming. The names generated for anonymous wild cards in TH type splices will
thus be collected as well.

For more details about renaming wild cards, see RnTypes.rnHsSigWcType

Note that partial type signatures are fully supported in TH declaration
splices, e.g.:

.. code-block:: haskell

     [d| foo :: _ => _
         foo x y = x == y |]

This is because in this case, the partial type signature can be treated as a
whole signature, instead of as an arbitrary type.

--------------------


Note [rnSplicePat]
~~~~~~~~~~~~~~~~~~
Renaming a pattern splice is a bit tricky, because we need the variables
bound in the pattern to be in scope in the RHS of the pattern. This scope
management is effectively done by using continuation-passing style in
RnPat, through the CpsRn monad. We don't wish to be in that monad here
(it would create import cycles and generally conflict with renaming other
splices), so we really want to return a (Pat RdrName) -- the result of
running the splice -- which can then be further renamed in RnPat, in
the CpsRn monad.

The problem is that if we're renaming a splice within a bracket, we
*don't* want to run the splice now. We really do just want to rename
it to an HsSplice Name. Of course, then we can't know what variables
are bound within the splice. So we accept any unbound variables and
rename them again when the bracket is spliced in.  If a variable is brought
into scope by a pattern splice all is fine.  If it is not then an error is
reported.

In any case, when we're done in rnSplicePat, we'll either have a
Pat RdrName (the result of running a top-level splice) or a Pat Name
(the renamed nested splice). Thus, the awkward return type of
rnSplicePat.


Note [Keeping things alive for Template Haskell]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  f x = x+1
  g y = [| f 3 |]

Here 'f' is referred to from inside the bracket, which turns into data
and mentions only f's *name*, not 'f' itself. So we need some other
way to keep 'f' alive, lest it get dropped as dead code.  That's what
keepAlive does. It puts it in the keep-alive set, which subsequently
ensures that 'f' stays as a top level binding.

This must be done by the renamer, not the type checker (as of old),
because the type checker doesn't typecheck the body of untyped
brackets (#8540).

A thing can have a bind_lvl of outerLevel, but have an internal name:
   foo = [d| op = 3
             bop = op + 1 |]
Here the bind_lvl of 'op' is (bogusly) outerLevel, even though it is
bound inside a bracket.  That is because we don't even even record
binding levels for top-level things; the binding levels are in the
LocalRdrEnv.

So the occurrence of 'op' in the rhs of 'bop' looks a bit like a
cross-stage thing, but it isn't really.  And in fact we never need
to do anything here for top-level bound things, so all is fine, if
a bit hacky.

For these chaps (which have Internal Names) we don't want to put
them in the keep-alive set.



Note [Quoting names]
~~~~~~~~~~~~~~~~~~~~
A quoted name 'n is a bit like a quoted expression [| n |], except that we
have no cross-stage lifting (c.f. TcExpr.thBrackId).  So, after incrementing
the use-level to account for the brackets, the cases are:

.. code-block:: haskell

        bind > use                      Error
        bind = use+1                    OK
        bind < use
                Imported things         OK
                Top-level things        OK
                Non-top-level           Error

where 'use' is the binding level of the 'n quote. (So inside the implied
bracket the level would be use+1.)

Examples:

.. code-block:: haskell

  f 'map        -- OK; also for top-level defns of this module

.. code-block:: haskell

  \x. f 'x      -- Not ok (bind = 1, use = 1)
                -- (whereas \x. f [| x |] might have been ok, by
                --                               cross-stage lifting

.. code-block:: haskell

  \y. [| \x. $(f 'y) |] -- Not ok (bind =1, use = 1)

.. code-block:: haskell

  [| \x. $(f 'x) |]     -- OK (bind = 2, use = 1)

