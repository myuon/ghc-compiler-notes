`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcSplice.hs>`_

Note [How top-level splices are handled]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Top-level splices (those not inside a [| .. |] quotation bracket) are handled
very straightforwardly:

  1. tcTopSpliceExpr: typecheck the body e of the splice $(e)

  2. runMetaT: desugar, compile, run it, and convert result back to
     HsSyn RdrName (of the appropriate flavour, eg HsType RdrName,
     HsExpr RdrName etc)

  3. treat the result as if that's what you saw in the first place
     e.g for HsType, rename and kind-check
         for HsExpr, rename and type-check

.. code-block:: haskell

     (The last step is different for decls, because they can *only* be
      top-level: we return the result of step 2.)



Note [How brackets and nested splices are handled]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Nested splices (those inside a [| .. |] quotation bracket),
are treated quite differently.

Remember, there are two forms of bracket
         typed   [|| e ||]
   and untyped   [|  e  |]

The life cycle of a typed bracket:
   * Starts as HsBracket

   * When renaming:
        * Set the ThStage to (Brack s RnPendingTyped)
        * Rename the body
        * Result is still a HsBracket

   * When typechecking:
        * Set the ThStage to (Brack s (TcPending ps_var lie_var))
        * Typecheck the body, and throw away the elaborated result
        * Nested splices (which must be typed) are typechecked, and
          the results accumulated in ps_var; their constraints
          accumulate in lie_var
        * Result is a HsTcBracketOut rn_brack pending_splices
          where rn_brack is the incoming renamed bracket

The life cycle of a un-typed bracket:
   * Starts as HsBracket

   * When renaming:
        * Set the ThStage to (Brack s (RnPendingUntyped ps_var))
        * Rename the body
        * Nested splices (which must be untyped) are renamed, and the
          results accumulated in ps_var
        * Result is still (HsRnBracketOut rn_body pending_splices)

   * When typechecking a HsRnBracketOut
        * Typecheck the pending_splices individually
        * Ignore the body of the bracket; just check that the context
          expects a bracket of that type (e.g. a [p| pat |] bracket should
          be in a context needing a (Q Pat)
        * Result is a HsTcBracketOut rn_brack pending_splices
          where rn_brack is the incoming renamed bracket


In both cases, desugaring happens like this:
  * HsTcBracketOut is desugared by DsMeta.dsBracket.  It

.. code-block:: haskell

      a) Extends the ds_meta environment with the PendingSplices
         attached to the bracket

.. code-block:: haskell

      b) Converts the quoted (HsExpr Name) to a CoreExpr that, when
         run, will produce a suitable TH expression/type/decl.  This
         is why we leave the *renamed* expression attached to the bracket:
         the quoted expression should not be decorated with all the goop
         added by the type checker

  * Each splice carries a unique Name, called a "splice point", thus
    ${n}(e).  The name is initialised to an (Unqual "splice") when the
    splice is created; the renamer gives it a unique.

  * When DsMeta (used to desugar the body of the bracket) comes across
    a splice, it looks up the splice's Name, n, in the ds_meta envt,
    to find an (HsExpr Id) that should be substituted for the splice;
    it just desugars it to get a CoreExpr (DsMeta.repSplice).

Example:
    Source:       f = [| Just $(g 3) |]
      The [| |] part is a HsBracket

.. code-block:: haskell

    Typechecked:  f = [| Just ${s7}(g 3) |]{s7 = g Int 3}
      The [| |] part is a HsBracketOut, containing *renamed*
        (not typechecked) expression
      The "s7" is the "splice point"; the (g Int 3) part
        is a typechecked expression

.. code-block:: haskell

    Desugared:    f = do { s7 <- g Int 3
                         ; return (ConE "Data.Maybe.Just" s7) }




Note [Template Haskell state diagram]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here are the ThStages, s, their corresponding level numbers
(the result of (thLevel s)), and their state transitions.
The top level of the program is stage Comp:

.. code-block:: haskell

     Start here
         |
         V
      -----------     $      ------------   $
      |  Comp   | ---------> |  Splice  | -----|
      |   1     |            |    0     | <----|
      -----------            ------------
        ^     |                ^      |
      $ |     | [||]         $ |      | [||]
        |     v                |      v
   --------------          ----------------
   | Brack Comp |          | Brack Splice |
   |     2      |          |      1       |
   --------------          ----------------

* Normal top-level declarations start in state Comp
       (which has level 1).
  Annotations start in state Splice, since they are
       treated very like a splice (only without a '$')

* Code compiled in state Splice (and only such code)
  will be *run at compile time*, with the result replacing
  the splice

* The original paper used level -1 instead of 0, etc.

* The original paper did not allow a splice within a
  splice, but there is no reason not to. This is the
  $ transition in the top right.



Note [Template Haskell levels]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Imported things are impLevel (= 0)

* However things at level 0 are not *necessarily* imported.
      eg  $( \b -> ... )   here b is bound at level 0

* In GHCi, variables bound by a previous command are treated
  as impLevel, because we have bytecode for them.

* Variables are bound at the "current level"

* The current level starts off at outerLevel (= 1)

* The level is decremented by splicing $(..)
               incremented by brackets [| |]
               incremented by name-quoting 'f

When a variable is used, we compare
        bind:  binding level, and
        use:   current level at usage site

.. code-block:: haskell

  Generally
        bind > use      Always error (bound later than used)
                        [| \x -> $(f x) |]

.. code-block:: haskell

        bind = use      Always OK (bound same stage as used)
                        [| \x -> $(f [| x |]) |]

.. code-block:: haskell

        bind < use      Inside brackets, it depends
                        Inside splice, OK
                        Inside neither, OK

  For (bind < use) inside brackets, there are three cases:
    - Imported things   OK      f = [| map |]
    - Top-level things  OK      g = [| f |]
    - Non-top-level     Only if there is a liftable instance
                                h = \(x:Int) -> [| x |]

.. code-block:: haskell

  To track top-level-ness we use the ThBindEnv in TcLclEnv

.. code-block:: haskell

  For example:
           f = ...
           g1 = $(map ...)         is OK
           g2 = $(f ...)           is not OK; because we havn't compiled f yet



Note [Running typed splices in the zonker]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

See #15471 for the full discussion.

For many years typed splices were run immediately after they were type checked
however, this is too early as it means to zonk some type variables before
they can be unified with type variables in the surrounding context.

For example,

```
module A where

test_foo :: forall a . Q (TExp (a -> a))
test_foo = [|| id ||]

module B where

import A

qux = $$(test_foo)
```

We would expect `qux` to have inferred type `forall a . a -> a` but if
we run the splices too early the unified variables are zonked to `Any`. The
inferred type is the unusable `Any -> Any`.

To run the splice, we must compile `test_foo` all the way to byte code.
But at the moment when the type checker is looking at the splice, test_foo
has type `Q (TExp (alpha -> alpha))` and we
certainly can't compile code involving unification variables!

We could default `alpha` to `Any` but then we infer `qux :: Any -> Any`
which definitely is not what we want.  Moreover, if we had
  qux = [$$(test_foo), (\x -> x +1::Int)]
then `alpha` would have to be `Int`.

Conclusion: we must defer taking decisions about `alpha` until the
typechecker is done; and *then* we can run the splice.  It's fine to do it
later, because we know it'll produce type-correct code.

Deferring running the splice until later, in the zonker, means that the
unification variables propagate upwards from the splice into the surrounding
context and are unified correctly.

This is implemented by storing the arguments we need for running the splice
in a `DelayedSplice`. In the zonker, the arguments are passed to
`TcSplice.runTopSplice` and the expression inserted into the AST as normal.





Note [Exceptions in TH]
~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have something like this
        $( f 4 )
where
        f :: Int -> Q [Dec]
        f n | n>3       = fail "Too many declarations"
            | otherwise = ...

The 'fail' is a user-generated failure, and should be displayed as a
perfectly ordinary compiler error message, not a panic or anything
like that.  Here's how it's processed:

  * 'fail' is the monad fail.  The monad instance for Q in TH.Syntax
    effectively transforms (fail s) to
        qReport True s >> fail
    where 'qReport' comes from the Quasi class and fail from its monad
    superclass.

  * The TcM monad is an instance of Quasi (see TcSplice), and it implements
    (qReport True s) by using addErr to add an error message to the bag of errors.
    The 'fail' in TcM raises an IOEnvFailure exception

 * 'qReport' forces the message to ensure any exception hidden in unevaluated
   thunk doesn't get into the bag of errors. Otherwise the following splice
   will triger panic (#8987):
        $(fail undefined)
   See also Note [Concealed TH exceptions]

  * So, when running a splice, we catch all exceptions; then for
        - an IOEnvFailure exception, we assume the error is already
                in the error-bag (above)
        - other errors, we add an error to the bag
    and then fail



Note [Concealed TH exceptions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When displaying the error message contained in an exception originated from TH
code, we need to make sure that the error message itself does not contain an
exception.  For example, when executing the following splice:

.. code-block:: haskell

    $( error ("foo " ++ error "bar") )

the message for the outer exception is a thunk which will throw the inner
exception when evaluated.

For this reason, we display the message of a TH exception using the
'safeShowException' function, which recursively catches any exception thrown
when showing an error message.


To call runQ in the Tc monad, we need to make TcM an instance of Quasi:


Note [Freshen reified GADT constructors' universal tyvars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose one were to reify this GADT:

.. code-block:: haskell

  data a :~: b where
    Refl :: forall a b. (a ~ b) => a :~: b

We ought to be careful here about the uniques we give to the occurrences of `a`
and `b` in this definition. That is because in the original DataCon, all uses
of `a` and `b` have the same unique, since `a` and `b` are both universally
quantified type variables--that is, they are used in both the (:~:) tycon as
well as in the constructor type signature. But when we turn the DataCon
definition into the reified one, the `a` and `b` in the constructor type
signature becomes differently scoped than the `a` and `b` in `data a :~: b`.

While it wouldn't technically be *wrong* per se to re-use the same uniques for
`a` and `b` across these two different scopes, it's somewhat annoying for end
users of Template Haskell, since they wouldn't be able to rely on the
assumption that all TH names have globally distinct uniques (#13885). For this
reason, we freshen the universally quantified tyvars that go into the reified
GADT constructor type signature to give them distinct uniques from their
counterparts in the tycon.
----------------------------


Note [Reifying field labels]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When reifying a datatype declared with DuplicateRecordFields enabled, we want
the reified names of the fields to be labels rather than selector functions.
That is, we want (reify ''T) and (reify 'foo) to produce

.. code-block:: haskell

    data T = MkT { foo :: Int }
    foo :: T -> Int

rather than

.. code-block:: haskell

    data T = MkT { $sel:foo:MkT :: Int }
    $sel:foo:MkT :: T -> Int

because otherwise TH code that uses the field names as strings will silently do
the wrong thing.  Thus we use the field label (e.g. foo) as the OccName, rather
than the selector (e.g. $sel:foo:MkT).  Since the Orig name M.foo isn't in the
environment, NameG can't be used to represent such fields.  Instead,
reifyFieldLabel uses NameQ.

However, this means that extracting the field name from the output of reify, and
trying to reify it again, may fail with an ambiguity error if there are multiple
such fields defined in the module (see the test case
overloadedrecflds/should_fail/T11103.hs).  The "proper" fix requires changes to
the TH AST to make it able to represent duplicate record fields.

