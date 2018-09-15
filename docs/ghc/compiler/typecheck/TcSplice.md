[[src]](https://github.com/ghc/ghc/tree/master/compiler/typecheck/TcSplice.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


TcSplice: Template Haskell splices


# \subsection{Main interface + stubs for the non-GHCI case


# \subsection{Quoting an expression}


### Note: How top-level splices are handled

Top-level splices (those not inside a [| .. |] quotation bracket) are handled
very straightforwardly:

  1. tcTopSpliceExpr: typecheck the body e of the splice $(e)

  2. runMetaT: desugar, compile, run it, and convert result back to
     HsSyn RdrName (of the appropriate flavour, eg HsType RdrName,
     HsExpr RdrName etc)

  3. treat the result as if that's what you saw in the first place
     e.g for HsType, rename and kind-check
         for HsExpr, rename and type-check

     (The last step is different for decls, because they can *only* be
      top-level: we return the result of step 2.)

### Note: How brackets and nested splices are handled

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

      a) Extends the ds_meta environment with the PendingSplices
         attached to the bracket

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

    Typechecked:  f = [| Just ${s7}(g 3) |]{s7 = g Int 3}
      The [| |] part is a HsBracketOut, containing *renamed*
        (not typechecked) expression
      The "s7" is the "splice point"; the (g Int 3) part
        is a typechecked expression

    Desugared:    f = do { s7 <- g Int 3
                         ; return (ConE "Data.Maybe.Just" s7) }

### Note: Template Haskell state diagram

Here are the ThStages, s, their corresponding level numbers
(the result of (thLevel s)), and their state transitions.
The top level of the program is stage Comp:

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

### Note: Template Haskell levels

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

  Generally
        bind > use      Always error (bound later than used)
                        [| \x -> $(f x) |]

        bind = use      Always OK (bound same stage as used)
                        [| \x -> $(f [| x |]) |]

        bind < use      Inside brackets, it depends
                        Inside splice, OK
                        Inside neither, OK

  For (bind < use) inside brackets, there are three cases:
    - Imported things   OK      f = [| map |]
    - Top-level things  OK      g = [| f |]
    - Non-top-level     Only if there is a liftable instance
                                h = \(x:Int) -> [| x |]

  To track top-level-ness we use the ThBindEnv in TcLclEnv

  For example:
           f = ...
           g1 = $(map ...)         is OK
           g2 = $(f ...)           is not OK; because we havn't compiled f yet



# \subsection{Splicing an expression}


### Note: Collecting modFinalizers in typed splices

### Note: Delaying modFinalizers in untyped splices



# \subsection{Error messages}


# Annotations


# \subsection{Running an expression}


 TH.Q a 

### Note: Exceptions in TH

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

### Note: Concealed TH exceptions

  * So, when running a splice, we catch all exceptions; then for
        - an IOEnvFailure exception, we assume the error is already
                in the error-bag (above)
        - other errors, we add an error to the bag
    and then fail

### Note: Concealed TH exceptions

When displaying the error message contained in an exception originated from TH
code, we need to make sure that the error message itself does not contain an
exception.  For example, when executing the following splice:

    $( error ("foo " ++ error "bar") )

the message for the outer exception is a thunk which will throw the inner
exception when evaluated.

For this reason, we display the message of a TH exception using the
'safeShowException' function, which recursively catches any exception thrown
when showing an error message.


To call runQ in the Tc monad, we need to make TcM an instance of Quasi:


### Note: TH recover with -fexternal-interpreter

Recover is slightly tricky to implement.

The meaning of "recover a b" is
 - Do a
   - If it finished successfully, then keep the messages it generated
   - If it failed, discard any messages it generated, and do b

The messages are managed by GHC in the TcM monad, whereas the
exception-handling is done in the ghc-iserv process, so we have to
coordinate between the two.

On the server:
  - emit a StartRecover message
  - run "a" inside a catch
    - if it finishes, emit EndRecover False
    - if it fails, emit EndRecover True, then run "b"

Back in GHC, when we receive:

  StartRecover
    save the current messages and start with an empty set.
  EndRecover caught_error
    Restore the previous messages,
    and merge in the new messages if caught_error is false.


# Instance Testing


# Reification


### Note: Freshen reified GADT constructors' universal tyvars

Suppose one were to reify this GADT:

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


### Note: Kind annotations on TyConApps

A poly-kinded tycon sometimes needs a kind annotation to be unambiguous.
For example:

   type family F a :: k
   type instance F Int  = (Proxy :: * -> *)
   type instance F Bool = (Proxy :: (* -> *) -> *)

It's hard to figure out where these annotations should appear, so we do this:
Suppose we have a tycon application (T ty1 ... tyn). Assuming that T is not
oversatured (more on this later), we can assume T's declaration is of the form
T (tvb1 :: s1) ... (tvbn :: sn) :: p. If any kind variable that
is free in p is not free in an injective position in tvb1 ... tvbn,
then we put on a kind annotation, since we would not otherwise be able to infer
the kind of the whole tycon application.

The injective positions in a tyvar binder are the injective positions in the
kind of its tyvar, provided the tyvar binder is either:

* Anonymous. For example, in the promoted data constructor '(:):

    '(:) :: forall a. a -> [a] -> [a]

  The second and third tyvar binders (of kinds `a` and `[a]`) are both
  anonymous, so if we had '(:) 'True '[], then the inferred kinds of 'True and
  '[] would contribute to the inferred kind of '(:) 'True '[].
* Has required visibility. For example, in the type family:

    type family Wurble k (a :: k) :: k
    Wurble :: forall k -> k -> k

  The first tyvar binder (of kind `forall k`) has required visibility, so if
  we had Wurble (Maybe a) Nothing, then the inferred kind of Maybe a would
  contribute to the inferred kind of Wurble (Maybe a) Nothing.

An injective position in a type is one that does not occur as an argument to
a non-injective type constructor (e.g., non-injective type families). See
injectiveVarsOfType.

How can be sure that this is correct? That is, how can we be sure that in the
event that we leave off a kind annotation, that one could infer the kind of the
tycon application from its arguments? It's essentially a proof by induction: if
we can infer the kinds of every subtree of a type, then the whole tycon
application will have an inferrable kind--unless, of course, the remainder of
the tycon application's kind has uninstantiated kind variables.

An earlier implementation of this algorithm only checked if p contained any
free variables. But this was unsatisfactory, since a datatype like this:

  data Foo = Foo (Proxy '[False, True])

Would be reified like this:

  data Foo = Foo (Proxy ('(:) False ('(:) True ('[] :: [Bool])
                                     :: [Bool]) :: [Bool]))

Which has a rather excessive amount of kind annotations. With the current
algorithm, we instead reify Foo to this:

  data Foo = Foo (Proxy ('(:) False ('(:) True ('[] :: [Bool]))))

Since in the case of '[], the kind p is [a], and there are no arguments in the
kind of '[]. On the other hand, in the case of '(:) True '[], the kind p is
(forall a. [a]), but a occurs free in the first and second arguments of the
full kind of '(:), which is (forall a. a -> [a] -> [a]). (See Trac #14060.)

What happens if T is oversaturated? That is, if T's kind has fewer than n
arguments, in the case that the concrete application instantiates a result
kind variable with an arrow kind? If we run out of arguments, we do not attach
a kind annotation. This should be a rare case, indeed. Here is an example:

   data T1 :: k1 -> k2 -> *
   data T2 :: k1 -> k2 -> *

   type family G (a :: k) :: k
   type instance G T1 = T2

   type instance F Char = (G T1 Bool :: (* -> *) -> *)   -- F from above

Here G's kind is (forall k. k -> k), and the desugared RHS of that last
instance of F is (G (* -> (* -> *) -> *) (T1 * (* -> *)) Bool). According to
the algorithm above, there are 3 arguments to G so we should peel off 3
arguments in G's kind. But G's kind has only two arguments. This is the
rare special case, and we choose not to annotate the application of G with
a kind signature. After all, we needn't do this, since that instance would
be reified as:

   type instance F Char = G (T1 :: * -> (* -> *) -> *) Bool

So the kind of G isn't ambiguous anymore due to the explicit kind annotation
on its argument. See #8953 and test th/T8953.


### Note: Reifying field labels

When reifying a datatype declared with DuplicateRecordFields enabled, we want
the reified names of the fields to be labels rather than selector functions.
That is, we want (reify ''T) and (reify 'foo) to produce

    data T = MkT { foo :: Int }
    foo :: T -> Int

rather than

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
