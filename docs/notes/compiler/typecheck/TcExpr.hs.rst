`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs>`_

compiler/typecheck/TcExpr.hs
============================


Note [Type-checking overloaded labels]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L278>`__

Recall that we have

::

  module GHC.OverloadedLabels where
    class IsLabel (x :: Symbol) a where
      fromLabel :: a

..

We translate `#foo` to `fromLabel @"foo"`, where we use

 * the in-scope `fromLabel` if `RebindableSyntax` is enabled; or if not
 * `GHC.OverloadedLabels.fromLabel`.

In the `RebindableSyntax` case, the renamer will have filled in the
first field of `HsOverLabel` with the `fromLabel` function to use, and
we simply apply it to the appropriate visible type argument.

In the `OverloadedLabels` case, when we see an overloaded label like
`#foo`, we generate a fresh variable `alpha` for the type and emit an
`IsLabel "foo" alpha` constraint.  Because the `IsLabel` class has a
single method, it is represented by a newtype, so we can coerce
`IsLabel "foo" alpha` to `alpha` (just like for implicit parameters).



Note [Left sections]
~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L311>`__

Left sections, like (4 *), are equivalent to
        \ x -> (*) 4 x,
or, if PostfixOperators is enabled, just
        (*) 4
With PostfixOperators we don't actually require the function to take
two arguments at all.  For example, (x `not`) means (not x); you get
postfix operators!  Not Haskell 98, but it's less work and kind of
useful.



Note [Typing rule for ($)]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L322>`__

People write
   runST $ blah
so much, where
   runST :: (forall s. ST s a) -> a
that I have finally given in and written a special type-checking
rule just for saturated applications of ($).
  * Infer the type of the first argument
  * Decompose it; should be of form (arg2_ty -> res_ty),
       where arg2_ty might be a polytype
  * Use arg2_ty to typecheck arg2



Note [Typing rule for seq]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L335>`__

We want to allow
       x `seq` (# p,q #)
which suggests this type for seq:
   seq :: forall (a:*) (b:Open). a -> b -> b,
with (b:Open) meaning that be can be instantiated with an unboxed
tuple.  The trouble is that this might accept a partially-applied
'seq', and I'm just not certain that would work.  I'm only sure it's
only going to work when it's fully applied, so it turns into
    case x of _ -> (# p,q #)

So it seems more uniform to treat 'seq' as if it was a language
construct.

See also Note [seqId magic] in MkId



Note [Type of a record update]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L687>`__

The main complication with RecordUpd is that we need to explicitly
handle the *non-updated* fields.  Consider:

::

        data T a b c = MkT1 { fa :: a, fb :: (b,c) }
                     | MkT2 { fa :: a, fb :: (b,c), fc :: c -> c }
                     | MkT3 { fd :: a }

..

::

        upd :: T a b c -> (b',c) -> T a b' c
        upd t x = t { fb = x}

..

The result type should be (T a b' c)
not (T a b c),   because 'b' *is not* mentioned in a non-updated field
not (T a b' c'), because 'c' *is*     mentioned in a non-updated field
NB that it's not good enough to look at just one constructor; we must
look at them all; cf #3219

After all, upd should be equivalent to:
        upd t x = case t of
                        MkT1 p q -> MkT1 p x
                        MkT2 a b -> MkT2 p b
                        MkT3 d   -> error ...

So we need to give a completely fresh type to the result record,
and then constrain it by the fields that are *not* updated ("p" above).
We call these the "fixed" type variables, and compute them in getFixedTyVars.

Note that because MkT3 doesn't contain all the fields being updated,
its RHS is simply an error, so it doesn't impose any type constraints.
Hence the use of 'relevant_cont'.



Note [Implicit type sharing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L719>`__

We also take into account any "implicit" non-update fields.  For example
        data T a b where { MkT { f::a } :: T a a; ... }
So the "real" type of MkT is: forall ab. (a~b) => a -> T a b

Then consider
        upd t x = t { f=x }
We infer the type
        upd :: T a b -> a -> T a b
        upd (t::T a b) (x::a)
           = case t of { MkT (co:a~b) (_:a) -> MkT co x }
We can't give it the more general type
        upd :: T a b -> c -> T c b



Note [Criteria for update]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L734>`__

We want to allow update for existentials etc, provided the updated
field isn't part of the existential. For example, this should be ok.
  data T a where { MkT { f1::a, f2::b->b } :: T a }
  f :: T a -> b -> T b
  f t b = t { f1=b }

The criterion we use is this:

::

  The types of the updated fields
  mention only the universally-quantified type variables
  of the data constructor

..

NB: this is not (quite) the same as being a "naughty" record selector
(See Note [Naughty record selectors]) in TcTyClsDecls), at least
in the case of GADTs. Consider
   data T a where { MkT :: { f :: a } :: T [a] }
Then f is not "naughty" because it has a well-typed record selector.
But we don't allow updates for 'f'.  (One could consider trying to
allow this, but it makes my head hurt.  Badly.  And no one has asked
for it.)

In principle one could go further, and allow
  g :: T a -> T a
  g t = t { f2 = \x -> x }
because the expression is polymorphic...but that seems a bridge too far.



Note [Data family example]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L762>`__

    data instance T (a,b) = MkT { x::a, y::b }
  --->
    data :TP a b = MkT { a::a, y::b }
    coTP a b :: T (a,b) ~ :TP a b

Suppose r :: T (t1,t2), e :: t3
Then  r { x=e } :: T (t3,t1)
  --->
      case r |> co1 of
        MkT x y -> MkT e y |> co2
      where co1 :: T (t1,t2) ~ :TP t1 t2
            co2 :: :TP t3 t2 ~ T (t3,t2)
The wrapping with co2 is done by the constructor wrapper for MkT

Outgoing invariants
~~~~~~~~~~~~~~~~~~~
In the outgoing (HsRecordUpd scrut binds cons in_inst_tys out_inst_tys):

  * cons are the data constructors to be updated

  * in_inst_tys, out_inst_tys have same length, and instantiate the
        *representation* tycon of the data cons.  In Note [Data
        family example], in_inst_tys = [t1,t2], out_inst_tys = [t3,t2]



Note [Mixed Record Field Updates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L788>`__

Consider the following pattern synonym.

::

  data MyRec = MyRec { foo :: Int, qux :: String }

..

::

  pattern HisRec{f1, f2} = MyRec{foo = f1, qux=f2}

..

This allows updates such as the following

::

  updater :: MyRec -> MyRec
  updater a = a {f1 = 1 }

..

It would also make sense to allow the following update (which we reject).

::

  updater a = a {f1 = 1, qux = "two" } ==? MyRec 1 "two"

..

This leads to confusing behaviour when the selectors in fact refer the same
field.

::

  updater a = a {f1 = 1, foo = 2} ==? ???

..

For this reason, we reject a mixture of pattern synonym and normal record
selectors in the same update block. Although of course we still allow the
following.

::

  updater a = (a {f1 = 1}) {foo = 2}

..

::

  > updater (MyRec 0 "str")
  MyRec 2 "str"

..



Note [Visible type application for the empty list constructor]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L1242>`__

Getting the expression [] @Int to typecheck is slightly tricky since [] isn't
an ordinary data constructor. By default, when tcExpr typechecks a list
expression, it wraps the expression in a coercion, which gives it a type to the
effect of p[a]. It isn't until later zonking that the type becomes
forall a. [a], but that's too late for visible type application.

The workaround is to check for empty list expressions that have a visible type
argument in tcApp, and if so, directly typecheck [] @ty data constructor name.
This avoids the intermediate coercion and produces an expression of type [ty],
as one would intuitively expect.

Unfortunately, this workaround isn't terribly robust, since more involved
expressions such as (let in []) @Int won't work. Until a more elegant fix comes
along, however, this at least allows direct type application on [] to work,
which is better than before.



Note [Required quantifiers in the type of a term]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L1363>`__

Consider (#15859)

::

  data A k :: k -> Type      -- A      :: forall k -> k -> Type
  type KindOf (a :: k) = k   -- KindOf :: forall k. k -> Type
  a = (undefind :: KindOf A) @Int

..

With ImpredicativeTypes (thin ice, I know), we instantiate
KindOf at type (forall k -> k -> Type), so
  KindOf A = forall k -> k -> Type
whose first argument is Required

We want to reject this type application to Int, but in earlier
GHCs we had an ASSERT that Required could not occur here.

The ice is thin; c.f. Note [No Required TyCoBinder in terms]
in TyCoRep.



Note [Visible type application zonk]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L1382>`__

* Substitutions should be kind-preserving, so we need kind(tv) = kind(ty_arg).

* tcHsTypeApp only guarantees that
    - ty_arg is zonked
    - kind(zonk(tv)) = kind(ty_arg)
  (checkExpectedKind zonks as it goes).

So we must zonk inner_ty as well, to guarantee consistency between zonk(tv)
and inner_ty.  Otherwise we can build an ill-kinded type.  An example was
#14158, where we had:
   id :: forall k. forall (cat :: k -> k -> *). forall (a :: k). cat a a
and we had the visible type application
  id @(->)

* We instantiated k := kappa, yielding
    forall (cat :: kappa -> kappa -> *). forall (a :: kappa). cat a a
* Then we called tcHsTypeApp (->) with expected kind (kappa -> kappa -> *).
* That instantiated (->) as ((->) q1 q1), and unified kappa := q1,
  Here q1 :: RuntimeRep
* Now we substitute
     cat  :->  (->) q1 q1 :: TYPE q1 -> TYPE q1 -> *
  but we must first zonk the inner_ty to get
      forall (a :: TYPE q1). cat a a
  so that the result of substitution is well-kinded
  Failing to do so led to #14158.



Note [tcSynArg]
~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L1464>`__

Because of the rich structure of SyntaxOpType, we must do the
contra-/covariant thing when working down arrows, to get the
instantiation vs. skolemisation decisions correct (and, more
obviously, the orientation of the HsWrappers). We thus have
two tcSynArgs.

works on "expected" types, skolemising where necessary
See Note [tcSynArg]



Note [Push result type in]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L1600>`__

Unify with expected result before type-checking the args so that the
info from res_ty percolates to args.  This is when we might detect a
too-few args situation.  (One can think of cases when the opposite
order would give a better error message.)
experimenting with putting this first.

Here's an example where it actually makes a real difference

   class C t a b | t a -> b
   instance C Char a Bool

::

   data P t a = forall b. (C t a b) => MkP b
   data Q t   = MkQ (forall a. P t a)

..

::

   f1, f2 :: Q Char;
   f1 = MkQ (MkP True)
   f2 = MkQ (MkP True :: forall a. P Char a)

..

With the change, f1 will type-check, because the 'Char' info from
the signature is propagated into MkQ's argument. With the check
in the other order, the extra signature in f2 is reqd.



Note [Partial expression signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L1692>`__

Partial type signatures on expressions are easy to get wrong.  But
here is a guiding principile
    e :: ty
should behave like
    let x :: ty
        x = e
    in x

So for partial signatures we apply the MR if no context is given.  So
   e :: IO _          apply the MR
   e :: _ => IO _     do not apply the MR
just like in TcBinds.decideGeneralisationPlan

This makes a difference (#11670):
   peek :: Ptr a -> IO CLong
   peek ptr = peekElemOff undefined 0 :: _
from (peekElemOff undefined 0) we get
          type: IO w
   constraints: Storable w

We must NOT try to generalise over 'w' because the signature specifies
no constraints so we'll complain about not being able to solve
Storable w.  Instead, don't generalise; then _ gets instantiated to
CLong, as it should.



Note [Adding the implicit parameter to 'assert']
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L1858>`__

The typechecker transforms (assert e1 e2) to (assertError e1 e2).
This isn't really the Right Thing because there's no way to "undo"
if you want to see the original source code in the typechecker
output.  We'll have fix this in due course, when we care more about
being able to reconstruct the exact original program.



Note [tagToEnum#]
~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L1866>`__

Nasty check to ensure that tagToEnum# is applied to a type that is an
enumeration TyCon.  Unification may refine the type later, but this
check won't see that, alas.  It's crude, because it relies on our
knowing *now* that the type is ok, which in turn relies on the
eager-unification part of the type checker pushing enough information
here.  In theory the Right Thing to do is to have a new form of
constraint but I definitely cannot face that!  And it works ok as-is.

Here's are two cases that should fail
        f :: forall a. a
        f = tagToEnum# 0        -- Can't do tagToEnum# at a type variable

::

        g :: Int
        g = tagToEnum# 0        -- Int is not an enumeration

..

When data type families are involved it's a bit more complicated.
     data family F a
     data instance F [Int] = A | B | C
Then we want to generate something like
     tagToEnum# R:FListInt 3# |> co :: R:FListInt ~ F [Int]
Usually that coercion is hidden inside the wrappers for
constructors of F [Int] but here we have to do it explicitly.

It's all grotesquely complicated.



Note [Instantiating stupid theta]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L1893>`__

Normally, when we infer the type of an Id, we don't instantiate,
because we wish to allow for visible type application later on.
But if a datacon has a stupid theta, we're a bit stuck. We need
to emit the stupid theta constraints with instantiated types. It's
difficult to defer this to the lazy instantiation, because a stupid
theta has no spot to put it in a type. So we just instantiate eagerly
in this case. Thus, users cannot use visible type application with
a data constructor sporting a stupid theta. I won't feel so bad for
the users that complain.



Note [Lifting strings]
~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L2063>`__

If we see $(... [| s |] ...) where s::String, we don't want to
generate a mass of Cons (CharL 'x') (Cons (CharL 'y') ...)) etc.
So this conditional short-circuits the lifting mechanism to generate
(liftString "xy") in that case.  I didn't want to use overlapping instances
for the Lift class in TH.Syntax, because that can lead to overlapping-instance
errors in a polymorphic situation.

If this check fails (which isn't impossible) we get another chance; see
Note [Converting strings] in Convert.hs

Local record selectors
~~~~~~~~~~~~~~~~~~~~~~
Record selectors for TyCons in this module are ordinary local bindings,
which show up as ATcIds rather than AGlobals.  So we need to check for
naughtiness in both branches.  c.f. TcTyClsBindings.mkAuxBinds.



Note [Disambiguating record fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L2114>`__

When the -XDuplicateRecordFields extension is used, and the renamer
encounters a record selector or update that it cannot immediately
disambiguate (because it involves fields that belong to multiple
datatypes), it will defer resolution of the ambiguity to the
typechecker.  In this case, the `Ambiguous` constructor of
`AmbiguousFieldOcc` is used.

Consider the following definitions:

::

        data S = MkS { foo :: Int }
        data T = MkT { foo :: Int, bar :: Int }
        data U = MkU { bar :: Int, baz :: Int }

..

When the renamer sees `foo` as a selector or an update, it will not
know which parent datatype is in use.

For selectors, there are two possible ways to disambiguate:

1. Check if the pushed-in type is a function whose domain is a
   datatype, for example:

::

       f s = (foo :: S -> Int) s

..

::

       g :: T -> Int
       g = foo

..

::

    This is checked by `tcCheckRecSelId` when checking `HsRecFld foo`.

..

2. Check if the selector is applied to an argument that has a type
   signature, for example:

::

       h = foo (s :: S)

..

::

    This is checked by `tcApp`.

..


Updates are slightly more complex.  The `disambiguateRecordBinds`
function tries to determine the parent datatype in three ways:

1. Check for types that have all the fields being updated. For example:

::

        f x = x { foo = 3, bar = 2 }

..

::

   Here `f` must be updating `T` because neither `S` nor `U` have
   both fields. This may also discover that no possible type exists.
   For example the following will be rejected:

..

::

        f' x = x { foo = 3, baz = 3 }

..

2. Use the type being pushed in, if it is already a TyConApp. The
   following are valid updates to `T`:

::

        g :: T -> T
        g x = x { foo = 3 }

..

::

        g' x = x { foo = 3 } :: T

..

3. Use the type signature of the record expression, if it exists and
   is a TyConApp. Thus this is valid update to `T`:

::

        h x = (x :: T) { foo = 3 }

..


Note that we do not look up the types of variables being updated, and
no constraint-solving is performed, so for example the following will
be rejected as ambiguous:

::

     let bad (s :: S) = foo s

..

::

     let r :: T
         r = blah
     in r { foo = 3 }

..

::

     \r. (r { foo = 3 },  r :: T )

..

We could add further tests, of a more heuristic nature. For example,
rather than looking for an explicit signature, we could try to infer
the type of the argument to a selector or the record expression being
updated, in case we are lucky enough to get a TyConApp straight
away. However, it might be hard for programmers to predict whether a
particular update is sufficiently obvious for the signature to be
omitted. Moreover, this might change the behaviour of typechecker in
non-obvious ways.

See also Note [HsRecField and HsRecUpdField] in HsPat.

Given a RdrName that refers to multiple record fields, and the type
of its argument, try to determine the name of the selector that is
meant.



Note [Splitting nested sigma types in mismatched function types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L2578>`__

When one applies a function to too few arguments, GHC tries to determine this
fact if possible so that it may give a helpful error message. It accomplishes
this by checking if the type of the applied function has more argument types
than supplied arguments.

Previously, GHC computed the number of argument types through tcSplitSigmaTy.
This is incorrect in the face of nested foralls, however! This caused Trac
#13311, for instance:

::

  f :: forall a. (Monoid a) => forall b. (Monoid b) => Maybe a -> Maybe b

..

If one uses `f` like so:

::

  do { f; putChar 'a' }

..

Then tcSplitSigmaTy will decompose the type of `f` into:

::

  Tyvars: [a]
  Context: (Monoid a)
  Argument types: []
  Return type: forall b. Monoid b => Maybe a -> Maybe b

..

That is, it will conclude that there are *no* argument types, and since `f`
was given no arguments, it won't print a helpful error message. On the other
hand, tcSplitNestedSigmaTys correctly decomposes `f`'s type down to:

  Tyvars: [a, b]
  Context: (Monoid a, Monoid b)
  Argument types: [Maybe a]
  Return type: Maybe b

So now GHC recognizes that `f` has one more argument type than it was actually
provided.



Note [Finding the conflicting fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L2673>`__

Suppose we have
  data A = A {a0, a1 :: Int}
         | B {b0, b1 :: Int}
and we see a record update
  x { a0 = 3, a1 = 2, b0 = 4, b1 = 5 }
Then we'd like to find the smallest subset of fields that no
constructor has all of.  Here, say, {a0,b0}, or {a0,b1}, etc.
We don't really want to report that no constructor has all of
{a0,a1,b0,b1}, because when there are hundreds of fields it's
hard to see what was really wrong.

We may need more than two fields, though; eg
  data T = A { x,y :: Int, v::Int }
          | B { y,z :: Int, v::Int }
          | C { z,x :: Int, v::Int }
with update
   r { x=e1, y=e2, z=e3 }, we

Finding the smallest subset is hard, so the code here makes
a decent stab, no more.  See #7989.



Note [Not-closed error messages]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L2859>`__

When variables in a static form are not closed, we go through the trouble
of explaining why they aren't.

Thus, the following program

> {-# LANGUAGE StaticPointers #-}
> module M where
>
> f x = static g
>   where
>     g = h
>     h = x

produces the error

::

   'g' is used in a static form but it is not closed because it
   uses 'h' which uses 'x' which is not let-bound.

..

And a program like

> {-# LANGUAGE StaticPointers #-}
> module M where
>
> import Data.Typeable
> import GHC.StaticPtr
>
> f :: Typeable a => a -> StaticPtr TypeRep
> f x = const (static (g undefined)) (h x)
>   where
>     g = h
>     h = typeOf

produces the error

::

   'g' is used in a static form but it is not closed because it
   uses 'h' which has a non-closed type because it contains the
   type variables: 'a'

..



Note [Checking closedness]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcExpr.hs#L2901>`__

@checkClosed@ checks if a binding is closed and returns a reason if it is
not.

The bindings define a graph where the nodes are ids, and there is an edge
from @id1@ to @id2@ if the rhs of @id1@ contains @id2@ among its free
variables.

When @n@ is not closed, it has to exist in the graph some node reachable
from @n@ that it is not a let-bound variable or that it has a non-closed
type. Thus, the "reason" is a path from @n@ to this offending node.

When @n@ is not closed, we traverse the graph reachable from @n@ to build
the reason.

