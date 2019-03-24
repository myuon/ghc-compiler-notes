`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcInstDcls.hs>`_

Note [How instance declarations are translated]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here is how we translate instance declarations into Core

Running example:
        class C a where
           op1, op2 :: Ix b => a -> b -> b
           op2 = <dm-rhs>

        instance C a => C [a]
           {-# INLINE [2] op1 #-}
           op1 = <rhs>
===>
        -- Method selectors
        op1,op2 :: forall a. C a => forall b. Ix b => a -> b -> b
        op1 = ...
        op2 = ...

.. code-block:: haskell

        -- Default methods get the 'self' dictionary as argument
        -- so they can call other methods at the same type
        -- Default methods get the same type as their method selector
        $dmop2 :: forall a. C a => forall b. Ix b => a -> b -> b
        $dmop2 = /\a. \(d:C a). /\b. \(d2: Ix b). <dm-rhs>
               -- NB: type variables 'a' and 'b' are *both* in scope in <dm-rhs>
               -- Note [Tricky type variable scoping]

.. code-block:: haskell

        -- A top-level definition for each instance method
        -- Here op1_i, op2_i are the "instance method Ids"
        -- The INLINE pragma comes from the user pragma
        {-# INLINE [2] op1_i #-}  -- From the instance decl bindings
        op1_i, op2_i :: forall a. C a => forall b. Ix b => [a] -> b -> b
        op1_i = /\a. \(d:C a).
               let this :: C [a]
                   this = df_i a d
                     -- Note [Subtle interaction of recursion and overlap]

.. code-block:: haskell

                   local_op1 :: forall b. Ix b => [a] -> b -> b
                   local_op1 = <rhs>
                     -- Source code; run the type checker on this
                     -- NB: Type variable 'a' (but not 'b') is in scope in <rhs>
                     -- Note [Tricky type variable scoping]

.. code-block:: haskell

               in local_op1 a d

.. code-block:: haskell

        op2_i = /\a \d:C a. $dmop2 [a] (df_i a d)

.. code-block:: haskell

        -- The dictionary function itself
        {-# NOINLINE CONLIKE df_i #-}   -- Never inline dictionary functions
        df_i :: forall a. C a -> C [a]
        df_i = /\a. \d:C a. MkC (op1_i a d) (op2_i a d)
                -- But see Note [Default methods in instances]
                -- We can't apply the type checker to the default-method call

.. code-block:: haskell

        -- Use a RULE to short-circuit applications of the class ops
        {-# RULE "op1@C[a]" forall a, d:C a.
                            op1 [a] (df_i d) = op1_i a d #-}



Note [Instances and loop breakers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* Note that df_i may be mutually recursive with both op1_i and op2_i.
  It's crucial that df_i is not chosen as the loop breaker, even
  though op1_i has a (user-specified) INLINE pragma.

* Instead the idea is to inline df_i into op1_i, which may then select
  methods from the MkC record, and thereby break the recursion with
  df_i, leaving a *self*-recursive op1_i.  (If op1_i doesn't call op at
  the same type, it won't mention df_i, so there won't be recursion in
  the first place.)

* If op1_i is marked INLINE by the user there's a danger that we won't
  inline df_i in it, and that in turn means that (since it'll be a
  loop-breaker because df_i isn't), op1_i will ironically never be
  inlined.  But this is OK: the recursion breaking happens by way of
  a RULE (the magic ClassOp rule above), and RULES work inside InlineRule
  unfoldings. See Note [RULEs enabled in SimplGently] in SimplUtils



Note [ClassOp/DFun selection]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
One thing we see a lot is stuff like
    op2 (df d1 d2)
where 'op2' is a ClassOp and 'df' is DFun.  Now, we could inline *both*
'op2' and 'df' to get
     case (MkD ($cop1 d1 d2) ($cop2 d1 d2) ... of
       MkD _ op2 _ _ _ -> op2
And that will reduce to ($cop2 d1 d2) which is what we wanted.

But it's tricky to make this work in practice, because it requires us to
inline both 'op2' and 'df'.  But neither is keen to inline without having
seen the other's result; and it's very easy to get code bloat (from the
big intermediate) if you inline a bit too much.

Instead we use a cunning trick.
 * We arrange that 'df' and 'op2' NEVER inline.

 * We arrange that 'df' is ALWAYS defined in the sylised form
      df d1 d2 = MkD ($cop1 d1 d2) ($cop2 d1 d2) ...

 * We give 'df' a magical unfolding (DFunUnfolding [$cop1, $cop2, ..])
   that lists its methods.

 * We make CoreUnfold.exprIsConApp_maybe spot a DFunUnfolding and return
   a suitable constructor application -- inlining df "on the fly" as it
   were.

 * ClassOp rules: We give the ClassOp 'op2' a BuiltinRule that
   extracts the right piece iff its argument satisfies
   exprIsConApp_maybe.  This is done in MkId mkDictSelId

 * We make 'df' CONLIKE, so that shared uses still match; eg
      let d = df d1 d2
      in ...(op2 d)...(op1 d)...



Note [Single-method classes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the class has just one method (or, more accurately, just one element
of {superclasses + methods}), then we use a different strategy.

.. code-block:: haskell

   class C a where op :: a -> a
   instance C a => C [a] where op = <blah>

We translate the class decl into a newtype, which just gives a
top-level axiom. The "constructor" MkC expands to a cast, as does the
class-op selector.

.. code-block:: haskell

   axiom Co:C a :: C a ~ (a->a)

.. code-block:: haskell

   op :: forall a. C a -> (a -> a)
   op a d = d |> (Co:C a)

.. code-block:: haskell

   MkC :: forall a. (a->a) -> C a
   MkC = /\a.\op. op |> (sym Co:C a)

The clever RULE stuff doesn't work now, because ($df a d) isn't
a constructor application, so exprIsConApp_maybe won't return
Just <blah>.

Instead, we simply rely on the fact that casts are cheap:

.. code-block:: haskell

   $df :: forall a. C a => C [a]
   {-# INLINE df #-}  -- NB: INLINE this
   $df = /\a. \d. MkC [a] ($cop_list a d)
       = $cop_list |> forall a. C a -> (sym (Co:C [a]))

.. code-block:: haskell

   $cop_list :: forall a. C a => [a] -> [a]
   $cop_list = <blah>

So if we see
   (op ($df a d))
we'll inline 'op' and '$df', since both are simply casts, and
good things happen.

Why do we use this different strategy?  Because otherwise we
end up with non-inlined dictionaries that look like
    $df = $cop |> blah
which adds an extra indirection to every use, which seems stupid.  See
#4138 for an example (although the regression reported there
wasn't due to the indirection).

There is an awkward wrinkle though: we want to be very
careful when we have
    instance C a => C [a] where
      {-# INLINE op #-}
      op = ...
then we'll get an INLINE pragma on $cop_list but it's important that
$cop_list only inlines when it's applied to *two* arguments (the
dictionary and the list argument).  So we must not eta-expand $df
above.  We ensure that this doesn't happen by putting an INLINE
pragma on the dfun itself; after all, it ends up being just a cast.

There is one more dark corner to the INLINE story, even more deeply
buried.  Consider this (#3772):

.. code-block:: haskell

    class DeepSeq a => C a where
      gen :: Int -> a

.. code-block:: haskell

    instance C a => C [a] where
      gen n = ...

.. code-block:: haskell

    class DeepSeq a where
      deepSeq :: a -> b -> b

.. code-block:: haskell

    instance DeepSeq a => DeepSeq [a] where
      {-# INLINE deepSeq #-}
      deepSeq xs b = foldr deepSeq b xs

That gives rise to these defns:

.. code-block:: haskell

    $cdeepSeq :: DeepSeq a -> [a] -> b -> b
    -- User INLINE( 3 args )!
    $cdeepSeq a (d:DS a) b (x:[a]) (y:b) = ...

.. code-block:: haskell

    $fDeepSeq[] :: DeepSeq a -> DeepSeq [a]
    -- DFun (with auto INLINE pragma)
    $fDeepSeq[] a d = $cdeepSeq a d |> blah

.. code-block:: haskell

    $cp1 a d :: C a => DeepSep [a]
    -- We don't want to eta-expand this, lest
    -- $cdeepSeq gets inlined in it!
    $cp1 a d = $fDeepSep[] a (scsel a d)

.. code-block:: haskell

    $fC[] :: C a => C [a]
    -- Ordinary DFun
    $fC[] a d = MkC ($cp1 a d) ($cgen a d)

Here $cp1 is the code that generates the superclass for C [a].  The
issue is this: we must not eta-expand $cp1 either, or else $fDeepSeq[]
and then $cdeepSeq will inline there, which is definitely wrong.  Like
on the dfun, we solve this by adding an INLINE pragma to $cp1.



Note [Subtle interaction of recursion and overlap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this
  class C a where { op1,op2 :: a -> a }
  instance C a => C [a] where
    op1 x = op2 x ++ op2 x
    op2 x = ...
  instance C [Int] where
    ...

When type-checking the C [a] instance, we need a C [a] dictionary (for
the call of op2).  If we look up in the instance environment, we find
an overlap.  And in *general* the right thing is to complain (see Note
[Overlapping instances] in InstEnv).  But in *this* case it's wrong to
complain, because we just want to delegate to the op2 of this same
instance.

Why is this justified?  Because we generate a (C [a]) constraint in
a context in which 'a' cannot be instantiated to anything that matches
other overlapping instances, or else we would not be executing this
version of op1 in the first place.

It might even be a bit disguised:

.. code-block:: haskell

  nullFail :: C [a] => [a] -> [a]
  nullFail x = op2 x ++ op2 x

.. code-block:: haskell

  instance C a => C [a] where
    op1 x = nullFail x

Precisely this is used in package 'regex-base', module Context.hs.
See the overlapping instances for RegexContext, and the fact that they
call 'nullFail' just like the example above.  The DoCon package also
does the same thing; it shows up in module Fraction.hs.

Conclusion: when typechecking the methods in a C [a] instance, we want to
treat the 'a' as an *existential* type variable, in the sense described
by Note [Binding when looking up instances].  That is why isOverlappableTyVar
responds True to an InstSkol, which is the kind of skolem we use in
tcInstDecl2.




Note [Tricky type variable scoping]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In our example
        class C a where
           op1, op2 :: Ix b => a -> b -> b
           op2 = <dm-rhs>

.. code-block:: haskell

        instance C a => C [a]
           {-# INLINE [2] op1 #-}
           op1 = <rhs>

note that 'a' and 'b' are *both* in scope in <dm-rhs>, but only 'a' is
in scope in <rhs>.  In particular, we must make sure that 'b' is in
scope when typechecking <dm-rhs>.  This is achieved by subFunTys,
which brings appropriate tyvars into scope. This happens for both
<dm-rhs> and for <rhs>, but that doesn't matter: the *renamer* will have
complained if 'b' is mentioned in <rhs>.





Note [Deriving inside TH brackets]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given a declaration bracket
  [d| data T = A | B deriving( Show ) |]

there is really no point in generating the derived code for deriving(
Show) and then type-checking it. This will happen at the call site
anyway, and the type check should never fail!  Moreover (#6005)
the scoping of the generated code inside the bracket does not seem to
work out.

The easy solution is simply not to generate the derived instances at
all.  (A less brutal solution would be to generate them with no
bindings.)  This will become moot when we shift to the new TH plan, so
the brutal solution will do.


Note [Associated type instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We allow this:
  class C a where
    type T x a
  instance C Int where
    type T (S y) Int = y
    type T Z     Int = Char

Note that
  a) The variable 'x' is not bound by the class decl
  b) 'x' is instantiated to a non-type-variable in the instance
  c) There are several type instance decls for T in the instance

All this is fine.  Of course, you can't give any *more* instances
for (T ty Int) elsewhere, because it's an *associated* type.




Note [Result kind signature for a data family instance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The expected type might have a forall at the type. Normally, we
can't skolemise in kinds because we don't have type-level lambda.
But here, we're at the top-level of an instance declaration, so
we actually have a place to put the regeneralised variables.
Thus: skolemise away. cf. Inst.deeplySkolemise and TcUnify.tcSkolemise
Examples in indexed-types/should_compile/T12369



Note [Eta-reduction for data families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   data D :: * -> * -> * -> * -> *

.. code-block:: haskell

   data instance D [(a,b)] p q :: * -> * where
      D1 :: blah1
      D2 :: blah2

Then we'll generate a representation data type
  data Drep a b p q z where
      D1 :: blah1
      D2 :: blah2

and an axiom to connect them
  axiom AxDrep forall a b p q z. D [(a,b]] p q z = Drep a b p q z

except that we'll eta-reduce the axiom to
  axiom AxDrep forall a b. D [(a,b]] = Drep a b
There are several fiddly subtleties lurking here

* The representation tycon Drep is parameerised over the free
  variables of the pattern, in no particular order. So there is no
  guarantee that 'p' and 'q' will come last in Drep's parameters, and
  in the right order.  So, if the /patterns/ of the family insatance
  are eta-redcible, we re-order Drep's parameters to put the
  eta-reduced type variables last.

* Although we eta-reduce the axiom, we eta-/expand/ the representation
  tycon Drep.  The kind of D says it takses four arguments, but the
  data instance header only supplies three.  But the AlgTyCOn for Drep
  itself must have enough TyConBinders so that its result kind is Type.
  So, with etaExpandAlgTyCon we make up some extra TyConBinders

* The result kind in the instance might be a polykind, like this:
     data family DP a :: forall k. k -> *
     data instance DP [b] :: forall k1 k2. (k1,k2) -> *

.. code-block:: haskell

  So in type-checking the LHS (DP Int) we need to check that it is
  more polymorphic than the signature.  To do that we must skolemise
  the siganture and istantiate the call of DP.  So we end up with
     data instance DP [b] @(k1,k2) (z :: (k1,k2)) where

.. code-block:: haskell

  Note that we must parameterise the representation tycon DPrep over
  'k1' and 'k2', as well as 'b'.

.. code-block:: haskell

  The skolemise bit is done in tc_kind_sig, while the instantiate bit
  is done by tcFamTyPats.

* Very fiddly point.  When we eta-reduce to
     axiom AxDrep forall a b. D [(a,b]] = Drep a b

.. code-block:: haskell

  we want the kind of (D [(a,b)]) to be the same as the kind of
  (Drep a b).  This ensures that applying the axiom doesn't change the
  kind.  Why is that hard?  Because the kind of (Drep a b) depends on
  the TyConBndrVis on Drep's arguments. In particular do we have
    (forall (k::*). blah) or (* -> blah)?

.. code-block:: haskell

  We must match whatever D does!  In #15817 we had
      data family X a :: forall k. * -> *   -- Note: a forall that is not used
      data instance X Int b = MkX

.. code-block:: haskell

  So the data instance is really
      data istance X Int @k b = MkX

.. code-block:: haskell

  The axiom will look like
      axiom    X Int = Xrep

.. code-block:: haskell

  and it's important that XRep :: forall k * -> *, following X.

.. code-block:: haskell

  To achieve this we get the TyConBndrVis flags from tcbVisibilities,
  and use those flags for any eta-reduced arguments.  Sigh.

* The final turn of the knife is that tcbVisibilities is itself
  tricky to sort out.  Consider
      data family D k :: k
  Then consider D (forall k2. k2 -> k2) Type Type
  The visibilty flags on an application of D may affected by the arguments
  themselves.  Heavy sigh.  But not truly hard; that's what tcbVisibilities
  does.



Note [Default methods in the type environment]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The default method Ids are already in the type environment (see Note
[Default method Ids and Template Haskell] in TcTyDcls), BUT they
don't have their InlinePragmas yet.  Usually that would not matter,
because the simplifier propagates information from binding site to
use.  But, unusually, when compiling instance decls we *copy* the
INLINE pragma from the default method to the method for that
particular operation (see Note [INLINE and default methods] below).

So right here in tcInstDecls2 we must re-extend the type envt with
the default method Ids replete with their INLINE pragmas.  Urk.


Note [Typechecking plan for instance declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For instance declarations we generate the following bindings and implication
constraints.  Example:

.. code-block:: haskell

   instance Ord a => Ord [a] where compare = <compare-rhs>

generates this:

.. code-block:: haskell

   Bindings:
      -- Method bindings
      $ccompare :: forall a. Ord a => a -> a -> Ordering
      $ccompare = /\a \(d:Ord a). let <meth-ev-binds> in ...

.. code-block:: haskell

      -- Superclass bindings
      $cp1Ord :: forall a. Ord a => Eq [a]
      $cp1Ord = /\a \(d:Ord a). let <sc-ev-binds>
               in dfEqList (dw :: Eq a)

.. code-block:: haskell

   Constraints:
      forall a. Ord a =>
                -- Method constraint
             (forall. (empty) => <constraints from compare-rhs>)
                -- Superclass constraint
          /\ (forall. (empty) => dw :: Eq a)

Notice that

 * Per-meth/sc implication.  There is one inner implication per
   superclass or method, with no skolem variables or givens.  The only
   reason for this one is to gather the evidence bindings privately
   for this superclass or method.  This implication is generated
   by checkInstConstraints.

 * Overall instance implication. There is an overall enclosing
   implication for the whole instance declaration, with the expected
   skolems and givens.  We need this to get the correct "redundant
   constraint" warnings, gathering all the uses from all the methods
   and superclasses.  See TcSimplify Note [Tracking redundant
   constraints]

 * The given constraints in the outer implication may generate
   evidence, notably by superclass selection.  Since the method and
   superclass bindings are top-level, we want that evidence copied
   into *every* method or superclass definition.  (Some of it will
   be usused in some, but dead-code elimination will drop it.)

.. code-block:: haskell

   We achieve this by putting the evidence variable for the overall
   instance implication into the AbsBinds for each method/superclass.
   Hence the 'dfun_ev_binds' passed into tcMethods and tcSuperClasses.
   (And that in turn is why the abs_ev_binds field of AbBinds is a
   [TcEvBinds] rather than simply TcEvBinds.

.. code-block:: haskell

   This is a bit of a hack, but works very nicely in practice.

 * Note that if a method has a locally-polymorphic binding, there will
   be yet another implication for that, generated by tcPolyCheck
   in tcMethodBody. E.g.
          class C a where
            foo :: forall b. Ord b => blah




Note [Recursive superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See #3731, #4809, #5751, #5913, #6117, #6161, which all
describe somewhat more complicated situations, but ones
encountered in practice.

See also tests tcrun020, tcrun021, tcrun033, and #11427.

----- THE PROBLEM --------
The problem is that it is all too easy to create a class whose
superclass is bottom when it should not be.

Consider the following (extreme) situation:
        class C a => D a where ...
        instance D [a] => D [a] where ...   (dfunD)
        instance C [a] => C [a] where ...   (dfunC)
Although this looks wrong (assume D [a] to prove D [a]), it is only a
more extreme case of what happens with recursive dictionaries, and it
can, just about, make sense because the methods do some work before
recursing.

To implement the dfunD we must generate code for the superclass C [a],
which we had better not get by superclass selection from the supplied
argument:
       dfunD :: forall a. D [a] -> D [a]
       dfunD = \d::D [a] -> MkD (scsel d) ..

Otherwise if we later encounter a situation where
we have a [Wanted] dw::D [a] we might solve it thus:
     dw := dfunD dw
Which is all fine except that now ** the superclass C is bottom **!

The instance we want is:
       dfunD :: forall a. D [a] -> D [a]
       dfunD = \d::D [a] -> MkD (dfunC (scsel d)) ...

----- THE SOLUTION --------
The basic solution is simple: be very careful about using superclass
selection to generate a superclass witness in a dictionary function
definition.  More precisely:

.. code-block:: haskell

  Superclass Invariant: in every class dictionary,
                        every superclass dictionary field
                        is non-bottom

To achieve the Superclass Invariant, in a dfun definition we can
generate a guaranteed-non-bottom superclass witness from:
  (sc1) one of the dictionary arguments itself (all non-bottom)
  (sc2) an immediate superclass of a smaller dictionary
  (sc3) a call of a dfun (always returns a dictionary constructor)

The tricky case is (sc2).  We proceed by induction on the size of
the (type of) the dictionary, defined by TcValidity.sizeTypes.
Let's suppose we are building a dictionary of size 3, and
suppose the Superclass Invariant holds of smaller dictionaries.
Then if we have a smaller dictionary, its immediate superclasses
will be non-bottom by induction.

What does "we have a smaller dictionary" mean?  It might be
one of the arguments of the instance, or one of its superclasses.
Here is an example, taken from CmmExpr:
       class Ord r => UserOfRegs r a where ...
(i1)   instance UserOfRegs r a => UserOfRegs r (Maybe a) where
(i2)   instance (Ord r, UserOfRegs r CmmReg) => UserOfRegs r CmmExpr where

For (i1) we can get the (Ord r) superclass by selection from (UserOfRegs r a),
since it is smaller than the thing we are building (UserOfRegs r (Maybe a).

But for (i2) that isn't the case, so we must add an explicit, and
perhaps surprising, (Ord r) argument to the instance declaration.

Here's another example from #6161:

       class       Super a => Duper a  where ...
       class Duper (Fam a) => Foo a    where ...
(i3)   instance Foo a => Duper (Fam a) where ...
(i4)   instance              Foo Float where ...

It would be horribly wrong to define
   dfDuperFam :: Foo a -> Duper (Fam a)  -- from (i3)
   dfDuperFam d = MkDuper (sc_sel1 (sc_sel2 d)) ...

.. code-block:: haskell

   dfFooFloat :: Foo Float               -- from (i4)
   dfFooFloat = MkFoo (dfDuperFam dfFooFloat) ...

Now the Super superclass of Duper is definitely bottom!

This won't happen because when processing (i3) we can use the
superclasses of (Foo a), which is smaller, namely Duper (Fam a).  But
that is *not* smaller than the target so we can't take *its*
superclasses.  As a result the program is rightly rejected, unless you
add (Super (Fam a)) to the context of (i3).



Note [Solving superclass constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
How do we ensure that every superclass witness is generated by
one of (sc1) (sc2) or (sc3) in Note [Recursive superclasses].
Answer:

  * Superclass "wanted" constraints have CtOrigin of (ScOrigin size)
    where 'size' is the size of the instance declaration. e.g.
          class C a => D a where...
          instance blah => D [a] where ...
    The wanted superclass constraint for C [a] has origin
    ScOrigin size, where size = size( D [a] ).

  * (sc1) When we rewrite such a wanted constraint, it retains its
    origin.  But if we apply an instance declaration, we can set the
    origin to (ScOrigin infinity), thus lifting any restrictions by
    making prohibitedSuperClassSolve return False.

  * (sc2) ScOrigin wanted constraints can't be solved from a
    superclass selection, except at a smaller type.  This test is
    implemented by TcInteract.prohibitedSuperClassSolve

  * The "given" constraints of an instance decl have CtOrigin
    GivenOrigin InstSkol.

  * When we make a superclass selection from InstSkol we use
    a SkolemInfo of (InstSC size), where 'size' is the size of
    the constraint whose superclass we are taking.  A similarly
    when taking the superclass of an InstSC.  This is implemented
    in TcCanonical.newSCWorkFromFlavored

Note [Silent superclass arguments] (historical interest only)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NB1: this note describes our *old* solution to the
     recursive-superclass problem. I'm keeping the Note
     for now, just as institutional memory.
     However, the code for silent superclass arguments
     was removed in late Dec 2014

NB2: the silent-superclass solution introduced new problems
     of its own, in the form of instance overlap.  Tests
     SilentParametersOverlapping, T5051, and T7862 are examples

NB3: the silent-superclass solution also generated tons of
     extra dictionaries.  For example, in monad-transformer
     code, when constructing a Monad dictionary you had to pass
     an Applicative dictionary; and to construct that you neede
     a Functor dictionary. Yet these extra dictionaries were
     often never used.  Test T3064 compiled *far* faster after
     silent superclasses were eliminated.

Our solution to this problem "silent superclass arguments".  We pass
to each dfun some ``silent superclass arguments’’, which are the
immediate superclasses of the dictionary we are trying to
construct. In our example:
       dfun :: forall a. C [a] -> D [a] -> D [a]
       dfun = \(dc::C [a]) (dd::D [a]) -> DOrd dc ...
Notice the extra (dc :: C [a]) argument compared to the previous version.

This gives us:

.. code-block:: haskell

     -----------------------------------------------------------
     DFun Superclass Invariant
     ~~~~~~~~~~~~~~~~~~~~~~~~
     In the body of a DFun, every superclass argument to the
     returned dictionary is
       either   * one of the arguments of the DFun,
       or       * constant, bound at top level
     -----------------------------------------------------------

This net effect is that it is safe to treat a dfun application as
wrapping a dictionary constructor around its arguments (in particular,
a dfun never picks superclasses from the arguments under the
dictionary constructor). No superclass is hidden inside a dfun
application.

The extra arguments required to satisfy the DFun Superclass Invariant
always come first, and are called the "silent" arguments.  You can
find out how many silent arguments there are using Id.dfunNSilent;
and then you can just drop that number of arguments to see the ones
that were in the original instance declaration.

DFun types are built (only) by MkId.mkDictFunId, so that is where we
decide what silent arguments are to be added.


Note [Mismatched class methods and associated type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's entirely possible for someone to put methods or associated type family
instances inside of a class in which it doesn't belong. For instance, we'd
want to fail if someone wrote this:

.. code-block:: haskell

  instance Eq () where
    type Rep () = Maybe
    compare = undefined

Since neither the type family `Rep` nor the method `compare` belong to the
class `Eq`. Normally, this is caught in the renamer when resolving RdrNames,
since that would discover that the parent class `Eq` is incorrect.

However, there is a scenario in which the renamer could fail to catch this:
if the instance was generated through Template Haskell, as in #12387. In that
case, Template Haskell will provide fully resolved names (e.g.,
`GHC.Classes.compare`), so the renamer won't notice the sleight-of-hand going
on. For this reason, we also put an extra validity check for this in the
typechecker as a last resort.



Note [Avoid -Winaccessible-code when deriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-Winaccessible-code can be particularly noisy when deriving instances for
GADTs. Consider the following example (adapted from #8128):

.. code-block:: haskell

  data T a where
    MkT1 :: Int -> T Int
    MkT2 :: T Bool
    MkT3 :: T Bool
  deriving instance Eq (T a)
  deriving instance Ord (T a)

In the derived Ord instance, GHC will generate the following code:

.. code-block:: haskell

  instance Ord (T a) where
    compare x y
      = case x of
          MkT2
            -> case y of
                 MkT1 {} -> GT
                 MkT2    -> EQ
                 _       -> LT
          ...

However, that MkT1 is unreachable, since the type indices for MkT1 and MkT2
differ, so if -Winaccessible-code is enabled, then deriving this instance will
result in unwelcome warnings.

One conceivable approach to fixing this issue would be to change `deriving Ord`
such that it becomes smarter about not generating unreachable cases. This,
however, would be a highly nontrivial refactor, as we'd have to propagate
through typing information everywhere in the algorithm that generates Ord
instances in order to determine which cases were unreachable. This seems like
a lot of work for minimal gain, so we have opted not to go for this approach.

Instead, we take the much simpler approach of always disabling
-Winaccessible-code for derived code. To accomplish this, we do the following:

1. In tcMethods (which typechecks method bindings), disable
   -Winaccessible-code.
2. When creating Implications during typechecking, record the Env
   (through ic_env) at the time of creation. Since the Env also stores
   DynFlags, this will remember that -Winaccessible-code was disabled over
   the scope of that implication.
3. After typechecking comes error reporting, where GHC must decide how to
   report inaccessible code to the user, on an Implication-by-Implication
   basis. If an Implication's DynFlags indicate that -Winaccessible-code was
   disabled, then don't bother reporting it. That's it!
----------------------


Note [Instance method signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With -XInstanceSigs we allow the user to supply a signature for the
method in an instance declaration.  Here is an artificial example:

.. code-block:: haskell

       data T a = MkT a
       instance Ord a => Ord (T a) where
         (>) :: forall b. b -> b -> Bool
         (>) = error "You can't compare Ts"

The instance signature can be *more* polymorphic than the instantiated
class method (in this case: Age -> Age -> Bool), but it cannot be less
polymorphic.  Moreover, if a signature is given, the implementation
code should match the signature, and type variables bound in the
singature should scope over the method body.

We achieve this by building a TcSigInfo for the method, whether or not
there is an instance method signature, and using that to typecheck
the declaration (in tcMethodBody).  That means, conveniently,
that the type variables bound in the signature will scope over the body.

What about the check that the instance method signature is more
polymorphic than the instantiated class method type?  We just do a
tcSubType call in tcMethodBodyHelp, and generate a nested AbsBind, like
this (for the example above

.. code-block:: haskell

 AbsBind { abs_tvs = [a], abs_ev_vars = [d:Ord a]
         , abs_exports
             = ABExport { (>) :: forall a. Ord a => T a -> T a -> Bool
                        , gr_lcl :: T a -> T a -> Bool }
         , abs_binds
             = AbsBind { abs_tvs = [], abs_ev_vars = []
                       , abs_exports = ABExport { gr_lcl :: T a -> T a -> Bool
                                                , gr_inner :: forall b. b -> b -> Bool }
                       , abs_binds = AbsBind { abs_tvs = [b], abs_ev_vars = []
                                             , ..etc.. }
               } }

Wow!  Three nested AbsBinds!
 * The outer one abstracts over the tyvars and dicts for the instance
 * The middle one is only present if there is an instance signature,
   and does the impedance matching for that signature
 * The inner one is for the method binding itself against either the
   signature from the class, or the instance signature.
--------------------


Note [Export helper functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We arrange to export the "helper functions" of an instance declaration,
so that they are not subject to preInlineUnconditionally, even if their
RHS is trivial.  Reason: they are mentioned in the DFunUnfolding of
the dict fun as Ids, not as CoreExprs, so we can't substitute a
non-variable for them.

We could change this by making DFunUnfoldings have CoreExprs, but it
seems a bit simpler this way.



Note [Default methods in instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this

.. code-block:: haskell

   class Baz v x where
      foo :: x -> x
      foo y = <blah>

.. code-block:: haskell

   instance Baz Int Int

From the class decl we get

.. code-block:: haskell

   $dmfoo :: forall v x. Baz v x => x -> x
   $dmfoo y = <blah>

Notice that the type is ambiguous.  So we use Visible Type Application
to disambiguate:

.. code-block:: haskell

   $dBazIntInt = MkBaz fooIntInt
   fooIntInt = $dmfoo @Int @Int

Lacking VTA we'd get ambiguity errors involving the default method.  This applies
equally to vanilla default methods (#1061) and generic default methods
(#12220).

Historical note: before we had VTA we had to generate
post-type-checked code, which took a lot more code, and didn't work for
generic default methods.



Note [INLINE and default methods]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Default methods need special case.  They are supposed to behave rather like
macros.  For example

.. code-block:: haskell

  class Foo a where
    op1, op2 :: Bool -> a -> a

.. code-block:: haskell

    {-# INLINE op1 #-}
    op1 b x = op2 (not b) x

.. code-block:: haskell

  instance Foo Int where
    -- op1 via default method
    op2 b x = <blah>

The instance declaration should behave

.. code-block:: haskell

   just as if 'op1' had been defined with the
   code, and INLINE pragma, from its original
   definition.

That is, just as if you'd written

.. code-block:: haskell

  instance Foo Int where
    op2 b x = <blah>

.. code-block:: haskell

    {-# INLINE op1 #-}
    op1 b x = op2 (not b) x

So for the above example we generate:

.. code-block:: haskell

  {-# INLINE $dmop1 #-}
  -- $dmop1 has an InlineCompulsory unfolding
  $dmop1 d b x = op2 d (not b) x

.. code-block:: haskell

  $fFooInt = MkD $cop1 $cop2

.. code-block:: haskell

  {-# INLINE $cop1 #-}
  $cop1 = $dmop1 $fFooInt

.. code-block:: haskell

  $cop2 = <blah>

Note carefully:

* We *copy* any INLINE pragma from the default method $dmop1 to the
  instance $cop1.  Otherwise we'll just inline the former in the
  latter and stop, which isn't what the user expected

* Regardless of its pragma, we give the default method an
  unfolding with an InlineCompulsory source. That means
  that it'll be inlined at every use site, notably in
  each instance declaration, such as $cop1.  This inlining
  must happen even though
    a) $dmop1 is not saturated in $cop1
    b) $cop1 itself has an INLINE pragma

.. code-block:: haskell

  It's vital that $dmop1 *is* inlined in this way, to allow the mutual
  recursion between $fooInt and $cop1 to be broken

* To communicate the need for an InlineCompulsory to the desugarer
  (which makes the Unfoldings), we use the IsDefaultMethod constructor
  in TcSpecPrags.




Note [SPECIALISE instance pragmas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

.. code-block:: haskell

   instance (Ix a, Ix b) => Ix (a,b) where
     {-# SPECIALISE instance Ix (Int,Int) #-}
     range (x,y) = ...

We make a specialised version of the dictionary function, AND
specialised versions of each *method*.  Thus we should generate
something like this:

.. code-block:: haskell

  $dfIxPair :: (Ix a, Ix b) => Ix (a,b)
  {-# DFUN [$crangePair, ...] #-}
  {-# SPECIALISE $dfIxPair :: Ix (Int,Int) #-}
  $dfIxPair da db = Ix ($crangePair da db) (...other methods...)

.. code-block:: haskell

  $crange :: (Ix a, Ix b) -> ((a,b),(a,b)) -> [(a,b)]
  {-# SPECIALISE $crange :: ((Int,Int),(Int,Int)) -> [(Int,Int)] #-}
  $crange da db = <blah>

The SPECIALISE pragmas are acted upon by the desugarer, which generate

.. code-block:: haskell

  dii :: Ix Int
  dii = ...

.. code-block:: haskell

  $s$dfIxPair :: Ix ((Int,Int),(Int,Int))
  {-# DFUN [$crangePair di di, ...] #-}
  $s$dfIxPair = Ix ($crangePair di di) (...)

.. code-block:: haskell

  {-# RULE forall (d1,d2:Ix Int). $dfIxPair Int Int d1 d2 = $s$dfIxPair #-}

.. code-block:: haskell

  $s$crangePair :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
  $c$crangePair = ...specialised RHS of $crangePair...

.. code-block:: haskell

  {-# RULE forall (d1,d2:Ix Int). $crangePair Int Int d1 d2 = $s$crangePair #-}

Note that

  * The specialised dictionary $s$dfIxPair is very much needed, in case we
    call a function that takes a dictionary, but in a context where the
    specialised dictionary can be used.  See #7797.

  * The ClassOp rule for 'range' works equally well on $s$dfIxPair, because
    it still has a DFunUnfolding.  See Note [ClassOp/DFun selection]

  * A call (range ($dfIxPair Int Int d1 d2)) might simplify two ways:
       --> {ClassOp rule for range}     $crangePair Int Int d1 d2
       --> {SPEC rule for $crangePair}  $s$crangePair
    or thus:
       --> {SPEC rule for $dfIxPair}    range $s$dfIxPair
       --> {ClassOpRule for range}      $s$crangePair
    It doesn't matter which way.

  * We want to specialise the RHS of both $dfIxPair and $crangePair,
    but the SAME HsWrapper will do for both!  We can call tcSpecPrag
    just once, and pass the result (in spec_inst_info) to tcMethods.

