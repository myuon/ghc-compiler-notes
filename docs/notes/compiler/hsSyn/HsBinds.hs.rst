`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/hsSyn/HsBinds.hs>`_

Note [AbsBinds]
~~~~~~~~~~~~~~~
The AbsBinds constructor is used in the output of the type checker, to
record *typechecked* and *generalised* bindings.  Specifically

         AbsBinds { abs_tvs      = tvs
                  , abs_ev_vars  = [d1,d2]
                  , abs_exports  = [ABE { abe_poly = fp, abe_mono = fm
                                        , abe_wrap = fwrap }
                                    ABE { slly for g } ]
                  , abs_ev_binds = DBINDS
                  , abs_binds    = BIND[fm,gm] }

where 'BIND' binds the monomorphic Ids 'fm' and 'gm', means

        fp = fwrap [/\ tvs. \d1 d2. letrec { DBINDS        ]
                   [                       ; BIND[fm,gm] } ]
                   [                 in fm                 ]

        gp = ...same again, with gm instead of fm

The 'fwrap' is an impedence-matcher that typically does nothing; see
Note [ABExport wrapper].

This is a pretty bad translation, because it duplicates all the bindings.
So the desugarer tries to do a better job:

        fp = /\ [a,b] -> \ [d1,d2] -> case tp [a,b] [d1,d2] of
                                        (fm,gm) -> fm
        ..ditto for gp..

        tp = /\ [a,b] -> \ [d1,d2] -> letrec { DBINDS; BIND }
                                      in (fm,gm)

In general:

  * abs_tvs are the type variables over which the binding group is
    generalised
  * abs_ev_var are the evidence variables (usually dictionaries)
    over which the binding group is generalised
  * abs_binds are the monomorphic bindings
  * abs_ex_binds are the evidence bindings that wrap the abs_binds
  * abs_exports connects the monomorphic Ids bound by abs_binds
    with the polymorphic Ids bound by the AbsBinds itself.

For example, consider a module M, with this top-level binding, where
there is no type signature for M.reverse,
    M.reverse []     = []
    M.reverse (x:xs) = M.reverse xs ++ [x]

In Hindley-Milner, a recursive binding is typechecked with the
*recursive* uses being *monomorphic*.  So after typechecking *and*
desugaring we will get something like this

    M.reverse :: forall a. [a] -> [a]
      = /\a. letrec
                reverse :: [a] -> [a] = \xs -> case xs of
                                                []     -> []
                                                (x:xs) -> reverse xs ++ [x]
             in reverse

Notice that 'M.reverse' is polymorphic as expected, but there is a local
definition for plain 'reverse' which is *monomorphic*.  The type variable
'a' scopes over the entire letrec.

That's after desugaring.  What about after type checking but before
desugaring?  That's where AbsBinds comes in.  It looks like this:

   AbsBinds { abs_tvs     = [a]
            , abs_ev_vars = []
            , abs_exports = [ABE { abe_poly = M.reverse :: forall a. [a] -> [a],
                                 , abe_mono = reverse :: [a] -> [a]}]
            , abs_ev_binds = {}
            , abs_binds = { reverse :: [a] -> [a]
                               = \xs -> case xs of
                                            []     -> []
                                            (x:xs) -> reverse xs ++ [x] } }

Here,

  * abs_tvs says what type variables are abstracted over the binding
    group, just 'a' in this case.
  * abs_binds is the *monomorphic* bindings of the group
  * abs_exports describes how to get the polymorphic Id 'M.reverse'
    from the monomorphic one 'reverse'

Notice that the *original* function (the polymorphic one you thought
you were defining) appears in the abe_poly field of the
abs_exports. The bindings in abs_binds are for fresh, local, Ids with
a *monomorphic* Id.

If there is a group of mutually recursive (see Note [Polymorphic
recursion]) functions without type signatures, we get one AbsBinds
with the monomorphic versions of the bindings in abs_binds, and one
element of abe_exports for each variable bound in the mutually
recursive group.  This is true even for pattern bindings.  Example:
        (f,g) = (\x -> x, f)
After type checking we get
   AbsBinds { abs_tvs     = [a]
            , abs_exports = [ ABE { abe_poly = M.f :: forall a. a -> a
                                  , abe_mono = f :: a -> a }
                            , ABE { abe_poly = M.g :: forall a. a -> a
                                  , abe_mono = g :: a -> a }]
            , abs_binds = { (f,g) = (\x -> x, f) }



Note [Polymorphic recursion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   Rec { f x = ...(g ef)...

       ; g :: forall a. [a] -> [a]
       ; g y = ...(f eg)...  }

These bindings /are/ mutually recursive (f calls g, and g calls f).
But we can use the type signature for g to break the recursion,
like this:

  1. Add g :: forall a. [a] -> [a] to the type environment

  2. Typecheck the definition of f, all by itself,
     including generalising it to find its most general
     type, say f :: forall b. b -> b -> [b]

  3. Extend the type environment with that type for f

  4. Typecheck the definition of g, all by itself,
     checking that it has the type claimed by its signature

Steps 2 and 4 each generate a separate AbsBinds, so we end
up with
   Rec { AbsBinds { ...for f ... }
       ; AbsBinds { ...for g ... } }

This approach allows both f and to call each other
polymorphically, even though only g has a signature.

We get an AbsBinds that encompasses multiple source-program
bindings only when
 * Each binding in the group has at least one binder that
   lacks a user type signature
 * The group forms a strongly connected component




Note [The abs_sig field of AbsBinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The abs_sig field supports a couple of special cases for bindings.
Consider

  x :: Num a => (# a, a #)
  x = (# 3, 4 #)

The general desugaring for AbsBinds would give

  x = /\a. \ ($dNum :: Num a) ->
      letrec xm = (# fromInteger $dNum 3, fromInteger $dNum 4 #) in
      xm

But that has an illegal let-binding for an unboxed tuple.  In this
case we'd prefer to generate the (more direct)

  x = /\ a. \ ($dNum :: Num a) ->
     (# fromInteger $dNum 3, fromInteger $dNum 4 #)

A similar thing happens with representation-polymorphic defns
(#11405):

  undef :: forall (r :: RuntimeRep) (a :: TYPE r). HasCallStack => a
  undef = error "undef"

Again, the vanilla desugaring gives a local let-binding for a
representation-polymorphic (undefm :: a), which is illegal.  But
again we can desugar without a let:

  undef = /\ a. \ (d:HasCallStack) -> error a d "undef"

The abs_sig field supports this direct desugaring, with no local
let-bining.  When abs_sig = True

 * the abs_binds is single FunBind

 * the abs_exports is a singleton

 * we have a complete type sig for binder
   and hence the abs_binds is non-recursive
   (it binds the mono_id but refers to the poly_id

These properties are exploited in DsBinds.dsAbsBinds to
generate code without a let-binding.



Note [ABExport wrapper]
~~~~~~~~~~~~~~~~~~~~~~~
Consider
   (f,g) = (\x.x, \y.y)
This ultimately desugars to something like this:
   tup :: forall a b. (a->a, b->b)
   tup = /\a b. (\x:a.x, \y:b.y)
   f :: forall a. a -> a
   f = /\a. case tup a Any of
               (fm::a->a,gm:Any->Any) -> fm
   ...similarly for g...

The abe_wrap field deals with impedance-matching between
    (/\a b. case tup a b of { (f,g) -> f })
and the thing we really want, which may have fewer type
variables.  The action happens in TcBinds.mkExport.



Note [Bind free vars]
~~~~~~~~~~~~~~~~~~~~~
The bind_fvs field of FunBind and PatBind records the free variables
of the definition.  It is used for the following purposes

a) Dependency analysis prior to type checking
    (see TcBinds.tc_group)

b) Deciding whether we can do generalisation of the binding
    (see TcBinds.decideGeneralisationPlan)

c) Deciding whether the binding can be used in static forms
    (see TcExpr.checkClosedInStaticForm for the HsStatic case and
     TcBinds.isClosedBndrGroup).

Specifically,

  * bind_fvs includes all free vars that are defined in this module
    (including top-level things and lexically scoped type variables)

  * bind_fvs excludes imported vars; this is just to keep the set smaller

  * Before renaming, and after typechecking, the field is unused;
    it's just an error thunk

