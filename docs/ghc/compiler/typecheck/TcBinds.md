[[src]](https://github.com/ghc/ghc/tree/master/compiler/typecheck/TcBinds.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

# TcBinds

# A useful helper function


# \subsection{Type-checking bindings}


@tcBindsAndThen@ typechecks a @HsBinds@.  The "and then" part is because
it needs to know something about the {\em usage} of the things bound,
so that it can create specialisations of them.  So @tcBindsAndThen@
takes a function which, given an extended environment, E, typechecks
the scope of the bindings returning a typechecked thing and (most
important) an LIE.  It is this LIE which is then used as the basis for
specialising the things bound.

@tcBindsAndThen@ also takes a "combiner" which glues together the
bindings and the "thing" to make a new "thing".

The real work is done by @tcBindWithSigsAndThen@.

Recursive and non-recursive binds are handled in essentially the same
way: because of uniques there are no scoping issues left.  The only
difference is that non-recursive bindings can bind primitive values.

Even for non-recursive binding groups we add typings for each binder
to the LVE for the following reason.  When each individual binding is
checked the type of its LHS is unified with that of its RHS; and
type-checking the LHS of course requires that the binder is in scope.

At the top-level the LIE is sure to contain nothing but constant
dictionaries, which we resolve at the module level.

### Note: Polymorphic recursion

The game plan for polymorphic recursion in the code above is

        * Bind any variable for which we have a type signature
          to an Id with a polymorphic type.  Then when type-checking
          the RHSs we'll make a full polymorphic call.

This fine, but if you aren't a bit careful you end up with a horrendous
amount of partial application and (worse) a huge space leak. For example:

        f :: Eq a => [a] -> [a]
        f xs = ...f...

If we don't take care, after typechecking we get

        f = /\a -> \d::Eq a -> let f' = f a d
                               in
                               \ys:[a] -> ...f'...

Notice the the stupid construction of (f a d), which is of course
identical to the function we're executing.  In this case, the
polymorphic recursion isn't being used (but that's a very common case).
This can lead to a massive space leak, from the following top-level defn
(post-typechecking)

        ff :: [Int] -> [Int]
        ff = f Int dEqInt

Now (f dEqInt) evaluates to a lambda that has f' as a free variable; but
f' is another thunk which evaluates to the same thing... and you end
up with a chain of identical values all hung onto by the CAF ff.

        ff = f Int dEqInt

           = let f' = f Int dEqInt in \ys. ...f'...

           = let f' = let f' = f Int dEqInt in \ys. ...f'...
                      in \ys. ...f'...

Etc.

NOTE: a bit of arity anaysis would push the (f a d) inside the (\ys...),
which would make the space leak go away in this case

Solution: when typechecking the RHSs we always have in hand the
*monomorphic* Ids for each binding.  So we just need to make sure that
if (Method f a d) shows up in the constraints emerging from (...f...)
we just use the monomorphic Id.  We achieve this by adding monomorphic Ids
to the "givens" when simplifying constraints.  That's what the "lies_avail"
is doing.

Then we get

        f = /\a -> \d::Eq a -> letrec
                                 fm = \ys:[a] -> ...fm...
                               in
                               fm


### Note: Implicit parameter untouchables

We add the type variables in the types of the implicit parameters
as untouchables, not so much because we really must not unify them,
but rather because we otherwise end up with constraints like this
    Num alpha, Implic { wanted = alpha ~ Int }
The constraint solver solves alpha~Int by unification, but then
doesn't float that solved constraint out (it's not an unsolved
wanted).  Result disaster: the (Num alpha) is again solved, this
time by defaulting.  No no no.

However [Oct 10] this is all handled automatically by the
untouchable-range idea.


# tcPolyNoGen


# tcPolyCheck


### Note: Instantiate sig with fresh variables

It's vital to instantiate a type signature with fresh variables.
For example:
      type T = forall a. [a] -> [a]
      f :: T;
      f = g where { g :: T; g = <rhs> }

 We must not use the same 'a' from the defn of T at both places!!
(Instantiation is only necessary because of type synonyms.  Otherwise,
it's all cool; each signature has distinct type variables from the renamer.)


# tcPolyInfer


### Note: Partial type signatures and generalisation

If /any/ of the signatures in the gropu is a partial type signature
   f :: _ -> Int
then we *always* use the InferGen plan, and hence tcPolyInfer.
We do this even for a local binding with -XMonoLocalBinds, when
we normally use NoGen.

Reasons:
  * The TcSigInfo for 'f' has a unification variable for the '_',
    whose TcLevel is one level deeper than the current level.
    (See pushTcLevelM in tcTySig.)  But NoGen doesn't increase
    the TcLevel like InferGen, so we lose the level invariant.

  * The signature might be   f :: forall a. _ -> a
    so it really is polymorphic.  It's not clear what it would
    mean to use NoGen on this, and indeed the ASSERT in tcLhs,
    in the (Just sig) case, checks that if there is a signature
    then we are using LetLclBndr, and hence a nested AbsBinds with
    increased TcLevel

It might be possible to fix these difficulties somehow, but there
doesn't seem much point.  Indeed, adding a partial type signature is a
way to get per-binding inferred generalisation.

We apply the MR if /all/ of the partial signatures lack a context.
In particular (Trac #11016):
   f2 :: (?loc :: Int) => _
   f2 = ?loc
It's stupid to apply the MR here.  This test includes an extra-constraints
wildcard; that is, we don't apply the MR if you write
   f3 :: _ => blah

### Note: Quantified variables in partial type signatures

Consider
  f :: forall a. a -> a -> _
  f x y = g x y
  g :: forall b. b -> b -> _
  g x y = [x, y]

Here, 'f' and 'g' are mutually recursive, and we end up unifying 'a' and 'b'
together, which is fine.  So we bind 'a' and 'b' to SigTvs, which can then
unify with each other.

But now consider:
  f :: forall a b. a -> b -> _
  f x y = [x, y]

We want to get an error from this, because 'a' and 'b' get unified.
So we make a test, one per parital signature, to check that the
explicitly-quantified type variables have not been unified together.
Trac #14449 showed this up.

### Note: Validity of inferred types

We need to check inferred type for validity, in case it uses language
extensions that are not turned on.  The principle is that if the user
simply adds the inferred type to the program source, it'll compile fine.
See #8883.

Examples that might fail:
 - the type might be ambiguous

 - an inferred theta that requires type equalities e.g. (F a ~ G b)
                                or multi-parameter type classes
 - an inferred type that includes unboxed tuples

### Note: Impedance matching

Consider
   f 0 x = x
   f n x = g [] (not x)

   g [] y = f 10 y
   g _  y = f 9  y

After typechecking we'll get
  f_mono_ty :: a -> Bool -> Bool
  g_mono_ty :: [b] -> Bool -> Bool
with constraints
  (Eq a, Num a)

Note that f is polymorphic in 'a' and g in 'b'; and these are not linked.
The types we really want for f and g are
   f :: forall a. (Eq a, Num a) => a -> Bool -> Bool
   g :: forall b. [b] -> Bool -> Bool

We can get these by "impedance matching":
   tuple :: forall a b. (Eq a, Num a) => (a -> Bool -> Bool, [b] -> Bool -> Bool)
   tuple a b d1 d1 = let ...bind f_mono, g_mono in (f_mono, g_mono)

   f a d1 d2 = case tuple a Any d1 d2 of (f, g) -> f
   g b = case tuple Integer b dEqInteger dNumInteger of (f,g) -> g

Suppose the shared quantified tyvars are qtvs and constraints theta.
Then we want to check that
     forall qtvs. theta => f_mono_ty   is more polymorphic than   f's polytype
and the proof is the impedance matcher.

Notice that the impedance matcher may do defaulting.  See Trac #7173.

It also cleverly does an ambiguity check; for example, rejecting
   f :: F a -> F a
where F is a non-injective type function.


# Vectorisation


### Note: SPECIALISE pragmas

### Note: Typechecking pattern bindings

Look at:
   - typecheck/should_compile/ExPat
   - Trac #12427, typecheck/should_compile/T12427{a,b}

  data T where
    MkT :: Integral a => a -> Int -> T

and suppose t :: T.  Which of these pattern bindings are ok?

  E1. let { MkT p _ = t } in <body>

  E2. let { MkT _ q = t } in <body>

  E3. let { MkT (toInteger -> r) _ = t } in <body>

* (E1) is clearly wrong because the existential 'a' escapes.
  What type could 'p' possibly have?

* (E2) is fine, despite the existential pattern, because
  q::Int, and nothing escapes.

* Even (E3) is fine.  The existential pattern binds a dictionary
  for (Integral a) which the view pattern can use to convert the
  a-valued field to an Integer, so r :: Integer.

An easy way to see all three is to imagine the desugaring.
For (E2) it would look like
    let q = case t of MkT _ q' -> q'
    in <body>


We typecheck pattern bindings as follows.  First tcLhs does this:

  1. Take each type signature q :: ty, partial or complete, and
     instantiate it (with tcLhsSigId) to get a MonoBindInfo.  This
     gives us a fresh "mono_id" qm :: instantiate(ty), where qm has
     a fresh name.

     Any fresh unification variables in instantiate(ty) born here, not
     deep under implications as would happen if we allocated them when
     we encountered q during tcPat.

  2. Build a little environment mapping "q" -> "qm" for those Ids
     with signatures (inst_sig_fun)

  3. Invoke tcLetPat to typecheck the pattern.

     - We pass in the current TcLevel.  This is captured by
       TcPat.tcLetPat, and put into the pc_lvl field of PatCtxt, in
       PatEnv.

     - When tcPat finds an existential constructor, it binds fresh
       type variables and dictionaries as usual, increments the TcLevel,
       and emits an implication constraint.

     - When we come to a binder (TcPat.tcPatBndr), it looks it up
       in the little environment (the pc_sig_fn field of PatCtxt).

         Success => There was a type signature, so just use it,
                    checking compatibility with the expected type.

         Failure => No type sigature.
             Infer case: (happens only outside any constructor pattern)
                         use a unification variable
                         at the outer level pc_lvl

             Check case: use promoteTcType to promote the type
                         to the outer level pc_lvl.  This is the
                         place where we emit a constraint that'll blow
                         up if existential capture takes place

       Result: the type of the binder is always at pc_lvl. This is
       crucial.

  4. Throughout, when we are making up an Id for the pattern-bound variables
     (newLetBndr), we have two cases:

     - If we are generalising (generalisation plan is InferGen or
       CheckGen), then the let_bndr_spec will be LetLclBndr.  In that case
       we want to bind a cloned, local version of the variable, with the
       type given by the pattern context, *not* by the signature (even if
       there is one; see Trac #7268). The mkExport part of the
       generalisation step will do the checking and impedance matching
       against the signature.

     - If for some some reason we are not generalising (plan = NoGen), the
       LetBndrSpec will be LetGblBndr.  In that case we must bind the
       global version of the Id, and do so with precisely the type given
       in the signature.  (Then we unify with the type from the pattern
       context type.)


And that's it!  The implication constraints check for the skolem
escape.  It's quite simple and neat, and more expressive than before
e.g. GHC 8.0 rejects (E2) and (E3).

Example for (E1), starting at level 1.  We generate
     p :: beta:1, with constraints (forall:3 a. Integral a => a ~ beta)
The (a~beta) can't float (because of the 'a'), nor be solved (because
beta is untouchable.)

Example for (E2), we generate
     q :: beta:1, with constraint (forall:3 a. Integral a => Int ~ beta)
The beta is untoucable, but floats out of the constraint and can
be solved absolutely fine.

# Generalisation


# Error contexts and messages
