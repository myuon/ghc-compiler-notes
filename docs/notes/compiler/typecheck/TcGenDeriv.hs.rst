Note [Generating Ord instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose constructors are K1..Kn, and some are nullary.
The general form we generate is:

* Do case on first argument
        case a of
          K1 ... -> rhs_1
          K2 ... -> rhs_2
          ...
          Kn ... -> rhs_n
          _ -> nullary_rhs

* To make rhs_i
     If i = 1, 2, n-1, n, generate a single case.
        rhs_2    case b of
                   K1 {}  -> LT
                   K2 ... -> ...eq_rhs(K2)...
                   _      -> GT

     Otherwise do a tag compare against the bigger range
     (because this is the one most likely to succeed)
        rhs_3    case tag b of tb ->
                 if 3 <# tg then GT
                 else case b of
                         K3 ... -> ...eq_rhs(K3)....
                         _      -> LT

* To make eq_rhs(K), which knows that
    a = K a1 .. av
    b = K b1 .. bv
  we just want to compare (a1,b1) then (a2,b2) etc.
  Take care on the last field to tail-call into comparing av,bv

* To make nullary_rhs generate this
     case con2tag a of a# ->
     case con2tag b of ->
     a# `compare` b#

Several special cases:

* Two or fewer nullary constructors: don't generate nullary_rhs

* Be careful about unlifted comparisons.  When comparing unboxed
  values we can't call the overloaded functions.
  See function unliftedOrdOp



Note [Game plan for deriving Ord]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's a bad idea to define only 'compare', and build the other binary
comparisons on top of it; see #2130, #4019.  Reason: we don't
want to laboriously make a three-way comparison, only to extract a
binary result, something like this:
     (>) (I# x) (I# y) = case <# x y of
                            True -> False
                            False -> case ==# x y of
                                       True  -> False
                                       False -> True

This being said, we can get away with generating full code only for
'compare' and '<' thus saving us generation of other three operators.
Other operators can be cheaply expressed through '<':
a <= b = not $ b < a
a > b = b < a
a >= b = not $ a < b

So for sufficiently small types (few constructors, or all nullary)
we generate all methods; for large ones we just use 'compare'.



Note [Use expectP]
~~~~~~~~~~~~~~~~~~
Note that we use
   expectP (Ident "T1")
rather than
   Ident "T1" <- lexP
The latter desugares to inline code for matching the Ident and the
string, and this can be very voluminous. The former is much more
compact.  Cf #7258, although that also concerned non-linearity in
the occurrence analyser, a separate issue.



Note [Read for empty data types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What should we get for this?  (#7931)
   data Emp deriving( Read )   -- No data constructors

Here we want
  read "[]" :: [Emp]   to succeed, returning []
So we do NOT want
   instance Read Emp where
     readPrec = error "urk"
Rather we want
   instance Read Emp where
     readPred = pfail   -- Same as choose []

Because 'pfail' allows the parser to backtrack, but 'error' doesn't.
These instances are also useful for Read (Either Int Emp), where
we want to be able to parse (Left 3) just fine.


Note [Newtype-deriving instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We take every method in the original instance and `coerce` it to fit
into the derived instance. We need type applications on the argument
to `coerce` to make it obvious what instantiation of the method we're
coercing from.  So from, say,

  class C a b where
    op :: forall c. a -> [b] -> c -> Int

  newtype T x = MkT <rep-ty>

  instance C a <rep-ty> => C a (T x) where
    op = coerce @ (a -> [<rep-ty>] -> c -> Int)
                @ (a -> [T x]      -> c -> Int)
                op :: forall c. a -> [T x] -> c -> Int

In addition to the type applications, we also have an explicit
type signature on the entire RHS. This brings the method-bound variable
`c` into scope over the two type applications.
See Note [GND and QuantifiedConstraints] for more information on why this
is important.

Giving 'coerce' two explicitly-visible type arguments grants us finer control
over how it should be instantiated. Recall

  coerce :: Coercible a b => a -> b

By giving it explicit type arguments we deal with the case where
'op' has a higher rank type, and so we must instantiate 'coerce' with
a polytype.  E.g.

   class C a where op :: a -> forall b. b -> b
   newtype T x = MkT <rep-ty>
   instance C <rep-ty> => C (T x) where
     op = coerce @ (<rep-ty> -> forall b. b -> b)
                 @ (T x      -> forall b. b -> b)
                op :: T x -> forall b. b -> b

The use of type applications is crucial here. If we had tried using only
explicit type signatures, like so:

   instance C <rep-ty> => C (T x) where
     op = coerce (op :: <rep-ty> -> forall b. b -> b)
                     :: T x      -> forall b. b -> b

Then GHC will attempt to deeply skolemize the two type signatures, which will
wreak havoc with the Coercible solver. Therefore, we instead use type
applications, which do not deeply skolemize and thus avoid this issue.
The downside is that we currently require -XImpredicativeTypes to permit this
polymorphic type instantiation, so we have to switch that flag on locally in
TcDeriv.genInst. See #8503 for more discussion.



Note [Newtype-deriving trickiness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#12768):
  class C a where { op :: D a => a -> a }

  instance C a  => C [a] where { op = opList }

  opList :: (C a, D [a]) => [a] -> [a]
  opList = ...

Now suppose we try GND on this:
  newtype N a = MkN [a] deriving( C )

The GND is expecting to get an implementation of op for N by
coercing opList, thus:

  instance C a => C (N a) where { op = opN }

  opN :: (C a, D (N a)) => N a -> N a
  opN = coerce @([a]   -> [a])
               @([N a] -> [N a]
               opList :: D (N a) => [N a] -> [N a]

But there is no reason to suppose that (D [a]) and (D (N a))
are inter-coercible; these instances might completely different.
So GHC rightly rejects this code.



Note [GND and QuantifiedConstraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following example from #15290:

  class C m where
    join :: m (m a) -> m a

  newtype T m a = MkT (m a)

  deriving instance
    (C m, forall p q. Coercible p q => Coercible (m p) (m q)) =>
    C (T m)

The code that GHC used to generate for this was:

  instance (C m, forall p q. Coercible p q => Coercible (m p) (m q)) =>
      C (T m) where
    join = coerce @(forall a.   m   (m a) ->   m a)
                  @(forall a. T m (T m a) -> T m a)
                  join

This instantiates `coerce` at a polymorphic type, a form of impredicative
polymorphism, so we're already on thin ice. And in fact the ice breaks,
as we'll explain:

The call to `coerce` gives rise to:

  Coercible (forall a.   m   (m a) ->   m a)
            (forall a. T m (T m a) -> T m a)

And that simplified to the following implication constraint:

  forall a <no-ev>. m (T m a) ~R# m (m a)

But because this constraint is under a `forall`, inside a type, we have to
prove it *without computing any term evidence* (hence the <no-ev>). Alas, we
*must* generate a term-level evidence binding in order to instantiate the
quantified constraint! In response, GHC currently chooses not to use such
a quantified constraint.
See Note [Instances in no-evidence implications] in TcInteract.

But this isn't the death knell for combining QuantifiedConstraints with GND.
On the contrary, if we generate GND bindings in a slightly different way, then
we can avoid this situation altogether. Instead of applying `coerce` to two
polymorphic types, we instead let an explicit type signature do the polymorphic
instantiation, and omit the `forall`s in the type applications.
More concretely, we generate the following code instead:

  instance (C m, forall p q. Coercible p q => Coercible (m p) (m q)) =>
      C (T m) where
    join = coerce @(  m   (m a) ->   m a)
                  @(T m (T m a) -> T m a)
                  join :: forall a. T m (T m a) -> T m a

Now the visible type arguments are both monotypes, so we need do any of this
funny quantified constraint instantiation business.

You might think that that second @(T m (T m a) -> T m a) argument is redundant
in the presence of the explicit `:: forall a. T m (T m a) -> T m a` type
signature, but in fact leaving it off will break this example (from the
T15290d test case):

  class C a where
    c :: Int -> forall b. b -> a

  instance C Int

  instance C Age where
    c = coerce @(Int -> forall b. b -> Int)
               c :: Int -> forall b. b -> Age

That is because the explicit type signature deeply skolemizes the forall-bound
`b`, which wreaks havoc with the `Coercible` solver. An additional visible type
argument of @(Int -> forall b. b -> Age) is enough to prevent this.

Be aware that the use of an explicit type signature doesn't /solve/ this
problem; it just makes it less likely to occur. For example, if a class has
a truly higher-rank type like so:

  class CProblem m where
    op :: (forall b. ... (m b) ...) -> Int

Then the same situation will arise again. But at least it won't arise for the
common case of methods with ordinary, prenex-quantified types.



Note [GND and ambiguity]
~~~~~~~~~~~~~~~~~~~~~~~~
We make an effort to make the code generated through GND be robust w.r.t.
ambiguous type variables. As one example, consider the following example
(from #15637):

  class C a where f :: String
  instance C () where f = "foo"
  newtype T = T () deriving C

A naïve attempt and generating a C T instance would be:

  instance C T where
    f = coerce @String @String f
          :: String

This isn't going to typecheck, however, since GHC doesn't know what to
instantiate the type variable `a` with in the call to `f` in the method body.
(Note that `f :: forall a. String`!) To compensate for the possibility of
ambiguity here, we explicitly instantiate `a` like so:

  instance C T where
    f = coerce @String @String (f @())
          :: String

All better now.


Note [Auxiliary binders]
~~~~~~~~~~~~~~~~~~~~~~~~
We often want to make a top-level auxiliary binding.  E.g. for comparison we haev

  instance Ord T where
    compare a b = $con2tag a `compare` $con2tag b

  $con2tag :: T -> Int
  $con2tag = ...code....

Of course these top-level bindings should all have distinct name, and we are
generating RdrNames here.  We can't just use the TyCon or DataCon to distinguish
because with standalone deriving two imported TyCons might both be called T!
(See #7947.)

So we use package name, module name and the name of the parent
(T in this example) as part of the OccName we generate for the new binding.
To make the symbol names short we take a base62 hash of the full name.

In the past we used the *unique* from the parent, but that's not stable across
recompilations as uniques are nondeterministic.
