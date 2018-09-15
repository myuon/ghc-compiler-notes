[[src]](https://github.com/ghc/ghc/tree/master/compiler/typecheck/TcGenDeriv.hs)

    %
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


TcGenDeriv: Generating derived instance declarations

This module is nominally ``subordinate'' to @TcDeriv@, which is the
``official'' interface to deriving-related things.

This is where we do all the grimy bindings' generation.


# Eq instances


Here are the heuristics for the code we generate for @Eq@. Let's
assume we have a data type with some (possibly zero) nullary data
constructors and some ordinary, non-nullary ones (the rest, also
possibly zero of them).  Here's an example, with both \tr{N}ullary and
\tr{O}rdinary data cons.

  data Foo ... = N1 | N2 ... | Nn | O1 a b | O2 Int | O3 Double b b | ...

* For the ordinary constructors (if any), we emit clauses to do The
  Usual Thing, e.g.,:

    (==) (O1 a1 b1)    (O1 a2 b2)    = a1 == a2 && b1 == b2
    (==) (O2 a1)       (O2 a2)       = a1 == a2
    (==) (O3 a1 b1 c1) (O3 a2 b2 c2) = a1 == a2 && b1 == b2 && c1 == c2

  Note: if we're comparing unlifted things, e.g., if 'a1' and
  'a2' are Float#s, then we have to generate
       case (a1 `eqFloat#` a2) of r -> r
  for that particular test.

* If there are a lot of (more than ten) nullary constructors, we emit a
  catch-all clause of the form:

      (==) a b  = case (con2tag_Foo a) of { a# ->
                  case (con2tag_Foo b) of { b# ->
                  case (a# ==# b#)     of {
                    r -> r }}}

  If con2tag gets inlined this leads to join point stuff, so
  it's better to use regular pattern matching if there aren't too
  many nullary constructors.  "Ten" is arbitrary, of course

* If there aren't any nullary constructors, we emit a simpler
  catch-all:

     (==) a b  = False

* For the @(/=)@ method, we normally just use the default method.
  If the type is an enumeration type, we could/may/should? generate
  special code that calls @con2tag_Foo@, much like for @(==)@ shown
  above.

We thought about doing this: If we're also deriving 'Ord' for this
tycon, we generate:
  instance ... Eq (Foo ...) where
    (==) a b  = case (compare a b) of { _LT -> False; _EQ -> True ; _GT -> False}
    (/=) a b  = case (compare a b) of { _LT -> True ; _EQ -> False; _GT -> True }
However, that requires that (Ord <whatever>) was put in the context
for the instance decl, which it probably wasn't, so the decls
produced don't get through the typechecker.


# Ord instances


### Note: Generating Ord instances

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

### Note: Game plan for deriving Ord

It's a bad idea to define only 'compare', and build the other binary
comparisons on top of it; see Trac #2130, #4019.  Reason: we don't
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



# Enum instances


@Enum@ can only be derived for enumeration types.  For a type
\begin{verbatim}
data Foo ... = N1 | N2 | ... | Nn
\end{verbatim}

we use both @con2tag_Foo@ and @tag2con_Foo@ functions, as well as a
@maxtag_Foo@ variable (all generated by @gen_tag_n_con_binds@).

\begin{verbatim}
instance ... Enum (Foo ...) where
    succ x   = toEnum (1 + fromEnum x)
    pred x   = toEnum (fromEnum x - 1)

    toEnum i = tag2con_Foo i

    enumFrom a = map tag2con_Foo [con2tag_Foo a .. maxtag_Foo]

    -- or, really...
    enumFrom a
      = case con2tag_Foo a of
          a# -> map tag2con_Foo (enumFromTo (I# a#) maxtag_Foo)

   enumFromThen a b
     = map tag2con_Foo [con2tag_Foo a, con2tag_Foo b .. maxtag_Foo]

    -- or, really...
    enumFromThen a b
      = case con2tag_Foo a of { a# ->
        case con2tag_Foo b of { b# ->
        map tag2con_Foo (enumFromThenTo (I# a#) (I# b#) maxtag_Foo)
        }}
\end{verbatim}

For @enumFromTo@ and @enumFromThenTo@, we use the default methods.


# Bounded instances


# Ix instances


Deriving @Ix@ is only possible for enumeration types and
single-constructor types.  We deal with them in turn.

For an enumeration type, e.g.,
\begin{verbatim}
    data Foo ... = N1 | N2 | ... | Nn
\end{verbatim}
things go not too differently from @Enum@:
\begin{verbatim}
instance ... Ix (Foo ...) where
    range (a, b)
      = map tag2con_Foo [con2tag_Foo a .. con2tag_Foo b]

    -- or, really...
    range (a, b)
      = case (con2tag_Foo a) of { a# ->
        case (con2tag_Foo b) of { b# ->
        map tag2con_Foo (enumFromTo (I# a#) (I# b#))
        }}

    -- Generate code for unsafeIndex, because using index leads
    -- to lots of redundant range tests
    unsafeIndex c@(a, b) d
      = case (con2tag_Foo d -# con2tag_Foo a) of
               r# -> I# r#

    inRange (a, b) c
      = let
            p_tag = con2tag_Foo c
        in
        p_tag >= con2tag_Foo a && p_tag <= con2tag_Foo b

    -- or, really...
    inRange (a, b) c
      = case (con2tag_Foo a)   of { a_tag ->
        case (con2tag_Foo b)   of { b_tag ->
        case (con2tag_Foo c)   of { c_tag ->
        if (c_tag >=# a_tag) then
          c_tag <=# b_tag
        else
          False
        }}}
\end{verbatim}
(modulo suitable case-ification to handle the unlifted tags)

For a single-constructor type (NB: this includes all tuples), e.g.,
\begin{verbatim}
    data Foo ... = MkFoo a b Int Double c c
\end{verbatim}
we follow the scheme given in Figure~19 of the Haskell~1.2 report
(p.~147).


# Read instances


Example

  infix 4 %%
  data T = Int %% Int
         | T1 { f1 :: Int }
         | T2 T

instance Read T where
  readPrec =
    parens
    ( prec 4 (
        do x <- ReadP.step Read.readPrec
           expectP (Symbol "%%")
           y <- ReadP.step Read.readPrec
           return (x %% y))
      +++
      prec (appPrec+1) (
        -- Note the "+1" part; "T2 T1 {f1=3}" should parse ok
        -- Record construction binds even more tightly than application
        do expectP (Ident "T1")
           expectP (Punc '{')
           x          <- Read.readField "f1" (ReadP.reset readPrec)
           expectP (Punc '}')
           return (T1 { f1 = x }))
      +++
      prec appPrec (
        do expectP (Ident "T2")
           x <- ReadP.step Read.readPrec
           return (T2 x))
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault

### Note: Use expectP

Note that we use
   expectP (Ident "T1")
rather than
   Ident "T1" <- lexP
The latter desugares to inline code for matching the Ident and the
string, and this can be very voluminous. The former is much more
compact.  Cf Trac #7258, although that also concerned non-linearity in
the occurrence analyser, a separate issue.

### Note: Read for empty data types

What should we get for this?  (Trac #7931)
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


# Show instances


Example

    infixr 5 :^:

    data Tree a =  Leaf a  |  Tree a :^: Tree a

    instance (Show a) => Show (Tree a) where

        showsPrec d (Leaf m) = showParen (d > app_prec) showStr
          where
             showStr = showString "Leaf " . showsPrec (app_prec+1) m

        showsPrec d (u :^: v) = showParen (d > up_prec) showStr
          where
             showStr = showsPrec (up_prec+1) u .
                       showString " :^: "      .
                       showsPrec (up_prec+1) v
                -- Note: right-associativity of :^: ignored

    up_prec  = 5    -- Precedence of :^:
    app_prec = 10   -- Application has precedence one more than
                    -- the most tightly-binding operator


# Data instances


From the data type

  data T a b = T1 a b | T2

we generate

  $cT1 = mkDataCon $dT "T1" Prefix
  $cT2 = mkDataCon $dT "T2" Prefix
  $dT  = mkDataType "Module.T" [] [$con_T1, $con_T2]
  -- the [] is for field labels.

  instance (Data a, Data b) => Data (T a b) where
    gfoldl k z (T1 a b) = z T `k` a `k` b
    gfoldl k z T2           = z T2
    -- ToDo: add gmapT,Q,M, gfoldr

    gunfold k z c = case conIndex c of
                        I# 1# -> k (k (z T1))
                        I# 2# -> z T2

    toConstr (T1 _ _) = $cT1
    toConstr T2       = $cT2

    dataTypeOf _ = $dT

    dataCast1 = gcast1   -- If T :: * -> *
    dataCast2 = gcast2   -- if T :: * -> * -> *


# Lift instances


Example:

    data Foo a = Foo a | a :^: a deriving Lift

    ==>

    instance (Lift a) => Lift (Foo a) where
        lift (Foo a)
          = appE
              (conE
                (mkNameG_d "package-name" "ModuleName" "Foo"))
              (lift a)
        lift (u :^: v)
          = infixApp
              (lift u)
              (conE
                (mkNameG_d "package-name" "ModuleName" ":^:"))
              (lift v)

Note that (mkNameG_d "package-name" "ModuleName" "Foo") is equivalent to what
'Foo would be when using the -XTemplateHaskell extension. To make sure that
-XDeriveLift can be used on stage-1 compilers, however, we explicitly invoke
makeG_d.


# Newtype-deriving instances


### Note: Newtype-deriving instances

We take every method in the original instance and `coerce` it to fit
into the derived instance. We need a type annotation on the argument
to `coerce` to make it obvious what instantiation of the method we're
coercing from.  So from, say,
  class C a b where
    op :: a -> [b] -> Int

  newtype T x = MkT <rep-ty>

  instance C a <rep-ty> => C a (T x) where
    op = coerce @ (a -> [<rep-ty>] -> Int)
                @ (a -> [T x]      -> Int)
                op

Notice that we give the 'coerce' two explicitly-visible type arguments
to say how it should be instantiated.  Recall

  coerce :: Coeercible a b => a -> b

By giving it explicit type arguments we deal with the case where
'op' has a higher rank type, and so we must instantiate 'coerce' with
a polytype.  E.g.
   class C a where op :: forall b. a -> b -> b
   newtype T x = MkT <rep-ty>
   instance C <rep-ty> => C (T x) where
     op = coerce @ (forall b. <rep-ty> -> b -> b)
                 @ (forall b. T x -> b -> b)
                op

The type checker checks this code, and it currently requires
-XImpredicativeTypes to permit that polymorphic type instantiation,
so we have to switch that flag on locally in TcDeriv.genInst.

See #8503 for more discussion.

### Note: Newtype-deriving trickiness

Consider (Trac #12768):
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
  opN = coerce @(D [a]   => [a] -> [a])
               @(D (N a) => [N a] -> [N a]
               opList

But there is no reason to suppose that (D [a]) and (D (N a))
are inter-coercible; these instances might completely different.
So GHC rightly rejects this code.


# \subsection{Generating extra binds (@con2tag@ and @tag2con@)}


\begin{verbatim}
data Foo ... = ...

con2tag_Foo :: Foo ... -> Int#
tag2con_Foo :: Int -> Foo ...   -- easier if Int, not Int#
maxtag_Foo  :: Int              -- ditto (NB: not unlifted)
\end{verbatim}

The `tags' here start at zero, hence the @fIRST_TAG@ (currently one)
fiddling around.


# \subsection{Utility bits for generating bindings}


of

### Note: Auxiliary binders

We often want to make a top-level auxiliary binding.  E.g. for comparison we haev

  instance Ord T where
    compare a b = $con2tag a `compare` $con2tag b

  $con2tag :: T -> Int
  $con2tag = ...code....

Of course these top-level bindings should all have distinct name, and we are
generating RdrNames here.  We can't just use the TyCon or DataCon to distinguish
because with standalone deriving two imported TyCons might both be called T!
(See Trac #7947.)

So we use package name, module name and the name of the parent
(T in this example) as part of the OccName we generate for the new binding.
To make the symbol names short we take a base62 hash of the full name.

In the past we used the *unique* from the parent, but that's not stable across
recompilations as uniques are nondeterministic.
