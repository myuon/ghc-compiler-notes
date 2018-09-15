[[src]](https://github.com/ghc/ghc/tree/master/compiler/prelude/TysWiredIn.hs)

(c) The GRASP Project, Glasgow University, 1994-1998

# Wired-in knowledge about {\em non-primitive} types

### Note: Wiring in RuntimeRep

The RuntimeRep type (and friends) in GHC.Types has a bunch of constructors,
making it a pain to wire in. To ease the pain somewhat, we use lists of
the different bits, like Uniques, Names, DataCons. These lists must be
kept in sync with each other. The rule is this: use the order as declared
in GHC.Types. All places where such lists exist should contain a reference
to this Note, so a search for this Note's name should find all the lists.

# \subsection{Wired in type constructors}


If you change which things are wired in, make sure you change their
names in PrelNames, so they use wTcQual, wDataQual, etc


### Note: Any types

The type constructor Any,

    type family Any :: k where { }

It has these properties:

  * Note that 'Any' is kind polymorphic since in some program we may
    need to use Any to fill in a type variable of some kind other than *
    (see #959 for examples).  Its kind is thus `forall k. k``.

  * It is defined in module GHC.Types, and exported so that it is
    available to users.  For this reason it's treated like any other
    wired-in type:
      - has a fixed unique, anyTyConKey,
      - lives in the global name cache

  * It is a *closed* type family, with no instances.  This means that
    if   ty :: '(k1, k2)  we add a given coercion
             g :: ty ~ (Fst ty, Snd ty)
    If Any was a *data* type, then we'd get inconsistency because 'ty'
    could be (Any '(k1,k2)) and then we'd have an equality with Any on
    one side and '(,) on the other. See also #9097 and #9636.

  * When instantiated at a lifted type it is inhabited by at least one value,
    namely bottom

  * You can safely coerce any /lifted/ type to Any, and back with unsafeCoerce.

  * It does not claim to be a *data* type, and that's important for
    the code generator, because the code gen may *enter* a data value
    but never enters a function value.

  * It is wired-in so we can easily refer to it where we don't have a name
    environment (e.g. see Rules.matchRule for one example)

### Note: TYPE and RuntimeRep

It's used to instantiate un-constrained type variables after type checking. For
example, 'length' has type

  length :: forall a. [a] -> Int

and the list datacon for the empty list has type

  [] :: forall a. [a]

In order to compose these two terms as @length []@ a type
application is required, but there is no constraint on the
choice.  In this situation GHC uses 'Any',

> length (Any *) ([] (Any *))

Above, we print kinds explicitly, as if with --fprint-explicit-kinds.

The Any tycon used to be quite magic, but we have since been able to
implement it merely with an empty kind polymorphic type family. See #10886 for a
bit of history.


# \subsection{mkWiredInTyCon}


# Kinds


# Stuff for dealing with tuples


### Note: How tuples work]  See also Note [Known-key names

* There are three families of tuple TyCons and corresponding
  DataCons, expressed by the type BasicTypes.TupleSort:
    data TupleSort = BoxedTuple | UnboxedTuple | ConstraintTuple

* All three families are AlgTyCons, whose AlgTyConRhs is TupleTyCon

* BoxedTuples
    - A wired-in type
    - Data type declarations in GHC.Tuple
    - The data constructors really have an info table

* UnboxedTuples
    - A wired-in type
    - Have a pretend DataCon, defined in GHC.Prim,
      but no actual declaration and no info table

* ConstraintTuples
    - Are known-key rather than wired-in. Reason: it's awkward to
      have all the superclass selectors wired-in.
    - Declared as classes in GHC.Classes, e.g.
         class (c1,c2) => (c1,c2)
    - Given constraints: the superclasses automatically become available
    - Wanted constraints: there is a built-in instance
         instance (c1,c2) => (c1,c2)
      See TcInteract.matchCTuple
    - Currently just go up to 62; beyond that
      you have to use manual nesting
    - Their OccNames look like (%,,,%), so they can easily be
      distinguished from term tuples.  But (following Haskell) we
      pretty-print saturated constraint tuples with round parens;
      see BasicTypes.tupleParens.

* In quite a lot of places things are restrcted just to
  BoxedTuple/UnboxedTuple, and then we used BasicTypes.Boxity to distinguish
  E.g. tupleTyCon has a Boxity argument

* When looking up an OccName in the original-name cache
  (IfaceEnv.lookupOrigNameCache), we spot the tuple OccName to make sure
  we get the right wired-in name.  This guy can't tell the difference
  between BoxedTuple and ConstraintTuple (same OccName!), so tuples
  are not serialised into interface files using OccNames at all.

### Note: Symbol table representation of names

### Note: One-tuples

GHC supports both boxed and unboxed one-tuples:
 - Unboxed one-tuples are sometimes useful when returning a
   single value after CPR analysis
 - A boxed one-tuple is used by DsUtils.mkSelectorBinds, when
   there is just one binder
Basically it keeps everythig uniform.

However the /naming/ of the type/data constructors for one-tuples is a
bit odd:
  3-tuples:  (,,)   (,,)#
  2-tuples:  (,)    (,)#
  1-tuples:  ??
  0-tuples:  ()     ()#

Zero-tuples have used up the logical name. So we use 'Unit' and 'Unit#'
for one-tuples.  So in ghc-prim:GHC.Tuple we see the declarations:
  data ()     = ()
  data Unit a = Unit a
  data (a,b)  = (a,b)

NB (Feb 16): for /constraint/ one-tuples I have 'Unit%' but no class
decl in GHC.Classes, so I think this part may not work properly. But
it's unused I think.


# Unboxed sums


# Equality types and classes


# Kinds and RuntimeRep


# The boxed primitive types: Char, Int, etc


### Note: Boxing primitive types

For a handful of primitive types (Int, Char, Word, Flaot, Double),
we can readily box and an unboxed version (Int#, Char# etc) using
the corresponding data constructor.  This is useful in a couple
of places, notably let-floating 

# The Bool type


An ordinary enumeration type, but deeply wired in.  There are no
magical operations on @Bool@ (just the regular Prelude code).

{\em BEGIN IDLE SPECULATION BY SIMON}

This is not the only way to encode @Bool@.  A more obvious coding makes
@Bool@ just a boxed up version of @Bool#@, like this:
\begin{verbatim}
type Bool# = Int#
data Bool = MkBool Bool#
\end{verbatim}

Unfortunately, this doesn't correspond to what the Report says @Bool@
looks like!  Furthermore, we get slightly less efficient code (I
think) with this coding. @gtInt@ would look like this:

\begin{verbatim}
gtInt :: Int -> Int -> Bool
gtInt x y = case x of I# x# ->
            case y of I# y# ->
            case (gtIntPrim x# y#) of
                b# -> MkBool b#
\end{verbatim}

Notice that the result of the @gtIntPrim@ comparison has to be turned
into an integer (here called @b#@), and returned in a @MkBool@ box.

The @if@ expression would compile to this:
\begin{verbatim}
case (gtInt x y) of
  MkBool b# -> case b# of { 1# -> e1; 0# -> e2 }
\end{verbatim}

I think this code is a little less efficient than the previous code,
but I'm not certain.  At all events, corresponding with the Report is
important.  The interesting thing is that the language is expressive
enough to describe more than one alternative; and that a type doesn't
necessarily need to be a straightforwardly boxed version of its
primitive counterpart.

{\em END IDLE SPECULATION BY SIMON}


# The List type
   Special syntax, deeply wired in,
   but otherwise an ordinary algebraic data type


       data [] a = [] | a : (List a)


 Declared infix 

# The tuple types


The tuple types are definitely magic, because they form an infinite
family.

\begin{itemize}
\item
They have a special family of type constructors, of type @TyCon@
These contain the tycon arity, but don't require a Unique.

\item
They have a special family of constructors, of type
@Id@. Again these contain their arity but don't need a Unique.

\item
There should be a magic way of generating the info tables and
entry code for all tuples.

But at the moment we just compile a Haskell source
file\srcloc{lib/prelude/...} containing declarations like:
\begin{verbatim}
data Tuple0             = Tup0
data Tuple2  a b        = Tup2  a b
data Tuple3  a b c      = Tup3  a b c
data Tuple4  a b c d    = Tup4  a b c d
...
\end{verbatim}
The print-names associated with the magic @Id@s for tuple constructors
``just happen'' to be the same as those generated by these
declarations.

\item
The instance environment should have a magic way to know
that each tuple type is an instances of classes @Eq@, @Ix@, @Ord@ and
so on. \ToDo{Not implemented yet.}

\item
There should also be a way to generate the appropriate code for each
of these instances, but (like the info tables and entry code) it is
done by enumeration\srcloc{lib/prelude/InTup?.hs}.
\end{itemize}


# The sum types


# The parallel-array type,  [::]


Special syntax for parallel arrays needs some wired in definitions.
