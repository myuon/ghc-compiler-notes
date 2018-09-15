[[src]](https://github.com/ghc/ghc/tree/master/compiler/coreSyn/CoreSyn.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


# \subsection{The main data types}


These data types are the heart of the compiler


### Note: Shadowing

While various passes attempt to rename on-the-fly in a manner that
avoids "shadowing" (thereby simplifying downstream optimizations),
neither the simplifier nor any other pass GUARANTEES that shadowing is
avoided. Thus, all passes SHOULD work fine even in the presence of
arbitrary shadowing in their inputs.

In particular, scrutinee variables `x` in expressions of the form
`Case e x t` are often renamed to variables with a prefix
"wild_". These "wild" variables may appear in the body of the
case-expression, and further, may be shadowed within the body.

So the Unique in a Var is not really unique at all.  Still, it's very
useful to give a constant-time equality/ordering for Vars, and to give
a key that can be used to make sets of Vars (VarSet), or mappings from
Vars to other things (VarEnv).   Moreover, if you do want to eliminate
shadowing, you can give a new Unique to an Id without changing its
printable name, which makes debugging easier.

### Note: Literal alternatives

Literal alternatives (LitAlt lit) are always for *un-lifted* literals.
We have one literal, a literal Integer, that is lifted, and we don't
allow in a LitAlt, because LitAlt cases don't do any evaluation. Also
(see Trac #5603) if you say
    case 3 of
      S# x -> ...
      J# _ _ -> ...
(where S#, J# are the constructors for Integer) we don't want the
simplifier calling findAlt with argument (LitAlt 3).  No no.  Integer
literals are an opaque encoding of an algebraic data type, not of
an unlifted literal, like all the others.

### Note: Rules for floating-point comparisons

-------------------------- CoreSyn INVARIANTS ---------------------------

### Note: CoreSyn top-level invariant

See #toplevel_invariant#

### Note: CoreSyn letrec invariant

See #letrec_invariant#

### Note: CoreSyn top-level string literals

As an exception to the usual rule that top-level binders must be lifted,
we allow binding primitive string literals (of type Addr#) of type Addr# at the
top level. This allows us to share string literals earlier in the pipeline and
crucially allows other optimizations in the Core2Core pipeline to fire.
Consider,

  f n = let a::Addr# = "foo"#
        in \x -> blah

In order to be able to inline `f`, we would like to float `a` to the top.
Another option would be to inline `a`, but that would lead to duplicating string
literals, which we want to avoid. See Trac #8472.

The solution is simply to allow top-level unlifted binders. We can't allow
arbitrary unlifted expression at the top-level though, unlifted binders cannot
be thunks, so we just allow string literals.

We allow the top-level primitive string literals to be wrapped in Ticks
in the same way they can be wrapped when nested in an expression.
CoreToSTG currently discards Ticks around top-level primitive string literals.
See Trac #14779.

### Note: Compilation plan for top-level string literals

### Note: Compilation plan for top-level string literals

Here is a summary on how top-level string literals are handled by various
parts of the compilation pipeline.

* In the source language, there is no way to bind a primitive string literal
  at the top level.

### Note: CoreSyn top-level string literals

* In STG, top-level string literals are explicitly represented in the syntax
  tree.

* A top-level string literal may end up exported from a module. In this case,
  in the object file, the content of the exported literal is given a label with
  the _bytes suffix.

### Note: CoreSyn let/app invariant

The let/app invariant
     the right hand side of a non-recursive 'Let', and
     the argument of an 'App',
    /may/ be of unlifted type, but only if
    the expression is ok-for-speculation
    or the 'Let' is for a join point.

This means that the let can be floated around
without difficulty. For example, this is OK:

   y::Int# = x +# 1#

But this is not, as it may affect termination if the
expression is floated out:

   y::Int# = fac 4#

In this situation you should use @case@ rather than a @let@. The function
'CoreUtils.needsCaseBinding' can help you determine which to generate, or
alternatively use 'MkCore.mkCoreLet' rather than this constructor directly,
which will generate a @case@ if necessary

The let/app invariant is initially enforced by mkCoreLet and mkCoreApp in
coreSyn/MkCore.

### Note: CoreSyn case invariants

See #case_invariants#

### Note: Levity polymorphism invariants

The levity-polymorphism invariants are these (as per "Levity Polymorphism",
PLDI '17):

### Note: Invariants on join points

* The type of the argument of an App must not be levity-polymorphic.

A type (t::TYPE r) is "levity polymorphic" if 'r' has any free variables.

For example
  \(r::RuntimeRep). \(a::TYPE r). \(x::a). e
is illegal because x's type has kind (TYPE r), which has 'r' free.

### Note: Levity polymorphism checking

### Note: CoreSyn let goal

* The simplifier tries to ensure that if the RHS of a let is a constructor
  application, its arguments are trivial, so that the constructor can be
  inlined vigorously.

### Note: Type let

See #type_let#

### Note: Empty case alternatives

The alternatives of a case expression should be exhaustive.  But
this exhaustive list can be empty!

### Note: Bottoming expressions

* The possiblity of empty alternatives is one reason we need a type on
  the case expression: if the alternatives are empty we can't get the
  type from the alternatives!

### Note: Bottoming expressions

* We can use the empty-alternative construct to coerce error values from
  one type to another.  For example

    f :: Int -> Int
    f n = error "urk"

### Note: Empty case is trivial

* An empty case is replaced by its scrutinee during the CoreToStg
  conversion; remember STG is un-typed, so there is no need for
  the empty case to do the type conversion.

### Note: Join points

In Core, a *join point* is a specially tagged function whose only occurrences
are saturated tail calls. A tail call can appear in these places:

  1. In the branches (not the scrutinee) of a case
  2. Underneath a let (value or join point)
  3. Inside another join point

We write a join-point declaration as
  join j @a @b x y = e1 in e2,
like a let binding but with "join" instead (or "join rec" for "let rec"). Note
that we put the parameters before the = rather than using lambdas; this is
because it's relevant how many parameters the join point takes *as a join
point.* This number is called the *join arity,* distinct from arity because it
counts types as well as values. Note that a join point may return a lambda! So
  join j x = x + 1
is different from
  join j = \x -> x + 1
The former has join arity 1, while the latter has join arity 0.

The identifier for a join point is called a join id or a *label.* An invocation
is called a *jump.* We write a jump using the jump keyword:

  jump j 3

The words *label* and *jump* are evocative of assembly code (or Cmm) for a
reason: join points are indeed compiled as labeled blocks, and jumps become
actual jumps (plus argument passing and stack adjustment). There is no closure
allocated and only a fraction of the function-call overhead. Hence we would
like as many functions as possible to become join points (see OccurAnal) and
the type rules for join points ensure we preserve the properties that make them
efficient.

In the actual AST, a join point is indicated by the IdDetails of the binder: a
local value binding gets 'VanillaId' but a join point gets a 'JoinId' with its
join arity.

For more details, see the paper:

  Luke Maurer, Paul Downen, Zena Ariola, and Simon Peyton Jones. "Compiling
  without continuations." Submitted to PLDI'17.

  https://www.microsoft.com/en-us/research/publication/compiling-without-continuations/

### Note: Invariants on join points

Join points must follow these invariants:

  1. All occurrences must be tail calls. Each of these tail calls must pass the
     same number of arguments, counting both types and values; we call this the
     "join arity" (to distinguish from regular arity, which only counts values).

  2. For join arity n, the right-hand side must begin with at least n lambdas.
     No ticks, no casts, just lambdas!  C.f. CoreUtils.joinRhsArity.

  2a. Moreover, this same constraint applies to any unfolding of the binder.
     Reason: if we want to push a continuation into the RHS we must push it
     into the unfolding as well.

  3. If the binding is recursive, then all other bindings in the recursive group
     must also be join points.

### Note: The polymorphism rule of join points

However, join points have simpler invariants in other ways

  5. A join point can have an unboxed type without the RHS being
     ok-for-speculation (i.e. drop the let/app invariant)
     e.g.  let j :: Int# = factorial x in ...

  6. A join point can have a levity-polymorphic RHS
     e.g.  let j :: r :: TYPE l = fail void# in ...
     This happened in an intermediate program Trac #13394

Examples:

  join j1  x = 1 + x in jump j (jump j x)  -- Fails 1: non-tail call
  join j1' x = 1 + x in if even a
                          then jump j1 a
                          else jump j1 a b -- Fails 1: inconsistent calls
  join j2  x = flip (+) x in j2 1 2        -- Fails 2: not enough lambdas
  join j2' x = \y -> x + y in j3 1         -- Passes: extra lams ok
  join j @a (x :: a) = x                   -- Fails 4: polymorphic in ret type

Invariant 1 applies to left-hand sides of rewrite rules, so a rule for a join
point must have an exact call as its LHS.

### Note: Join points

### Note: The polymorphism rule of join points

Core Lint will check these invariants, anticipating that any binder whose
OccInfo is marked AlwaysTailCalled will become a join point as soon as the
simplifier (or simpleOptPgm) runs.

### Note: The type of a join point

A join point has the same type it would have as a function. That is, if it takes
an Int and a Bool and its body produces a String, its type is `Int -> Bool ->
String`. Natural as this may seem, it can be awkward. A join point shouldn't be
thought to "return" in the same sense a function does---a jump is one-way. This
is crucial for understanding how case-of-case interacts with join points:

  case (join
          j :: Int -> Bool -> String
          j x y = ...
        in
          jump j z w) of
    "" -> True
    _  -> False

The simplifier will pull the case into the join point (see Note [Case-of-case
and join points] in Simplify):

  join
    j :: Int -> Bool -> Bool -- changed!
    j x y = case ... of "" -> True
                        _  -> False
  in
    jump j z w

The body of the join point now returns a Bool, so the label `j` has to have its
type updated accordingly. Inconvenient though this may be, it has the advantage
that 'CoreUtils.exprType' can still return a type for any expression, including
a jump.

### Note: Invariants on join points

  case (join
          j :: Int -> Bool -> forall a. a
          j x y = ...
        in
          jump j z w @String) of
    "" -> True
    _  -> False

Now we can move the case inward and we only have to change the jump:

  join
    j :: Int -> Bool -> forall a. a
    j x y = case ... of "" -> True
                        _  -> False
  in
    jump j z w @Bool

(Core Lint would still check that the body of the join point has the right type;
that type would simply not be reflected in the join id.)

### Note: The polymorphism rule of join points

### Note: Invariants on join points

  forall a1 ... ak. t1 -> ... -> tn -> r

where its join arity is k+n, none of the type parameters ai may occur free in r.
The most direct explanation is that given

  join j @a1 ... @ak x1 ... xn = e1 in e2

our typing rules require `e1` and `e2` to have the same type. Therefore the type
of `e1`---the return type of the join point---must be the same as the type of
e2. Since the type variables aren't bound in `e2`, its type can't include them,
and thus neither can the type of `e1`.

There's a deeper explanation in terms of the sequent calculus in Section 5.3 of
a previous paper:

  Paul Downen, Luke Maurer, Zena Ariola, and Simon Peyton Jones. "Sequent
  calculus as a compiler intermediate language." ICFP'16.

  https://www.microsoft.com/en-us/research/wp-content/uploads/2016/04/sequent-calculus-icfp16.pdf

The quick version: Consider the CPS term (the paper uses the sequent calculus,
but we can translate readily):

  \k -> join j @a1 ... @ak x1 ... xn = e1 k in e2 k

Since `j` is a join point, it doesn't bind a continuation variable but reuses
the variable `k` from the context. But the parameters `ai` are not in `k`'s
scope, and `k`'s type determines the return type of `j`; thus the `ai`s don't
appear in the return type of `j`. (Also, since `e1` and `e2` are passed the same
continuation, they must have the same type; hence the direct explanation above.)

# In/Out type synonyms


 Many passes apply a substitution, and it's very handy to have type
   synonyms to remind us whether or not the substitution has been applied 

# Ticks


# Orphans


### Note: Orphans

Class instances, rules, and family instances are divided into orphans
and non-orphans.  Roughly speaking, an instance/rule is an orphan if
its left hand side mentions nothing defined in this module.  Orphan-hood
has two major consequences

 * A module that contains orphans is called an "orphan module".  If
   the module being compiled depends (transitively) on an oprhan
   module M, then M.hi is read in regardless of whether M is oherwise
   needed. This is to ensure that we don't miss any instance decls in
   M.  But it's painful, because it means we need to keep track of all
   the orphan modules below us.

 * A non-orphan is not finger-printed separately.  Instead, for
   fingerprinting purposes it is treated as part of the entity it
   mentions on the LHS.  For example
      data T = T1 | T2
      instance Eq T where ....
   The instance (Eq T) is incorprated as part of T's fingerprint.

   In contrast, orphans are all fingerprinted together in the
   mi_orph_hash field of the ModIface.

   See MkIface.addFingerprints.

Orphan-hood is computed
  * For class instances:
      when we make a ClsInst
    (because it is needed during instance lookup)

  * For rules and family instances:
       when we generate an IfaceRule (MkIface.coreRuleToIfaceRule)
                     or IfaceFamInst (MkIface.instanceToIfaceInst)


# \subsection{Transformation rules}


The CoreRule type and its friends are dealt with mainly in CoreRules,
but CoreFVs, Subst, PprCore, CoreTidy also inspect the representation.


# \subsection{Vectorisation declarations}


Representation of desugared vectorisation declarations that are fed to the vectoriser (via
'ModGuts').


# Unfoldings


The @Unfolding@ type is declared here to avoid numerous loops


### Note: Historical note: unfoldings for wrappers

We used to have a nice clever scheme in interface files for
wrappers. A wrapper's unfolding can be reconstructed from its worker's
id and its strictness. This decreased .hi file size (sometimes
significantly, for modules like GHC.Classes with many high-arity w/w
splits) and had a slight corresponding effect on compile times.

However, when we added the second demand analysis, this scheme lead to
some Core lint errors. The second analysis could change the strictness
signatures, which sometimes resulted in a wrapper's regenerated
unfolding applying the wrapper to too many arguments.

Instead of repairing the clever .hi scheme, we abandoned it in favor
of simplicity. The .hi sizes are usually insignificant (excluding the
+1M for base libraries), and compile time barely increases (~+1% for
nofib). The nicer upshot is that the UnfoldingSource no longer mentions
an Id, so, eg, substitutions need not traverse them.

### Note: DFun unfoldings

The Arity in a DFunUnfolding is total number of args (type and value)
that the DFun needs to produce a dictionary.  That's not necessarily
related to the ordinary arity of the dfun Id, esp if the class has
one method, so the dictionary is represented by a newtype.  Example

     class C a where { op :: a -> Int }
     instance C a -> C [a] where op xs = op (head xs)

The instance translates to

     $dfCList :: forall a. C a => C [a]  -- Arity 2!
     $dfCList = /\a.\d. $copList {a} d |> co

     $copList :: forall a. C a => [a] -> Int  -- Arity 2!
     $copList = /\a.\d.\xs. op {a} d (head xs)

Now we might encounter (op (dfCList {ty} d) a1 a2)
and we want the (op (dfList {ty} d)) rule to fire, because $dfCList
has all its arguments, even though its (value) arity is 2.  That's
why we record the number of expected arguments in the DFunUnfolding.

Note that although it's an Arity, it's most convenient for it to give
the *total* number of arguments, both type and value.  See the use
site in exprIsConApp_maybe.


### Note: Fragile unfoldings

An unfolding is "fragile" if it mentions free variables (and hence would
need substitution) or might be affected by optimisation.  The non-fragile
ones are

   NoUnfolding, BootUnfolding

   OtherCon {}    If we know this binder (say a lambda binder) will be
                  bound to an evaluated thing, we want to retain that
                  info in simpleOptExpr; see Trac #13077.

We consider even a StableUnfolding as fragile, because it needs substitution.

### Note: InlineStable

# \subsection{Useful synonyms}


### Note: CoreProgram

The top level bindings of a program, a CoreProgram, are represented as
a list of CoreBind

 * Later bindings in the list can refer to earlier ones, but not vice
   versa.  So this is OK
      NonRec { x = 4 }
      Rec { p = ...q...x...
          ; q = ...p...x }
      Rec { f = ...p..x..f.. }
      NonRec { g = ..f..q...x.. }
   But it would NOT be ok for 'f' to refer to 'g'.

 * The occurrence analyser does strongly-connected component analysis
   on each Rec binding, and splits it into a sequence of smaller
   bindings where possible.  So the program typically starts life as a
   single giant Rec, which is then dependency-analysed into smaller
   chunks.


# \subsection{Tagging}


# \subsection{Core-constructing functions with checking}


# Getting a result type


These are defined here to avoid a module loop between CoreUtils and CoreFVs



# \subsection{Simple access functions}


# \subsection{Predicates}


At one time we optionally carried type arguments through to runtime.
@isRuntimeVar v@ returns if (Lam v _) really becomes a lambda at runtime,
i.e. if type applications are actual lambdas because types are kept around
at runtime.  Similarly isRuntimeArg.


# \subsection{Annotated core}
