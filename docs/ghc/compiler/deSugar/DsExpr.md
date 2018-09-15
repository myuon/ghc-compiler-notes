[[src]](https://github.com/ghc/ghc/tree/master/compiler/deSugar/DsExpr.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Desugaring exporessions.


# dsLocalBinds, dsValBinds


# \subsection[DsExpr-vars-and-cons]{Variables, constructors, literals}


### Note: Desugaring vars

In one situation we can get a *coercion* variable in a HsVar, namely
the support method for an equality superclass:
   class (a~b) => C a b where ...
   instance (blah) => C (T a) (T b) where ..
Then we get
   $dfCT :: forall ab. blah => C (T a) (T b)
   $dfCT ab blah = MkC ($c$p1C a blah) ($cop a blah)

   $c$p1C :: forall ab. blah => (T a ~ T b)
   $c$p1C ab blah = let ...; g :: T a ~ T b = ... } in g

That 'g' in the 'in' part is an evidence variable, and when
converting to core it must become a CO.

Operator sections.  At first it looks as if we can convert
\begin{verbatim}
        (expr op)
\end{verbatim}
to
\begin{verbatim}
        \x -> op expr x
\end{verbatim}

But no!  expr might be a redex, and we can lose laziness badly this
way.  Consider
\begin{verbatim}
        map (expr op) xs
\end{verbatim}
for example.  So we convert instead to
\begin{verbatim}
        let y = expr in \x -> op y x
\end{verbatim}
If \tr{expr} is actually just a variable, say, then the simplifier
will sort it out.



\noindent
\underline{\bf Various data construction things}
             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# Static Pointers

### Note: Grand plan for static forms

    g = ... static f ...
==>
    g = ... makeStatic loc f ...



\noindent
\underline{\bf Record construction and update}
             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For record construction we do this (assuming T has three arguments)
\begin{verbatim}
        T { op2 = e }
==>
        let err = /\a -> recConErr a
        T (recConErr t1 "M.hs/230/op1")
          e
          (recConErr t1 "M.hs/230/op3")
\end{verbatim}
@recConErr@ then converts its argument string into a proper message
before printing it as
\begin{verbatim}
        M.hs, line 230: missing field op1 was evaluated
\end{verbatim}

We also handle @C{}@ as valid construction syntax for an unlabelled
constructor @C@, setting all of @C@'s fields to bottom.



Record update is a little harder. Suppose we have the decl:
\begin{verbatim}
        data T = T1 {op1, op2, op3 :: Int}
               | T2 {op4, op2 :: Int}
               | T3
\end{verbatim}
Then we translate as follows:
\begin{verbatim}
        r { op2 = e }
===>
        let op2 = e in
        case r of
          T1 op1 _ op3 -> T1 op1 op2 op3
          T2 op4 _     -> T2 op4 op2
          other        -> recUpdError "M.hs/230"
\end{verbatim}
It's important that we use the constructor Ids for @T1@, @T2@ etc on the
RHSs, and do not generate a Core constructor application directly, because the constructor
might do some argument-evaluation first; and may have to throw away some
dictionaries.

### Note: Update for GADTs

Consider
   data T a b where
     T1 :: { f1 :: a } -> T a Int

Then the wrapper function for T1 has type
   $WT1 :: a -> T a Int
But if x::T a b, then
   x { f1 = v } :: T a b   (not T a Int!)
So we need to cast (T a Int) to (T a b).  Sigh.




%--------------------------------------------------------------------

### Note: Desugaring explicit lists

Explicit lists are desugared in a cleverer way to prevent some
fruitless allocations.  Essentially, whenever we see a list literal
[x_1, ..., x_n] we generate the corresponding expression in terms of
build:

Explicit lists (literals) are desugared to allow build/foldr fusion when
beneficial. This is a bit of a trade-off,

 * build/foldr fusion can generate far larger code than the corresponding
   cons-chain (e.g. see #11707)

 * even when it doesn't produce more code, build can still fail to fuse,
   requiring that the simplifier do more work to bring the expression
   back into cons-chain form; this costs compile time

 * when it works, fusion can be a significant win. Allocations are reduced
   by up to 25% in some nofib programs. Specifically,

        Program           Size    Allocs   Runtime  CompTime
        rewrite          +0.0%    -26.3%      0.02     -1.8%
           ansi          -0.3%    -13.8%      0.00     +0.0%
           lift          +0.0%     -8.7%      0.00     -2.3%

At the moment we use a simple heuristic to determine whether build will be
fruitful: for small lists we assume the benefits of fusion will be worthwhile;
for long lists we assume that the benefits will be outweighted by the cost of
code duplication. This magic length threshold is @maxBuildLength@. Also, fusion
won't work at all if rewrite rules are disabled, so we don't use the build-based
desugaring in this case.

We used to have a more complex heuristic which would try to break the list into
"static" and "dynamic" parts and only build-desugar the dynamic part.
Unfortunately, determining "static-ness" reliably is a bit tricky and the
heuristic at times produced surprising behavior (see #11710) so it was dropped.


 | The longest list length which we will desugar using @build@.

### Note: Desugaring explicit lists

The effect of reducing this number will be that 'build' fusion is applied
less often. From a runtime performance perspective, applying 'build' more
liberally on "moderately" sized lists should rarely hurt and will often it can
only expose further optimization opportunities; if no fusion is possible it will
eventually get rule-rewritten back to a list). We do, however, pay in compile
time.



Desugar 'do' and 'mdo' expressions (NOT list comprehensions, they're
handled in DsListComp).  Basically does the translation given in the
Haskell 98 report:


# Desugaring Variables


# \subsection{Errors and contexts}


# Forced eta expansion and levity polymorphism


### Note: Detecting forced eta expansion

### Note: Levity polymorphism invariants

However, we absolutely cannot do this for functions that have no
binding (i.e., say True to Id.hasNoBinding), like primops and unboxed
tuple constructors. These get eta-expanded in CorePrep.maybeSaturate.

Detecting when this is about to happen is a bit tricky, though. When
the desugarer is looking at the Id itself (let's be concrete and
suppose we have (#,#)), we don't know whether it will be levity
polymorphic. So the right spot seems to be to look after the Id has
been applied to its type arguments. To make the algorithm efficient,
it's important to be able to spot ((#,#) @a @b @c @d) without looking
past all the type arguments. We thus require that
  * The body of an HsWrap is not an HsWrap.
With that representation invariant, we simply look inside every HsWrap
to see if its body is an HsVar whose Id hasNoBinding. Then, we look
at the wrapped type. If it has any levity polymorphic arguments, reject.

Interestingly, this approach does not look to see whether the Id in
question will be eta expanded. The logic is this:
  * Either the Id in question is saturated or not.
  * If it is, then it surely can't have levity polymorphic arguments.
    If its wrapped type contains levity polymorphic arguments, reject.
  * If it's not, then it can't be eta expanded with levity polymorphic
    argument. If its wrapped type contains levity polymorphic arguments, reject.
So, either way, we're good to reject.

Wrinkle
~~~~~~~

Not all polymorphic Ids are wrapped in
HsWrap, due to the lazy instantiation of TypeApplications. (See "Visible type
application", ESOP '16.) But if we spot a levity-polymorphic hasNoBinding Id
without a wrapper, then that is surely problem and we can reject.

We thus have a parameter to `dsExpr` that tracks whether or not we are
directly in an HsWrap. If we find a levity-polymorphic hasNoBinding Id when
we're not directly in an HsWrap, reject.

