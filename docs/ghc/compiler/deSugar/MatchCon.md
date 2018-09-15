[[src]](https://github.com/ghc/ghc/tree/master/compiler/deSugar/MatchCon.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Pattern-matching constructors



We are confronted with the first column of patterns in a set of
equations, all beginning with constructors from one ``family'' (e.g.,
@[]@ and @:@ make up the @List@ ``family'').  We want to generate the
alternatives for a @Case@ expression.  There are several choices:
\begin{enumerate}
\item
Generate an alternative for every constructor in the family, whether
they are used in this set of equations or not; this is what the Wadler
chapter does.
\begin{description}
\item[Advantages:]
(a)~Simple.  (b)~It may also be that large sparsely-used constructor
families are mainly handled by the code for literals.
\item[Disadvantages:]
(a)~Not practical for large sparsely-used constructor families, e.g.,
the ASCII character set.  (b)~Have to look up a list of what
constructors make up the whole family.
\end{description}

\item
Generate an alternative for each constructor used, then add a default
alternative in case some constructors in the family weren't used.
\begin{description}
\item[Advantages:]
(a)~Alternatives aren't generated for unused constructors.  (b)~The
STG is quite happy with defaults.  (c)~No lookup in an environment needed.
\item[Disadvantages:]
(a)~A spurious default alternative may be generated.
\end{description}

\item
``Do it right:'' generate an alternative for each constructor used,
and add a default alternative if all constructors in the family
weren't used.
\begin{description}
\item[Advantages:]
(a)~You will get cases with only one alternative (and no default),
which should be amenable to optimisation.  Tuples are a common example.
\item[Disadvantages:]
(b)~Have to look up constructor families in TDE (as above).
\end{description}
\end{enumerate}

We are implementing the ``do-it-right'' option for now.  The arguments
to @matchConFamily@ are the same as to @match@; the extra @Int@
returned is the number of constructors in the family.

The function @matchConFamily@ is concerned with this
have-we-used-all-the-constructors? question; the local function
@match_cons_used@ does all the real work.


### Note: Record patterns

Consider
         data T = T { x,y,z :: Bool }

         f (T { y=True, x=False }) = ...

We must match the patterns IN THE ORDER GIVEN, thus for the first
one we match y=True before x=False.  See Trac #246; or imagine
matching against (T { y=False, x=undefined }): should fail without
touching the undefined.

Now consider:

         f (T { y=True, x=False }) = ...
         f (T { x=True, y= False}) = ...

In the first we must test y first; in the second we must test x
first.  So we must divide even the equations for a single constructor
T into sub-goups, based on whether they match the same field in the
same order.  That's what the (groupBy compatible_pats) grouping.

All non-record patterns are "compatible" in this sense, because the
positional patterns (T a b) and (a `T` b) all match the arguments
in order.  Also T {} is special because it's equivalent to (T _ _).
Hence the (null rpats) checks here and there.

### Note: Existentials in shift_con_pat

Consider
        data T = forall a. Ord a => T a (a->Int)

        f (T x f) True  = ...expr1...
        f (T y g) False = ...expr2..

When we put in the tyvars etc we get

        f (T a (d::Ord a) (x::a) (f::a->Int)) True =  ...expr1...
        f (T b (e::Ord b) (y::a) (g::a->Int)) True =  ...expr2...

After desugaring etc we'll get a single case:

        f = \t::T b::Bool ->
            case t of
               T a (d::Ord a) (x::a) (f::a->Int)) ->
            case b of
                True  -> ...expr1...
                False -> ...expr2...

*** We have to substitute [a/b, d/e] in expr2! **
Hence
                False -> ....((/\b\(e:Ord b).expr2) a d)....

Originally I tried to use
        (\b -> let e = d in expr2) a
to do this substitution.  While this is "correct" in a way, it fails
Lint, because e::Ord b but d::Ord a.

