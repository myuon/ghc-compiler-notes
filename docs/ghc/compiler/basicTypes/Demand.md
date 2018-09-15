[[src]](https://github.com/ghc/ghc/tree/master/compiler/basicTypes/Demand.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

# @Demand@: A decoupled implementation of a demand domain

# Joint domain for Strictness and Absence


# Strictness domain


        Lazy
         |
  ExnStr x -
           |
        HeadStr
        /     \
    SCall      SProd
        \      /
        HyperStr

### Note: Exceptions and strictness

Exceptions need rather careful treatment, especially because of 'catch'
('catch#'), 'catchSTM' ('catchSTM#'), and 'orElse' ('catchRetry#').
See Trac #11555, #10712 and #13330, and for some more background, #11222.

There are three main pieces.

* The Termination type includes ThrowsExn, meaning "under the given
  demand this expression either diverges or throws an exception".

  This is relatively uncontroversial. The primops raise# and
  raiseIO# both return ThrowsExn; nothing else does.

* An ArgStr has an ExnStr flag to say how to process the Termination
  result of the argument.  If the ExnStr flag is ExnStr, we squash
  ThrowsExn to topRes.  (This is done in postProcessDmdResult.)

Here is the key example

    catchRetry# (\s -> retry# s) blah

We analyse the argument (\s -> retry# s) with demand
    Str ExnStr (SCall HeadStr)
i.e. with the ExnStr flag set.
  - First we analyse the argument with the "clean-demand" (SCall
    HeadStr), getting a DmdResult of ThrowsExn from the saturated
    application of retry#.
  - Then we apply the post-processing for the shell, squashing the
    ThrowsExn to topRes.

This also applies uniformly to free variables.  Consider

    let r = \st -> retry# st
    in catchRetry# (\s -> ...(r s')..) handler st

If we give the first argument of catch a strict signature, we'll get a demand
'C(S)' for 'r'; that is, 'r' is definitely called with one argument, which
indeed it is.  But when we post-process the free-var demands on catchRetry#'s
argument (in postProcessDmdEnv), we'll give 'r' a demand of (Str ExnStr (SCall
HeadStr)); and if we feed that into r's RHS (which would be reasonable) we'll
squash the retry just as if we'd inlined 'r'.

* We don't try to get clever about 'catch#' and 'catchSTM#' at the moment. We
previously (#11222) tried to take advantage of the fact that 'catch#' calls its
first argument eagerly. See especially commit
9915b6564403a6d17651e9969e9ea5d7d7e78e7f. We analyzed that first argument with
a strict demand, and then performed a post-processing step at the end to change
ThrowsExn to TopRes.  The trouble, I believe, is that to use this approach
correctly, we'd need somewhat different information about that argument.
Diverges, ThrowsExn (i.e., diverges or throws an exception), and Dunno are the
wrong split here.  In order to evaluate part of the argument speculatively,
we'd need to know that it *does not throw an exception*. That is, that it
either diverges or succeeds. But we don't currently have a way to talk about
that. Abstractly and approximately,

catch# m f s = case ORACLE m s of
  DivergesOrSucceeds -> m s
  Fails exc -> f exc s

where the magical ORACLE determines whether or not (m s) throws an exception
when run, and if so which one. If we want, we can safely consider (catch# m f s)
strict in anything that both branches are strict in (by performing demand
analysis for 'catch#' in the same way we do for case). We could also safely
consider it strict in anything demanded by (m s) that is guaranteed not to
throw an exception under that demand, but I don't know if we have the means
to express that.

My mind keeps turning to this model (not as an actual change to the type, but
as a way to think about what's going on in the analysis):

Thinking about it this way seems likely to be productive for analyzing IO
exception behavior, but imprecise exceptions and asynchronous exceptions remain
quite slippery beasts. Can we incorporate them? I think we can. We can imagine
applying 'seq#' to evaluate @m s@, determining whether it throws an imprecise
or asynchronous exception or whether it succeeds or throws an IO exception.
This confines the peculiarities to 'seq#', which is indeed rather essentially
peculiar.


# Absence domain


         Used
         /   \
     UCall   UProd
         \   /
         UHead
          |
  Count x -
        |
       Abs


### Note: Demand on case-alternative binders

The demand on a binder in a case alternative comes
  (a) From the demand on the binder itself
  (b) From the demand on the case binder
Forgetting (b) led directly to Trac #10148.

Example. Source code:
  f x@(p,_) = if p then foo x else True

  foo (p,True) = True
  foo (p,q)    = foo (q,p)

After strictness analysis:
  f = \ (x_an1 [Dmd=<S(SL),1*U(U,1*U)>] :: (Bool, Bool)) ->
      case x_an1
      of wild_X7 [Dmd=<L,1*U(1*U,1*U)>]
      { (p_an2 [Dmd=<S,1*U>], ds_dnz [Dmd=<L,A>]) ->
      case p_an2 of _ {
        False -> GHC.Types.True;
        True -> foo wild_X7 }

It's true that ds_dnz is *itself* absent, but the use of wild_X7 means
that it is very much alive and demanded.  See Trac #10148 for how the
consequences play out.

This is needed even for non-product types, in case the case-binder
is used but the components of the case alternative are not.

### Note: Don't optimise UProd(Used) to Used

These two UseDmds:
   UProd [Used, Used]   and    Used
are semantically equivalent, but we do not turn the former into
the latter, for a regrettable-subtle reason.  Suppose we did.
then
  f (x,y) = (y,x)
would get
  StrDmd = Str  = SProd [Lazy, Lazy]
  UseDmd = Used = UProd [Used, Used]
But with the joint demand of <Str, Used> doesn't convey any clue
that there is a product involved, and so the worthSplittingFun
will not fire.  (We'd need to use the type as well to make it fire.)
Moreover, consider
  g h p@(_,_) = h p
This too would get <Str, Used>, but this time there really isn't any
point in w/w since the components of the pair are not used at all.

So the solution is: don't aggressively collapse UProd [Used,Used] to
Used; intead leave it as-is. In effect we are using the UseDmd to do a
little bit of boxity analysis.  Not very nice.

### Note: Used should win

Both in lubUse and bothUse we want (Used `both` UProd us) to be Used.
Why?  Because Used carries the implication the whole thing is used,
box and all, so we don't want to w/w it.  If we use it both boxed and
unboxed, then we are definitely using the box, and so we are quite
likely to pay a reboxing cost.  So we make Used win here.

Example is in the Buffer argument of GHC.IO.Handle.Internals.writeCharBuffer

Baseline: (A) Not making Used win (UProd wins)
Compare with: (B) making Used win for lub and both

            Min          -0.3%     -5.6%    -10.7%    -11.0%    -33.3%
            Max          +0.3%    +45.6%    +11.5%    +11.5%     +6.9%
 Geometric Mean          -0.0%     +0.5%     +0.3%     +0.2%     -0.8%

Baseline: (B) Making Used win for both lub and both
Compare with: (C) making Used win for both, but UProd win for lub

            Min          -0.1%     -0.3%     -7.9%     -8.0%     -6.5%
            Max          +0.1%     +1.0%    +21.0%    +21.0%     +0.5%
 Geometric Mean          +0.0%     +0.0%     -0.0%     -0.1%     -0.1%


# Clean demand for Strictness and Usage


This domain differst from JointDemand in the sence that pure absence
is taken away, i.e., we deal *only* with non-absent demands.

### Note: Strict demands

isStrictDmd returns true only of demands that are
   both strict
   and  used
In particular, it is False for <HyperStr, Abs>, which can and does
arise in, say (Trac #7319)
   f x = raise# <some exception>
Then 'x' is not used, so f gets strictness <HyperStr,Abs> -> .
Now the w/w generates
   fx = let x <HyperStr,Abs> = absentError "unused"
        in raise <some exception>
At this point we really don't want to convert to
   fx = case absentError "unused" of x -> raise <some exception>
Since the program is going to diverge, this swaps one error for another,
but it's really a bad idea to *ever* evaluate an absent argument.
In Trac #7319 we get
   T7319.exe: Oops!  Entered absent arg w_s1Hd{v} [lid] [base:GHC.Base.String{tc 36u}]

### Note: Dealing with call demands

Call demands are constructed and deconstructed coherently for
strictness and absence. For instance, the strictness signature for the
following function

f :: (Int -> (Int, Int)) -> (Int, Bool)
f g = (snd (g 3), True)

should be: <L,C(U(AU))>m


# Demand: combining stricness and usage


### Note: Trimming a demand to a type

Consider this:

  f :: a -> Bool
  f x = case ... of
          A g1 -> case (x |> g1) of (p,q) -> ...
          B    -> error "urk"

where A,B are the constructors of a GADT.  We'll get a U(U,U) demand
on x from the A branch, but that's a stupid demand for x itself, which
has type 'a'. Indeed we get ASSERTs going off (notably in
splitUseProdDmd, Trac #8569).

Bottom line: we really don't want to have a binder whose demand is more
deeply-nested than its type.  There are various ways to tackle this.
When processing (x |> g1), we could "trim" the incoming demand U(U,U)
to match x's type.  But I'm currently doing so just at the moment when
we pin a demand on a binder, in DmdAnal.findBndrDmd.

### Note: Threshold demands

Threshold usage demand is generated to figure out if
cardinality-instrumented demands of a binding's free variables should
be unleashed. See also [Aggregated demand for cardinality].

### Note: Replicating polymorphic demands

Some demands can be considered as polymorphic. Generally, it is
applicable to such beasts as tops, bottoms as well as Head-Used and
Head-stricts demands. For instance,

S ~ S(L, ..., L)

Also, when top or bottom is occurred as a result demand, it in fact
can be expanded to saturate a callee's arity.


# Demand results



DmdResult:     Dunno CPRResult
               /
           ThrowsExn
             /
        Diverges


CPRResult:         NoCPR
                   /    \
            RetProd    RetSum ConTag


Product constructors return (Dunno (RetProd rs))
In a fixpoint iteration, start from Diverges
We have lubs, but not glbs; but that is ok.


### Note: defaultDmd and resTypeArgDmd


These functions are similar: They express the demand on something not
explicitly mentioned in the environment resp. the argument list. Yet they are
different:
 * Variables not mentioned in the free variables environment are definitely
   unused, so we can use absDmd there.
 * Further arguments *can* be used, of course. Hence topDmd is used.

### Note: Worthy functions for Worker-Wrapper split

For non-bottoming functions a worker-wrapper transformation takes into
account several possibilities to decide if the function is worthy for
splitting:

1. The result is of product type and the function is strict in some
(or even all) of its arguments. The check that the argument is used is
more of sanity nature, since strictness implies usage. Example:

f :: (Int, Int) -> Int
f p = (case p of (a,b) -> a) + 1

should be splitted to

f :: (Int, Int) -> Int
f p = case p of (a,b) -> $wf a

$wf :: Int -> Int
$wf a = a + 1

2. Sometimes it also makes sense to perform a WW split if the
strictness analysis cannot say for sure if the function is strict in
components of its argument. Then we reason according to the inferred
usage information: if the function uses its product argument's
components, the WW split can be beneficial. Example:

g :: Bool -> (Int, Int) -> Int
g c p = case p of (a,b) ->
          if c then a else b

The function g is strict in is argument p and lazy in its
components. However, both components are used in the RHS. The idea is
since some of the components (both in this case) are used in the
right-hand side, the product must presumable be taken apart.

Therefore, the WW transform splits the function g to

g :: Bool -> (Int, Int) -> Int
g c p = case p of (a,b) -> $wg c a b

$wg :: Bool -> Int -> Int -> Int
$wg c a b = if c then a else b

3. If an argument is absent, it would be silly to pass it to a
function, hence the worker with reduced arity is generated.

### Note: Worker-wrapper for bottoming functions

We used not to split if the result is bottom.
[Justification:  there's no efficiency to be gained.]

But it's sometimes bad not to make a wrapper.  Consider
        fw = \x# -> let x = I# x# in case e of
                                        p1 -> error_fn x
                                        p2 -> error_fn x
                                        p3 -> the real stuff
The re-boxing code won't go away unless error_fn gets a wrapper too.
[We don't do reboxing now, but in general it's better to pass an
unboxed thing to f, and have it reboxed in the error cases....]

However we *don't* want to do this when the argument is not actually
taken apart in the function at all.  Otherwise we risk decomposing a
massive tuple which is barely used.  Example:

        f :: ((Int,Int) -> String) -> (Int,Int) -> a
        f g pr = error (g pr)

        main = print (f fst (1, error "no"))

Here, f does not take 'pr' apart, and it's stupid to do so.
Imagine that it had millions of fields. This actually happened
in GHC itself where the tuple was DynFlags

# Demand environments and types


### Note: Nature of result demand

A DmdResult contains information about termination (currently distinguishing
definite divergence and no information; it is possible to include definite
convergence here), and CPR information about the result.

The semantics of this depends on whether we are looking at a DmdType, i.e. the
demand put on by an expression _under a specific incoming demand_ on its
environment, or at a StrictSig describing a demand transformer.

For a
 * DmdType, the termination information is true given the demand it was
   generated with, while for
 * a StrictSig it holds after applying enough arguments.

The CPR information, though, is valid after the number of arguments mentioned
in the type is given. Therefore, when forgetting the demand on arguments, as in
dmdAnalRhs, this needs to be considere (via removeDmdTyArgs).

Consider
  b2 x y = x `seq` y `seq` error (show x)
this has a strictness signature of
  <S><S>b
meaning that "b2 `seq` ()" and "b2 1 `seq` ()" might well terminate, but
for "b2 1 2 `seq` ()" we get definite divergence.

For comparison,
  b1 x = x `seq` error (show x)
has a strictness signature of
  <S>b
and "b1 1 `seq` ()" is known to terminate.

Now consider a function h with signature "<C(S)>", and the expression
  e1 = h b1
now h puts a demand of <C(S)> onto its argument, and the demand transformer
turns it into
  <S>b
Now the DmdResult "b" does apply to us, even though "b1 `seq` ()" does not
diverge, and we do not anything being passed to b.

### Note: Asymmetry of 'both' for DmdType and DmdResult

'both' for DmdTypes is *asymmetrical*, because there is only one
result!  For example, given (e1 e2), we get a DmdType dt1 for e1, use
its arg demand to analyse e2 giving dt2, and then do (dt1 `bothType` dt2).
Similarly with
  case e of { p -> rhs }
we get dt_scrut from the scrutinee and dt_rhs from the RHS, and then
compute (dt_rhs `bothType` dt_scrut).

We
 1. combine the information on the free variables,
 2. take the demand on arguments from the first argument
 3. combine the termination results, but
 4. take CPR info from the first argument.

3 and 4 are implementd in bothDmdResult.


### Note: The need for BothDmdArg

Previously, the right argument to bothDmdType, as well as the return value of
dmdAnalStar via postProcessDmdType, was a DmdType. But bothDmdType only needs
to know about the free variables and termination information, but nothing about
the demand put on arguments, nor cpr information. So we make that explicit by
only passing the relevant information.


### Note: Demands from unsaturated function calls


Consider a demand transformer d1 -> d2 -> r for f.
If a sufficiently detailed demand is fed into this transformer,
e.g <C(C(S)), C1(C1(S))> arising from "f x1 x2" in a strict, use-once context,
then d1 and d2 is precisely the demand unleashed onto x1 and x2 (similar for
the free variable environment) and furthermore the result information r is the
one we want to use.

An anonymous lambda is also an unsaturated function all (needs one argument,
none given), so this applies to that case as well.

But the demand fed into f might be less than <C(C(S)), C1(C1(S))>. There are a few cases:
 * Not enough demand on the strictness side:
   - In that case, we need to zap all strictness in the demand on arguments and
     free variables.
   - Furthermore, we remove CPR information. It could be left, but given the incoming
     demand is not enough to evaluate so far we just do not bother.
   - And finally termination information: If r says that f diverges for sure,
     then this holds when the demand guarantees that two arguments are going to
     be passed. If the demand is lower, we may just as well converge.
     If we were tracking definite convegence, than that would still hold under
     a weaker demand than expected by the demand transformer.
 * Not enough demand from the usage side: The missing usage can be expanded
   using UCall Many, therefore this is subsumed by the third case:
 * At least one of the uses has a cardinality of Many.
   - Even if f puts a One demand on any of its argument or free variables, if
     we call f multiple times, we may evaluate this argument or free variable
     multiple times. So forget about any occurrence of "One" in the demand.

In dmdTransformSig, we call peelManyCalls to find out if we are in any of these
cases, and then call postProcessUnsat to reduce the demand appropriately.

Similarly, dmdTransformDictSelSig and dmdAnal, when analyzing a Lambda, use
peelCallDmd, which peels only one level, but also returns the demand put on the
body of the function.


### Note: Default demand on free variables

If the variable is not mentioned in the environment of a demand type,
its demand is taken to be a result demand of the type.
    For the stricness component,
     if the result demand is a Diverges, then we use HyperStr
                                         else we use Lazy
    For the usage component, we use Absent.
So we use either absDmd or botDmd.

Also note the equations for lubDmdResult (resp. bothDmdResult) noted there.

### Note: Always analyse in virgin pass

Tricky point: make sure that we analyse in the 'virgin' pass. Consider
   rec { f acc x True  = f (...rec { g y = ...g... }...)
         f acc x False = acc }
In the virgin pass for 'f' we'll give 'f' a very strict (bottom) type.
That might mean that we analyse the sub-expression containing the
E = "...rec g..." stuff in a bottom demand.  Suppose we *didn't analyse*
E, but just returned botType.

Then in the *next* (non-virgin) iteration for 'f', we might analyse E
in a weaker demand, and that will trigger doing a fixpoint iteration
for g.  But *because it's not the virgin pass* we won't start g's
iteration at bottom.  Disaster.  (This happened in $sfibToList' of
nofib/spectral/fibheaps.)

So in the virgin pass we make sure that we do analyse the expression
at least once, to initialise its signatures.

### Note: Analyzing with lazy demand and lambdas

The insight for analyzing lambdas follows from the fact that for
strictness S = C(L). This polymorphic expansion is critical for
cardinality analysis of the following example:

### Note: Demand transformer for a dictionary selector

If we evaluate (op dict-expr) under demand 'd', then we can push the demand 'd'
into the appropriate field of the dictionary. What *is* the appropriate field?
We just look at the strictness signature of the class op, which will be
something like: U(AAASAAAAA).  Then replace the 'S' by the demand 'd'.

For single-method classes, which are represented by newtypes the signature
of 'op' won't look like U(...), so the splitProdDmd_maybe will fail.
That's fine: if we are doing strictness analysis we are also doing inlining,
so we'll have inlined 'op' into a cast.  So we can bale out in a conservative
way, returning nopDmdType.

It is (just.. Trac #8329) possible to be running strictness analysis *without*
having inlined class ops from single-method classes.  Suppose you are using
ghc --make; and the first module has a local -O0 flag.  So you may load a class
without interface pragmas, ie (currently) without an unfolding for the class
ops.   Now if a subsequent module in the --make sweep has a local -O flag
you might do strictness analysis, but there is no inlining for the class op.
This is weird, so I'm not worried about whether this optimises brilliantly; but
it should not fall over.


### Note: Computing one-shot info

### Note: Use one-shot information

### Note: Unsaturated applications

If a function having bottom as its demand result is applied to a less
number of arguments than its syntactic arity, we cannot say for sure
that it is going to diverge. This is the reason why we use the
function appIsBottom, which, given a strictness signature and a number
of arguments, says conservatively if the function is going to diverge
or not.

Zap absence or one-shot information, under control of flags

### Note: Killing usage information

The flags -fkill-one-shot and -fkill-absence let you switch off the generation
of absence or one-shot information altogether.  This is only used for performance
tests, to see how important they are.


### Note: HyperStr and Use demands


The information "HyperStr" needs to be in the strictness signature, and not in
the demand signature, because we still want to know about the demand on things. Consider

    f (x,y) True  = error (show x)
    f (x,y) False = x+1

The signature of f should be <S(SL),1*U(1*U(U),A)><S,1*U>m. If we were not
distinguishing the uses on x and y in the True case, we could either not figure
out how deeply we can unpack x, or that we do not have to pass y.

# Serialisation
