[[src]](https://github.com/ghc/ghc/tree/master/compiler/simplCore/SimplCore.hs)

(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

# Driver for simplifying @Core@ programs

# \subsection{The driver for the simplifier}


# Generating the main optimisation pipeline


 opt_level >= 1 

### Note: Inline in InitialPhase

In GHC 8 and earlier we did not inline anything in the InitialPhase. But that is
confusing for users because when they say INLINE they expect the function to inline
right away.

So now we do inlining immediately, even in the InitialPhase, assuming that the
Id's Activation allows it.

This is a surprisingly big deal. Compiler performance improved a lot
when I made this change:

   perf/compiler/T5837.run            T5837 [stat too good] (normal)
   perf/compiler/parsing001.run       parsing001 [stat too good] (normal)
   perf/compiler/T12234.run           T12234 [stat too good] (optasm)
   perf/compiler/T9020.run            T9020 [stat too good] (optasm)
   perf/compiler/T3064.run            T3064 [stat too good] (normal)
   perf/compiler/T9961.run            T9961 [stat too good] (normal)
   perf/compiler/T13056.run           T13056 [stat too good] (optasm)
   perf/compiler/T9872d.run           T9872d [stat too good] (normal)
   perf/compiler/T783.run             T783 [stat too good] (normal)
   perf/compiler/T12227.run           T12227 [stat too good] (normal)
   perf/should_run/lazy-bs-alloc.run  lazy-bs-alloc [stat too good] (normal)
   perf/compiler/T1969.run            T1969 [stat too good] (normal)
   perf/compiler/T9872a.run           T9872a [stat too good] (normal)
   perf/compiler/T9872c.run           T9872c [stat too good] (normal)
   perf/compiler/T9872b.run           T9872b [stat too good] (normal)
   perf/compiler/T9872d.run           T9872d [stat too good] (normal)

### Note: RULEs enabled in SimplGently

RULES are enabled when doing "gentle" simplification.  Two reasons:

  * We really want the class-op cancellation to happen:
        op (df d1 d2) --> $cop3 d1 d2
    because this breaks the mutual recursion between 'op' and 'df'

  * I wanted the RULE
        lift String ===> ...
    to work in Template Haskell when simplifying
    splices, so we get simpler code for literal strings

But watch out: list fusion can prevent floating.  So use phase control
to switch off those rules until after floating.

# The CoreToDo interpreter


# \subsection{Core pass combinators}


# Gentle simplification


# \subsection{The driver for the simplifier}


# Shorting out indirections


If we have this:

        x_local = <expression>
        ...bindings...
        x_exported = x_local

where x_exported is exported, and x_local is not, then we replace it with this:

        x_exported = <expression>
        x_local = x_exported
        ...bindings...

Without this we never get rid of the x_exported = x_local thing.  This
save a gratuitous jump (from \tr{x_exported} to \tr{x_local}), and
makes strictness information propagate better.  This used to happen in
the final phase, but it's tidier to do it here.

### Note: Transferring IdInfo

We want to propagage any useful IdInfo on x_local to x_exported.

STRICTNESS: if we have done strictness analysis, we want the strictness info on
x_local to transfer to x_exported.  Hence the copyIdInfo call.

RULES: we want to *add* any RULES for x_local to x_exported.

### Note: Messing up the exported Id's RULES

We must be careful about discarding (obviously) or even merging the
RULES on the exported Id. The example that went bad on me at one stage
was this one:

    iterate :: (a -> a) -> a -> [a]
        [Exported]
    iterate = iterateList

    iterateFB c f x = x `c` iterateFB c f (f x)
    iterateList f x =  x : iterateList f (f x)
        [Not exported]

    {-# RULES
    "iterate"   forall f x.     iterate f x = build (\c _n -> iterateFB c f x)
    "iterateFB"                 iterateFB (:) = iterateList
     #

# RULES
    "iterate"   forall f x.     iterate f x = build (\c _n -> iterateFB c f x)
    "iterateFB"                 iterateFB (:) = iterate
     #