[[src]](https://github.com/ghc/ghc/tree/master/compiler/deSugar/Match.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


The @match@ function


#SOURCE#

# The main matching function


The function @match@ is basically the same as in the Wadler chapter
from "The Implementation of Functional Programming Languages",
except it is monadised, to carry around the name supply, info about
annotations, etc.

Notes on @match@'s arguments, assuming $m$ equations and $n$ patterns:
\begin{enumerate}
\item
A list of $n$ variable names, those variables presumably bound to the
$n$ expressions being matched against the $n$ patterns.  Using the
list of $n$ expressions as the first argument showed no benefit and
some inelegance.

\item
The second argument, a list giving the ``equation info'' for each of
the $m$ equations:
\begin{itemize}
\item
the $n$ patterns for that equation, and
\item
a list of Core bindings [@(Id, CoreExpr)@ pairs] to be ``stuck on
the front'' of the matching code, as in:
\begin{verbatim}
let <binds>
in  <matching-code>
\end{verbatim}
\item
and finally: (ToDo: fill in)

The right way to think about the ``after-match function'' is that it
is an embryonic @CoreExpr@ with a ``hole'' at the end for the
final ``else expression''.
\end{itemize}

There is a data type, @EquationInfo@, defined in module @DsMonad@.

An experiment with re-ordering this information about equations (in
particular, having the patterns available in column-major order)
showed no benefit.

\item
A default expression---what to evaluate if the overall pattern-match
fails.  This expression will (almost?) always be
a measly expression @Var@, unless we know it will only be used once
(as we do in @glue_success_exprs@).

Leaving out this third argument to @match@ (and slamming in lots of
@Var "fail"@s) is a positively {\em bad} idea, because it makes it
impossible to share the default expressions.  (Also, it stands no
chance of working in our post-upheaval world of @Locals@.)
\end{enumerate}

Note: @match@ is often called via @matchWrapper@ (end of this module),
a function that does much of the house-keeping that goes with a call
to @match@.

It is also worth mentioning the {\em typical} way a block of equations
is desugared with @match@.  At each stage, it is the first column of
patterns that is examined.  The steps carried out are roughly:
\begin{enumerate}
\item
Tidy the patterns in column~1 with @tidyEqnInfo@ (this may add
bindings to the second component of the equation-info):
\item
Now {\em unmix} the equations into {\em blocks} [w\/ local function
@match_groups@], in which the equations in a block all have the same
 match group.
(see ``the mixture rule'' in SLPJ).
\item
Call the right match variant on each block of equations; it will do the
appropriate thing for each kind of column-1 pattern.
\end{enumerate}

We are a little more paranoid about the ``empty rule'' (SLPJ, p.~87)
than the Wadler-chapter code for @match@ (p.~93, first @match@ clause).
And gluing the ``success expressions'' together isn't quite so pretty.

This  @match@ uses @tidyEqnInfo@
to get `as'- and `twiddle'-patterns out of the way (tidying), before
applying ``the mixture rule'' (SLPJ, p.~88) [which really {\em
un}mixes the equations], producing a list of equation-info
blocks, each block having as its first column patterns compatible with each other.

### Note: Match Ids

Most of the matching functions take an Id or [Id] as argument.  This Id
is the scrutinee(s) of the match. The desugared expression may
sometimes use that Id in a local binding or as a case binder.  So it
should not have an External name; Lint rejects non-top-level binders
with External names (Trac #13043).


### Note: Empty case alternatives

The list of EquationInfo can be empty, arising from
    case x of {}   or    \case {}
In that situation we desugar to
    case x of { _ -> error "pattern match failure" }
The *desugarer* isn't certain whether there really should be no
alternatives, so it adds a default case, as it always does.  A later
pass may remove it if it's inaccessible.  (See also Note [Empty case
alternatives] in CoreSyn.)

### Note: Case elimination: lifted case

# Tidying patterns


Tidy up the leftmost pattern in an @EquationInfo@, given the variable @v@
which will be scrutinised.

This makes desugaring the pattern match simpler by transforming some of
the patterns to simpler forms. (Tuples to Constructor Patterns)

Among other things in the resulting Pattern:
* Variables and irrefutable(lazy) patterns are replaced by Wildcards
* As patterns are replaced by the patterns they wrap.

The bindings created by the above patterns are put into the returned wrapper
instead.

This means a definition of the form:
  f x = rhs
when called with v get's desugared to the equivalent of:
  let x = v
  in
  f _ = rhs

The same principle holds for as patterns (@) and
irrefutable/lazy patterns (~).
In the case of irrefutable patterns the irrefutable pattern is pushed into
the binding.

Pattern Constructors which only represent syntactic sugar are converted into
their desugared representation.
This usually means converting them to Constructor patterns but for some
depends on enabled extensions. (Eg OverloadedLists)

GHC also tries to convert overloaded Literals into regular ones.

The result of this tidying is that the column of patterns will include
only these which can be assigned a PatternGroup (see patGroup).



 now, here we handle lazy patterns:
    tidy1 v ~p bs = (v, v1 = case v of p -> v1 :
                        v2 = case v of p -> v2 : ... : bs )

    where the v_i's are the binders in the pattern.

    ToDo: in "v_i = ... -> v_i", are the v_i's really the same thing?

    The case expr for v_i is just: match [v] [(p, [], \ x -> Var v_i)] any_expr


### Note: Bang patterns and newtypes

For the pattern  !(Just pat)  we can discard the bang, because
the pattern is strict anyway. But for !(N pat), where
  newtype NT = N Int
we definitely can't discard the bang.  Trac #9844.

So what we do is to push the bang inwards, in the hope that it will
get discarded there.  So we transform
   !(N pat)   into    (N !pat)

But what if there is nothing to push the bang onto? In at least one instance
a user has written !(N {}) which we translate into (N !_). See #13215


\noindent
{\bf Previous @matchTwiddled@ stuff:}

Now we get to the only interesting part; note: there are choices for
translation [from Simon's notes]; translation~1:
\begin{verbatim}
deTwiddle [s,t] e
\end{verbatim}
returns
\begin{verbatim}
[ w = e,
  s = case w of [s,t] -> s
  t = case w of [s,t] -> t
]
\end{verbatim}

Here \tr{w} is a fresh variable, and the \tr{w}-binding prevents multiple
evaluation of \tr{e}.  An alternative translation (No.~2):
\begin{verbatim}
[ w = case e of [s,t] -> (s,t)
  s = case w of (s,t) -> s
  t = case w of (s,t) -> t
]
\end{verbatim}

# \subsubsection[improved-unmixing]{UNIMPLEMENTED idea for improved unmixing}


We might be able to optimise unmixing when confronted by
only-one-constructor-possible, of which tuples are the most notable
examples.  Consider:
\begin{verbatim}
f (a,b,c) ... = ...
f d ... (e:f) = ...
f (g,h,i) ... = ...
f j ...       = ...
\end{verbatim}
This definition would normally be unmixed into four equation blocks,
one per equation.  But it could be unmixed into just one equation
block, because if the one equation matches (on the first column),
the others certainly will.

You have to be careful, though; the example
\begin{verbatim}
f j ...       = ...
-------------------
f (a,b,c) ... = ...
f d ... (e:f) = ...
f (g,h,i) ... = ...
\end{verbatim}
{\em must} be broken into two blocks at the line shown; otherwise, you
are forcing unnecessary evaluation.  In any case, the top-left pattern
always gives the cue.  You could then unmix blocks into groups of...
\begin{description}
\item[all variables:]
As it is now.
\item[constructors or variables (mixed):]
Need to make sure the right names get bound for the variable patterns.
\item[literals or variables (mixed):]
Presumably just a variant on the constructor case (as it is now).
\end{description}

# matchWrapper: a convenient way to call @match@                      

Calls to @match@ often involve similar (non-trivial) work; that work
is collected here, in @matchWrapper@.  This function takes as
arguments:
\begin{itemize}
\item
Typechecked @Matches@ (of a function definition, or a case or lambda
expression)---the main input;
\item
An error message to be inserted into any (runtime) pattern-matching
failure messages.
\end{itemize}

As results, @matchWrapper@ produces:
\begin{itemize}
\item
A list of variables (@Locals@) that the caller must ``promise'' to
bind to appropriate values; and
\item
a @CoreExpr@, the desugared output (main result).
\end{itemize}

The main actions of @matchWrapper@ include:
\begin{enumerate}
\item
Flatten the @[TypecheckedMatch]@ into a suitable list of
@EquationInfo@s.
\item
Create as many new variables as there are patterns in a pattern-list
(in any one of the @EquationInfo@s).
\item
Create a suitable ``if it fails'' expression---a call to @error@ using
the error-string input; the {\em type} of this fail value can be found
by examining one of the RHS expressions in one of the @EquationInfo@s.
\item
Call @match@ with all of this information!
\end{enumerate}



 There is one small problem with the Lambda Patterns, when somebody
 writes something similar to:
\begin{verbatim}
    (\ (x:xs) -> ...)
\end{verbatim}
 he/she don't want a warning about incomplete patterns, that is done with
 the flag @opt_WarnSimplePatterns@.
 This problem also appears in the:
\begin{itemize}
\item @do@ patterns, but if the @do@ can fail
      it creates another equation if the match can fail
      (see @DsExpr.doDo@ function)
\item @let@ patterns, are treated by @matchSimply@
   List Comprension Patterns, are treated by @matchSimply@ also
\end{itemize}

We can't call @matchSimply@ with Lambda patterns,
due to the fact that lambda patterns can have more than
one pattern, and match simply only accepts one pattern.

JJQC 30-Nov-1997


# \subsection[matchSimply]{@matchSimply@: match a single expression against a single pattern}


@mkSimpleMatch@ is a wrapper for @match@ which deals with the
situation where we want to match a single expression against a single
pattern. It returns an expression.


# Pattern classification


### Note: Don't use Literal for PgN

Previously we had, as PatGroup constructors

  | ...
  | PgN   Literal       -- Overloaded literals
  | PgNpK Literal       -- n+k patterns
  | ...

But Literal is really supposed to represent an *unboxed* literal, like Int#.
We were sticking the literal from, say, an overloaded numeric literal pattern
into a MachInt constructor. This didn't really make sense; and we now have
the invariant that value in a MachInt must be in the range of the target
machine's Int# type, and an overloaded literal could meaningfully be larger.

Solution: For pattern grouping purposes, just store the literal directly in
the PgN constructor as a Rational if numeric, and add a PgOverStr constructor
for overloaded strings.


### Note: Pattern synonym groups

If we see
  f (P a) = e1
  f (P b) = e2
    ...
where P is a pattern synonym, can we put (P a -> e1) and (P b -> e2) in the
same group?  We can if P is a constructor, but /not/ if P is a pattern synonym.
Consider (Trac #11224)
   -- readMaybe :: Read a => String -> Maybe a
   pattern PRead :: Read a => () => a -> String
   pattern PRead a <- (readMaybe -> Just a)

   f (PRead (x::Int))  = e1
   f (PRead (y::Bool)) = e2
This is all fine: we match the string by trying to read an Int; if that
fails we try to read a Bool. But clearly we can't combine the two into a single
match.

Conclusion: we can combine when we invoke PRead /at the same type/.  Hence
in PgSyn we record the instantiaing types, and use them in sameGroup.

### Note: Take care with pattern order

In the subGroup function we must be very careful about pattern re-ordering,
Consider the patterns [ (True, Nothing), (False, x), (True, y) ]
Then in bringing together the patterns for True, we must not
swap the Nothing and y!


### Note: Grouping overloaded literal patterns

WATCH OUT!  Consider

        f (n+1) = ...
        f (n+2) = ...
        f (n+1) = ...

We can't group the first and third together, because the second may match
the same thing as the first.  Same goes for *overloaded* literal patterns
        f 1 True = ...
        f 2 False = ...
        f 1 False = ...
If the first arg matches '1' but the second does not match 'True', we
cannot jump to the third equation!  Because the same argument might
match '2'!
Hence we don't regard 1 and 2, or (n+1) and (n+2), as part of the same group.
