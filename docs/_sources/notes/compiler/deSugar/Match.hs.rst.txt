Note [Match Ids]
~~~~~~~~~~~~~~~~
Most of the matching functions take an Id or [Id] as argument.  This Id
is the scrutinee(s) of the match. The desugared expression may
sometimes use that Id in a local binding or as a case binder.  So it
should not have an External name; Lint rejects non-top-level binders
with External names (#13043).

See also Note [Localise pattern binders] in DsUtils


Note [Empty case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The list of EquationInfo can be empty, arising from
    case x of {}   or    \case {}
In that situation we desugar to
    case x of { _ -> error "pattern match failure" }
The *desugarer* isn't certain whether there really should be no
alternatives, so it adds a default case, as it always does.  A later
pass may remove it if it's inaccessible.  (See also Note [Empty case
alternatives] in CoreSyn.)

We do *not* desugar simply to
   error "empty case"
or some such, because 'x' might be bound to (error "hello"), in which
case we want to see that "hello" exception, not (error "empty case").
See also Note [Case elimination: lifted case] in Simplify.




Note [Bang patterns and newtypes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For the pattern  !(Just pat)  we can discard the bang, because
the pattern is strict anyway. But for !(N pat), where
  newtype NT = N Int
we definitely can't discard the bang.  #9844.

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



Note [Don't use Literal for PgN]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Previously we had, as PatGroup constructors

  | ...
  | PgN   Literal       -- Overloaded literals
  | PgNpK Literal       -- n+k patterns
  | ...

But Literal is really supposed to represent an *unboxed* literal, like Int#.
We were sticking the literal from, say, an overloaded numeric literal pattern
into a LitInt constructor. This didn't really make sense; and we now have
the invariant that value in a LitInt must be in the range of the target
machine's Int# type, and an overloaded literal could meaningfully be larger.

Solution: For pattern grouping purposes, just store the literal directly in
the PgN constructor as a Rational if numeric, and add a PgOverStr constructor
for overloaded strings.


Note [Pattern synonym groups]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we see
  f (P a) = e1
  f (P b) = e2
    ...
where P is a pattern synonym, can we put (P a -> e1) and (P b -> e2) in the
same group?  We can if P is a constructor, but /not/ if P is a pattern synonym.
Consider (#11224)
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



Note [Take care with pattern order]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the subGroup function we must be very careful about pattern re-ordering,
Consider the patterns [ (True, Nothing), (False, x), (True, y) ]
Then in bringing together the patterns for True, we must not
swap the Nothing and y!


Note [Grouping overloaded literal patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
