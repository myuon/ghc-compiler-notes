`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/LiberateCase.hs>`_

compiler/simplCore/LiberateCase.hs
==================================


Note [Scrutinee with cast]
~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/LiberateCase.hs#L62>`__

Consider this:
    f = \ t -> case (v `cast` co) of
                 V a b -> a : f t

Exactly the same optimisation (unrolling one call to f) will work here,
despite the cast.  See mk_alt_env in the Case branch of libCase.


To think about (Apr 94)
~~~~~~~~~~~~~~
Main worry: duplicating code excessively.  At the moment we duplicate
the entire binding group once at each recursive call.  But there may
be a group of recursive calls which share a common set of evaluated
free variables, in which case the duplication is a plain waste.

Another thing we could consider adding is some unfold-threshold thing,
so that we'll only duplicate if the size of the group rhss isn't too
big.

Data types
~~~~~~~~~~
The ``level'' of a binder tells how many
recursive defns lexically enclose the binding
A recursive defn "encloses" its RHS, not its
scope.  For example:
\begin{verbatim}
        letrec f = let g = ... in ...
        in
        let h = ...
        in ...
\end{verbatim}
Here, the level of @f@ is zero, the level of @g@ is one,
and the level of @h@ is zero (NB not one).



Note [Not bottoming Ids]
~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/LiberateCase.hs#L164>`__

Do not specialise error-functions (this is unusual, but I once saw it,
(acually in Data.Typable.Internal)



Note [Only functions!]
~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/LiberateCase.hs#L169>`__

Consider the following code

::

       f = g (case v of V a b -> a : t f)

where g is expensive. If we aren't careful, liberate case will turn this into

::

       f = g (case v of
               V a b -> a : t (letrec f = g (case v of V a b -> a : f t)
                                in f)
             )

Yikes! We evaluate g twice. This leads to a O(2^n) explosion
if g calls back to the same code recursively.

Solution: make sure that we only do the liberate-case thing on *functions*



Note [Small enough]
~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/LiberateCase.hs#L187>`__

Consider
  \fv. letrec
         f = \x. BIG...(case fv of { (a,b) -> ...g.. })...
         g = \y. SMALL...f...

Then we *can* in principle do liberate-case on 'g' (small RHS) but not
for 'f' (too big).  But doing so is not profitable, because duplicating
'g' at its call site in 'f' doesn't get rid of any cases.  So we just
ask for the whole group to be small enough.



Note [Need to localiseId in libCaseBind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/LiberateCase.hs#L199>`__

The call to localiseId is needed for two subtle reasons
(a)  Reset the export flags on the binders so
        that we don't get name clashes on exported things if the
        local binding floats out to top level.  This is most unlikely
        to happen, since the whole point concerns free variables.
        But resetting the export flag is right regardless.

(b)  Make the name an Internal one.  External Names should never be
        nested; if it were floated to the top level, we'd get a name
        clash at code generation time.

Expressions
~~~~~~~~~~~



Note [When to specialise]
~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/LiberateCase.hs#L292>`__

Consider
  f = \x. letrec g = \y. case x of
                           True  -> ... (f a) ...
                           False -> ... (g b) ...

We get the following levels
          f  0
          x  1
          g  1
          y  2

Then 'x' is being scrutinised at a deeper level than its binding, so
it's added to lc_sruts:  [(x,1)]

We do *not* want to specialise the call to 'f', because 'x' is not free
in 'f'.  So here the bind-level of 'x' (=1) is not <= the bind-level of 'f' (=0).

We *do* want to specialise the call to 'g', because 'x' is free in g.
Here the bind-level of 'x' (=1) is <= the bind-level of 'g' (=1).



Note [Avoiding fruitless liberate-case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/LiberateCase.hs#L314>`__

Consider also:
  f = \x. case top_lvl_thing of
                I# _ -> let g = \y. ... g ...
                        in ...

Here, top_lvl_thing is scrutinised at a level (1) deeper than its
binding site (0).  Nevertheless, we do NOT want to specialise the call
to 'g' because all the structure in its free variables is already
visible at the definition site for g.  Hence, when considering specialising
an occurrence of 'g', we want to check that there's a scruted-var v st

::

   a) v's binding site is *outside* g
   b) v's scrutinisation site is *inside* g

