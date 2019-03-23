`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplCore/CoreMonad.hs>`_

Note [Which transformations are innocuous]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At one point (Jun 18) I wondered if some transformations (ticks)
might be  "innocuous", in the sense that they do not unlock a later
transformation that does not occur in the same pass.  If so, we could
refrain from bumping the overall tick-count for such innocuous
transformations, and perhaps terminate the simplifier one pass
earlier.

BUt alas I found that virtually nothing was innocuous!  This Note
just records what I learned, in case anyone wants to try again.

These transformations are not innocuous:

*** NB: I think these ones could be made innocuous
          EtaExpansion
          LetFloatFromLet

LetFloatFromLet
    x = K (let z = e2 in Just z)
  prepareRhs transforms to
    x2 = let z=e2 in Just z
    x  = K xs
  And now more let-floating can happen in the
  next pass, on x2

PreInlineUnconditionally
  Example in spectral/cichelli/Auxil
     hinsert = ...let lo = e in
                  let j = ...lo... in
                  case x of
                    False -> ()
                    True -> case lo of I# lo' ->
                              ...j...
  When we PreInlineUnconditionally j, lo's occ-info changes to once,
  so it can be PreInlineUnconditionally in the next pass, and a
  cascade of further things can happen.

PostInlineUnconditionally
  let x = e in
  let y = ...x.. in
  case .. of { A -> ...x...y...
               B -> ...x...y... }
  Current postinlineUnconditinaly will inline y, and then x; sigh.

  But PostInlineUnconditionally might also unlock subsequent
  transformations for the same reason as PreInlineUnconditionally,
  so it's probably not innocuous anyway.

KnownBranch, BetaReduction:
  May drop chunks of code, and thereby enable PreInlineUnconditionally
  for some let-binding which now occurs once

EtaExpansion:
  Example in imaginary/digits-of-e1
    fail = \void. e          where e :: IO ()
  --> etaExpandRhs
    fail = \void. (\s. (e |> g) s) |> sym g      where g :: IO () ~ S -> (S,())
  --> Next iteration of simplify
    fail1 = \void. \s. (e |> g) s
    fail = fail1 |> Void#->sym g
  And now inline 'fail'

CaseMerge:
  case x of y {
    DEFAULT -> case y of z { pi -> ei }
    alts2 }
  ---> CaseMerge
    case x of { pi -> let z = y in ei
              ; alts2 }
  The "let z=y" case-binder-swap gets dealt with in the next pass


Note [Annotations]
~~~~~~~~~~~~~~~~~~
A Core-to-Core pass that wants to make use of annotations calls
getAnnotations or getFirstAnnotations at the beginning to obtain a UniqFM with
annotations of a specific type. This produces all annotations from interface
files read so far. However, annotations from interface files read during the
pass will not be visible until getAnnotations is called again. This is similar
to how rules work and probably isn't too bad.

The current implementation could be optimised a bit: when looking up
annotations for a thing from the HomePackageTable, we could search directly in
the module where the thing is defined rather than building one UniqFM which
contains all annotations we know of. This would work because annotations can
only be given to things defined in the same module. However, since we would
only want to deserialise every annotation once, we would have to build a cache
for every module in the HTP. In the end, it's probably not worth it as long as
we aren't using annotations heavily.


