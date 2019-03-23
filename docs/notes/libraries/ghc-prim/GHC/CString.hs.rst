Note [Inlining unpackCString#]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There's really no point in ever inlining things like unpackCString# as the loop
doesn't specialise in an interesting way and we can't deforest the list
constructors (we'd want to use unpackFoldrCString# for this). Moreover, it's
pretty small, so there's a danger that it'll be inlined at every literal, which
is a waste.

Moreover, inlining early may interfere with a variety of rules that are supposed
to match unpackCString#,

 * BuiltInRules in PrelRules.hs; e.g.
       eqString (unpackCString# (Lit s1)) (unpackCString# (Lit s2)
          = s1 == s2

 * unpacking rules; e.g. in GHC.Base,
       unpackCString# a
          = build (unpackFoldrCString# a)

 * stream fusion rules; e.g. in the `text` library,
       unstream (S.map safe (S.streamList (GHC.unpackCString# a)))
          = unpackCString# a

Moreover, we want to make it CONLIKE, so that:

* the rules in PrelRules will fire when the string is let-bound.
  E.g. the eqString rule in PrelRules
   eqString (unpackCString# (Lit s1)) (unpackCString# (Lit s2) = s1==s2

* exprIsConApp_maybe will see the string when we have
     let x = unpackCString# "foo"#
     ...(case x of algs)...

All of this goes for unpackCStringUtf8# too.
