`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/backpack/RnModIface.hs>`_

Note [rnIfaceNeverExported]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
For the high-level overview, see
Note [Handling never-exported TyThings under Backpack]

When we see a reference to an entity that was defined in a signature,
'rnIfaceGlobal' relies on the identifier in question being part of the
exports of the implementing 'ModIface', so that we can use the exports to
decide how to rename the identifier.  Unfortunately, references to 'DFun's
and 'CoAxiom's will run into trouble under this strategy, because they are
never exported.

Let us consider first what should happen in the absence of promotion.  In
this setting, a reference to a 'DFun' or a 'CoAxiom' can only occur inside
the signature *that is defining it* (as there are no Core terms in
typechecked-only interface files, there's no way for a reference to occur
besides from the defining 'ClsInst' or closed type family).  Thus,
it doesn't really matter what names we give the DFun/CoAxiom, as long
as it's consistent between the declaration site and the use site.

We have to make sure that these bogus names don't get propagated,
but it is fine: see Note [Signature merging DFuns] for the fixups
to the names we do before writing out the merged interface.
(It's even easier for instantiation, since the DFuns all get
dropped entirely; the instances are reexported implicitly.)

Unfortunately, this strategy is not enough in the presence of promotion
(see bug #13149), where modules which import the signature may make
reference to their coercions.  It's not altogether clear how to
fix this case, but it is definitely a bug!
PILES AND PILES OF BOILERPLATE

