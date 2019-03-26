`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/iface/TcIface.hs>`_

compiler/iface/TcIface.hs
=========================


Note [Knot-tying typecheckIface]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/iface/TcIface.hs#L116>`__

Suppose we are typechecking an interface A.hi, and we come across
a Name for another entity defined in A.hi.  How do we get the
'TyCon', in this case?  There are three cases:

    1) tcHiBootIface in TcIface: We're typechecking an hi-boot file in
    preparation of checking if the hs file we're building
    is compatible.  In this case, we want all of the internal
    TyCons to MATCH the ones that we just constructed during
    typechecking: the knot is thus tied through if_rec_types.

    2) retypecheckLoop in GhcMake: We are retypechecking a
    mutually recursive cluster of hi files, in order to ensure
    that all of the references refer to each other correctly.
    In this case, the knot is tied through the HPT passed in,
    which contains all of the interfaces we are in the process
    of typechecking.

    3) genModDetails in HscMain: We are typechecking an
    old interface to generate the ModDetails.  In this case,
    we do the same thing as (2) and pass in an HPT with
    the HomeModInfo being generated to tie knots.

The upshot is that the CLIENT of this function is responsible
for making sure that the knot is tied correctly.  If you don't,
then you'll get a message saying that we couldn't load the
declaration you wanted.

BTW, in one-shot mode we never call typecheckIface; instead,
loadInterface handles type-checking interface.  In that case,
knots are tied through the EPS.  No problem!
Clients of this function be careful, see Note [Knot-tying typecheckIface]



Note [Role merging]
~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/iface/TcIface.hs#L239>`__

First, why might it be necessary to do a non-trivial role
merge?  It may rescue a merge that might otherwise fail:

::

     signature A where
         type role T nominal representational
         data T a b

::

     signature A where
         type role T representational nominal
         data T a b

A module that defines T as representational in both arguments
would successfully fill both signatures, so it would be better
if we merged the roles of these types in some nontrivial
way.

However, we have to be very careful about how we go about
doing this, because role subtyping is *conditional* on
the supertype being NOT representationally injective, e.g.,
if we have instead:

::

     signature A where
         type role T nominal representational
         data T a b = T a b

::

     signature A where
         type role T representational nominal
         data T a b = T a b

Should we merge the definitions of T so that the roles are R/R (or N/N)?
Absolutely not: neither resulting type is a subtype of the original
types (see Note [Role subtyping]), because data is not representationally
injective.

Thus, merging only occurs when BOTH TyCons in question are
representationally injective.  If they're not, no merge.



Note [Resolving never-exported Names in TcIface]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/iface/TcIface.hs#L440>`__

For the high-level overview, see
Note [Handling never-exported TyThings under Backpack]

As described in 'typecheckIfacesForMerging', the splendid innovation
of signature merging is to rewrite all Names in each of the signatures
we are merging together to a pre-merged structure; this is the key
ingredient that lets us solve some problems when merging type
synonyms.

However, when a 'Name' refers to a NON-exported entity, as is the
case with the DFun of a ClsInst, or a CoAxiom of a type family,
this strategy causes problems: if we pick one and rewrite all
references to a shared 'Name', we will accidentally fail to check
if the DFun or CoAxioms are compatible, as they will never be
checked--only exported entities are checked for compatibility,
and a non-exported TyThing is checked WHEN we are checking the
ClsInst or type family for compatibility in checkBootDeclM.
By virtue of the fact that everything's been pointed to the merged
declaration, you'll never notice there's a difference even if there
is one.

Fortunately, there are only a few places in the interface declarations
where this can occur, so we replace those calls with 'tcIfaceImplicit',
which will consult a local TypeEnv that records any never-exported
TyThings which we should wire up with.

Note that we actually knot-tie this local TypeEnv (the 'fixM'), because a
type family can refer to a coercion axiom, all of which are done in one go
when we typecheck 'mi_decls'.  An alternate strategy would be to typecheck
coercions first before type families, but that seemed more fragile.



Note [Synonym kind loop]
~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/iface/TcIface.hs#L991>`__

Notice that we eagerly grab the *kind* from the interface file, but
build a forkM thunk for the *rhs* (and family stuff).  To see why,
consider this (#2412)

M.hs:       module M where { import X; data T = MkT S }
X.hs:       module X where { import {-# SOURCE #-} M; type S = T }
M.hs-boot:  module M where { data T }

When kind-checking M.hs we need S's kind.  But we do not want to
find S's kind from (typeKind S-rhs), because we don't want to look at
S-rhs yet!  Since S is imported from X.hi, S gets just one chance to
be defined, and we must not do that until we've finished with M.T.

Solution: record S's kind in the interface file; now we can safely
look at it.



Note [Tying the knot]
~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/iface/TcIface.hs#L1616>`__

The if_rec_types field is used when we are compiling M.hs, which indirectly
imports Foo.hi, which mentions M.T Then we look up M.T in M's type
environment, which is splatted into if_rec_types after we've built M's type
envt.

This is a dark and complicated part of GHC type checking, with a lot
of moving parts.  Interested readers should also look at:

     * Note [Knot-tying typecheckIface]
     * Note [DFun knot-tying]
     * Note [hsc_type_env_var hack]
     * Note [Knot-tying fallback on boot]

There is also a wiki page on the subject, see:

::

     https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/TyingTheKnot



Note [Knot-tying fallback on boot]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/iface/TcIface.hs#L1635>`__

Suppose that you are typechecking A.hs, which transitively imports,
via B.hs, A.hs-boot. When we poke on B.hs and discover that it
has a reference to a type T from A, what TyThing should we wire
it up with? Clearly, if we have already typechecked T and
added it into the type environment, we should go ahead and use that
type. But what if we haven't typechecked it yet?

For the longest time, GHC adopted the policy that this was

