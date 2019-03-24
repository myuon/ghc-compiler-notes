`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/backpack/NameShape.hs>`_

====================
compiler/backpack/NameShape.hs.rst
====================

Note [NameShape]
~~~~~~~~~~~~~~~~
When we write a declaration in a signature, e.g., data T, we
ascribe to it a *name variable*, e.g., {m.T}.  This
name variable may be substituted with an actual original
name when the signature is implemented (or even if we
merge the signature with one which reexports this entity
from another module).
When we instantiate a signature m with a module M,
we also need to substitute over names.  To do so, we must
compute the *name substitution* induced by the *exports*
of the module in question.  A NameShape represents
such a name substitution for a single module instantiation.
The "shape" in the name comes from the fact that the computation
of a name substitution is essentially the *shaping pass* from
Backpack'14, but in a far more restricted form.
The name substitution for an export list is easy to explain.  If we are
filling the module variable <m>, given an export N of the form
M.n or {m'.n} (where n is an OccName), the induced name
substitution is from {m.n} to N.  So, for example, if we have
A=impl:B, and the exports of impl:B are impl:B.f and
impl:C.g, then our name substitution is {A.f} to impl:B.f
and {A.g} to impl:C.g
The 'NameShape' type is defined in TcRnTypes, because TcRnTypes
needs to refer to NameShape, and having TcRnTypes import
NameShape (even by SOURCE) would cause a large number of
modules to be pulled into the DynFlags cycle.
data NameShape = NameShape {
        ns_mod_name :: ModuleName,
        ns_exports :: [AvailInfo],
        ns_map :: OccEnv Name
    }
NB: substitution functions need 'HscEnv' since they need the name cache
to allocate new names if we change the 'Module' of a 'Name'

