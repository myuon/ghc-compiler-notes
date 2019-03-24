`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/CoreMap.hs>`_

====================
compiler/coreSyn/CoreMap.hs.rst
====================

Note [Binders]
~~~~~~~~~~~~~~
 * In general we check binders as late as possible because types are
   less likely to differ than expression structure.  That's why
      cm_lam :: CoreMapG (TypeMapG a)
   rather than
      cm_lam :: TypeMapG (CoreMapG a)

 * We don't need to look at the type of some binders, notably
     - the case binder in (Case _ b _ _)
     - the binders in an alternative
   because they are totally fixed by the context



Note [Empty case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* For a key (Case e b ty (alt:alts))  we don't need to look the return type
  'ty', because every alternative has that type.

* For a key (Case e b ty []) we MUST look at the return type 'ty', because
  otherwise (Case (error () "urk") _ Int  []) would compare equal to
            (Case (error () "urk") _ Bool [])
  which is utterly wrong (#6097)

We could compare the return type regardless, but the wildly common case
is that it's unnecessary, so we have two fields (cm_case and cm_ecase)
for the two possibilities.  Only cm_ecase looks at the type.

See also Note [Empty case alternatives] in CoreSyn.


Note [Binders]
~~~~~~~~~~~~~~
We need to use 'BndrMap' for 'Coercion', 'CoreExpr' AND 'Type', since all
of these data types have binding forms.

