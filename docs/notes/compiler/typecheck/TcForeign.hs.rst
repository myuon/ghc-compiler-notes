`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcForeign.hs>`_

compiler/typecheck/TcForeign.hs
===============================


Note [Don't recur in normaliseFfiType']
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcForeign.hs#L80>`__

normaliseFfiType' is the workhorse for normalising a type used in a foreign
declaration. If we have

newtype Age = MkAge Int

we want to see that Age -> IO () is the same as Int -> IO (). But, we don't
need to recur on any type parameters, because no paramaterized types (with
interesting parameters) are marshalable! The full list of marshalable types
is in the body of boxedMarshalableTyCon in TcType. The only members of that
list not at kind * are Ptr, FunPtr, and StablePtr, all of which get marshaled
the same way regardless of type parameter. So, no need to recur into
parameters.

Similarly, we don't need to look in AppTy's, because nothing headed by
an AppTy will be marshalable.



Note [FFI type roles]
~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcForeign.hs#L98>`__

The 'go' helper function within normaliseFfiType' always produces
representational coercions. But, in the "children_only" case, we need to
use these coercions in a TyConAppCo. Accordingly, the roles on the coercions
must be twiddled to match the expectation of the enclosing TyCon. However,
we cannot easily go from an R coercion to an N one, so we forbid N roles
on FFI type constructors. Currently, only two such type constructors exist:
IO and FunPtr. Thus, this is not an onerous burden.

If we ever want to lift this restriction, we would need to make 'go' take
the target role as a parameter. This wouldn't be hard, but it's a complication
not yet necessary and so is not yet implemented.

normaliseFfiType takes the type from an FFI declaration, and
evaluates any type synonyms, type functions, and newtypes. However,
we are only allowed to look through newtypes if the constructor is
in scope.  We return a bag of all the newtype constructors thus found.
Always returns a Representational coercion



Note [Newtype constructor usage in foreign declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/typecheck/TcForeign.hs#L201>`__

GHC automatically "unwraps" newtype constructors in foreign import/export
declarations.  In effect that means that a newtype data constructor is
used even though it is not mentioned expclitly in the source, so we don't
want to report it as "defined but not used" or "imported but not used".
eg     newtype D = MkD Int
       foreign import foo :: D -> IO ()
Here 'MkD' us used.  See #7408.

GHC also expands type functions during this process, so it's not enough
just to look at the free variables of the declaration.
eg     type instance F Bool = D
       foreign import bar :: F Bool -> IO ()
Here again 'MkD' is used.

So we really have wait until the type checker to decide what is used.
That's why tcForeignImports and tecForeignExports return a (Bag GRE)
for the newtype constructors they see. Then TcRnDriver can add them
to the module's usages.

