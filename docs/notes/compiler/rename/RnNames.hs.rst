`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnNames.hs>`_

====================
compiler/rename/RnNames.hs.rst
====================

Note [Tracking Trust Transitively]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we import a package as well as checking that the direct imports are safe
according to the rules outlined in the Note [HscMain . Safe Haskell Trust Check]
we must also check that these rules hold transitively for all dependent modules
and packages. Doing this without caching any trust information would be very
slow as we would need to touch all packages and interface files a module depends
on. To avoid this we make use of the property that if a modules Safe Haskell
mode changes, this triggers a recompilation from that module in the dependcy
graph. So we can just worry mostly about direct imports.

There is one trust property that can change for a package though without
recompliation being triggered: package trust. So we must check that all
packages a module tranitively depends on to be trusted are still trusted when
we are compiling this module (as due to recompilation avoidance some modules
below may not be considered trusted any more without recompilation being
triggered).

We handle this by augmenting the existing transitive list of packages a module M
depends on with a bool for each package that says if it must be trusted when the
module M is being checked for trust. This list of trust required packages for a
single import is gathered in the rnImportDecl function and stored in an
ImportAvails data structure. The union of these trust required packages for all
imports is done by the rnImports function using the combine function which calls
the plusImportAvails function that is a union operation for the ImportAvails
type. This gives us in an ImportAvails structure all packages required to be
trusted for the module we are currently compiling. Checking that these packages
are still trusted (and that direct imports are trusted) is done in
HscMain.checkSafeImports.

See the note below, [Trust Own Package] for a corner case in this method and
how its handled.




Note [Trust Own Package]
~~~~~~~~~~~~~~~~~~~~~~~~
There is a corner case of package trust checking that the usual transitive check
doesn't cover. (For how the usual check operates see the Note [Tracking Trust
Transitively] below). The case is when you import a -XSafe module M and M
imports a -XTrustworthy module N. If N resides in a different package than M,
then the usual check works as M will record a package dependency on N's package
and mark it as required to be trusted. If N resides in the same package as M
though, then importing M should require its own package be trusted due to N
(since M is -XSafe so doesn't create this requirement by itself). The usual
check fails as a module doesn't record a package dependency of its own package.
So instead we now have a bool field in a modules interface file that simply
states if the module requires its own package to be trusted. This field avoids
us having to load all interface files that the module depends on to see if one
is trustworthy.




Note [Trust Transitive Property]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
So there is an interesting design question in regards to transitive trust
checking. Say I have a module B compiled with -XSafe. B is dependent on a bunch
of modules and packages, some packages it requires to be trusted as its using
-XTrustworthy modules from them. Now if I have a module A that doesn't use safe
haskell at all and simply imports B, should A inherit all the trust
requirements from B? Should A now also require that a package p is trusted since
B required it?

We currently say no but saying yes also makes sense. The difference is, if a
module M that doesn't use Safe Haskell imports a module N that does, should all
the trusted package requirements be dropped since M didn't declare that it cares
about Safe Haskell (so -XSafe is more strongly associated with the module doing
the importing) or should it be done still since the author of the module N that
uses Safe Haskell said they cared (so -XSafe is more strongly associated with
the module that was compiled that used it).

Going with yes is a simpler semantics we think and harder for the user to stuff
up but it does mean that Safe Haskell will affect users who don't care about
Safe Haskell as they might grab a package from Cabal which uses safe haskell (say
network) and that packages imports -XTrustworthy modules from another package
(say bytestring), so requires that package is trusted. The user may now get
compilation errors in code that doesn't do anything with Safe Haskell simply
because they are using the network package. They will have to call 'ghc-pkg
trust network' to get everything working. Due to this invasive nature of going
with yes we have gone with no for now.


Note [Combining ImportAvails]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
imp_finsts in ImportAvails is a list of family instance modules
transitively depended on by an import. imp_finsts for a currently
compiled module is a union of all the imp_finsts of imports.
Computing the union of two lists of size N is O(N^2) and if we
do it to M imports we end up with O(M*N^2). That can get very
expensive for bigger module hierarchies.

Union can be optimized to O(N log N) if we use a Set.
imp_finsts is converted back and forth between dep_finsts, so
changing a type of imp_finsts means either paying for the conversions
or changing the type of dep_finsts as well.

I've measured that the conversions would cost 20% of allocations on my
test case, so that can be ruled out.

Changing the type of dep_finsts forces checkFamInsts to
get the module lists in non-deterministic order. If we wanted to restore
the deterministic order, we'd have to sort there, which is an additional
cost. As far as I can tell, using a non-deterministic order is fine there,
but that's a brittle nonlocal property which I'd like to avoid.

Additionally, dep_finsts is read from an interface file, so its "natural"
type is a list. Which makes it a natural type for imp_finsts.

Since rnImports.combine is really the only place that would benefit from
it being a Set, it makes sense to optimize the hot loop in rnImports.combine
without changing the representation.

So here's what we do: instead of naively merging ImportAvails with
plusImportAvails in a loop, we make plusImportAvails merge empty imp_finsts
and compute the union on the side using Sets. When we're done, we can
convert it back to a list. One nice side effect of this approach is that
if there's a lot of overlap in the imp_finsts of imports, the
Set doesn't really need to grow and we don't need to allocate.

Running generateModules from #14693 with DEPTH=16, WIDTH=30 finishes in
23s before, and 11s after.


Note [Top-level Names in Template Haskell decl quotes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also: Note [Interactively-bound Ids in GHCi] in HscTypes
          Note [Looking up Exact RdrNames] in RnEnv

Consider a Template Haskell declaration quotation like this:
      module M where
        f x = h [d| f = 3 |]
When renaming the declarations inside [d| ...|], we treat the
top level binders specially in two ways

1.  We give them an Internal Name, not (as usual) an External one.
    This is done by RnEnv.newTopSrcBinder.

2.  We make them *shadow* the outer bindings.
    See Note [GlobalRdrEnv shadowing]

3. We find out whether we are inside a [d| ... |] by testing the TH
   stage. This is a slight hack, because the stage field was really
   meant for the type checker, and here we are not interested in the
   fields of Brack, hence the error thunks in thRnBrack.


Note [Looking up family names in family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

.. code-block:: haskell

  module M where
    type family T a :: *
    type instance M.T Int = Bool

We might think that we can simply use 'lookupOccRn' when processing the type
instance to look up 'M.T'.  Alas, we can't!  The type family declaration is in
the *same* HsGroup as the type instance declaration.  Hence, as we are
currently collecting the binders declared in that HsGroup, these binders will
not have been added to the global environment yet.

Solution is simple: process the type family declarations first, extend
the environment, and then process the type instances.




Note [Dealing with imports]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
For import M( ies ), we take the mi_exports of M, and make
   imp_occ_env :: OccEnv (Name, AvailInfo, Maybe Name)
One entry for each Name that M exports; the AvailInfo is the
AvailInfo exported from M that exports that Name.

The situation is made more complicated by associated types. E.g.
   module M where
     class    C a    where { data T a }
     instance C Int  where { data T Int = T1 | T2 }
     instance C Bool where { data T Int = T3 }
Then M's export_avails are (recall the AvailTC invariant from Avails.hs)
  C(C,T), T(T,T1,T2,T3)
Notice that T appears *twice*, once as a child and once as a parent. From
this list we construct a raw list including
   T -> (T, T( T1, T2, T3 ), Nothing)
   T -> (C, C( C, T ),       Nothing)
and we combine these (in function 'combine' in 'imp_occ_env' in
'filterImports') to get
   T  -> (T,  T(T,T1,T2,T3), Just C)

So the overall imp_occ_env is
   C  -> (C,  C(C,T),        Nothing)
   T  -> (T,  T(T,T1,T2,T3), Just C)
   T1 -> (T1, T(T,T1,T2,T3), Nothing)   -- similarly T2,T3

If we say
   import M( T(T1,T2) )
then we get *two* Avails:  C(T), T(T1,T2)

Note that the imp_occ_env will have entries for data constructors too,
although we never look up data constructors.


Note [Children for duplicate record fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the module

.. code-block:: haskell

    {-# LANGUAGE DuplicateRecordFields #-}
    module M (F(foo, MkFInt, MkFBool)) where
      data family F a
      data instance F Int = MkFInt { foo :: Int }
      data instance F Bool = MkFBool { foo :: Bool }

The `foo` in the export list refers to *both* selectors! For this
reason, lookupChildren builds an environment that maps the FastString
to a list of items, rather than a single item.


Note [The ImportMap]
~~~~~~~~~~~~~~~~~~~~~~~
The ImportMap is a short-lived intermediate data structure records, for
each import declaration, what stuff brought into scope by that
declaration is actually used in the module.

The SrcLoc is the location of the END of a particular 'import'
declaration.  Why *END*?  Because we don't want to get confused
by the implicit Prelude import. Consider (#7476) the module
    import Foo( foo )
    main = print foo
There is an implicit 'import Prelude(print)', and it gets a SrcSpan
of line 1:1 (just the point, not a span). If we use the *START* of
the SrcSpan to identify the import decl, we'll confuse the implicit
import Prelude with the explicit 'import Foo'.  So we use the END.
It's just a cheap hack; we could equally well use the Span too.

The [GlobalRdrElt] are the things imported from that decl.


Note [Do not warn about Prelude hiding]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We do not warn about
   import Prelude hiding( x, y )
because even if nothing else from Prelude is used, it may be essential to hide
x,y to avoid name-shadowing warnings.  Example (#9061)
   import Prelude hiding( log )
   f x = log where log = ()





Note [Printing minimal imports]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To print the minimal imports we walk over the user-supplied import
decls, and simply trim their import lists.  NB that

  * We do *not* change the 'qualified' or 'as' parts!

  * We do not disard a decl altogether; we might need instances
    from it.  Instead we just trim to an empty import list


Note [Partial export]
~~~~~~~~~~~~~~~~~~~~~
Suppose we have

.. code-block:: haskell

   module A( op ) where
     class C a where
       op :: a -> a

.. code-block:: haskell

   module B where
   import A
   f = ..op...

Then the minimal import for module B is
   import A( op )
not
   import A( C( op ) )
which we would usually generate if C was exported from B.  Hence
the (x `elem` xs) test when deciding what to generate.




Note [Overloaded field import]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
On the other hand, if we have

.. code-block:: haskell

    {-# LANGUAGE DuplicateRecordFields #-}
    module A where
      data T = MkT { foo :: Int }

.. code-block:: haskell

    module B where
      import A
      f = ...foo...

then the minimal import for module B must be
    import A ( T(foo) )
because when DuplicateRecordFields is enabled, field selectors are
not in scope without their enclosing datatype.



