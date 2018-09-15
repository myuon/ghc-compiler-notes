[[src]](https://github.com/ghc/ghc/tree/master/compiler/rename/RnNames.hs)

(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

# Extracting imported and top-level names in scope

# \subsection{rnImports}


### Note: Tracking Trust Transitively

### Note: HscMain . Safe Haskell Trust Check

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

### Note: Trust Own Package

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

### Note: Trust Transitive Property

So there is an interesting design question in regards to transitive trust
checking. Say I have a module B compiled with -XSafe. B is dependent on a bunch
of modules and packages, some packages it requires to be trusted as its using
-XTrustworthy modules from them. Now if I have a module A that doesn't use safe
haskell at all and simply imports B, should A inherit all the the trust
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


 SOURCE 

 SOURCE 

# \subsection{importsFromLocalDecls}


From the top-level declarations of this module produce
        * the lexical environment
        * the ImportAvails
created by its bindings.

### Note: Top-level Names in Template Haskell decl quotes

### Note: Interactively-bound Ids in GHCi

Consider a Template Haskell declaration quotation like this:
      module M where
        f x = h [d| f = 3 |]
When renaming the declarations inside [d| ...|], we treat the
top level binders specially in two ways

1.  We give them an Internal Name, not (as usual) an External one.
    This is done by RnEnv.newTopSrcBinder.

### Note: GlobalRdrEnv shadowing

3. We find out whether we are inside a [d| ... |] by testing the TH
   stage. This is a slight hack, because the stage field was really
   meant for the type checker, and here we are not interested in the
   fields of Brack, hence the error thunks in thRnBrack.


# 

### Note: The Naming story

### Note: Looking up family names in family instances

Consider

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

# \subsection{Filtering imports}


@filterImports@ takes the @ExportEnv@ telling what the imported module makes
available, and filters it through the import spec (if any).

### Note: Dealing with imports

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


# \subsection{Import/Export Utils}


### Note: Children for duplicate record fields

Consider the module

# \subsection{Unused names}


# \subsection{Unused imports}


This code finds which import declarations are unused.  The
specification and implementation notes are here:
  http://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/UnusedImports


### Note: The ImportMap

The ImportMap is a short-lived intermediate data structure records, for
each import declaration, what stuff brought into scope by that
declaration is actually used in the module.

The SrcLoc is the location of the END of a particular 'import'
declaration.  Why *END*?  Because we don't want to get confused
by the implicit Prelude import. Consider (Trac #7476) the module
    import Foo( foo )
    main = print foo
There is an implicit 'import Prelude(print)', and it gets a SrcSpan
of line 1:1 (just the point, not a span). If we use the *START* of
the SrcSpan to identify the import decl, we'll confuse the implicit
import Prelude with the explicit 'import Foo'.  So we use the END.
It's just a cheap hack; we could equally well use the Span too.

The AvailInfos are the things imported from that decl (just a list,
not normalised).


### Note: Do not warn about Prelude hiding

We do not warn about
   import Prelude hiding( x, y )
because even if nothing else from Prelude is used, it may be essential to hide
x,y to avoid name-shadowing warnings.  Example (Trac #9061)
   import Prelude hiding( log )
   f x = log where log = ()



### Note: Printing minimal imports

To print the minimal imports we walk over the user-supplied import
decls, and simply trim their import lists.  NB that

  * We do *not* change the 'qualified' or 'as' parts!

  * We do not disard a decl altogether; we might need instances
    from it.  Instead we just trim to an empty import list


### Note: Partial export

Suppose we have

   module A( op ) where
     class C a where
       op :: a -> a

   module B where
   import A
   f = ..op...

Then the minimal import for module B is
   import A( op )
not
   import A( C( op ) )
which we would usually generate if C was exported from B.  Hence
the (x `elem` xs) test when deciding what to generate.

### Note: Overloaded field import

On the other hand, if we have