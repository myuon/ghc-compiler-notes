Note [Exporting constructors of marshallable foreign types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
One might expect that IntPtr, WordPtr, and the other newtypes in the
Foreign.C.Types and System.Posix.Types modules to be abstract, but this is not
the case in GHC (see #5229 and #11983). In fact, we deliberately export
the constructors for these datatypes in order to satisfy a requirement of the
Haskell 2010 Report (§ 8.4.2) that if a newtype is used in a foreign
declaration, then its constructor must be visible.

This requirement was motivated by the fact that using a type in a foreign
declaration necessarily exposes some information about the type to the user,
so being able to use abstract types in a foreign declaration breaks their
abstraction (see #3008). As a result, the constructors of all FFI-related
newtypes in base must be exported in order to be useful for FFI programming,
even at the cost of exposing their underlying, architecture-dependent types.
