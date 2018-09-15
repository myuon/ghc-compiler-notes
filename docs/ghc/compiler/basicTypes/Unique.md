[[src]](https://github.com/ghc/ghc/tree/master/compiler/basicTypes/Unique.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


@Uniques@ are used to distinguish entities in the compiler (@Ids@,
@Classes@, etc.) from each other.  Thus, @Uniques@ are the basic
comparison key in the compiler.

If there is any single operation that needs to be fast, it is @Unique@

comparison.  Unsurprisingly, there is quite a bit of huff-and-puff
directed to that end.

Some of the other hair in this code is to be able to use a
``splittable @UniqueSupply@'' if requested/possible (not standard
Haskell).


# \subsection[Unique-type]{@Unique@ type and operations}


The @Chars@ are ``tag letters'' that identify the @UniqueSupply@.
Fast comparison is everything on @Uniques@:



Now come the functions which construct uniques from their pieces, and vice versa.
The stuff about unique *supplies* is handled further down this module.


# \subsection[Uniquable-class]{The @Uniquable@ class}


# \subsection[Unique-instances]{Instance declarations for @Unique@}


And the whole point (besides uniqueness) is fast equality.  We don't
use `deriving' because we want {\em precise} control of ordering
(equality on @Uniques@ is v common).


### Note: No Ord for Unique

### Note: Unique Determinism

### Note: Deterministic UniqFM

# \subsection[Utils-base62]{Base-62 numbers}


A character-stingy way to read/write numbers (notably Uniques).
The ``62-its'' are \tr{[0-9a-zA-Z]}.  We don't handle negative Ints.
Code stolen from Lennart.


# \subsection[Uniques-prelude]{@Uniques@ for wired-in Prelude things}


Allocation of unique supply characters:
        v,t,u : for renumbering value-, type- and usage- vars.
        B:   builtin
        C-E: pseudo uniques     (used in native-code generator)
        X:   uniques derived by deriveUnique
        _:   unifiable tyvars   (above)
        0-9: prelude things below
             (no numbers left any more..)
        ::   (prelude) parallel array data constructors

        other a-z: lower case chars for unique supplies.  Used so far:

        d       desugarer
        f       AbsC flattener
        g       SimplStg
        k       constraint tuple tycons
        m       constraint tuple datacons
        n       Native codegen
        r       Hsc name cache
        s       simplifier
        z       anonymous sums
