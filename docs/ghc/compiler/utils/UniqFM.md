[[src]](https://github.com/ghc/ghc/tree/master/compiler/utils/UniqFM.hs)

(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1994-1998


UniqFM: Specialised finite maps, for things with @Uniques@.

Basically, the things need to be in class @Uniquable@, and we use the
@getUnique@ method to grab their @Uniques@.

(A similar thing to @UniqSet@, as opposed to @Set@.)

The interface is based on @FiniteMap@s, but the implementation uses
@Data.IntMap@, which is both maintained and faster than the past
implementation (see commit log).

The @UniqFM@ interface maps directly to Data.IntMap, only
``Data.IntMap.union'' is left-biased and ``plusUFM'' right-biased
and ``addToUFM\_C'' and ``Data.IntMap.insertWith'' differ in the order
of arguments of combining function.
