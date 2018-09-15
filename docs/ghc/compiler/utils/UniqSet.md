[[src]](https://github.com/ghc/ghc/tree/master/compiler/utils/UniqSet.hs)

(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1994-1998

# Specialised sets, for things with @Uniques@

Based on @UniqFMs@ (as you would expect).

Basically, the things need to be in class @Uniquable@.
