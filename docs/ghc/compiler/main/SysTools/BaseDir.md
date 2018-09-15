[[src]](https://github.com/ghc/ghc/tree/master/compiler/main/SysTools/BaseDir.hs)

-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2001-2017
--
-- Finding the compiler's base directory.
--
-----------------------------------------------------------------------------


### Note: topdir: How GHC finds its files


GHC needs various support files (library packages, RTS etc), plus
various auxiliary programs (cp, gcc, etc).  It starts by finding topdir,
the root of GHC's support files

On Unix:
  - ghc always has a shell wrapper that passes a -B<dir> option

On Windows:
  - ghc never has a shell wrapper.
  - we can find the location of the ghc binary, which is
        $topdir/<foo>/<something>.exe
    where <something> may be "ghc", "ghc-stage2", or similar
  - we strip off the "<foo>/<something>.exe" to leave $topdir.

from topdir we can find package.conf, ghc-asm, etc.

