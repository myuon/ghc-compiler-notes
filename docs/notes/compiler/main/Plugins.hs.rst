`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/main/Plugins.hs>`_

compiler/main/Plugins.hs
========================


Note [Source plugins]
~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/main/Plugins.hs#L108>`__

The `Plugin` datatype have been extended by fields that allow access to the
different inner representations that are generated during the compilation
process. These fields are `parsedResultAction`, `renamedResultAction`,
`typeCheckResultAction`, `spliceRunAction` and `interfaceLoadAction`.

The main purpose of these plugins is to help tool developers. They allow
development tools to extract the information about the source code of a big
Haskell project during the normal build procedure. In this case the plugin
acts as the tools access point to the compiler that can be controlled by
compiler flags. This is important because the manipulation of compiler flags
is supported by most build environment.

For the full discussion, check the full proposal at:
https://ghc.haskell.org/trac/ghc/wiki/ExtendedPluginsProposal

