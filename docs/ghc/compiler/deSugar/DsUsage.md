[[src]](https://github.com/ghc/ghc/tree/master/compiler/deSugar/DsUsage.hs)
### Note: Module self-dependency


RnNames.calculateAvails asserts the invariant that a module must not occur in
its own dep_orphs or dep_finsts. However, if we aren't careful this can occur
in the presence of hs-boot files: Consider that we have two modules, A and B,
both with hs-boot files,

    A.hs contains a SOURCE import of B B.hs-boot contains a SOURCE import of A
    A.hs-boot declares an orphan instance A.hs defines the orphan instance

In this case, B's dep_orphs will contain A due to its SOURCE import of A.
Consequently, A will contain itself in its imp_orphs due to its import of B.
This fact would end up being recorded in A's interface file. This would then
break the invariant asserted by calculateAvails that a module does not itself in
its dep_orphs. This was the cause of Trac #14128.



 True
              Even if we used 'import M ()', we have to register a
              usage on the export list because we are sensitive to
              changes in orphan instances/rules.
           False
              In GHC 6.8.x we always returned true, and in
              fact it recorded a dependency on *all* the
              modules underneath in the dependency tree.  This
              happens to make orphans work right, but is too
              expensive: it'll read too many interface files.
              The 'isNothing maybe_iface' check above saved us
              from generating many of these usages (at least in
              one-shot mode), but that's even more bogus!
        