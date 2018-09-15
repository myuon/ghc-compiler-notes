[[src]](https://github.com/ghc/ghc/tree/master/compiler/main/CmdLineParser.hs)
### Note: Handling errors when parsing commandline flags

Parsing of static and mode flags happens before any session is started, i.e.,
before the first call to 'GHC.withGhc'. Therefore, to report errors for
invalid usage of these two types of flags, we can not call any function that
needs DynFlags, as there are no DynFlags available yet (unsafeGlobalDynFlags
is not set either). So we always print "on the commandline" as the location,
which is true except for Api users, which is probably ok.

When reporting errors for invalid usage of dynamic flags we /can/ make use of
DynFlags, and we do so explicitly in DynFlags.parseDynamicFlagsFull.

Before, we called unsafeGlobalDynFlags when an invalid (combination of)
flag(s) was given on the commandline, resulting in panics (#9963).
