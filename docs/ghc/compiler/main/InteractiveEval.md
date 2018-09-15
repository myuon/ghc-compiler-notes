[[src]](https://github.com/ghc/ghc/tree/master/compiler/main/InteractiveEval.hs)
# LANGUAGE CPP, MagicHash, NondecreasingIndentation, UnboxedTuples,
    RecordWildCards, BangPatterns #

 | Finds the enclosing top level function name 

### Note: What to show to users

We don't want to display internally-generated bindings to users.
Things like the coercion axiom for newtypes. These bindings all get
OccNames that users can't write, to avoid the possiblity of name
clashes (in linker symbols).  That gives a convenient way to suppress
them. The relevant predicate is OccName.isDerivedOccName.
See Trac #11051 for more background and examples.
