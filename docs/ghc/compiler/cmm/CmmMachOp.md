[[src]](https://github.com/ghc/ghc/tree/master/compiler/cmm/CmmMachOp.hs)
 |
Machine-level primops; ones which we can reasonably delegate to the
native code generators to handle.

Most operations are parameterised by the 'Width' that they operate on.
Some operations have separate signed and unsigned versions, and float
and integer versions.


 |
Returns 'True' if the MachOp has commutable arguments.  This is used
in the platform-independent Cmm optimisations.

If in doubt, return 'False'.  This generates worse code on the
native routes, but is otherwise harmless.


 |
Returns 'True' if the MachOp is associative (i.e. @(x+y)+z == x+(y+z)@)
This is used in the platform-independent Cmm optimisations.

If in doubt, return 'False'.  This generates worse code on the
native routes, but is otherwise harmless.


 |
Returns 'True' if the MachOp is a comparison.

If in doubt, return False.  This generates worse code on the
native routes, but is otherwise harmless.


 |
Returns @Just w@ if the operation is an integer comparison with width
@w@, or @Nothing@ otherwise.


 |
Returns the MachRep of the result of a MachOp.
