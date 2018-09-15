[[src]](https://github.com/ghc/ghc/tree/master/compiler/codeGen/StgCmmMonad.hs)
### Note: Virtual and real heap pointers

The code generator can allocate one or more objects contiguously, performing
one heap check to cover allocation of all the objects at once.  Let's call
this little chunk of heap space an "allocation chunk".  The code generator
will emit code to
  * Perform a heap-exhaustion check
  * Move the heap pointer to the end of the allocation chunk
  * Allocate multiple objects within the chunk

The code generator uses VirtualHpOffsets to address words within a
single allocation chunk; these start at one and increase positively.
The first word of the chunk has VirtualHpOffset=1, the second has
VirtualHpOffset=2, and so on.

 * The field realHp tracks (the VirtualHpOffset) where the real Hp
   register is pointing.  Typically it'll be pointing to the end of the
   allocation chunk.

 * The field virtHp gives the VirtualHpOffset of the highest-allocated
   word so far.  It starts at zero (meaning no word has been allocated),
   and increases whenever an object is allocated.

The difference between realHp and virtHp gives the offset from the
real Hp register of a particular word in the allocation chunk. This
is what getHpRelOffset does.  Since the returned offset is relative
to the real Hp register, it is valid only until you change the real
Hp register.  (Changing virtHp doesn't matter.)
