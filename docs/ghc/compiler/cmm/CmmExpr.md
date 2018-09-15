[[src]](https://github.com/ghc/ghc/tree/master/compiler/cmm/CmmExpr.hs)
### Note: Old Area

There is a single call area 'Old', allocated at the extreme old
end of the stack frame (ie just younger than the return address)
which holds:
  * incoming (overflow) parameters,
  * outgoing (overflow) parameter to tail calls,
  * outgoing (overflow) result values
  * the update frame (if any)

Its size is the max of all these requirements.  On entry, the stack
pointer will point to the youngest incoming parameter, which is not
necessarily at the young end of the Old area.

End of note 

### Note: CmmStackSlot aliasing

When do two CmmStackSlots alias?

 - T[old+N] aliases with U[young(L)+M] for all T, U, L, N and M
 - T[old+N] aliases with U[old+M] only if the areas actually overlap

Or more informally, different Areas may overlap with each other.

An alternative semantics, that we previously had, was that different
Areas do not overlap.  The problem that lead to redefining the
semantics of stack areas is described below.

e.g. if we had

    x = Sp[old + 8]
    y = Sp[old + 16]

    Sp[young(L) + 8]  = L
    Sp[young(L) + 16] = y
    Sp[young(L) + 24] = x
    call f() returns to L

if areas semantically do not overlap, then we might optimise this to

    Sp[young(L) + 8]  = L
    Sp[young(L) + 16] = Sp[old + 8]
    Sp[young(L) + 24] = Sp[old + 16]
    call f() returns to L

and now young(L) cannot be allocated at the same place as old, and we
are doomed to use more stack.

  - old+8  conflicts with young(L)+8
  - old+16 conflicts with young(L)+16 and young(L)+8

so young(L)+8 == old+24 and we get

    Sp[-8]  = L
    Sp[-16] = Sp[8]
    Sp[-24] = Sp[0]
    Sp -= 24
    call f() returns to L

However, if areas are defined to be "possibly overlapping" in the
semantics, then we cannot commute any loads/stores of old with
young(L), and we will be able to re-use both old+8 and old+16 for
young(L).

    x = Sp[8]
    y = Sp[0]

    Sp[8] = L
    Sp[0] = y
    Sp[-8] = x
    Sp = Sp - 8
    call f() returns to L

Now, the assignments of y go away,

    x = Sp[8]
    Sp[8] = L
    Sp[-8] = x
    Sp = Sp - 8
    call f() returns to L


### Note: Overlapping global registers

The backend might not faithfully implement the abstraction of the STG
machine with independent registers for different values of type
GlobalReg. Specifically, certain pairs of registers (r1, r2) may
overlap in the sense that a store to r1 invalidates the value in r2,
and vice versa.

Currently this occurs only on the x86_64 architecture where FloatReg n
and DoubleReg n are assigned the same microarchitectural register, in
order to allow functions to receive more Float# or Double# arguments
in registers (as opposed to on the stack).

There are no specific rules about which registers might overlap with
which other registers, but presumably it's safe to assume that nothing
will overlap with special registers like Sp or BaseReg.

Use CmmUtils.regsOverlap to determine whether two GlobalRegs overlap
on a particular platform. The instance Eq GlobalReg is syntactic
equality of STG registers and does not take overlap into
account. However it is still used in UserOfRegs/DefinerOfRegs and
there are likely still bugs there, beware!
