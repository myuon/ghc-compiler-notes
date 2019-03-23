Note [Cmm Switches, the general plan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Compiling a high-level switch statement, as it comes out of a STG case
expression, for example, allows for a surprising amount of design decisions.
Therefore, we cleanly separated this from the Stg → Cmm transformation, as
well as from the actual code generation.

The overall plan is:
 * The Stg → Cmm transformation creates a single `SwitchTargets` in
   emitSwitch and emitCmmLitSwitch in StgCmmUtils.hs.
   At this stage, they are unsuitable for code generation.
 * A dedicated Cmm transformation (CmmImplementSwitchPlans) replaces these
   switch statements with code that is suitable for code generation, i.e.
   a nice balanced tree of decisions with dense jump tables in the leafs.
   The actual planning of this tree is performed in pure code in createSwitchPlan
   in this module. See Note [createSwitchPlan].
 * The actual code generation will not do any further processing and
   implement each CmmSwitch with a jump tables.

When compiling to LLVM or C, CmmImplementSwitchPlans leaves the switch
statements alone, as we can turn a SwitchTargets value into a nice
switch-statement in LLVM resp. C, and leave the rest to the compiler.

See Note [CmmSwitch vs. CmmImplementSwitchPlans] why the two module are
separated.
---------------------------------------------------------------------------
 Note [Magic Constants in CmmSwitch]

 There are a lot of heuristics here that depend on magic values where it is
 hard to determine the "best" value (for whatever that means). These are the
 magic values:


Note [Jump Table Offset]
~~~~~~~~~~~~~~~~~~~~~~~~

Usually, the code for a jump table starting at x will first subtract x from
the value, to avoid a large amount of empty entries. But if x is very small,
the extra entries are no worse than the subtraction in terms of code size, and
not having to do the subtraction is quicker.

I.e. instead of
    _u20N:
            leaq -1(%r14),%rax
            jmp *_n20R(,%rax,8)
    _n20R:
            .quad   _c20p
            .quad   _c20q
do
    _u20N:
            jmp *_n20Q(,%r14,8)

    _n20Q:
            .quad   0
            .quad   _c20p
            .quad   _c20q
            .quad   _c20r


Note [createSwitchPlan]
~~~~~~~~~~~~~~~~~~~~~~~

A SwitchPlan describes how a Switch statement is to be broken down into
smaller pieces suitable for code generation.

createSwitchPlan creates such a switch plan, in these steps:
 1. It splits the switch statement at segments of non-default values that
    are too large. See splitAtHoles and Note [Magic Constants in CmmSwitch]
 2. Too small jump tables should be avoided, so we break up smaller pieces
    in breakTooSmall.
 3. We fill in the segments between those pieces with a jump to the default
    label (if there is one), returning a SeparatedList in mkFlatSwitchPlan
 4. We find and replace two less-than branches by a single equal-to-test in
    findSingleValues
 5. The thus collected pieces are assembled to a balanced binary tree.
  Note [Two alts + default]
  ~~~~~~~~~~~~~~~~~~~~~~~~~

Discussion and a bit more info at #14644

When dealing with a switch of the form:
switch(e) {
  case 1: goto l1;
  case 3000: goto l2;
  default: goto ldef;
}

If we treat it as a sparse jump table we would generate:

if (e > 3000) //Check if value is outside of the jump table.
    goto ldef;
else {
    if (e < 3000) { //Compare to upper value
        if(e != 1) //Compare to remaining value
            goto ldef;
          else
            goto l2;
    }
    else
        goto l1;
}

Instead we special case this to :

if (e==1) goto l1;
else if (e==3000) goto l2;
else goto l3;

This means we have:
* Less comparisons for: 1,<3000
* Unchanged for 3000
* One more for >3000

This improves code in a few ways:
* One comparison less means smaller code which helps with cache.
* It exchanges a taken jump for two jumps no taken in the >range case.
  Jumps not taken are cheaper (See Agner guides) making this about as fast.
* For all other cases the first range check is removed making it faster.

The end result is that the change is not measurably slower for the case
>3000 and faster for the other cases.

This makes running this kind of match in an inner loop cheaper by 10-20%
depending on the data.
In nofib this improves wheel-sieve1 by 4-9% depending on problem
size.

We could also add a second conditional jump after the comparison to
keep the range check like this:
    cmp 3000, rArgument
    jg <default>
    je <branch 2>
While this is fairly cheap it made no big difference for the >3000 case
and slowed down all other cases making it not worthwhile.


Note [CmmSwitch vs. CmmImplementSwitchPlans]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
I (Joachim) separated the two somewhat closely related modules

 - CmmSwitch, which provides the CmmSwitchTargets type and contains the strategy
   for implementing a Cmm switch (createSwitchPlan), and
 - CmmImplementSwitchPlans, which contains the actuall Cmm graph modification,

for these reasons:

 * CmmSwitch is very low in the dependency tree, i.e. does not depend on any
   GHC specific modules at all (with the exception of Output and Hoople
   (Literal)). CmmImplementSwitchPlans is the Cmm transformation and hence very
   high in the dependency tree.
 * CmmSwitch provides the CmmSwitchTargets data type, which is abstract, but
   used in CmmNodes.
 * Because CmmSwitch is low in the dependency tree, the separation allows
   for more parallelism when building GHC.
 * The interaction between the modules is very explicit and easy to
   understand, due to the small and simple interface.
