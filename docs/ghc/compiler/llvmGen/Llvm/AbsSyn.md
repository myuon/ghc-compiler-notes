[[src]](https://github.com/ghc/ghc/tree/master/compiler/llvmGen/Llvm/AbsSyn.hs)
 |
    Assign an expression to a variable:
      * dest:   Variable to assign to
      * source: Source expression
  

 |
    Memory fence operation
  

 |
    Always branch to the target label
  

 |
    Branch to label targetTrue if cond is true otherwise to label targetFalse
      * cond:        condition that will be tested, must be of type i1
      * targetTrue:  label to branch to if cond is true
      * targetFalse: label to branch to if cond is false
  

 |
    Comment
    Plain comment.
  

 |
    Set a label on this position.
      * name: Identifier of this label, unique for this module
  

 |
    Store variable value in pointer ptr. If value is of type t then ptr must
    be of type t*.
      * value: Variable/Constant to store.
      * ptr:   Location to store the value in
  

 |
    Multiway branch
      * scrutinee: Variable or constant which must be of integer type that is
                   determines which arm is chosen.
      * def:       The default label if there is no match in target.
      * target:    A list of (value,label) where the value is an integer
                   constant and label the corresponding label to jump to if the
                   scrutinee matches the value.
  

 |
    Return a result.
      * result: The variable or constant to return
  

 |
    An instruction for the optimizer that the code following is not reachable
  

 |
    Raise an expression to a statement (if don't want result or want to use
    Llvm unnamed values.
  

 |
    A nop LLVM statement. Useful as its often more efficient to use this
    then to wrap LLvmStatement in a Just or [].
  

 |
    A LLVM statement with metadata attached to it.
  

 |
    Allocate amount * sizeof(tp) bytes on the stack
      * tp:     LlvmType to reserve room for
      * amount: The nr of tp's which must be allocated
  

 |
    Perform the machine operator op on the operands left and right
      * op:    operator
      * left:  left operand
      * right: right operand
  

 |
    Perform a compare operation on the operands left and right
      * op:    operator
      * left:  left operand
      * right: right operand
  

 |
    Extract a scalar element from a vector
      * val: The vector
      * idx: The index of the scalar within the vector
  

 |
    Extract a scalar element from a structure
      * val: The structure
      * idx: The index of the scalar within the structure
    Corresponds to "extractvalue" instruction.
  

 |
    Insert a scalar element into a vector
      * val:   The source vector
      * elt:   The scalar to insert
      * index: The index at which to insert the scalar
  

 |
    Allocate amount * sizeof(tp) bytes on the heap
      * tp:     LlvmType to reserve room for
      * amount: The nr of tp's which must be allocated
  

 |
    Load the value at location ptr
  

 |
    Atomic load of the value at location ptr
  

 |
    Navigate in a structure, selecting elements
      * inbound: Is the pointer inbounds? (computed pointer doesn't overflow)
      * ptr:     Location of the structure
      * indexes: A list of indexes to select the correct value.
  

 |
    Cast the variable from to the to type. This is an abstraction of three
    cast operators in Llvm, inttoptr, ptrtoint and bitcast.
       * cast: Cast type
       * from: Variable to cast
       * to:   type to cast to
  

 |
    Atomic read-modify-write operation
       * op:       Atomic operation
       * addr:     Address to modify
       * operand:  Operand to operation
       * ordering: Ordering requirement
  

 |
    Compare-and-exchange operation
       * addr:     Address to modify
       * old:      Expected value
       * new:      New value
       * suc_ord:  Ordering required in success case
       * fail_ord: Ordering required in failure case, can be no stronger than
                   suc_ord

    Result is an @i1@, true if store was successful.
  

 |
    Call a function. The result is the value of the expression.
      * tailJumps: CallType to signal if the function should be tail called
      * fnptrval:  An LLVM value containing a pointer to a function to be
                   invoked. Can be indirect. Should be LMFunction type.
      * args:      Concrete arguments for the parameters
      * attrs:     A list of function attributes for the call. Only NoReturn,
                   NoUnwind, ReadOnly and ReadNone are valid here.
  

 |
    Call a function as above but potentially taking metadata as arguments.
      * tailJumps: CallType to signal if the function should be tail called
      * fnptrval:  An LLVM value containing a pointer to a function to be
                   invoked. Can be indirect. Should be LMFunction type.
      * args:      Arguments that may include metadata.
      * attrs:     A list of function attributes for the call. Only NoReturn,
                   NoUnwind, ReadOnly and ReadNone are valid here.
  

 |
    Merge variables from different basic blocks which are predecessors of this
    basic block in a new variable of type tp.
      * tp:         type of the merged variable, must match the types of the
                    predecessor variables.
      * predecessors: A list of variables and the basic block that they originate
                      from.
  

 |
    Inline assembly expression. Syntax is very similar to the style used by GCC.
      * assembly:    Actual inline assembly code.
      * constraints: Operand constraints.
      * return ty:   Return type of function.
      * vars:        Any variables involved in the assembly code.
      * sideeffect:  Does the expression have side effects not visible from the
                     constraints list.
      * alignstack:  Should the stack be conservatively aligned before this
                     expression is executed.
  

 |
    A LLVM expression with metadata attached to it.
  