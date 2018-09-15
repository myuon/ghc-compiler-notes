[[src]](https://github.com/ghc/ghc/tree/master/compiler/cmm/CLabel.hs)
 instances 

 |
  'CLabel' is an abstract type that supports the following operations:

  - Pretty printing

  - In a C file, does it need to be declared before use?  (i.e. is it
    guaranteed to be already in scope in the places we need to refer to it?)

  - If it needs to be declared, what type (code or data) should it be
    declared to have?

  - Is it visible outside this object file or not?

  - Is it "dynamic" (see details below)

  - Eq and Ord, so that we can make sets of CLabels (currently only
    used in outputting C as far as I can tell, to avoid generating
    more than one declaration for any given label).

  - Converting an info table label into an entry label.

  CLabel usage is a bit messy in GHC as they are used in a number of different
  contexts:

  - By the C-- AST to identify labels

  - By the unregisterised C code generator ("PprC") for naming functions (hence
    the name 'CLabel')

  - By the native and LLVM code generators to identify labels

  For extra fun, each of these uses a slightly different subset of constructors
  (e.g. 'AsmTempLabel' and 'AsmTempDerivedLabel' are used only in the NCG and
  LLVM backends).

  In general, we use 'IdLabel' to represent Haskell things early in the
  pipeline. However, later optimization passes will often represent blocks they
  create with 'LocalBlockLabel' where there is no obvious 'Name' to hang off the
  label.


updatable

offset

updatable

offset

updatable

arity

updatable

arity


Convention:

      <name>_<type>

where <name> is <Module>_<name> for external names and <unique> for
internal names. <type> is one of the following:

         info                   Info table
         srt                    Static reference table
         srtd                   Static reference table descriptor
         entry                  Entry code (function, closure)
         slow                   Slow entry code (if any)
         ret                    Direct return address
         vtbl                   Vector table
         <n>_alt                Case alternative (tag n)
         dflt                   Default case alternative
         btm                    Large bitmap vector
         closure                Static closure
         con_entry              Dynamic Constructor entry code
         con_info               Dynamic Constructor info table
         static_entry           Static Constructor entry code
         static_info            Static Constructor info table
         sel_info               Selector info table
         sel_entry              Selector entry code
         cc                     Cost centre
         ccs                    Cost centre stack

Many of these distinctions are only for documentation reasons.  For
example, _ret is only distinguished from _entry to make it easy to
tell whether a code fragment is a return point or a closure/function
entry.

### Note: Closure and info labels

For a function 'foo, we have:
   foo_info    : Points to the info table describing foo's closure
                 (and entry code for foo with tables next to code)
   foo_closure : Static (no-free-var) closure only:
                 points to the statically-allocated closure

For a data constructor (such as Just or Nothing), we have:
    Just_con_info: Info table for the data constructor itself
                   the first word of a heap-allocated Just
    Just_info:     Info table for the *worker function*, an
                   ordinary Haskell function of arity 1 that
                   allocates a (Just x) box:
                      Just = \x -> Just x
    Just_closure:  The closure for this worker

    Nothing_closure: a statically allocated closure for Nothing
    Nothing_static_info: info table for Nothing_closure

All these must be exported symbol, EXCEPT Just_info.  We don't need to
export this because in other modules we either have
       * A reference to 'Just'; use Just_closure
       * A saturated call 'Just x'; allocate using Just_con_info
Not exporting these Just_info labels reduces the number of symbols
somewhat.

### Note: Bytes label

For a top-level string literal 'foo', we have just one symbol 'foo_bytes', which
points to a static data block containing the content of the literal.

### Note: Proc-point local block entry-points

A label for a proc-point local block entry-point has no "_entry" suffix. With
`infoTblLbl` we derive an info table label from a proc-point block ID. If
we convert such an info table label into an entry label we must produce
the label without an "_entry" suffix. So an info table label records
the fact that it was derived from a block ID in `IdLabelInfo` as
`BlockInfoTable`.

The info table label and the local block label are both local labels
and are not externally visible.
