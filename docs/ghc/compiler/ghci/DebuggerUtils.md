[[src]](https://github.com/ghc/ghc/tree/master/compiler/ghci/DebuggerUtils.hs)
 To find the string in the constructor's info table we need to consider
      the layout of info tables relative to the entry code for a closure.

      An info table can be next to the entry code for the closure, or it can
      be separate. The former (faster) is used in registerised versions of ghc,
      and the latter (portable) is for non-registerised versions.

      The diagrams below show where the string is to be found relative to
      the normal info table of the closure.

      1) Code next to table:

         --------------
         |            |   <- pointer to the start of the string
         --------------
         |            |   <- the (start of the) info table structure
         |            |
         |            |
         --------------
         | entry code |
         |    ....    |

         In this case the pointer to the start of the string can be found in
         the memory location _one word before_ the first entry in the normal info
         table.

      2) Code NOT next to table:

                                 --------------
         info table structure -> |     *------------------> --------------
                                 |            |             | entry code |
                                 |            |             |    ....    |
                                 --------------
         ptr to start of str ->  |            |
                                 --------------

         In this case the pointer to the start of the string can be found
         in the memory location: info_table_ptr + info_table_size
   