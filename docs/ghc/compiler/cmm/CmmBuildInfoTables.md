[[src]](https://github.com/ghc/ghc/tree/master/compiler/cmm/CmmBuildInfoTables.hs)
 EXAMPLE

f = \x. ... g ...
  where
    g = \y. ... h ... c1 ...
    h = \z. ... c2 ...

c1 & c2 are CAFs

g and h are local functions, but they have no static closures.  When
we generate code for f, we start with a CmmGroup of four CmmDecls:

   [ f_closure, f_entry, g_entry, h_entry ]

we process each CmmDecl separately in cpsTop, giving us a list of
CmmDecls. e.g. for f_entry, we might end up with

   [ f_entry, f1_ret, f2_proc ]

where f1_ret is a return point, and f2_proc is a proc-point.  We have
a CAFSet for each of these CmmDecls, let's suppose they are

   [ f_entry{g_closure}, f1_ret{g_closure}, f2_proc{} ]
   [ g_entry{h_closure, c1_closure} ]
   [ h_entry{c2_closure} ]

Now, note that we cannot use g_closure and h_closure in an SRT,
because there are no static closures corresponding to these functions.
So we have to flatten out the structure, replacing g_closure and
h_closure with their contents:

   [ f_entry{c2_closure, c1_closure}, f1_ret{c2_closure,c1_closure}, f2_proc{} ]
   [ g_entry{c2_closure, c1_closure} ]
   [ h_entry{c2_closure} ]

This is what flattenCAFSets is doing.



### Note: reverse gs


- In each CmmDecl there is a mapping from BlockId -> CmmInfoTable
- The one corresponding to g_entry is the closure info table, the
  rest are continuations.
- Each one needs an SRT.
- We get the CAFSet for each one from the CAFEnv
- flatten gives us
    [(LabelMap CAFSet, CmmDecl)]
-


### Note: reverse gs

   It is important to keep the code blocks in the same order,
   otherwise binary sizes get slightly bigger.  I'm not completely
   sure why this is, perhaps the assembler generates bigger jump
   instructions for forward refs.  --SDM
