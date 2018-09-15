[[src]](https://github.com/ghc/ghc/tree/master/compiler/codeGen/StgCmmProf.hs)
### Note: Saving the current cost centre

The current cost centre is like a global register.  Like other
global registers, it's a caller-saves one.  But consider
        case (f x) of (p,q) -> rhs
Since 'f' may set the cost centre, we must restore it
before resuming rhs.  So we want code like this:
        local_cc = CCC  -- save
        r = f( x )
        CCC = local_cc  -- restore
That is, we explicitly "save" the current cost centre in
a LocalReg, local_cc; and restore it after the call. The
C-- infrastructure will arrange to save local_cc across the
call.

The same goes for join points;
        let j x = join-stuff
        in blah-blah
We want this kind of code:
        local_cc = CCC  -- save
        blah-blah
     J:
        CCC = local_cc  -- restore
