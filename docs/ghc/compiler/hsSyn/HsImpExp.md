[[src]](https://github.com/ghc/ghc/tree/master/compiler/hsSyn/HsImpExp.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


HsImpExp: Abstract syntax: imports, exports, interfaces


# \subsection{Import and export declaration lists}


One per \tr{import} declaration in a module.


\# SOURCE \#

# \subsection{Imported and exported entities}


### Note: IEThingWith


A definition like

    module M ( T(MkT, x) ) where
      data T = MkT { x :: Int }

gives rise to

    IEThingWith T [MkT] [FieldLabel "x" False x)]           (without DuplicateRecordFields)
    IEThingWith T [MkT] [FieldLabel "x" True $sel:x:MkT)]   (with    DuplicateRecordFields)

### Note: Representing fields in AvailInfo