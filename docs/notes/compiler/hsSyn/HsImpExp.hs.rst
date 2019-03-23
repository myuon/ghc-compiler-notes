Note [IEThingWith]
~~~~~~~~~~~~~~~~~~

A definition like

    module M ( T(MkT, x) ) where
      data T = MkT { x :: Int }

gives rise to

    IEThingWith T [MkT] [FieldLabel "x" False x)]           (without DuplicateRecordFields)
    IEThingWith T [MkT] [FieldLabel "x" True $sel:x:MkT)]   (with    DuplicateRecordFields)

See Note [Representing fields in AvailInfo] in Avail for more details.
