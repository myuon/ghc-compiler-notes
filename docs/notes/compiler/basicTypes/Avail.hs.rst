`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/Avail.hs>`_

compiler/basicTypes/Avail.hs
============================


Note [Representing fields in AvailInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/basicTypes/Avail.hs#L76>`__

When -XDuplicateRecordFields is disabled (the normal case), a
datatype like

::

  data T = MkT { foo :: Int }

..

gives rise to the AvailInfo

::

  AvailTC T [T, MkT] [FieldLabel "foo" False foo]

..

whereas if -XDuplicateRecordFields is enabled it gives

::

  AvailTC T [T, MkT] [FieldLabel "foo" True $sel:foo:MkT]

..

since the label does not match the selector name.

The labels in a field list are not necessarily unique:
data families allow the same parent (the family tycon) to have
multiple distinct fields with the same label. For example,

::

  data family F a
  data instance F Int  = MkFInt { foo :: Int }
  data instance F Bool = MkFBool { foo :: Bool}

..

gives rise to

::

  AvailTC F [ F, MkFInt, MkFBool ]
            [ FieldLabel "foo" True $sel:foo:MkFInt
            , FieldLabel "foo" True $sel:foo:MkFBool ]

..

Moreover, note that the flIsOverloaded flag need not be the same for
all the elements of the list.  In the example above, this occurs if
the two data instances are defined in different modules, one with
`-XDuplicateRecordFields` enabled and one with it disabled.  Thus it
is possible to have

::

  AvailTC F [ F, MkFInt, MkFBool ]
            [ FieldLabel "foo" True $sel:foo:MkFInt
            , FieldLabel "foo" False foo ]

..

If the two data instances are defined in different modules, both
without `-XDuplicateRecordFields`, it will be impossible to export
them from the same module (even with `-XDuplicateRecordfields`
enabled), because they would be represented identically.  The
workaround here is to enable `-XDuplicateRecordFields` on the defining
modules.

