[[src]](https://github.com/ghc/ghc/tree/master/compiler/basicTypes/FieldLabel.hs)

%
% (c) Adam Gundry 2013-2015
%

This module defines the representation of FieldLabels as stored in
TyCons.  As well as a selector name, these have some extra structure
to support the DuplicateRecordFields extension.

In the normal case (with NoDuplicateRecordFields), a datatype like

    data T = MkT { foo :: Int }

has

    FieldLabel { flLabel        = "foo"
               , flIsOverloaded = False
               , flSelector     = foo }.

In particular, the Name of the selector has the same string
representation as the label.  If DuplicateRecordFields
is enabled, however, the same declaration instead gives

    FieldLabel { flLabel        = "foo"
               , flIsOverloaded = True
               , flSelector     = $sel:foo:MkT }.

Now the name of the selector ($sel:foo:MkT) does not match the label of
the field (foo).  We must be careful not to show the selector name to
the user!  The point of mangling the selector name is to allow a
module to define the same field label in different datatypes:

    data T = MkT { foo :: Int }
    data U = MkU { foo :: Bool }

Now there will be two FieldLabel values for 'foo', one in T and one in
U.  They share the same label (FieldLabelString), but the selector
functions differ.

### Note: Representing fields in AvailInfo

### Note: Why selector names include data constructors


As explained above, a selector name includes the name of the first
data constructor in the type, so that the same label can appear
multiple times in the same module.  (This is irrespective of whether
the first constructor has that field, for simplicity.)

We use a data constructor name, rather than the type constructor name,
because data family instances do not have a representation type
constructor name generated until relatively late in the typechecking
process.

Of course, datatypes with no constructors cannot have any fields.

