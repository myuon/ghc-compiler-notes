[[src]](https://github.com/ghc/ghc/tree/master/compiler/vectorise/Vectorise/Generic/PAMethods.hs)
### Note: Empty PDatas

We don't support "empty" data types like the following:

  data Empty0
  data Empty1 = MkEmpty1
  data Empty2 = MkEmpty2 Empty0
  ...

There is no parallel data associcated with these types, so there is no where
to store the length of the PDatas array with our standard representation.

Enumerations like the following are ok:
  data Bool = True | False

The native and generic representations are:
  type instance (PDatas Bool)        = VPDs:Bool Sels2
  type instance (PDatas (Repr Bool)) = PSum2s Sels2 (PDatas Void) (PDatas Void)

To take the length of a (PDatas Bool) we take the length of the contained Sels2.
When converting a (PDatas Bool) to a (PDatas (Repr Bool)) we use this length to
initialise the two (PDatas Void) arrays.

However, with this:
  data Empty1 = MkEmpty1

The native and generic representations would be:
  type instance (PDatas Empty1)        = VPDs:Empty1
  type instance (PDatas (Repr Empty1)) = PVoids Int

The 'Int' argument of PVoids is supposed to store the length of the PDatas
array. When converting the (PDatas Empty1) to a (PDatas (Repr Empty1)) we
need to come up with a value for it, but there isn't one.

To fix this we'd need to add an Int field to VPDs:Empty1 as well, but that's
too much hassle and there's no point running a parallel computation on no
data anyway.
