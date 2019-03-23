Note [What to show to users]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't want to display internally-generated bindings to users.
Things like the coercion axiom for newtypes. These bindings all get
OccNames that users can't write, to avoid the possibility of name
clashes (in linker symbols).  That gives a convenient way to suppress
them. The relevant predicate is OccName.isDerivedOccName.
See #11051 for more background and examples.
