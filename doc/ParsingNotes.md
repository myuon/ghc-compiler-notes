# Parsing Notes

The implementation: `GHC.Compiler.Notes.Parser.Internal`

## State Transition

### Waiting Note

Any notes start as a new section at top-level comment.

```haskell
{-
Note [Usual Note]
~~~~~~~~~~~~~~~~~
many notes start by multi-line comment.
-}
```

```haskell
{- Note [Example Note]
~~~~~~~~~~~~~~~~~~~~~~
Titles of some notes start by multi-line comment without newline.
-}

```haskell
-- Note [Example Note]
-- ~~~~~~~~~~~~~~~~~~~
-- Some notes often start by single-line comment.
```

```haskell
{-
Note [Example Note]
===================
Some notes use not `~` symbol for new section.
We can use `~` / `=` / `-` / etc. for underline.
See also the reference of reStructuredText: http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#sections
-}
```

### Parsing Note

Continue:

```haskell
-- Some notes often include single-line comments which contents are indented.
```

```haskell
-- Some notes often continue

-- with empty line.
```

```haskell
-- Some notes often include single-line and

{-
multi-line comments.
-}
```

```haskell
{-
Some notes often include

$named-doc
named document comments.
-}
```

End:

```haskell
{-
Note [Example Note]
~~~~~~~~~~~~~~~~~~~
Many notes end with a next document comment.
-}

-- | Document comment for @someFunc@
someFunc :: a
```

```haskell
{-
Note [Example Note]
~~~~~~~~~~~~~~~~~~~
A note.

Note [New Note]
~~~~~~~~~~~~~~~
Many notes end by a new title of note.
-}
```

```haskell
{-
Note [Example Note]
~~~~~~~~~~~~~~~~~~~
Many notes end by a new token not comment.
-}

someFunc :: a
```

```haskell
{-
Note [Example Note]
~~~~~~~~~~~~~~~~~~~
Some notes end by a subsection.
-}

{-
************************************************************************
*                                                                      *
                Sub section
*                                                                      *
************************************************************************
-}
```

```haskell
{-
Note [Example Note]
~~~~~~~~~~~~~~~~~~~
Some notes end by a indented comment.
-}

  -- an indented comment.
```
