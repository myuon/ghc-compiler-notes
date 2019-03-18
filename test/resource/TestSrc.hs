{- Note [Head note]
~~~~~~~~~~~~~~~~~~~
This is a note on head.
-}

{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

module TestSrc where

-- Note [Line comment note]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
-- A note is on line comments.

{-
************************************************************************
*                                                                      *
    sub section
*                                                                      *
************************************************************************

Sub section.
-}

{-
Note [Standard note]
~~~~~~~~~~~~~~~~~~~~
Many notes are multi-line comments.
-}

-- | Document comment
someFunc :: Bool -> Int
someFunc = \case
  True  -> 0
  False -> 1

{-
Note [Underlines of notes allow some symbols]
=============================================
See http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#sections
-}

str :: String
str = "str\
  with CPP"
