`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnFixity.hs>`_

Note [Fixity signature lookup]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A fixity declaration like

.. code-block:: haskell

    infixr 2 ?

can refer to a value-level operator, e.g.:

.. code-block:: haskell

    (?) :: String -> String -> String

or a type-level operator, like:

.. code-block:: haskell

    data (?) a b = A a | B b

so we extend the lookup of the reader name '?' to the TcClsName namespace, as
well as the original namespace.

The extended lookup is also used in other places, like resolution of
deprecation declarations, and lookup of names in GHCi.
------------------------------

