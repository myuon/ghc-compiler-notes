`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/cmm/CLabel.hs>`_

====================
compiler/cmm/CLabel.hs.rst
====================

Note [ticky for LNE]
~~~~~~~~~~~~~~~~~~~~~
Until 14 Feb 2013, every ticky counter was associated with a
closure. Thus, ticky labels used IdLabel. It is odd that
CmmBuildInfoTables.cafTransfers would consider such a ticky label
reason to add the name to the CAFEnv (and thus eventually the SRT),
but it was harmless because the ticky was only used if the closure
was also.

Since we now have ticky counters for LNEs, it is no longer the case
that every ticky counter has an actual closure. So I changed the
generation of ticky counters' CLabels to not result in their
associated id ending up in the SRT.

NB IdLabel is still appropriate for ticky ids (as opposed to
CmmLabel) because the LNE's counter is still related to an .hs Id,
that Id just isn't for a proper closure.
-----------------------------------------------------------------------------
Does a CLabel need declaring before use or not?

See wiki:Commentary/Compiler/Backends/PprC#Prototypes


Note [Closure and info labels]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For a function 'foo, we have:
   foo_info    : Points to the info table describing foo's closure
                 (and entry code for foo with tables next to code)
   foo_closure : Static (no-free-var) closure only:
                 points to the statically-allocated closure

For a data constructor (such as Just or Nothing), we have:
    Just_con_info: Info table for the data constructor itself
                   the first word of a heap-allocated Just
    Just_info:     Info table for the *worker function*, an
                   ordinary Haskell function of arity 1 that
                   allocates a (Just x) box:
                      Just = \x -> Just x
    Just_closure:  The closure for this worker

.. code-block:: haskell

    Nothing_closure: a statically allocated closure for Nothing
    Nothing_static_info: info table for Nothing_closure

All these must be exported symbol, EXCEPT Just_info.  We don't need to
export this because in other modules we either have
       * A reference to 'Just'; use Just_closure
       * A saturated call 'Just x'; allocate using Just_con_info
Not exporting these Just_info labels reduces the number of symbols
somewhat.



Note [Bytes label]
~~~~~~~~~~~~~~~~~~
For a top-level string literal 'foo', we have just one symbol 'foo_bytes', which
points to a static data block containing the content of the literal.



Note [Proc-point local block entry-points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A label for a proc-point local block entry-point has no "_entry" suffix. With
`infoTblLbl` we derive an info table label from a proc-point block ID. If
we convert such an info table label into an entry label we must produce
the label without an "_entry" suffix. So an info table label records
the fact that it was derived from a block ID in `IdLabelInfo` as
`BlockInfoTable`.

The info table label and the local block label are both local labels
and are not externally visible.

