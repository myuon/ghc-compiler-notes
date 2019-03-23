`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/ghci/ByteCodeGen.hs>`_

Note [Implementing tagToEnum#]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(implement_tagToId arg names) compiles code which takes an argument
'arg', (call it i), and enters the i'th closure in the supplied list
as a consequence.  The [Name] is a list of the constructors of this
(enumeration) type.

The code we generate is this:
                push arg
                push bogus-word

                TESTEQ_I 0 L1
                  PUSH_G <lbl for first data con>
                  JMP L_Exit

        L1:     TESTEQ_I 1 L2
                  PUSH_G <lbl for second data con>
                  JMP L_Exit
        ...etc...
        Ln:     TESTEQ_I n L_fail
                  PUSH_G <lbl for last data con>
                  JMP L_Exit

        L_fail: CASEFAIL

        L_exit: SLIDE 1 n
                ENTER

The 'bogus-word' push is because TESTEQ_I expects the top of the stack
to have an info-table, and the next word to have the value to be
tested.  This is very weird, but it's the way it is right now.  See
Interpreter.c.  We don't acutally need an info-table here; we just
need to have the argument to be one-from-top on the stack, hence pushing
a 1-word null. See #8383.

