[[src]](https://github.com/ghc/ghc/tree/master/compiler/main/SysTools/Info.hs)
### Note: Run-time linker info

See also: Trac #5240, Trac #6063, Trac #10110

Before 'runLink', we need to be sure to get the relevant information
about the linker we're using at runtime to see if we need any extra
options. For example, GNU ld requires '--reduce-memory-overheads' and
'--hash-size=31' in order to use reasonable amounts of memory (see
trac #5240.) But this isn't supported in GNU gold.

Generally, the linker changing from what was detected at ./configure
time has always been possible using -pgml, but on Linux it can happen
'transparently' by installing packages like binutils-gold, which
change what /usr/bin/ld actually points to.

Clang vs GCC notes:

For gcc, 'gcc -Wl,--version' gives a bunch of output about how to
invoke the linker before the version information string. For 'clang',
the version information for 'ld' is all that's output. For this
reason, we typically need to slurp up all of the standard error output
and look through it.

Other notes:

We cache the LinkerInfo inside DynFlags, since clients may link
multiple times. The definition of LinkerInfo is there to avoid a
circular dependency.



### Note: ELF needed shared libs

Some distributions change the link editor's default handling of
ELF DT_NEEDED tags to include only those shared objects that are
needed to resolve undefined symbols. For Template Haskell we need
the last temporary shared library also if it is not needed for the
currently linked temporary shared library. We specify --no-as-needed
to override the default. This flag exists in GNU ld and GNU gold.

The flag is only needed on ELF systems. On Windows (PE) and Mac OS X
(Mach-O) the flag is not needed.



### Note: Windows static libGCC

The GCC versions being upgraded to in #10726 are configured with
dynamic linking of libgcc supported. This results in libgcc being
linked dynamically when a shared library is created.

This introduces thus an extra dependency on GCC dll that was not
needed before by shared libraries created with GHC. This is a particular
issue on Windows because you get a non-obvious error due to this missing
dependency. This dependent dll is also not commonly on your path.

For this reason using the static libgcc is preferred as it preserves
the same behaviour that existed before. There are however some very good
reasons to have the shared version as well as described on page 181 of
https://gcc.gnu.org/onlinedocs/gcc-5.2.0/gcc.pdf :

"There are several situations in which an application should use the
 shared ‘libgcc’ instead of the static version. The most common of these
 is when the application wishes to throw and catch exceptions across different
 shared libraries. In that case, each of the libraries as well as the application
 itself should use the shared ‘libgcc’. "

