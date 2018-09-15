[[src]](https://github.com/ghc/ghc/tree/master/compiler/basicTypes/Name.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

# @Name@: to transmit name info from renamer to typechecker

# \subsection[Name-datatype]{The @Name@ datatype, and name construction}



Notes about the NameSorts:

1.  Initially, top-level Ids (including locally-defined ones) get External names,
    and all other local Ids get Internal names

### Note: The Name Cache

3.  Things with a External name are given C static labels, so they finally
    appear in the .o file's symbol table.  They appear in the symbol table
    in the form M.n.  If originally-local things have this property they
    must be made @External@ first.

4.  In the tidy-core phase, a External that is not visible to an importer
    is changed to Internal, and a Internal that is visible is changed to External

5.  A System Name differs in the following ways:
        a) has unique attached when printing dumps
        b) unifier eliminates sys tyvars in favour of user provs where possible

    Before anything gets printed in interface files or output code, it's
    fed through a 'tidy' processor, which zaps the OccNames to have
    unique names; and converts all sys-locals to user locals
    If any desugarer sys-locals have survived that far, they get changed to
    "ds1", "ds2", etc.

Built-in syntax => It's a syntactic form, not "in scope" (e.g. [])

Wired-in thing  => The thing (Id, TyCon) is fully known to the compiler,
                   not read from an interface file.
                   E.g. Bool, True, Int, Float, and many others

All built-in syntax is for wired-in things.


# \subsection{Predicates on names}


# \subsection{Making names}


# \subsection{Hashing and comparison}


# \subsection[Name-instances]{Instance declarations}


# \subsection{Binary}


# \subsection{Pretty printing}


# \subsection{Overloaded functions related to Names}


### Note: Special treatment for kind *

Do not put parens around the kind '*'.  Even though it looks like
an operator, it is really a special case.

This pprPrefixName stuff is really only used when printing HsSyn,
which has to be polymorphic in the name type, and hence has to go via
the overloaded function pprPrefixOcc.  It's easier where we know the
type being pretty printed; eg the pretty-printing code in TyCoRep.

See Trac #7645, which led to this.
