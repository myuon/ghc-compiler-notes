[[src]](https://github.com/ghc/ghc/tree/master/compiler/main/Elf.hs)

-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2015
--
-- ELF format tools
--
-----------------------------------------------------------------------------


### Note: ELF specification


   ELF (Executable and Linking Format) is described in the System V Application
   Binary Interface (or ABI). The latter is composed of two parts: a generic
   part and a processor specific part. The generic ABI describes the parts of
   the interface that remain constant across all hardware implementations of
   System V.

   The latest release of the specification of the generic ABI is the version
   4.1 from March 18, 1997:

     - http://www.sco.com/developers/devspecs/gabi41.pdf

   Since 1997, snapshots of the draft for the "next" version are published:

     - http://www.sco.com/developers/gabi/

   Quoting the notice on the website: "There is more than one instance of these
   chapters to permit references to older instances to remain valid. All
   modifications to these chapters are forward-compatible, so that correct use
   of an older specification will not be invalidated by a newer instance.
   Approximately on a yearly basis, a new instance will be saved, as it reaches
   what appears to be a stable state."

   Nevertheless we will see that since 1998 it is not true for Note sections.

   Many ELF sections
   -----------------

   ELF-4.1: the normal section number fields in ELF are limited to 16 bits,
   which runs out of bits when you try to cram in more sections than that. Two
   fields are concerned: the one containing the number of the sections and the
   one containing the index of the section that contains section's names. (The
   same thing applies to the field containing the number of segments, but we
   don't care about it here).

   ELF-next: to solve this, theses fields in the ELF header have an escape
   value (different for each case), and the actual section number is stashed
   into unused fields in the first section header.

   We support this extension as it is forward-compatible with ELF-4.1.
   Moreover, GHC may generate objects with a lot of sections with the
   "function-sections" feature (one section per function).

   Note sections
   -------------

   Sections with type "note" (SHT_NOTE in the specification) are used to add
   arbitrary data into an ELF file. An entry in a note section is composed of a
   name, a type and a value.

   ELF-4.1: "The note information in sections and program header elements holds
   any number of entries, each of which is an array of 4-byte words in the
   format of the target processor." Each entry has the following format:
         | namesz |   Word32: size of the name string (including the ending \0)
         | descsz |   Word32: size of the value
         |  type  |   Word32: type of the note
         |  name  |   Name string (with \0 padding to ensure 4-byte alignment)
         |  ...   |
         |  desc  |   Value (with \0 padding to ensure 4-byte alignment)
         |  ...   |

   ELF-next: "The note information in sections and program header elements
   holds a variable amount of entries. In 64-bit objects (files with
   e_ident[EI_CLASS] equal to ELFCLASS64), each entry is an array of 8-byte
   words in the format of the target processor. In 32-bit objects (files with
   e_ident[EI_CLASS] equal to ELFCLASS32), each entry is an array of 4-byte
   words in the format of the target processor." (from 1998-2015 snapshots)

   This is not forward-compatible with ELF-4.1. In practice, for almost all
   platforms namesz, descz and type fields are 4-byte words for both 32-bit and
   64-bit objects (see elf.h and readelf source code).

   The only exception in readelf source code is for IA_64 machines with OpenVMS
   OS: "This OS has so many departures from the ELF standard that we test it at
   many places" (comment for is_ia64_vms() in readelf.c). In this case, namesz,
   descsz and type fields are 8-byte words and name and value fields are padded
   to ensure 8-byte alignment.

   We don't support this platform in the following code. Reading a note section
   could be done easily (by testing Machine and OS fields in the ELF header).
   Writing a note section, however, requires that we generate a different
   assembly code for GAS depending on the target platform and this is a little
   bit more involved.

