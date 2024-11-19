``Stdcompat``: compatibility module for OCaml standard library
==============================================================

``Stdcompat`` is a compatibility layer allowing programs to use some
recent additions to the OCaml standard library while preserving the
ability to be compiled on former versions of OCaml.

The ``Stdcompat`` API is not intended to be stable, but there will be
efforts to allow future versions of ``Stdcompat`` to be compiled on a
large range of versions of OCaml: ``Stdcompat`` should compile (at least)
on every version of OCaml from 3.08 (included).

The module ``Stdcompat`` provides some definitions for values and
types introduced in recent versions of the standard library. These
definitions are just aliases to the matching definition of the standard
library if the latter is recent enough. Otherwise, the module
``Stdcompat`` provides an alternative implementation.

The signature of ``Stdcompat`` follows the signature of the standard library.
All the modules are defined as sub-modules of ``Stdcompat.Stdlib``, such as
``Stdcompat.Stdlib.Pervasives``, ``Stdcompat.Stdlib.List``, etc. The module
``Stdcompat.Stdlib.Pervasives`` is included in ``Stdcompat.Stdlib``, and
the module ``Stdcompat.Stdlib`` is included in ``Stdcompat`` itself.
For instance, the function
``Stdcompat.really_input_string`` is an alias for
``Stdcompat.Pervasives.really_input_string``,
which is an alias for ``Stdcompat.Stdlib.Pervasives.really_input_string``,
which is itself an alias for
``Pervasives.really_input_string`` when the version of the OCaml
compiler is above 4.02.0, or an alternative definition otherwise.

The types ``Stdcompat.bytes`` and ``Stdcompat.floatarray`` are aliases
to the built-in ones is the latter are available (above 4.02.0 for
``bytes`` and above 4.06.0 for ``floatarray``), and are aliases to
``string`` and ``float array`` respectively otherwise.

Sub-modules match the names of the standard library modules.  For
instance, ``Stdcompat.List.find_opt`` is an alias for
``List.find_opt`` on 4.05.0 and above, or an alternative definition
otherwise. Definitions from the standard library are reexported so that
``Stdcompat`` can be open without hiding unchanged definitions.

Functors ``Set.Make``, ``Map.Make``, ``Hashtbl.Make``, ``Weak.Make``
are redefined to provide the additional definitions appeared on recent
OCaml releases.

If ``Pervasives.result``, ``Uchar.t`` and/or ``Seq.t`` are not
provided by the standard library and if the compatibility packages
``result``, ``uchar``, and/or ``seq`` are available via ``ocamlfind``,
then the types ``Stdcompat.Pervasives.result``, ``Stdcompat.Uchar.t``
and ``Stdcompat.Seq.t`` are defined as alias to the types defined in
those packages (these packages should then appear before ``Stdcompat``
in the linking chain).

Some redefinitions access to the internal representation of
the data structures when they are abstracted: it is the case for
``{Set,Map,Hashtbl,Queue,Stack}.to_seq*``,
``Hashtbl.filter_map_inplace``, ``Hashtbl.stats``, ``Stack.fold``,
``Set.find*``, ``Set.map``.
Pure (but less efficient) implementations are available by configuring
``Stdcompat`` with ``./configure --disable-magic``.

Redefinitions cannot even guarantee some security fixes: for instance,
seeds and randomization are ignored with ``Hashtbl`` prior to 4.00.0.

See the generated documentation (in ``doc/``) for available
definitions.

Configure and install
---------------------

Execute `make -f Makefile.bootstrap` first, to generate the files
`configure`, `Makefile.in` and others.

Then execute `./configure` (or `./configure --disable-magic` to use
the pure and less efficient implementation).

Then execute `make` and `make install`.

Windows support
---------------

`stdcompat` can be used on Windows, both with Cygwin and with
Microsoft Visual Studio port.

The build system relies on `autoconf`, `automake` and `make`,
therefore the easiest way to compile `stdcompat` is to use Cygwin
for compiling, with the same environment as for compiling OCaml:
- `cl.exe` should be in `PATH`: for instance, with Microsoft
  Visual Studio 2017 Community, the `PATH` can be initialized with
  "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvars64.bat";
- `eval $(<ocaml-source-tree>/tools/msvs-promote-path)"`
  should have been executed, so that the command `link.exe` points
  to the right tool;
- moreover, `ocaml.exe`, `ocamlc.exe`, etc., should be in `PATH`.
