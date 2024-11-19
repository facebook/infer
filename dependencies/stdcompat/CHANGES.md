# Version 20

- Compatibility with OCaml 5.1 and 5.2

- Fix `Illegal seek` exception in `In_channel.input_all`

- #23, #24, #25: Fix `dune` build on Windows.
  (reported by Kay-Uwe Kirstein)

- Fix `Makefile`-based build system on Windows.
  (reported by Noteeeeeee, https://github.com/thierry-martinez/pyml/issues/88 )

- Upstream `opam-repository-mingw` patch for DLL extensions
  (suggested by Kay-Uwe Kirstein)

# Version 19, 2022-07-08

- Compatibility with OCaml 5.0

- Add `String.{hash, seeded_hash}`

- Restore compatibility with OCaml 3.07, and fix order execution for
  `Set` and `Map.{iter, fold, filter_map}` on OCaml 3.07

- Updated port to `dune`
  (Marek Kubica, https://github.com/thierry-martinez/stdcompat/pull/16,
   https://github.com/thierry-martinez/stdcompat/pull/19
   with contribution from Kate,
   https://github.com/thierry-martinez/stdcompat/pull/21 )

- Add support for `flambda2`
  (Guillaume Bury, https://github.com/thierry-martinez/stdcompat/pull/14 )

- Prevent replacing `/dev/null/` by a regular file when `./configure` is
  run as root
  (reported by Marc Chevalier, https://github.com/ocaml/ocaml/issues/11302 )

# Version 18, 2022-02-09

- Support for OCaml 4.14 with
  - Lot of new functions in `Seq`
  - `Uchar.utf_decode` and co
  - `In_channel` and `Out_channel` modules
  - `Sys.{development_version, ocaml_release}`

- Add `Stdlib.{acosh,asinh,atanh}` missing from 4.13 and
  `Stdlib.__FUNCTION__` missing from 4.12

- Add module `Unit` missing from 4.08

- Add module `Random`, with new functions introduced from 4.13

- `Filename.chop_suffix` checks that suffixes match and fails otherwise
  (behavior introduced in 4.14)

- `Buffer.add_channel` adds data read from the channel even if `End_of_file` has
  been reached
  (behavior introduced in 4.03)

- Add dependency for generating `.cmt` files
  (reported by Sabyrzhan Tasbolatov)

# Version 17, 2021-09-28

- Fix: missing prototype for `caml_alloc_initialized_string` in `stdcompat.h`

- Fix: linking with `stdcompat__stubs` in bytecode

# Version 16, 2021-07-09

- Support for OCaml 4.13.0 with
  - `Seq.{concat, concat_map}`
  - `{Int{32, 64}, Nativeint}.{min, max}`
  - `Array.{fold_left_map, find_opt, find_map, split, combine}`
  - `Bytes.{fold_left, fold_right, for_all, exists, starts_with, ends_with,
      split_on_char}`
  - `String.{fold_left, fold_right, for_all, exists, starts_with, ends_with,
      get_{u,}int8, get_{{u,}int16,int32,int64}{_ne,_be,_le}}`
  - `Format.{pp_print_either, pp_print_bytes, print_bytes}`

- Add module `Atomic`

- Fix `{Array,Bytes,List,More,String}Labels` modules

- Fix: equality between `Stdcompat.Lexing.lexbuf` and `Lexing.lexbuf` types
  even for OCaml <4.02 (the equality was not preserved since the type of
  lex_buffer was syntactically different, string instead of bytes)

# Version 15, 2021-02-17

- Support for OCaml 4.12.0 with
  - `Sys.{mkdir, rmdir}`
  - `{Set,Map}.to_rev_seq`
  - `Either` module
  - `List.{partition_map, compare, equal}`
  - `Hashtbl.rebuild`
  - `Format.pp_print_seq`

- VERSION file was missing in distributed archive

# Version 14, 2020-05-10

- Support for OCaml 4.11.0 with
  - Array.{for_all2, exists2}
  - Lexing.{set_position, set_filename}
  - List.{filteri, fold_left_map}
  - Printexc.{default_uncaught_exception_handler, Slot.name}
  - Seq.{cons, append, unfold}
  - {Set,Map}.filter_map
  - Printf.{ibprintf, ikbprintf}

- More efficient implementation of Set functions

- Support for version mismatch between ocamlc and ocamlfind packages

# Version 13, 2020-02-22

- Add: stdcompat.h, provides prototype for caml_alloc_initialized_string
  for OCaml <4.06.0 (already defined in a C library linked with
  stdcompat.{cma,cmxa}).

- Fix: module Format was not exported

- Fix: remove reference to suppressed module Sort

- Fix: reference to the native plugin (.cmxs) in META

# Version 12, 2020-02-08

- Support for OCaml 4.10.0

- Support for Windows

- Add Bytes.unsafe_blit_string, Filename.quote_command, List.concat_map,
  List.find_map, Sys.Immediate64

- Equality Lexing.lexbuf = Stdcompat.Lexing.lexbuf is available even for
  OCaml <4.02.0 (before the introduction of bytes)

# Version 11, 2019-09-30

- caml_alloc_initialized_string is now available for OCaml 4.05.0

- Add Printexc.to_string_default and Printexc.use_printers

# Version 10, 2019-06-26

- New C stub with definition of caml_alloc_initialized_string for OCaml <4.05.0

- Fix: Printexc is no longer opaque

- Generate -bin-annot .cmt files for OCaml >=4.00.0

# Version 9, 2019-03-12

- [compatibility break] Pervasives.protect has been moved to Fun.protect
  since its introduction in OCaml trunk

- Support for OCaml 4.08.0: additions to modules Float, Format (mostly
  unimplemented), Fun, List (filter_map), Sys (max_floatarray_length)

- ocamlfind has been added as depopt since its presence changes the behvior
  of configure script

- Printexc has been added: substitution functions are mostly unimplemented,
  except for get_raw_backtrace (which returns ()) and raise_with_backtrace
  (which calls raise)

- Fix: equality between aliases of Seq.t

# Version 8, 2018-12-10

- Fix: module Fun was not exported

- Fix: value Stdlib.protect was not exported

- Fix: equality between aliases of Stdlib.result and of Seq.t

- Fix: Stdlib.result redefined before 4.03 (instead of 4.06)

# Version 7, 2018-11-28

- OCaml 4.08.0 additions: Bool, Fun, Array.t, List.t, Bytes and Buffer
  operations on u?int(8|16|32|64), operations on Float, Filename.chop_suffix_opt

- Modules Format and Printf added to the set of redefined modules

# Version 6, 2018-09-10

- Support for OCaml 4.08.0

- Support VPATH build, i.e. when configure is executed in a build directory
  distinct from the source directory

- Lexing.new_line

- Bigarray: only available from 4.02.0. From 4.02.0 and before 4.07.0,
  --no-alias-deps is used to allow the alias to appear in Stdcompat without
  requiring the library to be linked. Bigarray library should be linked if it is
  effectively used.

- Fix implementations with --disable-magic

- Fix license conflict: The project was intended to be under BSD license
  since the beginning, and a LICENSE file was provided accordingly.
  However, automake generated a COPYING file with the GPLv3 by default. The
  COPYING file now contains the BSD license, and the LICENSE file is removed.
  (reported by Török Edwin,
   https://github.com/thierry-martinez/stdcompat/issues/5)

- Fix auto-generated interfaces for Hashtbl.MakeSeeded

- Exceptions are reexported again. They were missing in auto-generated
  interfaces.

- min_binding_opt and max_binding_opt are not redefined if OCaml >=4.05.

- Array.of_seq is redefined in OCaml 4.07.0 to circumvent a bug in the
  implementation of the standard library. See:
    - https://caml.inria.fr/mantis/view.php?id=7820
    - https://github.com/ocaml/ocaml/pull/1897

# Version 5, 2018-07-11
- Interfaces are auto-generated.

- stdcompat is now free from any required dependency. There are still
  optional dependencies with respect to the packages result, seq and
  uchar: stdcompat takes care of providing types compatible with
  these packages if they are installed.

- Preprocessing is performed by "./configure" script (generated by
  autoconf). cppo and the C preprocessor are no longer used.

- Makefile is generated by automake. ocamlfind is no longer required
  for installation.

- Split implementation into one module for each standard library module
  (suggested by Yotam Barnoy:
   https://github.com/thierry-martinez/stdcompat/issues/4)

- All modules are now exported as sub-modules of Stdlib module
  (as in OCaml 4.07) -- Bigarray is not exported to allow programs not
  to be compiled with this module, this may change in the future.
  (suggested by Yotam Barnoy:
   https://github.com/thierry-martinez/stdcompat/issues/4)

- Compatibility with uchar package

# Version 4, 2018-05-30
- Fix link problems with hypot, copy_sign, ldexp and classify_float

# Version 3, 2018-04-25
- Missing List.of_seq/List.to_seq
- Remove spurious Float.seeded_hash_param
  (suggested by Hezekiah M. Carty:
   https://github.com/thierry-martinez/stdcompat/pull/2)
- Compatibility with seq and result packages
  (suggested by Hezekiah M. Carty:
   https://github.com/thierry-martinez/stdcompat/issues/1)
- Magic implementations of {Set,Map,Hashtbl,Queue,Stack}.to_seq*,
  Hashtbl.filter_map_inplace, Hashtbl.stats, Stack.fold,
  Set.find*, Set.map.
  Pure implementations are available by building with
  "make USE_MAGIC=false"
- jbuild script (Hezekiah M. Carty)

# Version 2, 2018-04-19
- Redefinitions for Pervasives are now in Stdcompat.Pervasives (they were
  previously defined at the root of Stdcompat), and the module
  Stdcompat.Pervasives is included in Stdcompat, so that opening Stdcompat
  makes the redefinitions visible in the scope.
- Float module (OCaml 4.07.0)
- Seq module and of_seq/to_seq/...

# Version 1, 2017-11-14
- Initial release
