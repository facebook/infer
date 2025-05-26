[*] marks changes that break compatibility with previous versions.

# 2022-09-05

- Support for OCaml 5.0

- Support for Python 3.11.
  All OCaml exceptions raised in callbacks are now encapsulated with their
  backtrace in Python exceptions instead of bypassing the Python interpreter.
  The former behavior led to segmentation faults in the test-suite, and nothing
  indicate that previous versions of Python were supposed to support that well.
  *The new behavior can break existing code*, especially code relying on
  `Py.Run.simple_string`, which now catches all exceptions, including OCaml
  exceptions. If you need proper exception handling, you can use `Py.Run.eval`.
  (reported by Jerry James,
  https://github.com/thierry-martinez/pyml/issues/84)

- New function `Py.Object.dir`.

- New functions `Py.Err.set_interrupt` and, for Python >=3.10,
 `Py.Err.set_interrupt_ex`.

- New functions
  `Py.Dict.{to_bindings_seq, to_bindings_seq_map, to_bindings_string_seq}`.

- New function `Py.Capsule.create`, equivalent to `Py.Capsule.make`, but
  returning the record `{ wrap; unwrap }` of the new type `'a Py.Capsule.t`
  instead of a pair.

- Do not let `python` capture `sigint` by default (can be changed by passing
  `~python_sigint:true` to `Py.initialize`): `Ctrl+C` now interrupts the
  program, even after `Py.initialize` is called.
  (reported by Arulselvan Madhavan,
  https://github.com/thierry-martinez/pyml/issues/83)

- Bindings for exceptions: `PyExc_EncodingWarning` (added in Python 3.10),
  `PyExc_ResourceWarning` (added in Python 3.2)
  (reported by Jerry James,
  https://github.com/thierry-martinez/pyml/issues/84)

- Fixes in bindings for `PyCompilerFlags`, `PyMarshal_WriteObjectToFile`,
  and `PySet_Clear`
  (reported by Jerry James,
  https://github.com/thierry-martinez/pyml/issues/84)

# 2022-06-15

- `Numpy.to_bigarray_k` is continuation-passing-style version of `Numpy.to_bigarray`,
  allowing caller to convert Numpy arrays to bigarrays without having to know
  the kind and the layout of the array
  (suggested by Lindsay Errington and Andie Sigler,
  https://github.com/thierry-martinez/pyml/issues/81)

# 2022-03-25

- Fix debug build detection

- Expose the function `Py.Callable.handle_errors`

# 2022-03-22

- New function `Py.Import.exec_code_module_from_string`
  (suggested by Francois Berenger, https://github.com/thierry-martinez/pyml/issues/78)

- New function `Py.Module.compile` provides a better API than `Py.compile`.

- `Py.Object.t` can now be serialized (with Marshal or output_value), using Python
  pickle module

- Cross-compiling friendly architecture detection
  (suggested by @EduardoRFS, https://discuss.ocaml.org/t/a-zoo-of-values-for-system/8525/20)

- Detect macro `unix` instead of `__linux__`, to handle *BSD OSes
  (reported by Chris Pinnock, https://github.com/thierry-martinez/pyml/issues/74)

- Fix bug in Windows

- Null checks for many functions raising OCaml exceptions, instead of segmentation fault
  (initial implementation by Laurent Mazare, https://github.com/thierry-martinez/pyml/pull/72)

- Fix wide character conversion bugs leading to segmentation fault in Py_wfopen
  (fixed by Jerry James, https://github.com/thierry-martinez/pyml/pull/75)

- `Gc.full_major ()` before unloading `libpython` in `Py.finalize`, to prevent
  segfaulting on finalizing dangling references to Python values after the library
  had been unloaded
  (reported by Denis Efremov on coccinelle mailing list)

- Fix segmentation fault when `~debug_build:true` was passed to `Py.initialize`
  (reported by Stéphane Glondu, https://github.com/thierry-martinez/pyml/issues/79)

# 2021-10-15

- More portable architecture detection
  (inspired by the discussion https://discuss.ocaml.org/t/a-zoo-of-values-for-system/8525
   initiated by Olaf Hering, with helpful comments from Daniel Bünzli,
   @EduardoRFS, David Allsopp, kit-ty-kate and jbeckford)

- Better compatibility with Windows

- Correct version detection for Python 3.10

# 2021-09-24

- Use `dune` as default build system
  (dunification done by Laurent Mazare, https://github.com/thierry-martinez/pyml/pull/28)

  This should in particular fix build problems of reverse dependencies
  with the byte-code compiler
  (reported by @nicoTolly, https://github.com/thierry-martinez/pyml/issues/62)

    - Handle more platforms with dune
      (reported by Olaf Hering, https://github.com/thierry-martinez/pyml/issues/68)

    - `pyutils` is no longer used by generate and is shipped with `pyml` package
      as it was the case with Makefile-based build system
      (reported by Olaf Hering, https://github.com/thierry-martinez/pyml/issues/69)

- Support for raising exceptions with traceback from OCaml
  (implemented by Laurent Mazare, https://github.com/thierry-martinez/pyml/pull/65)

- Fix soundness bug with `numpy`
  (reported by Richard Alligier, https://github.com/thierry-martinez/pyml/pull/65)

- Fix `Py.Array.numpy` arrays on 32-bit platforms
  (reported by Olaf Hering, https://github.com/thierry-martinez/pyml/pull/70)

- Fix soundness bug on strings with OCaml <4.06 (reported by OCaml CI)

# 2021-02-26

- Compatibility with Python 3.10 (reported by Richard W.M. Jones)

    - `PyObject_AsCharBuffer`, `PyObject_AsReadBuffer`,
      `PyObject_AsWriteBuffer` bindings are marked optional as they have
      been removed in Python 3.10.

    - `Py_fopen` is optional and `Py_wfopen` is used instead if available.

- More general handling of Unix architectures.
  (Fixed by Pino Toscano, https://github.com/thierry-martinez/pyml/pull/57)

- Add `Py.Set` module for Python sets.
  (Added by Laurent Mazare, https://github.com/thierry-martinez/pyml/pull/58)

- Fix #61: `Numpy.to_bigarray` raises an exception if source value is
  not a Numpy array instead of segfaulting.
  (Reported by Jonathan Laurent,
   https://github.com/thierry-martinez/pyml/issues/61)

- Fix #56, #59: Add `python-config` heuristics to find Python library.
  (Reported by Anders Thuné and Nils Becker,
   https://github.com/thierry-martinez/pyml/issues/56
   https://github.com/thierry-martinez/pyml/issues/59)

- Fix `import_module_opt` for Python <3.6
  (Reported by opam CI.)

# 2020-05-18

- Fix: Add an `__iter__` method to python iterators.
  (Fixed by Laurent Mazare, https://github.com/thierry-martinez/pyml/pull/47)

- Add `Py.Seq.{of_seq_map, to_seq_map, unsafe_to_seq_map, of_list,
  of_list_map}` functions.

- Remove `Py.Import.cleanup`, which has been removed from Python 3.9, and
  was marked "for internal use only" before.
  (Reported by Victor Stinner,
   https://github.com/thierry-martinez/pyml/issues/49)

- Fix: memory leak in `pyml_wrap_closure`
  (Fixed by Laurent Mazare, https://github.com/thierry-martinez/pyml/pull/53)

- Add `Py.Module.set_docstring`, for Python >=3.5.
  (Added by Laurent Mazare, https://github.com/thierry-martinez/pyml/pull/54)

- Fix: install `.cmx` files
  (Reported by Jonathan Laurent,
  https://github.com/thierry-martinez/pyml/issues/55)

# 2020-02-22

- Fix: do not fail if GIL functions are unavailable

- Fix: include `stdcompat.h` provided with stdcompat version 13 for the
  prototype of `caml_alloc_initialized_string`.
 
- Fix: reference to the native plugin (`.cmxs`) in META

# 2020-01-15

- Compatible with OCaml 4.10.0.

- [PR 36] GC issue when registering a function with a dynamically
  allocated docstring.
  (Fixed by Laurent Mazare, https://github.com/thierry-martinez/pyml/pull/36)

- [PR 34] Ensure that every function starting with "CAMLparamK" ends with
  "CAMLreturnX".
  (Fixed by Xavier Clerc, https://github.com/thierry-martinez/pyml/pull/34)

- [GitHub issue #37] Fix test suite: 'list' object has no attribute 'clear'
  (Reported by Olaf Hering, https://github.com/thierry-martinez/pyml/issues/37)

- [PR 38] Check for executable called python3.
  (Fixed by Olaf Hering, https://github.com/thierry-martinez/pyml/pull/38)

- [PR 39] Expose is-instance and is-subclass.
  (Contribution by Laurent Mazare,
   https://github.com/thierry-martinez/pyml/pull/39)

- [PR 44] Expose some GIL functions (functions from the Python C API
  related to the global interpreter lock.
  (Contribution by Laurent Mazare,
   https://github.com/thierry-martinez/pyml/pull/44)

- Fix dynamic loading of stubs.
  (Fixed by Stéphane Glondu)

# 2019-06-26

- Support for debug build of Python library
  (Suggested by Arlen Cox:
   https://github.com/thierry-martinez/pyml/issues/18)
- Bug fix in pyml_check_symbol_available
- `Py.compile` is a wrapper for the built-in function `compile`
  (Suggested by Dhruv Makwana:
   https://github.com/thierry-martinez/pyml/issues/25)
- Guarantees for structural and physical equalities on `Py.Object.t`
  are now documented. New predicates Py.is_none, Py.is_null, Py.Bool.is_true,
  Py.Bool.is_false, Py.Tuple.is_empty.
  (Suggested by Laurent Mazare:
   https://github.com/thierry-martinez/pyml/pull/31)
- Fix Py.Array.numpy to handle OCaml GC's moving the floatarray
  (Reported by Ilias Garnier:
   https://github.com/thierry-martinez/pyml/issues/30)

# 2018-05-30

- `Py.import` is an alias for `Py.Import.import_module`.
- Use `*_opt` naming convention for the functions that return an option
  instead of an exception: `Py.import_opt`, `Py.Object.find_opt`,...
- of_seq/to_seq converters
- [*] get_attr/get_attr_string now returns option type
- Indexing operators (for OCaml 4.06.0 and above) defined in Pyops
