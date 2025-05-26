``py.ml``: OCaml bindings for Python
====================================

``py.ml`` provides OCaml bindings for Python 2 and Python 3.
This library subsumes the ``pycaml`` library, which is no longer
actively maintained.

*OPAM:* ``opam install pyml``

The Python library is linked at runtime and the same executable can be
run in a Python 2 or a Python 3 environment. ``py.ml`` does not
require any Python library at compile time.
The only compile time dependency is
[``Stdcompat``](https://github.com/thierry-martinez/stdcompat) to ensure compatibility
with all OCaml compiler versions from 3.12.

Bindings are split in three modules:

- ``Py`` provides the initialization functions and some high-level
  bindings, with error handling and naming conventions closer to OCaml
  usages.

- ``Pycaml`` provides a signature close to the old ``Pycaml``
  module, so as to ease migration.

- ``Pywrappers`` provides low-level bindings, which follow closely the
  conventions of the C bindings for Python. Submodules
  ``Pywrappers.Python2`` and ``Pywrappers.Python3`` contain version-specific
  bindings.

Custom top-level
----------------

A custom top-level with the C bindings can be compiled by ``make pymltop``.

If you have ``utop`` and ``ocamlfind``, you can ``make pymlutop``.

*For OPAM users:* ``pymltop`` is installed by default by ``opam install pyml``.
``pymlutop`` is installed whenever ``utop`` is available.

Getting started
---------------

``Py.initialize ()`` loads the Python library.

``Py.Run.simple_string "print('Hello, world!')"`` executes a Python phrase
and returns ``true`` if the execution succeeded, ``false`` otherwise.

``Py.Run.eval "18 + 42"`` evaluates a Python phrase and returns a value
of type ``Py.Object.t``. Such a value can then be converted to an OCaml
value: ``assert (Py.Int.to_int (Py.Run.eval "18 + 42") = 60)``. In case of
error (either the phrase is syntactically incorrect or the evaluation raises
an exception), an OCaml exception ``Py.E (type, msg)`` is raised. By default,
``Py.Run.eval`` evaluates an expression; use
``Py.Run.eval ~start:Py.File`` to evaluate a Python phrase as a module/file
(with ``import`` directives and so on).

To make an OCaml value accessible from Python, we create a module (called
``ocaml`` in this example, but it can be any valid Python module name):

```ocaml
let m = Py.Import.add_module "ocaml" in
Py.Module.set m "example_value"
  (Py.List.of_list_map Py.Int.of_int [1;2;3]);
Py.Run.eval ~start:Py.File "
from ocaml import example_value
print(example_value)"
```

OCaml functions can be passed in the same way.

``` ocaml
let m = Py.Import.add_module "ocaml" in
let hello args =
  Printf.printf "Hello, %s!\n" (Py.String.to_string args.(0));
  Py.none in
Py.Module.set m "hello" (Py.Callable.of_function hello);
Py.Run.eval ~start:Py.File "
from ocaml import hello
hello('World')"
```

``Py.Module.set m "hello" (Py.Callable.of_function hello)``
can be written
``Py.Module.set_function m "hello" hello``.

Python functions can be called from OCaml too.

```ocaml
let builtins = Py.Eval.get_builtins () in
let sorted_python = Py.Dict.find_string builtins "sorted" in
let sorted = Py.Callable.to_function sorted_python in
let result =
  sorted [| Py.List.of_list_map Py.Float.of_float [3.0; 2.0] |] in
assert (Py.List.to_list_map Py.Float.to_float result = [2.0; 3.0])
```

``Py.Run.interactive ()`` runs the Python top-loop.
It can be run after some OCaml values have been made accessible through a module
as above. If IPython is available, the top-loop can be run with
``Py.Run.ipython ()``.


With OCaml 4.06 and greater, the module ``Pyops`` declares indexing operators.

| Indexing operator | Getter | Setter |
|-------------------|--------|--------|
| ``x.@(v)`` | ``Py.Object.find_attr`` | ``Py.Object.set_attr`` |
| ``x.@$(v)`` | ``Py.Object.find_attr_string`` / ``Py.Module.get`` | ``Py.Object.set_attr_string`` / ``Py.Module.set `` |
| ``x.![v]`` | ``Py.Object.find`` | ``Py.Object.set_item`` |
| ``x.!$[v]`` | ``Py.Object.find_string`` | ``Py.Object.set_item_string`` |
| ``x.%[v]`` | ``Py.Dict.find`` | ``Py.Dict.set_item`` |
| ``x.%$[v]`` | ``Py.Dict.find_string`` | ``Py.Dict.set_item_string`` |
| ``x.&(v)`` | ``Py.Module.get_function`` | ``Py.Module.set_function`` |

The "hello world" example above can be written:

``` ocaml
let m = Py.Import.add_module "ocaml" in
let open Pyops in
m.&("hello") <- (fun args ->
  Printf.printf "Hello, %s!\n" (Py.String.to_string args.(0));
  Py.none);
Py.Run.eval ~start:Py.File "
from ocaml import hello
hello('World')"
```

Error handling
--------------

All Python exceptions are caught as OCaml exceptions ``Py.E (type, msg)``, where
``type`` and ``msg`` are two Python objects. Typically, one can convert ``type``
and ``msg`` to strings with ``Py.Object.to_string`` to display or analyse them.

When an OCaml function ``f`` is called from Python
(passed by ``Py.Callable.of_function``),
``f`` can raise a Python exception by raising an OCaml exception of the form
``Py.E (type, msg)``. To raise standard errors more conveniently,
``f`` can raise an exception of the form
``Py.Err (type, msg)`` instead,
where ``type`` belongs to the enumeration ``Py.Err.t``
and ``msg`` is an OCaml string.
If ``f`` raises an exception that is neither
of the form ``Py.E`` nor ``Py.Err``, then
this exception is encapsulated with its backtrace in a Python exception
(of class `ocaml exception` derived from `BaseException` and not from
`Exception`, so as not to be caught), leading the Python interpreter to
be interrupted, and the exception is raised back in OCaml.

``Py.Run.simple_string`` catches all Python exceptions (and OCaml
exceptions as well) and returns a single Boolean to indicate
success. One can prefer ``Py.Run.eval`` to get proper error handling.

Data types
----------

Python values have type ``Py.Object.t``.

``Py.String.of_string`` and ``Py.String.to_string`` convert back and forth
OCaml to Python strings: Unicode strings and legacy strings are handled
uniformly.

``Py.Int.of_int`` and ``Py.Int.to_int`` convert back and forth
OCaml to Python integers. For big numbers, one should use an intermediate
textual representation with ``Py.Int.of_string`` and ``Py.Int.to_string``.

``Py.Float.of_float`` and ``Py.Float.to_float`` convert back and forth
OCaml to Python floating-point values.

The module `Py.Number` define common arithmetic and bitwise operations on
Python numbers. It can be open locally to take benefit from operator
overloading. E.g.:

``` ocaml
let m = Py.Import.add_module "ocaml" in
let square args = Py.Number.(args.(0) ** of_int 2) in
Py.Module.set_function m "square" square;
ignore (Py.Run.eval ~start:Py.File "
from ocaml import square
print(square(3))")
```

``Py.Tuple.of_array`` and ``Py.Tuple.to_array`` can be used to construct
and destruct Python tuples.
``Py.Tuple.of_list`` and ``Py.Tuple.to_list`` are available as well.
The empty tuple is ``Py.Tuple.empty``.
Converters can be composed with the ``_map`` suffix:
for example, ``Py.Tuple.of_list_map Py.Int.of_int`` converts a list of integers
to a Python value.
Short tuples can be constructed and destructed with ``Py.Tuple.of_tuple1``,
...,  ``Py.Tuple.of_tuple5`` and ``Py.Tuple.to_tuple1``, ...,
``Py.Tuple.to_tuple5``.

For Python lists,
``Py.List.of_array``, ``Py.List.to_array``,
``Py.List.of_list`` and ``Py.List.to_list`` are available
(along with their ``_map`` variant).
``Py.List`` and ``Py.Tuple`` include the ``Py.Sequence`` module:
along with ``to_array`` and ``to_list``, iterators like ``fold_left``
``fold_right``, ``for_all``, etc. are available.

Python iterators can be iterated with ``Py.Iter.next`` which returns an option
value, where ``None`` represents the end of the iteration. ``Py.Iter.iter``,
``Py.Iter.fold_left``,``Py.Iter.fold_right`` and ``Py.Iter.to_list`` can be
used as well. A Python iterator can be created with ``Py.Iter.create`` from an
OCaml function that returns an option.

Python dictionaries can be constructed and destructed with
``Py.Dict.of_bindings_string`` and ``Py.Dict.to_bindings_string`` from and to
associative lists between string keys and Python values.
``Py.Dict.find_string`` can be used to find a single value (the exception
``Not_found`` is raised if the key is not in the dictionary;
``Py.Dict.get_item_string`` provides the option variant).

Python closures can be called from OCaml with
``Py.Callable.to_function``, or ``Py.Callable.to_function_with_keywords`` to
pass keywords as an associative list.
Symmetrically, OCaml functions can be turned into Python closures
with
``Py.Callable.of_function`` and ``Py.Callable.of_function_with_keywords``
(the latter function passes keywords as a dictionary to the OCaml callback:
values can be retrieved efficiently with ``Py.Dict.find_string``).

```ocaml
let m = Py.Import.add_module "ocaml" in
Py.Module.set_function_with_keywords m "length"
  (fun args kw ->
    let x = Py.Dict.find_string kw "x" in
    let y = Py.Dict.find_string kw "y" in
    Py.Number.((x ** of_int 2 + y ** of_int 2) ** of_float 0.5));
ignore (Py.Run.eval ~start:Py.File "
from ocaml import length
print(length(x=3, y=4))")
```


Modules
-------

New modules can be defined with ``Py.Import.add_module``
and existing modules can be imported with ``Py.Import.import_module``
(or the shorter ``Py.import`` alias).
Trying to import a module that does not exist leads to a Python exception:
use ``Py.Import.import_module_opt`` to get an option result instead
(or the shorter ``Py.import_opt`` alias).

``Module.get`` and ``Module.set`` allow to retrieve and define module
members.
For function members, there are shortcuts to do the conversion with
``Py.Callable``: ``Module.get_function`` and ``Module.set_function``
(and ``Module.get_function_with_keywords``
and ``Module.set_function_with_keywords``).

If we consider the following Python code taken from ``matplotlib``
documentation:

```python
import numpy as np
import matplotlib.pyplot as plt

x = np.arange(0, 5, 0.1);
y = np.sin(x)
plt.plot(x, y)
plt.show()
```

The code can be written directly in OCaml as such:

```ocaml
let np = Py.import "numpy" in
let plt = Py.import "matplotlib.pyplot" in
let x = Py.Module.get_function np "arange"
  (Array.map Py.Float.of_float [| 0.; 5.; 0.1 |]) in
let y = Py.Module.get_function np "sin" [| x |] in
ignore (Py.Module.get_function plt "plot" [| x; y |]);
assert (Py.Module.get_function plt "show" [| |] = Py.none)
```

or, using indexing operators (OCaml 4.06):

```ocaml
let np = Py.import "numpy" in
let plt = Py.import "matplotlib.pyplot" in
let open Pyops in
let x = np.&("arange")(Array.map Py.Float.of_float [| 0.; 5.; 0.1 |]) in
let y = np.&("sin")[| x |] in
ignore (plt.&("plot")[| x; y |]);
assert (plt.&("show")[| |] = Py.none)
```

NumPy
-----

If the NumPy library is installed, then OCaml float arrays and bigarrays
can be shared in place with Python code as NumPy arrays (without copy).
Python code can then directly read and write from and to the OCaml arrays
and changes are readable from OCaml.

```ocaml
let array = [| 1.; 2. ; 3. |] in
let m = Py.Import.add_module "ocaml" in
Py.Module.set m "array" (Py.Array.numpy array);
ignore (Py.Run.eval ~start:Py.File "
from ocaml import array
array *= 2");
assert (array = [| 2.; 4.; 6. |])
```

Bigarrays are handled by the ``Numpy`` module that is shipped in
``numpy.cma/cmxa`` and requires ``bigarray.cma/cmxa``. Numpy arrays can
be obtained from bigarrays with ``Numpy.of_bigarray`` and bigarrays can
be obtained from Numpy arrays with ``Numpy.to_bigarray`` (the provided
kind and layout should match the format of the Numpy array).

```ocaml
let m = Py.Import.add_module "test" in
let callback arg =
  let bigarray =
    Numpy.to_bigarray Bigarray.nativeint Bigarray.c_layout arg.(0) in
  let array1 = Bigarray.array1_of_genarray bigarray in
  assert (Bigarray.Array1.get array1 0 = 0n);
  assert (Bigarray.Array1.get array1 1 = 1n);
  assert (Bigarray.Array1.get array1 2 = 2n);
  assert (Bigarray.Array1.get array1 3 = 3n);
  Py.none in
Py.Module.set m "callback" (Py.Callable.of_function callback);
assert (Py.Run.simple_string "
from test import callback
import numpy
callback(numpy.array([0,1,2,3]))
")
```
