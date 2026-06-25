(** Embedding Python into OCaml.

 (C) arty 2002

 This library is free software; you can redistribute it and/or modify
 it under the terms of the GNU Lesser General Public License as
 published by the Free Software Foundation; either version 2.1 of the
 License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 USA

   A Derivative of Art Yerkes' 2002 Pycaml module.


   Modifications (C) 2005 Dr. Thomas Fischbacher, Giuliano Bordignon,
   Dr. Hans Fangohr, SES, University of Southampton

   More modifications are by Barry Schwartz.
   Copyright (C) 2009 Barry Schwartz.

   Adapted for py.ml by Thierry Martinez.
   Copyright (C) 2016 Thierry Martinez.
*)

(** {2 Background Information} *)

(** The original code is available in Debian as package "pycaml".

    For various reasons, we hijacked it so that we can easily both fix bugs
    and extend it. This is permitted by the Pycaml license (the GNU LGPL).

    Note: the layout and hierarchical structure of the documentation
    could need some more work.
*)

(** {3 OCaml Types, Python Types, and general issues of typing} *)

(** Python objects are wrapped up within OCaml as entities of type [pyobject].
*)
type pyobject = Py.Object.t

(** The following types are slightly esoteric; normally, users of this
    module should not have any need to access them.
*)

type funcptr
type funcent = (funcptr * int * int * bool)

type pymodule_func = {
  pyml_name : string ;
  pyml_func : (pyobject -> pyobject) ;
  pyml_flags : int ;
  pyml_doc : string;
}

val py_profile_reset: unit -> unit
val py_profile_report: unit -> (string * float * float) array
(* name * total_time * nr_calls - Note that we use float to count nr_calls
   to work around fixnum limits! *)

val py_activate_profiling: unit -> bool
val py_deactivate_profiling: unit -> bool

(** As Python is a dynamically typed language, [pyobject] values
    may represent entities of very different nature. The [pytype]
    function maps a pyobject to its type, or rather, a selection
    of types that have been made known to OCaml. The default for
    "unknown" values is OtherType.

    Note (for advanced users only): This in particular holds
    for [PyCObject] values, which are used at present to wrap
    up OCaml values opaquely within Python values: at present,
    these appear to be of type [OtherType], but there might be
    good reason to change this in the future.
*)

type pyobject_type =
  | TupleType
  | BytesType
  | UnicodeType
  | BoolType
  | IntType
  | FloatType
  | ListType
  | NoneType
  | CallableType
  | ModuleType
  | ClassType
  | TypeType
  | DictType
  | NullType
  | CamlpillType
  | OtherType
  | EitherStringType (* Signifies that either of BytesType or UnicodeType is allowed. *)
  | CamlpillSubtype of string (* Signifies that only the particular Camlpill variety is allowed. *)
  | AnyType                   (* Allow any python object. *)

type pyerror_type =
  | Pyerr_Exception
  | Pyerr_StandardError
  | Pyerr_ArithmeticError
  | Pyerr_LookupError
  | Pyerr_AssertionError
  | Pyerr_AttributeError
  | Pyerr_EOFError
  | Pyerr_EnvironmentError
  | Pyerr_FloatingPointError
  | Pyerr_IOError
  | Pyerr_ImportError
  | Pyerr_IndexError
  | Pyerr_KeyError
  | Pyerr_KeyboardInterrupt
  | Pyerr_MemoryError
  | Pyerr_NameError
  | Pyerr_NotImplementedError
  | Pyerr_OSError
  | Pyerr_OverflowError
  | Pyerr_ReferenceError
  | Pyerr_RuntimeError
  | Pyerr_SyntaxError
  | Pyerr_SystemExit
  | Pyerr_TypeError
  | Pyerr_ValueError
  | Pyerr_ZeroDivisionError

exception Pycaml_exn of (pyerror_type * string)

val pytype : pyobject -> pyobject_type

(** Also note the existence of [pytype_name], which maps python types to
    human-readable strings. *)

(* Pycaml contains quite some stuff which we should not
   tell the outside world about. For now, this is in comments,
   but eventually, we should perhaps even remove it from there.
*)

(** {3 Initialization} *)

(** The Python interpreter has to be initialized,
    which is done via [py_initialize]. Note that this module
    does call this function automatically when it is initialized itself,
    so the end user does not have to worry about this.

    Note that Python initialization seems to be idempotent,
    so there should not be any problems if one starts up
    a python interpreter first, and then loads a shared object
    via Python's foreign function interface which itself initializes
    OCaml and pycaml. (Note: However, this still needs more testing!)
*)

val py_initialize : unit -> unit
val py_finalize : unit -> unit

(** {3 Functions from the original Pycaml} *)

(** There is a collection of functions from the original Pycaml module
    which are not-too-well-documented. For some of them, there are
    examples available, and often, one can guess what they are
    supposed to do from their name and type. (Admittedly, this
    is a quite unsatisfactory state of affairs, but on the other
    hand, as it turns out, we will have to use only very few
    of them. So for now, if there is a question, look at
    the source, or ask [t.fischbacher\@soton.ac.uk].)

    In order not to clutter the Pycaml documentation with a block of
    unreadable code, they have been moved to the last section.
*)

(** {3 On wrapping up Ocaml values for Python} *)

(** The Pycaml functions [pywrap_value] and [pyunwrap_value] are
    elementary low-level primitives to make opaque Python values that hold
    OCaml values. As Python is a dynamically typed language, and OCaml is
    a statically typed language, and both achieve safety in a somewhat
    misaligned way, this interface may be considered as dangerous. In
    fact, it allows one to break OCaml type safety by mapping a statically
    typed value to a dynamically typed Python value and back.

    This means that a Python user handing a wrapped-up ocaml value of
    a different type than expected over to an OCaml callback may crash
    the system.

    This module provides an extension to the original Pycaml which
    will have added checks that prevent precisely such a situation and
    therefore is safer. Note however, that at the moment, it is only
    foolproof if one does not mix this up with other [PyCObject]
    Python values. (Presumably, it can be tightened up by introducing
    a new primitive Python type [PyCamlObject]. TODO.)
*)

val pywrap_value : 'a -> pyobject
val pyunwrap_value : pyobject -> 'a

(** {2 Genuine Extensions to the original Pycaml} *)

(** {3 Converting values and handling errors} *)

val py_repr : pyobject -> string

val pylist_fromarray : pyobject array -> pyobject
val pylist_toarray : pyobject -> pyobject array
  (** Map an OCaml array of Python values to a Python list and vice versa.
      (This was just missing.)
  *)

val pylist_set : pyobject -> int -> pyobject -> unit
val pylist_get : pyobject -> int -> pyobject

val pyrefcount: pyobject -> int

val pywrap_closure_docstring : string -> (pyobject -> pyobject) -> pyobject
  (** While the functions in ocaml.* should not be made visible
      to end users directly, it may nevertheless be helpful to be
      able to set docstrings on them.
  *)

(** Return a name-string for an Ocaml Python-Object-Type value.
    Used mainly for debugging and in error messages. *)
val pytype_name: pyobject_type -> string

(** Return the last value that was computed interactively at the Python prompt *)
val python_last_value: unit -> pyobject

val py_true : unit -> pyobject
val py_false : unit -> pyobject
val py_is_true : pyobject -> bool

(**
   A convenient function to make a collection of [pyobject] values
   (which usually will be OCaml callbacks) known to Python in one go.
   The strings give the names under which the corresponding values
   should appear in Python's "[ocaml]" module, which Pycaml will add
   to Python and automatically [import] on the Python side.

   Note that as a convention, one must not register names that start with
   the string "[example_]" or "[sys_]", as those are reserved for internal
   use by Pycaml.
*)
val register_for_python : (string * pyobject) array -> unit
val register_pre_functions_for_python : (string * (string ->pyobject)) array -> unit

val float_array_to_python : float array -> pyobject
val int_array_to_python : int array -> pyobject
  (** These functions provides a quick and convenient way to pass a
      simple array of numbers to Python. Note that {i neither} on the
      OCaml nor on the Python side, the special data structure for efficient
      manipulation of large numerical arrays is used
      (OCaml: bigarray, Python: numarray). Rather, this just maps
      ordinary arrays.
  *)

val py_float_tensor : ?init:(int array -> float) -> int array -> pyobject * (int array -> pyobject -> unit)

(** This little helper creates a nested float array python structure
    that is supposed to represent a multi-indexed tensor, plus a function
    to set tensor entries.
*)

(* XXX These have to be documented! *)
val py_homogeneous_list_as_array :
  ?error_label:string ->
  ?length:int ->
  string -> (pyobject -> bool) -> (pyobject -> 'a) -> pyobject -> 'a array

val py_float_list_as_array :
  ?error_label:string -> ?length:int -> pyobject -> float array

val py_number_list_as_float_array :
  ?error_label:string -> ?length:int -> pyobject -> float array

val py_int_list_as_array :
  ?error_label:string -> ?length:int -> pyobject -> int array

val py_string_list_as_array :
  ?error_label:string -> ?length:int -> pyobject -> string array

val py_list_list_as_array :
  ?error_label:string -> ?length:int -> pyobject -> pyobject array

val py_list_list_as_array2 :
  ?error_label:string -> ?length:int -> pyobject -> pyobject array array

val py_float_list_list_as_array :
  ?error_label:string ->
  ?length_outer:int -> ?length_inner:int -> pyobject -> float array array

val py_number_list_list_as_float_array :
  ?error_label:string ->
  ?length_outer:int -> ?length_inner:int -> pyobject -> float array array


val py_int_list_list_as_array :
  ?error_label:string ->
  ?length_outer:int -> ?length_inner:int -> pyobject -> int array array

val py_string_list_list_as_array :
  ?error_label:string ->
  ?length_outer:int -> ?length_inner:int -> pyobject -> string array array

val unpythonizing_function :
  ?name:string ->
  ?catch_weird_exceptions:bool ->
  ?extra_guards:(pyobject -> string option) array ->
  ?expect_tuple:bool ->
  pyobject_type array -> (pyobject array -> 'a) -> pyobject -> 'a

val pythonize_string : string -> pyobject
val unpythonize_string : pyobject -> string

(** This helper simplifies the creation of OCaml callbacks that
    can be registered in Python's "[ocaml]" module.

    First argument: An array of [pyobject_type] Python types.

    Second argument: A "body" function B mapping an OCaml array
    of Python values to a Python return value.

    Optional argument: An array of extra checks to be performed
    on the arguments, one by one, returning an optional error
    message.

    The body function (as well as the optional checks) will be wrapped
    up in code that first checks for the correct number and the specified
    Python types of arguments, so B can rely on the n'th entry of its Python
    argument array being of the Python type specified in the n'th
    position of the type array.

    XXX Note: we need examples in the documentation!
*)
val python_interfaced_function :
  ?name:string ->
  ?catch_weird_exceptions:bool ->
  ?doc:string ->
  ?extra_guards:(pyobject -> string option) array ->
  pyobject_type array ->
  (pyobject array -> pyobject) -> pyobject

val python_pre_interfaced_function :
  ?catch_weird_exceptions:bool ->
  ?doc:string ->
  ?extra_guards:(pyobject -> string option) array ->
  pyobject_type array ->
  (pyobject array -> pyobject) ->
  (string -> pyobject)


(** Sometimes, we want to manipulate complicated structures via Python
    which are implemented in OCaml, and about whose interna only OCaml
    should know and have to worry. So, all that one can do from Python is
    to place such values in containers (tuples, lists) and retrieve them back,
    pass them around, and hand them over to OCaml callbacks.

    In order to ensure type safety, we have to extend OCaml by a
    primitive dynamic type system for Python-wrapped OCaml
    values. This is based on the following assumptions:

    {ol
    {li The number of different OCaml types we might want to make visible
    to Python is quite limited. (In particular, we do not even try
    to properly support polymorphism.)}
    {li Python should be allowed to take a peek at the type name of a
    wrapped OCaml value at runtime}
    }

    Thus, before one can opaquely wrap up OCaml values in "ocamlpills"
    for Python, one has to register a type name with Pycaml. From the
    Python side, the function [ocaml.sys_ocamlpill_type(x)] will map
    the ocamlpill [x] to the registered type string.
*)
val register_ocamlpill_types : string array -> unit
(** py.ml: Pill types are not required to be registered.
    This function does nothing and is provided for compatibility only. *)

val ocamlpill_type_of : pyobject -> string

(** Given an ocamlpill type name (which was registered before using
    [register_ocamlpill_type]), as well as a witness of the type in
    question in form of a prototypical OCaml value, make a function
    that maps other OCaml values of the same type as the prototype to
    Python ocamlpills. This function is exported to python as
    [ocaml.sys_ocamlpill_type].

    Note: a simple type system hack is used to ensure that the wrapper
    function generated can only be applied to OCaml values of the proper
    type. One major drawback of this is that presumably, the prototypical
    object provided cannot be garbage collected until the wrapper function
    is. (A clever compiler might be able to figure out how to get rid of
    that, though.)

    XXX Provide example code!
*)
val pill_type_mismatch_exception : ?position:'a -> ?exn_name:string -> string -> string -> exn
val check_pill_type : ?position:'a -> ?exn_name:string -> string -> pyobject -> unit
val make_ocamlpill_wrapper_unwrapper : string -> 'a -> ('a -> pyobject) * (pyobject -> 'a) (* Deprecated, I guess.
                                                                                              Use |make_pill_wrapping|
                                                                                              instead. *)
(** py.ml: the signature has been changed from
    [string -> 'a -> ('a -> pyobject) * (pyobject -> 'b)]
    to
    [string -> 'a -> ('a -> pyobject) * (pyobject -> 'a)].
    The second argument is ignored and the function calls {!Py.Capsule.make}.
    Applying the function twice to the same type name raises a failure
    ([Failure _]). *)

val make_pill_wrapping : string -> 'a -> ('a -> pyobject) * (pyobject -> 'a) (* A less cumbersome synonym. *)
(** py.ml: the signature has been changed from
    [string -> 'a -> ('a -> pyobject) * (pyobject -> 'b)]
    to
    [string -> 'a -> ('a -> pyobject) * (pyobject -> 'a)].
    The second argument is ignored and the function calls {!Py.Capsule.make}.
    Applying the function twice to the same type name raises a failure
    ([Failure _]). *)

(** Also, we want to be able to pass optional arguments from python to OCaml.
    The convention which we use for now is as follows:

    - Conceptually, an optional argument has to be a container monadic type.
    - The only thing offered by python which looks like such a thing is the list.
    - Hence, optional values are represented as 0-element or 1-element lists
    on the python side.

    We then need ocaml functions that make it convenient to handle the automatic
    unpacking of such values. (XXX Note: we need examples in the documentation
    that show how to use this!)
*)
val py_optionally : (pyobject -> 'a) -> pyobject -> 'a option

val guarded_pyint_asint: pyobject -> int
val guarded_pyfloat_asfloat: pyobject -> float
val guarded_pynumber_asfloat: pyobject -> float
val guarded_pybytes_asstring: pyobject -> string
val guarded_pylist_toarray: pyobject -> pyobject array
val guarded_pytuple_toarray: pyobject -> pyobject array

val pycallable_asfun : pyobject -> pyobject array -> pyobject


(** This is semi-internal - It should only be used for writing other
    convenience type applicators that have their own way of doing the checking.
*)
val ocamlpill_hard_unwrap : pyobject -> 'a

(** {3 Running and evaluating Python from within OCaml} *)

(** This function allows us to set Python's [sys.argv] *)

val set_python_argv : string array -> unit

(** A convenience function for just letting the Python interpreter
    evaluate a block of Python code.
*)
val python_eval : string -> int

(** One may use [python_eval "execfile(...)"] to load Python code
    into the interpreter. This function provides a slightly nicer way
    to do the same.

    Note 1: Internally, this uses [python_eval]. The [int] return value
    is ignored, however.

    Note 2: As we do not bother to properly escape quotation marks,
    this will not work as supposed on filenames containing double quotes.
    (Yes, this is a bug and should better be fixed!)
*)
val python_load: string -> unit

(** Start the interactive python toplevel: *)
val python : unit -> int

(** Start the ipython toplevel. Note: for still unknown reasons,
    this does not seem to be 100% reliable, and especially seems to fail
    in many situations where [Pycaml.ipython()] is called not from the
    OCaml toplevel. May be some crazy terminal handling bug.

    Addition: 23/01/2006 fangohr:

    On Mac OS X, one of the problems is that there are often several Python
    installations. (One is provided by Apple, but usually a fink or Darwinport
    installation is actually meant to use.) For fink-python (the binary
    installed in /sw/bin, it helps to set the shell environment variable
    PYTHONHOME=/sw .

    Then the call to ipython works fine.

*)
val ipython : unit -> int

(** {2 Python Functions} *)

(** All functions which are made visible from OCaml to python by means of
    [register_for_python] go into the Python module [ocaml]. Usually, one wants
    to place low-level interface functions there and build higher levels of
    abstraction on the python side on top of it which are more convenient
    (maybe object-oriented) to the Python end user. So, the user of a Python
    library that uses OCaml callbacks internally should (ideally) never notice
    the existence of the [ocaml] Python module.

    The following names are pre-registered in the [ocaml] module.
    Note that they all start with the reserved prefixes [sys_] or [example_].

    - [sys_ocamlpill_type]: Function that maps an OCaml pill to a type string,
    so that Python can find out what a given pill is supposed to be. (The OCaml
    function name is [ocamlpill_type_of].)

    - [sys_python]: Function that starts a recursive Python toplevel.
    This may seem strange at first, but actually is highly useful e.g. for
    providing some interactive control deep inside a contrived function during
    debugging. Return value is the value computed last on the recursive python
    command prompt.

    - [example_test_interface]: Function that just prints a test string.

    - [example_the_answer]: The number "42", put in the [ocaml] module by OCaml.

    - [example_make_powers]: A function mapping an integer [n] and a float [p]
    to the array {v [|1.0**p,2.0**p,...,(float_of_int n)**p|] v}.

    - [example_hypotenuse]: A function mapping two floatingpoint values [x,y] to
    [sqrt(x**2+y**2)].

    It is instructive to have a look at the pycaml source providing the
    [example_] entries to see how one can publish other constants and
    functions to Python.
*)

(** {2 Code Examples} *)

(** The implementations of [example_make_powers] and [example_hypotenuse]
    demonstrate how to use [python_interfaced_function]:

    {v
    let _py_make_powers =
    python_interfaced_function
    ~extra_guards:
    [|(fun py_len ->
	let len = pyint_asint py_len in
	if len < 0
	then Some "Negative Length"
	else None);
    (fun _ -> None); (* This check never fails *)
    |]
    [|IntType;FloatType|]
    (fun py_args ->
    let len = pyint_asint py_args.(0)
    and pow = pyfloat_asdouble py_args.(1)
    in
	float_array_to_python
	(Array.init len (fun n -> let nn = float_of_int (n+1) in nn**pow)))
    and
    _py_hypotenuse_2d =
    python_interfaced_function
    [|FloatType;FloatType|]
    (fun py_args ->
    let x = pyfloat_asdouble py_args.(0)
    and y = pyfloat_asdouble py_args.(1)
    in pyfloat_fromdouble (sqrt(x*.x+.y*.y)))
    in
    register_for_python
    [|("example_make_powers", _py_make_powers);
    ("example_hypotenuse", _py_hypotenuse_2d);
    |]
    ;;
    v}

*)

(** {2 Appendix: signatures of undocumented functions from the original Pycaml} *)

val pyerr_print : unit -> unit
val py_exit : int -> unit
val pyerr_printex : int -> unit
val py_setprogramname : string -> unit
val py_setpythonhome : string -> unit
val py_isinitialized : unit -> int
val pyrun_simplestring : string -> int
val pyrun_anyfile : int * string -> int
val pyrun_simplefile : int * string -> int
val pyrun_interactiveone : int * string -> int
val pyrun_interactiveloop : int * string -> int
val py_fdisinteractive : int * string -> int
val pyrun_anyfileex : int * string * int -> int
val pyrun_simplefileex : int * string * int -> int
val py_getprogramname : unit -> string
val py_getpythonhome : unit -> string
val py_getprogramfullpath : unit -> string
val py_getprefix : unit -> string
val py_getexecprefix : unit -> string
val py_getpath : unit -> string
val py_getversion : unit -> string
val py_getplatform : unit -> string
val py_getcopyright : unit -> string
val py_getcompiler : unit -> string
val py_getbuildinfo : unit -> string
val pyrun_string : string * int * pyobject * pyobject -> pyobject
val pyrun_file : int * string * int * pyobject * pyobject -> pyobject
val pyrun_fileex : int * string * int * pyobject * pyobject * int -> pyobject

val py_compilestring : string * string * int -> pyobject

val pyobject_print : pyobject * int * int -> int
val pyobject_repr : pyobject -> pyobject
val pyobject_str : pyobject -> pyobject
val pyobject_unicode : pyobject -> pyobject
val pyobject_richcompare : pyobject * pyobject * int -> pyobject
val pyobject_getattrstring : pyobject * string -> pyobject
val pyobject_getattr : pyobject * pyobject -> pyobject
val pyobject_istrue : pyobject -> int
val pyobject_not : pyobject -> int

val pycallable_check : pyobject -> int

val pyobject_hasattr : pyobject * pyobject -> int
val pyobject_richcomparebool : pyobject * pyobject * int -> int
val pyobject_setattrstring : pyobject * string * pyobject -> int
val pyobject_hasattrstring : pyobject * string -> int

(*  IFDEF PYCAML2 THEN*)
val pyobject_compare : pyobject * pyobject -> int
(*  END*)

(* Currently not implemented.
   val pynumber_coerce : pyobject * pyobject -> (pyobject * pyobject) option
   val pynumber_coerceex : pyobject * pyobject -> (pyobject * pyobject) option
*)

val pyobject_setattr : pyobject * pyobject * pyobject -> int
val pyobject_hash : pyobject -> int64

val pybytes_size : pyobject -> int
val pystring_size : pyobject -> int     (* Legacy support *)

val pybytes_asstring : pyobject -> string
val pystring_asstring : pyobject -> string (* Legacy support *)

val pybytes_asstringandsize : pyobject -> string
val pystring_asstringandsize : pyobject -> string (* Legacy support *)

val pybytes_fromstring : string -> pyobject
val pystring_fromstring : string -> pyobject (* Legacy support *)

(*  IFDEF PYMAJOR2 THEN*)
val pybytes_format : pyobject * pyobject -> pyobject
val pystring_format : pyobject * pyobject -> pyobject (* Legacy support *)
(*  END*)

val pyunicode_asutf8string : pyobject -> pyobject
val pyunicode_asutf16string : pyobject -> pyobject
val pyunicode_asutf32string : pyobject -> pyobject
val pyunicode_decodeutf8 : (string * string option) -> pyobject
val pyunicode_decodeutf16 : (string * string option * int option) -> pyobject
val pyunicode_decodeutf32 : (string * string option * int option) -> pyobject
val pyunicode_fromunicode : (int -> int) -> int -> pyobject
val pyunicode_asunicode   : pyobject -> int array
val pyunicode_getsize     : pyobject -> int

val pydict_new : unit -> pyobject
val pydict_getitem : pyobject * pyobject -> pyobject
val pydict_setitem : pyobject * pyobject * pyobject -> int
val pydict_delitem : pyobject * pyobject -> int
val pydict_clear : pyobject -> unit
  (* val pydict_next : pyobject * int -> (pyobject * pyobject * int) option  <-- currently not implemented *)
val pydict_keys : pyobject -> pyobject
val pydict_values : pyobject -> pyobject
val pydict_items : pyobject -> pyobject
val pydict_copy : pyobject -> pyobject
val pydict_size : pyobject -> int
val pydict_getitemstring : pyobject * string -> pyobject
val pydict_delitemstring : pyobject * string -> int
val pydict_setitemstring : pyobject * string * pyobject -> int

val pyint_fromlong : int64 -> pyobject
val pyint_aslong : pyobject -> int64
(*  IFDEF PYMAJOR2 THEN*)
val pyint_getmax : unit -> int64
(*  END*)

val pyfloat_fromdouble : float -> pyobject
val pyfloat_asdouble : pyobject -> float

val pymodule_new : string -> pyobject
val pymodule_getdict : pyobject -> pyobject
val pymodule_getname : pyobject -> string
val pymodule_getfilename : pyobject -> string

val pytuple_new : int -> pyobject
val pytuple_size : pyobject -> int
val pytuple_getitem : pyobject * int -> pyobject
val pytuple_setitem : pyobject * int * pyobject -> int
val pytuple_getslice : pyobject * int * int -> pyobject
(** py.ml: the result type has been changed from [int] to [pyobject]. *)

val pyslice_new : pyobject * pyobject * pyobject -> pyobject
  (* val pyslice_getindices : pyobject * int -> (int * int * int) option   <- Currently not supported *)

val pyerr_setnone : pyobject -> unit
val pyerr_setobject : pyobject * pyobject -> unit
val pyerr_setstring : pyobject * string -> unit
val pyerr_occurred : unit -> pyobject
val pyerr_clear : unit -> unit
val pyerr_fetch : pyobject * pyobject * pyobject -> pyobject * pyobject * pyobject
val pyerr_givenexceptionmatches : pyobject * pyobject -> int
val pyerr_exceptionmatches : pyobject -> int
val pyerr_normalizeexception : pyobject * pyobject * pyobject -> pyobject * pyobject * pyobject

(*  IFDEF PYMAJOR2 THEN*)
val pyclass_new : pyobject * pyobject * pyobject -> pyobject
val pyinstance_new : pyobject * pyobject * pyobject -> pyobject
val pyinstance_newraw : pyobject * pyobject -> pyobject
(*  END*)

(*  IFDEF PYMAJOR2 THEN*)
val pymethod_new : pyobject * pyobject * pyobject -> pyobject
(*  ELSE*)
(*val pymethod_new : pyobject * pyobject -> pyobject *)
(*  END*)

val pymethod_function : pyobject -> pyobject
val pymethod_self : pyobject -> pyobject
(*  IFDEF PYMAJOR2 THEN*)
val pymethod_class : pyobject -> pyobject
(*  END*)

val pyimport_getmagicnumber : unit -> int64
val pyimport_execcodemodule : pyobject * string -> pyobject
val pyimport_execcodemoduleex : string * pyobject * string -> pyobject
val pyimport_getmoduledict : unit -> pyobject
val pyimport_addmodule : string -> pyobject
val pyimport_importmodule : string -> pyobject
val pyimport_importmoduleex : string * pyobject * pyobject * pyobject -> pyobject
val pyimport_import : pyobject -> pyobject
val pyimport_reloadmodule : pyobject -> pyobject
val pyimport_cleanup : unit -> unit
val pyimport_importfrozenmodule : string -> int

val pyeval_callobjectwithkeywords : pyobject * pyobject * pyobject -> pyobject
val pyeval_callobject : pyobject * pyobject -> pyobject
val pyeval_getbuiltins : unit -> pyobject
val pyeval_getglobals : unit -> pyobject
val pyeval_getlocals : unit -> pyobject
  (* val pyeval_getframe : unit -> pyobject  -- FIX: see comment in stubs code. *)

(*  IFDEF PYMAJOR2 THEN*)
val pyeval_getrestricted : unit -> int
(*  END*)

val pyobject_type : pyobject -> pyobject
val pyobject_size : pyobject -> int
val pyobject_getitem : pyobject * pyobject -> pyobject
val pyobject_setitem : pyobject * pyobject * pyobject -> int
val pyobject_delitem : pyobject * pyobject -> int
val pyobject_ascharbuffer : pyobject -> string
val pyobject_asreadbuffer : pyobject -> string
val pyobject_aswritebuffer : pyobject -> string

val pynumber_check : pyobject -> int
val pynumber_add : pyobject * pyobject -> pyobject
val pynumber_subtract : pyobject * pyobject -> pyobject
val pynumber_multiply : pyobject * pyobject -> pyobject
val pynumber_truedivide : pyobject * pyobject -> pyobject
val pynumber_floordivide : pyobject * pyobject -> pyobject
(*  IFDEF PYMAJOR2 THEN*)
val pynumber_divide : pyobject * pyobject -> pyobject
(*  END*)
val pynumber_remainder : pyobject * pyobject -> pyobject
val pynumber_divmod : pyobject * pyobject -> pyobject
val pynumber_power : pyobject * pyobject * pyobject -> pyobject
val pynumber_negative : pyobject -> pyobject
val pynumber_positive : pyobject -> pyobject
val pynumber_absolute : pyobject -> pyobject
val pynumber_invert : pyobject -> pyobject
val pynumber_lshift : pyobject * pyobject -> pyobject
val pynumber_rshift : pyobject * pyobject -> pyobject
val pynumber_and : pyobject * pyobject -> pyobject
val pynumber_xor : pyobject * pyobject -> pyobject
val pynumber_or : pyobject * pyobject -> pyobject
(*  IFDEF PYMAJOR2 THEN*)
val pynumber_int : pyobject -> pyobject
(*  END*)
val pynumber_long : pyobject -> pyobject
val pynumber_float : pyobject -> pyobject
val pynumber_inplaceadd : pyobject * pyobject -> pyobject
val pynumber_inplacesubtract : pyobject * pyobject -> pyobject
val pynumber_inplacemultiply : pyobject * pyobject -> pyobject
val pynumber_inplacetruedivide : pyobject * pyobject -> pyobject
val pynumber_inplacefloordivide : pyobject * pyobject -> pyobject
(*  IFDEF PYMAJOR2 THEN*)
val pynumber_inplacedivide : pyobject * pyobject -> pyobject
(*  END*)
val pynumber_inplaceremainder : pyobject * pyobject -> pyobject
val pynumber_inplacelshift : pyobject * pyobject -> pyobject
val pynumber_inplacershift : pyobject * pyobject -> pyobject
val pynumber_inplaceand : pyobject * pyobject -> pyobject
val pynumber_inplacexor : pyobject * pyobject -> pyobject
val pynumber_inplaceor : pyobject * pyobject -> pyobject
val pynumber_inplacepower : pyobject * pyobject * pyobject -> pyobject

val pysequence_check : pyobject -> int
val pysequence_size : pyobject -> int
val pysequence_length : pyobject -> int
val pysequence_concat : pyobject * pyobject -> pyobject
val pysequence_repeat : pyobject * int -> pyobject
val pysequence_getitem : pyobject * int -> pyobject
(** py.ml: the result type has been changed from [int] to [pyobject]. *)

val pysequence_getslice : pyobject * int * int -> pyobject
val pysequence_setitem : pyobject * int * pyobject -> int
val pysequence_delitem : pyobject * int -> int
(** py.ml: one of the two [pyobject] arguments has been removed. *)

val pysequence_setslice : pyobject * int * int * pyobject -> int
val pysequence_delslice : pyobject * int * int -> int
val pysequence_tuple : pyobject -> pyobject
val pysequence_list : pyobject -> pyobject
val pysequence_fast : pyobject * string -> pyobject
val pysequence_count : pyobject * pyobject -> int
val pysequence_contains : pyobject * pyobject -> int
val pysequence_in : pyobject * pyobject -> int
val pysequence_index : pyobject * pyobject -> int
val pysequence_inplaceconcat : pyobject * pyobject -> pyobject
val pysequence_inplacerepeat : pyobject * int -> pyobject

val pymapping_check : pyobject -> int
val pymapping_size : pyobject -> int
val pymapping_length : pyobject -> int
val pymapping_haskeystring : pyobject * string -> int
val pymapping_haskey : pyobject * pyobject -> int
val pymapping_getitemstring : pyobject * string -> pyobject
val pymapping_setitemstring : pyobject * string * pyobject -> int

val pyiter_check : pyobject -> int
val pyiter_next : pyobject -> pyobject

val pynull : unit -> pyobject
val pynone : unit -> pyobject

val pytuple_fromarray : pyobject array -> pyobject
val pytuple_fromsingle : pyobject -> pyobject
val pytuple_empty : pyobject
val pytuple2 : pyobject * pyobject -> pyobject
val pytuple3 : pyobject * pyobject * pyobject -> pyobject
val pytuple4 : pyobject * pyobject * pyobject * pyobject -> pyobject
val pytuple5 : pyobject * pyobject * pyobject * pyobject * pyobject -> pyobject

val pyint_fromint : int -> pyobject
val pyint_asint : pyobject -> int
val pytuple_toarray : pyobject -> pyobject array
val pywrap_closure : (pyobject -> pyobject) -> pyobject

(* val version : unit -> string (* This function returns a unique code version string. *) *)
