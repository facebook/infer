(** OCaml Interface for Python. *)

(** Call [initialize ()] first. *)

val initialize: ?library_name:string -> ?interpreter:string -> ?version:int ->
  ?minor:int -> ?verbose:bool -> ?debug_build:bool -> ?python_sigint:bool ->
  unit -> unit
(** [initialize ~interpreter ~version ~minor ~verbose ~debug_build ()] finds
    and loads the Python library.
    This function should be called before any other functions, except
    if explicitely mentioned.
    If [library_name] is given, it is used as the path for the library to
    be loaded: in this case, version parameters are ignored.
    If [library_name] is not given, the library is searched as described
    below.
    [version] should specify the major version number of Python (2 or 3).
    [minor] should specify the minor version number.
    If no version number is given, the version of Python is determined by the
    output of the shell command [python --version].
    If an [interpreter] executable name is given, this executable is
    used in place of [python] in the previous command line.
    The library is searched by
    using [pkg-config] if available, by considering system paths, and
    in the directory [../lib] relatively to the directory where the
    [python] executable is. If the library has been statically linked
    with the executable, it will be used.
    When [verbose] is [true] (default: [false]), library filenames that are
    tried to be loaded are printed on standard error.
    [debug_build] specifies whether the Python library is a debug build:
    if the argument is left unspecified, debug build is detected
    automatically.
    If [python_sigint] is [true] (default: [false]), the function let
    [pythonlib] take handle on [sigint], preventing programs from
    being interrupted by [Ctrl+C]. When [python_sigint] is [false]
    (the default), the previous signal behavior of [sigint] is restored after
    the library has been loaded (so, [Ctrl+C] will still interrupt the
    program, unless this behavior was changed elsewhere). *)

val finalize: unit -> unit
(** [finalize ()] unloads the library. No other functions except
    [initialize ()] should be called afterwards. *)

val on_finalize: (unit -> unit) -> unit
(** [on_finalize f] registers [f ()] to be executed when [finalize] is
    executed. *)

val is_initialized: unit -> bool
(** [is_initialized ()] returns [true] if the library is initialized
    ([initialize ()] has been called and [finalize ()] has not been
    called afterwards). *)

val is_debug_build: unit -> bool
(** [is_debug_build ()] returns [true] if the library is a debug build. *)

val get_library_filename: unit -> string option
(** [get_library_filename ()] returns [Some filename] where [filename] is the
    path to the Python library that has been loaded, or [None] if no Python
    library has been loaded (for example, if the library has been statically
    linked with the executable). *)

val version: unit -> string
(** [version ()] returns the version of the Python library. E.g. ["3.5.1"]. *)

val version_major: unit -> int
(** [version_major ()] returns the major number (the first component) of the
    version of the Python library, either [2] or [3]. *)

val version_minor: unit -> int
(** [version_minor ()] returns the minor number (the second component) of the
    version of the Python library. *)

val version_pair: unit -> int * int
(** [version_pair ()] returns the major and the minor numbers of the
    version of the Python library. *)

type compare = Pytypes.compare = LT | LE | EQ | NE | GT | GE

(** Either a filename or a channel.
    Channels suppose that the same C runtime has been used to compile both the
    Python library and the OCaml runtime.
    Warning: using channels is unsafe if runtimes differ (can lead to
    segmentation fault).*)
type 'a file = 'a Pytypes.file = Filename of string | Channel of 'a

val check_error: unit -> unit

(** General functions to handle Python values *)
module Object: sig
  type t = Pytypes.pyobject
  (** The type of a Python value.

   Structural comparison of values of type [Py.Object.t] rely on
   Python comparison of underlying values. That is to say, if [u] and [v]
   are two values of type [Py.Object.t], and by abuse of notations, if we
   denote also [u] and [v] their respective value in Python, we have [u = v]
   in OCaml if and only if [u == v] in Python, and [u < v] in OCaml if and
   only if [u < v] in Python, etc.

   Moreover, there are five values which are handled specially:
   - {!val:Py.null}: the value [NULL] used in the Python API for error case
   - {!val:Py.none}: the value [None];
   - {!val:Py.Bool.t}: the value [True];
   - {!val:Py.Bool.f}: the value [False];
   - {!val:Py.Tuple.empty}: the value [()].

   These values are guaranteed to be unique, so that the physical equality
   can be used to compare against their definitions: for instance, a value
   [v] of type [Py.Object.t] is [None] if and only if [v == Py.none].
   *)

  val del_attr: t -> t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_DelAttr} PyObject_DelAttr} *)

  val del_attr_string: t -> string -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_DelAttrString} PyObject_DelAttrString} *)

  val del_item: t -> t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_DelItem} PyObject_DelItem} *)

  val del_item_string: t -> string -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_DelItemString} PyObject_DelItemString} *)

  val get_attr: t -> t -> t option
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_GetAttr} PyObject_GetAttr} *)

  val find_attr: t -> t -> t
  (** Equivalent to {!get_attr} but raises a [Not_found] exception in
      case of failure. *)

  val find_attr_opt: t -> t -> t option
  (** Alias for {!get_attr}. *)

  val get_attr_string: t -> string -> t option
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_GetAttrString} PyObject_GetAttrString} *)

  val find_attr_string: t -> string -> t
  (** Equivalent to {!get_attr_string} but raises a [Not_found] exception in
      case of failure. *)

  val find_attr_string_opt: t -> string -> t option
  (** Alias for {!get_attr_string}. *)

  val get_item: t -> t -> t option
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_GetItem} PyObject_GetItem} *)

  val find: t -> t -> t
  (** Equivalent to {!get_item} but raises a [Not_found] exception in
      case of failure. *)

  val find_opt: t -> t -> t option
  (** Alias for {!get_item}. *)

  val get_item_string: t -> string -> t option
  (** [get_item_string o key] returns the element corresponding to the object
      [key] or [None] on failure. *)

  val find_string: t -> string -> t
  (** Equivalent to {!get_item_string} but raises a [Not_found] exception in
      case of failure. *)

  val find_string_opt: t -> string -> t option
  (** Alias for {!get_item_string}. *)

  val get_iter: t -> t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_GetIter} PyObject_GetIter} *)

  val get_type: t -> t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_GetType} PyObject_GetType} *)

  val has_attr: t -> t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_HasAttr} PyObject_HasAttr} *)

  val has_attr_string: t -> string -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_HasAttrString} PyObject_HasAttrString} *)

  val hash: t -> int64
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_Hash} PyObject_Hash} *)

  val is_true: t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_IsTrue} PyObject_IsTrue} *)

  val not: t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_Not} PyObject_Not} *)

  val is_instance: t -> t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_IsInstance} PyObject_IsInstance} *)

  val is_subclass: t -> t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_IsSubclass} PyObject_IsSubclass} *)

  val print: t -> out_channel file -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_Print} PyObject_Print} *)

  val repr: t -> t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_Repr} PyObject_Repr} *)

  val rich_compare: t -> t -> compare -> t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_RichCompare} PyObject_RichCompare} *)

  val rich_compare_bool: t -> t -> compare -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_RichCompareBool} PyObject_RichCompareBool} *)

  val set_attr: t -> t -> t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_SetAttr} PyObject_SetAttr} *)

  val set_attr_string: t -> string -> t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_SetAttrString} PyObject_SetAttrString} *)

  val set_item: t -> t -> t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_SetItem} PyObject_SetItem} *)

  val set_item_string: t -> string -> t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_SetItemString} PyObject_SetItemString} *)

  val str: t -> t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_Str} PyObject_Str} *)

  val string_of_repr: t -> string
  (** [string_of_repr o] returns the string [repr o].
      We have
      [Py.Object.to_string o = Py.String.to_string (Py.Object.repr o)]. *)

  val to_string: t -> string
  (** [to_string o] returns the string [str o].
      We have
      [Py.Object.to_string o = Py.String.to_string (Py.Object.str o)]. *)

  val as_char_buffer: t -> string
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/objbuffer.html#c.PyObject_AsCharBuffer} PyObject_AsCharBuffer} *)

  val as_read_buffer: t -> string
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/objbuffer.html#c.PyObject_AsReadBuffer} PyObject_AsReadBuffer} *)

  val as_write_buffer: t -> string
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/objbuffer.html#c.PyObject_AsWriteBuffer} PyObject_AsWriteBuffer} *)

  val reference_count: t -> int
  (** [reference_count o] returns the number of references to the Python
      object [o]. *)

  val format: Format.formatter -> t -> unit
  (** [Py.Object.format fmt v] is equivalent to
      [Format.pp_print_string fmt (Py.Object.to_string v)].
      Can be used as printer for the top-level:
      [#install_printer Py.Object.format]. *)

  val format_repr: Format.formatter -> t -> unit
  (** [Py.Object.format_repr fmt v] is equivalent to
      [Format.pp_print_string fmt (Py.Object.string_of_repr v)].
      Can be used as printer for the top-level:
      [#install_printer Py.Object.format_repr]. *)

  val call_function_obj_args: t -> t array -> t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_CallFunctionObjArgs} PyObject_CallFunctionObjArgs} *)

  val call_method_obj_args: t -> t -> t array -> t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_CallMethodObjArgs} PyObject_CallMethodObjArgs} *)

  val call_method: t -> string -> t array -> t
  (** [Py.Object.call_method o m args] is equivalent to
      [Py.Object.call_method_obj_args o (Py.String.of_string m) args]. *)

  val call: t -> t -> t -> t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_Call} PyObject_Call} *)

  val size: t -> int
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_Size} PyObject_Size} *)

  val dir: t -> t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/object.html#c.PyObject_Dir} PyObject_Dir} *)
end

exception E of Object.t * Object.t
(** [E (errtype, errvalue)] is a Python error.
    [errtype] is the type of the exception.
    [errvalue] is the value. *)

val null: Object.t
(** The value [NULL] of the C Python API. [null] is useful for calling
    directly the functions of {!Pywrappers} module.
    The value should not appear when using the functions of the [Py] module.
    This value is guaranteed to be the unique value associated to [NULL]. *)

val is_null: Object.t -> bool
(** [Py.is_null v] is true if and only if [v] is [NULL].
    Since [Py.none] is guaranteed to be the unique value associated to [NULL],
    [Py.is_null v] is equivalent to [v == Py.null]. *)

val check_not_null: Object.t -> Object.t
(** [check_not_null v] checks that [v] is not [null] and returns [v].
    Raises the current Python error as exception otherwise. *)

val none: Object.t
(** The value [None] of Python.
    This value is guaranteed to be the unique value associated to [None]. *)

val is_none: Object.t -> bool
(** [Py.is_none v] is true if and only if [v] is [None].
    Since [Py.none] is guaranteed to be the unique value associated to [None],
    [Py.is_none v] is equivalent to [v == Py.none]. *)

val set_program_name: string -> unit
(** Sets the program name (by default, [Sys.argv.(0)]).
    The function can be called before [initialize ()] and the value is preserved
    from one initialization to the other. *)

val set_python_home: string -> unit
(** Sets the path of the Python home.
    The function can be called before [initialize ()] and the value is preserved
    from one initialization to the other. *)

val add_python_path: string -> unit
(** Adds a path to Python search path.
    The function can be called before [initialize ()] and the value is preserved
    from one initialization to the other. *)

val get_program_name: unit -> string
(** Gets the program name (by default, [Sys.argv.(0)]).
    The function can be called before [initialize ()]. *)

val get_python_home: unit -> string
(** Gets the path of the Python home.
    The function can be called before [initialize ()]. *)

val get_program_full_path: unit -> string
(** Wrapper for
    {{: https://docs.python.org/3/c-api/init.html#Py_GetProgramFullPath} Py_GetProgramFullPath}. *)

val get_prefix: unit -> string
(** Wrapper for
    {{: https://docs.python.org/3/c-api/init.html#Py_GetPrefix} Py_GetPrefix}. *)

val get_exec_prefix: unit -> string
(** Wrapper for
    {{: https://docs.python.org/3/c-api/init.html#Py_GetExecPrefix} Py_GetExecPrefix}. *)

val get_path: unit -> string
(** Wrapper for
    {{: https://docs.python.org/3/c-api/init.html#Py_GetPath} Py_GetPath}. *)

val get_version: unit -> string
(** Wrapper for
    {{: https://docs.python.org/3/c-api/init.html#Py_GetVersion} Py_GetVersion}. *)

val get_platform: unit -> string
(** Wrapper for
    {{: https://docs.python.org/3/c-api/init.html#Py_GetPlatform} Py_GetPlatform}. *)

val get_copyright: unit -> string
(** Wrapper for
    {{: https://docs.python.org/3/c-api/init.html#Py_GetCopyright} Py_GetCopyright}. *)

val get_compiler: unit -> string
(** Wrapper for
    {{: https://docs.python.org/3/c-api/init.html#Py_GetCompiler} Py_GetCompiler}. *)

val get_build_info: unit -> string
(** Wrapper for
    {{: https://docs.python.org/3/c-api/init.html#Py_GetBuildInfo} Py_GetBuildInfo}. *)

(** Interface for Python values of type [Bool]. *)
module Bool: sig
  val t: Object.t
  (** The Python value [True].
      This value is guaranteed to be the unique value associated to [True]. *)

  val is_true: Object.t -> bool
  (** [Py.is_true v] is true if and only if [v] is [True].
      Since [Py.Bool.t] is guaranteed to be the unique value associated to [True],
      [Py.is_true v] is equivalent to [v == Py.t]. *)

  val f: Object.t
  (** The Python value [False].
      This value is guaranteed to be the unique value associated to [False]. *)

  val is_false: Object.t -> bool
  (** [Py.is_false v] is true if and only if [v] is [False].
      Since [Py.Bool.f] is guaranteed to be the unique value associated to [False],
      [Py.is_false f] is equivalent to [v == Py.f]. *)

  val check: Object.t -> bool
  (** [check v] returns [true] if [v = t] or [v = f]. *)

  val of_bool: bool -> Object.t
  (** [of_bool b] returns [t] if [b = true], and [f] if [b = false]. *)

  val to_bool: Object.t -> bool
  (** [to_bool b] returns [true] if [b = t], and [false] if [b = f].
      [Failure] is raised if [b] is neither [t] nor [f]. *)
end

(** Interface for Python values of type [Callable]. *)
module Callable: sig
  val check: Object.t -> bool
  (** [check v] returns [true] if [v] is callable.
      Wrapper for
      {{: https://docs.python.org/3/c-api/object.html#c.PyCallable_Check} PyCallable_Check}. *)

  val handle_errors : ('a -> Object.t) -> 'a -> Object.t
  (** [handle_errors f x] calls [f x] and returns its result if the call
      succeeds. If [f x] raises a Python exception
      ([Py.E (errtype, errvalue)] or [Py.Err (errtype, msg)]),
      this exception is raised as a Python exception
      (via {!Err.set_object} or {!Err.set_error} respectively). *)

  val of_function_as_tuple: ?name:string -> ?docstring:string -> (Object.t -> Object.t) ->
    Object.t
  (** [of_function_as_tuple f] returns a Python callable object that calls the
      function [f].
      Arguments are passed as a tuple.
      If [f] raises a Python exception
      ([Py.E (errtype, errvalue)] or [Py.Err (errtype, msg)]),
      this exception is raised as a Python exception
      (via {!Err.set_object} or {!Err.set_error} respectively).
      If [f] raises any other exception, this exception bypasses the Python
      interpreter. *)

  val of_function_as_tuple_and_dict: ?name:string -> ?docstring:string ->
    (Object.t -> Object.t -> Object.t) -> Object.t
  (** [of_function_as_tuple_and_dict f] returns a Python callable object that
      calls the function [f].
      Arguments are passed as a tuple and a dictionary of keywords. *)

  val of_function: ?name:string -> ?docstring:string -> (Object.t array -> Object.t) -> Object.t
  (** Equivalent to {!of_function_as_tuple} but with an array of Python objects
      instead of a tuple for passing arguments. *)

  val of_function_with_keywords: ?name:string -> ?docstring:string ->
    (Object.t array -> Object.t -> Object.t) -> Object.t
  (** Equivalent to {!of_function_as_tuple_and_dict} but with an array of
      Python objects instead of a tuple for passing arguments.
      The dictionary of keywords is passed as such as it is more efficient
      to access arguments with ``Py.Dict.find_string``, rather than using
      ``List.assoc`` with an associative list. *)

  val to_function_as_tuple: Object.t -> Object.t -> Object.t
  (** [to_function_as_tuple c] returns a function [f] such that
      [f args] calls the Python callable [c] with the Python tuple [args]
      as arguments. *)

  val to_function_as_tuple_and_dict: Object.t -> Object.t -> Object.t ->
    Object.t
  (** [to_function_as_tuple_and_dict c] returns a function [f] such that
      [f args dict] calls the Python callable [c] with the Python tuple [args]
      and the dictionary of keywords [dict] as arguments. *)

  val to_function: Object.t -> Object.t array -> Object.t
  (** Equivalent to {!to_function_as_tuple} but with an array of
      Python objects instead of a tuple for passing arguments. *)

  val to_function_with_keywords: Object.t -> Object.t array ->
    (string * Object.t) list -> Object.t
  (** Equivalent to {!to_function_as_tuple_and_dict} but with an array of
      Python objects instead of a tuple and an associative list instead of a
      dictionary for passing arguments. *)
end

(** Embedding of OCaml values in Python. *)
module Capsule: sig
  type 'a t = {
    wrap : 'a -> Object.t;
    unwrap : Object.t -> 'a;
  }

  val check: Object.t -> bool
  (** [check v] returns [true] if [v] contains an OCaml value. *)

  val create: string -> 'a t
  (** For a given type ['a], [create s] returns a pair [{ wrap; unwrap }].
      [wrap v] transforms the value [v] of type 'a to an opaque Python object.
      [unwrap w] transforms the opaque Python object [w] previously obtained
      with [wrap v] into the original OCaml value [v],
      such that [unwrap (wrap v) = v].
      [Failure _] is raised if a wrapper has already been generated for a type
      of the same name. *)

  val make: string -> ('a -> Object.t) * (Object.t -> 'a)
  (** Same as {!val:create}, but returns a plain pair instead of a record. *)

  val type_of: Object.t -> string
  (** [type_of w] returns the type string associated to the opaque Python
      object [w]. *)

  val is_valid: Object.t -> string -> bool
  (** Wrapper for
      {{: https://docs.python.org/3/c-api/capsule.html#c.PyCapsule_IsValid} PyCapsule_IsValid}.
      OCaml capsules have the name ["ocaml-capsule"].
      We have [check v = is_valid v "ocaml-capsule"]. *)

  val unsafe_wrap_value: 'a -> Object.t
  (** [unsafe_wrap_value v] transforms the value [v] to an opaque Python
      object. *)

  val unsafe_unwrap_value: Object.t -> 'a
  (** [unsafe_unwrap_value v] transforms the opaque Python object [w]
      previously obtained with [unsafe_wrap_value v] into the original OCaml
      value [v]. *)
end

(** Defining a new class type *)
module Class: sig
  val init: ?parents:(Object.t list) -> ?fields:((string * Object.t) list) ->
      ?methods:((string * Object.t) list) ->
        string -> Object.t
  (** [init ~parents ~fields ~methods classname] Returns a new class type.
      @param parents list of base classes (default: [[]]).
      @param fields associative list for field values (default : [[]]).
      @param methods associative list for method closures
      (default : [[]]). *)
end

(** Interface for Python values of type [Long]. *)
module Long: sig
  val check: Object.t -> bool
  (** [check o] returns [true] if [o] is a Python long. *)

  val of_int64: int64 -> Object.t
  (** [of_int i] returns the Python long with the value [i].
      Wrapper for
      {{: https://docs.python.org/3/c-api/long.html#c.PyLong_FromLong} PyLong_FromLong}. *)

  val to_int64: Object.t -> int64
  (** [to_int o] takes a Python long [o] as arguments
      and returns the corresponding 64-bit integer value.
      A Python exception ([Py.E _]) is raised if [o] is not a long.
      Wrapper for
      {{: https://docs.python.org/3/c-api/long.html#c.PyLong_AsLong} PyLong_AsLong}. *)

  val of_int: int -> Object.t
  (** [of_int i] returns the Python long with the value [i].
      We have [of_int i = of_int64 (Int64.of_int i)]. *)

  val to_int: Object.t -> int
  (** [to_int o] takes a Python long [o] as arguments
      and returns the corresponding integer value.
      A Python exception ([Py.E _]) is raised if [o] is not a long.
      We have [to_int o = Int64.to_int (to_int 64 o)]. *)

  val from_string: string -> int -> Object.t * int
  (** [from_string s base] parses [s] as a number written in [base] and
      returns [(o, l)] where [o] is the Python long which has been read,
      and [l] is the number of characters that has been parsed.
      Wrapper for
      {{: https://docs.python.org/3/c-api/long.html#c.PyLong_FromString} PyLong_FromString}. *)

  val of_string: ?base:int -> string -> Object.t
  (** [of_string ?base s] parses [s] and returns the Python long that has
      been read. By default, [base] is [0]: the radix is determined based
      on the leading characters of [s]. *)

  val to_string: Object.t -> string
  (** Synonym for [Py.Object.to_string]. *)
end

(** Interface for Python values of type [Int] if Python 2, [Long] if Python 3. *)
module Int: sig
  val check: Object.t -> bool
  (** [check o] returns [true] if [o] is a Python int. *)

  val of_int64: int64 -> Object.t
  (** [of_int i] returns the Python int with the value [i].
      Wrapper for
      {{: https://docs.python.org/2/c-api/int.html#c.PyInt_FromLong} PyInt_FromLong}. *)

  val to_int64: Object.t -> int64
  (** [to_int o] takes a Python int [o] as arguments
      and returns the corresponding 64-bit integer value.
      A Python exception ([Py.E _]) is raised if [o] is not a long.
      Wrapper for
      {{: https://docs.python.org/2/c-api/int.html#c.PyInt_AsLong} PyInt_AsLong}. *)

  val of_int: int -> Object.t
  (** [of_int i] returns the Python int with the value [i].
      We have [of_int i = of_int64 (Int64.of_int i)]. *)

  val to_int: Object.t -> int
  (** [to_int o] takes a Python int [o] as arguments
      and returns the corresponding integer value.
      A Python exception ([Py.E _]) is raised if [o] is not a long.
      We have [to_int o = Int64.to_int (to_int 64 o)]. *)

  val of_string: ?base:int -> string -> Object.t
  (** Synonym for [Py.Long.of_string]. *)

  val to_string: Object.t -> string
  (** Synonym for [Py.Long.to_string]. *)
end

(** Interface for Python values of type [Dict]. *)
module Dict: sig
  val check: Object.t -> bool
  (** [check o] returns [true] if [o] is a Python dictionary. *)

  val clear: Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_Clear} PyDict_Clear} *)

  val copy: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_Copy} PyDict_Copy} *)

  val create: unit -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_New} PyDict_New} *)

  val del_item: Object.t -> Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_DelItem} PyDict_DelItem} *)

  val del_item_string: Object.t -> string -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_DelItemString} PyDict_DelItemString} *)

  val get_item: Object.t -> Object.t -> Object.t option
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_GetItem} PyDict_GetItem} *)

  val find: Object.t -> Object.t -> Object.t
  (** [find p key] returns the object from Python dictionary [p] which has a key
      [key]. Equivalent to {!get_item} but [find] raises [Not_found] if the
      key [key] is not present. *)

  val find_opt: Object.t -> Object.t -> Object.t option
  (** Alias for {!get_item}. *)

  val get_item_string: Object.t -> string -> Object.t option
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_GetItemString} PyDict_GetItemString} *)

  val find_string: Object.t -> string -> Object.t
  (** [find_string p key] returns the object from Python dictionary [p]
      which has a key [key]. Equivalent to {!get_item_string} but [find_string]
      raises [Not_found] if the key [key] is not present. *)

  val find_string_opt: Object.t -> string -> Object.t option
  (** Alias for {!get_item_string}. *)

  val keys: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_Keys} PyDict_Keys} *)

  val items: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_Items} PyDict_Items} *)

  val set_item: Object.t -> Object.t -> Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_SetItem} PyDict_SetItem} *)

  val set_item_string: Object.t -> string -> Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_SetItemString} PyDict_SetItemString} *)

  val size: Object.t -> int
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_Size} PyDict_Size} *)

  val values: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/dict.html#c.PyDict_Clear} PyDict_Str} *)

  val iter: (Object.t -> Object.t -> unit) -> Object.t -> unit
  (** [iter f dict] applies [f key value] for each pair [(key, value)]
      in the Python dictionary [dict]. *)

  val fold: (Object.t -> Object.t -> 'a -> 'a) -> Object.t -> 'a -> 'a
  (** [fold f dict v] returns [f key1 value1 (... (f keyn valuen dict))]
      where [(key1, value1)], ..., [(keyn, valuen)] are the bindings of
      the Python dictionary [dict]. *)

  val for_all: (Object.t -> Object.t -> bool) -> Object.t -> bool
  (** [for_all p dict] checks whether all the bindings [(key, value)] of the
      Python dictionary [dict] satisfy the predicate [p key value]. *)

  val exists: (Object.t -> Object.t -> bool) -> Object.t -> bool
  (** [for_all p dict] checks that there is at least one binding [(key, value)]
      among those of the Python dictionary [dict] that satisfies the predicate
      [p key value]. *)

  val to_bindings: Object.t -> (Object.t * Object.t) list
  (** [to_bindings o] returns all the pairs [(key, value)] in the Python
      dictionary [o]. *)

  val to_bindings_map: (Object.t -> 'a) -> (Object.t -> 'b) -> Object.t ->
    ('a * 'b) list
  (** [to_bindings_map fkey fvalue o] returns all the pairs
      [(fkey key, fvalue value)] in the Python dictionary [o]. *)

  val to_bindings_string: Object.t -> (string * Object.t) list
  (** [to_bindings_string o] returns all the pairs [(key, value)] in the Python
      dictionary [o]. *)

  val to_bindings_seq: Object.t -> (Object.t * Object.t) Seq.t
  (** [to_bindings_seq o] returns the ephemeral sequence of all the pairs
      (key, value) in the Python dictionary [o]. *)

  val to_bindings_seq_map: (Object.t -> 'a) -> (Object.t -> 'b) -> Object.t ->
    ('a * 'b) Seq.t
  (** [to_bindings_seq_map fkey fvalue o] returns the ephemeral sequence of all
      the pairs (fkey key, fvalue value) in the Python dictionary [o]. *)

  val to_bindings_string_seq: Object.t -> (string * Object.t) Seq.t
  (** [to_bindings_string_seq o] returns the ephemeral sequence of all the pairs
      (key, value) in the Python dictionary [o]. *)

  val of_bindings: (Object.t * Object.t) list -> Object.t
  (** [of_bindings b] returns then Python dictionary mapping all the pairs
      [(key, value)] in [b]. *)

  val of_bindings_map: ('a -> Object.t) -> ('b -> Object.t) -> ('a * 'b) list
    -> Object.t
  (** [of_bindings_map fkey fvalue b] returns then Python dictionary mapping
      all the pairs [(fkey key, fvalue value)] in [b]. *)

  val of_bindings_string: (string * Object.t) list -> Object.t
  (** [of_bindings_string b] returns then Python dictionary mapping all the
      pairs [(key, value)] in [b]. *)

  val singleton: Object.t -> Object.t -> Object.t
  (** [singleton key value] returns the one-element Python dictionary that maps
      [key] to [value] *)

  val singleton_string: string -> Object.t -> Object.t
  (** [singleton key value] returns the one-element Python dictionary that maps
      [key] to [value] *)
end

(** Interface for Python values of type [Set]. *)
module Set: sig
  val check: Object.t -> bool
  (** [check o] returns [true] if [o] is a Python set. *)

  val add: Object.t -> Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/set.html#c.PySet_Add} PySet_Add} *)

  val clear: Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/set.html#c.PySet_Clear} PySet_Clear} *)

  val contains: Object.t -> Object.t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/set.html#c.PySet_Contains} PySet_Contains} *)

  val copy: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/set.html#c.PySet_New} PySet_New} *)

  val create: unit -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/set.html#c.PySet_New} PySet_New} *)

  val discard: Object.t -> Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/set.html#c.PySet_Discard} PySet_Discard} *)

  val size: Object.t -> int
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/set.html#c.PySet_Size} PySet_Size} *)

  val to_list: Object.t -> Object.t list
  (** [to_list o] returns the list of all elements in Python set [o]. *)

  val to_list_map: (Object.t -> 'a) -> Object.t -> 'a list
  (** [to_list_map f o] returns the list of [f v] for all elements v in
      Python set [o]. *)

  val of_list: Object.t list -> Object.t
  (** [of_list l] returns then Python set containing all elements from [l]. *)

  val of_list_map: ('a -> Object.t) -> 'a list -> Object.t
  (** [of_list_map f l] returns then Python set containing [f e] for any
      [e] from [l]. *)
end

module Err: sig
  type t =
      Exception
    | StandardError
    | ArithmeticError
    | LookupError
    | AssertionError
    | AttributeError
    | EOFError
    | EnvironmentError
    | FloatingPointError
    | IOError
    | ImportError
    | IndexError
    | KeyError
    | KeyboardInterrupt
    | MemoryError
    | NameError
    | NotImplementedError
    | OSError
    | OverflowError
    | ReferenceError
    | RuntimeError
    | SyntaxError
    | SystemExit
    | TypeError
    | ValueError
    | ZeroDivisionError
    | StopIteration

  val clear: unit -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_Clear} PyErr_Clear} *)

  val exception_matches: Object.t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_ExceptionMatches} PyErr_ExceptionMatches} *)

  val fetch: unit -> (Object.t * Object.t * Object.t) option
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_Fetch} PyErr_Fetch}.
   *)

  val fetched: unit -> (Object.t * Object.t * Object.t) option
  (** Exception fetched when {!Py.E} has been raised. *)

  val given_exception_matches: Object.t -> Object.t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_GivenExceptionMatches} PyErr_GivenExceptionMatches} *)

  val occurred: unit -> Object.t option
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_Occurred} PyErr_Occurred} *)

  val print: unit -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_Print} PyErr_Print} *)

  val print_ex: int -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_PrintEx} PyErr_PrintEx} *)

  val restore: Object.t -> Object.t -> Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_Restore} PyErr_Restore} *)

  val restore_tuple: Object.t * Object.t * Object.t -> unit
  (** [restore_tuple (ptype, pvalue, ptraceback)] is equivalent to
      [Py.Err.restore ptype pvalue ptraceback]. *)

  val restore_fetch: unit -> unit
  (** Restore the exception returned by [Py.Err.fetch ()] and raise
      [Failure] if [None]. *)

  val restore_fetched: unit -> unit
  (** Restore the exception returned by [Py.Err.fetched ()] and raise
      [Failure] if [None]. *)

  val set_error: t -> string -> unit
  (** [set_error e msg] calls [Py.Err.set_string e msg] with a predefined error type.
      In a closure/method/callback, it is recommended to raise a [Py.Err _] exception
      instead. *)

  val set_none: Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_SetNone} PyErr_SetNone} *)

  val set_string: Object.t -> string -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_SetString} PyErr_SetString} *)

  val set_object: Object.t -> Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_SetObject} PyErr_SetObject}.
      In a closure/method/callback, it is recommended to raise a [Py.E _] exception
      instead. *)

  val set_interrupt: unit -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_SetInterrupt} PyErr_SetInterrupt} *)

  val set_interrupt_ex: int -> unit
  (** Since Python 3.10. Wrapper for
      {{:https://docs.python.org/3/c-api/exceptions.html#c.PyErr_SetInterruptEx} PyErr_SetInterruptEx} *)
end

module Traceback : sig
  type frame =
    { filename : string
    ; function_name : string
    ; line_number : int
    }

  val create_frame : frame -> Object.t

  type t = frame list
end

exception Err of Err.t * string
(** Represents an exception to be set with {!Err.set_error} in a callback. *)

exception Err_with_traceback of Err.t * string * Traceback.t
(** Represents an exception with traceback information to be set with {!Err.restore}. *)

module Eval: sig
  val call_object: Object.t -> Object.t -> Object.t
 (** See {{:https://docs.python.org/3.0/extending/extending.html} Extending Python with C or C++} *)

  val call_object_with_keywords: Object.t -> Object.t -> Object.t -> Object.t
 (** See {{:https://docs.python.org/3.0/extending/extending.html} Extending Python with C or C++} *)

  val get_builtins: unit -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/reflection.html#c.PyEval_GetBuiltins} PyEval_GetBuiltins} *)

  val get_globals: unit -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/reflection.html#c.PyEval_GetGlobals} PyEval_GetGlobals} *)

  val get_locals: unit -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/reflection.html#c.PyEval_GetLocals} PyEval_GetLocals} *)
end

(** Interface for Python values of type [Float]. *)
module Float: sig
  val check: Object.t -> bool
  (** [check o] returns [true] if [o] is a Python float. *)

  val of_float: float -> Object.t
  (** [of_float f] returns the Python long with the value [f].
      Wrapper for
      {{:https://docs.python.org/3/c-api/float.html#c.PyFloat_AsDouble} PyFloat_AsDouble}. *)

  val to_float: Object.t -> float
  (** [to_float o] returns the floating-point vale stored in [o].
      A Python exception ([Py.E _]) is raised if [o] is not a float.
      Wrapper for
      {{:https://docs.python.org/3/c-api/float.html#c.PyFloat_FromDouble} PyFloat_FromDouble}. *)
end

type optimize = Default | Debug | Normal | RemoveDocstrings

val int_of_optimize : optimize -> int

(** Importing Modules *)
module Import: sig
  (* This function has been removed from Python 3.9, and was marked
  "for internal use only" before.
  val cleanup: unit -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/import.html#c.PyImport_Cleanup} PyImport_Cleanup} *)
   *)

  val add_module: string -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/import.html#c.PyImport_AddModule} PyImport_AddModule} *)

  val exec_code_module: string -> Object.t -> Object.t
  (** [exec_code_module name bytecode] imports the module [name] compiled in
      [bytecode]. [bytecode] can be obtained with {!val:Py.Module.compile}
      (you may also consider {!val:Py.Import.exec_code_module_from_string}.
      Wrapper for
      {{:https://docs.python.org/3/c-api/import.html#c.PyImport_ExecCodeModule} PyImport_ExecCodeModule} *)

  val exec_code_module_ex: string -> Object.t -> string -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/import.html#c.PyImport_ExecCodeModuleEx} PyImport_ExecCodeModuleEx} *)

  val exec_code_module_from_string : name:string -> ?filename:string ->
      ?dont_inherit:bool -> ?optimize:optimize -> string -> Object.t
  (** [exec_code_module ~name ?filename ?dont_inherit ?optimize source_code]
      compiles [source_code] and imports the resulting bytecode as
      module [name]. [filename] is equal to [name] by default and is used
      in error messages. [dont_inherit] and [optimize] are passed to
      {!val:Py.Module.compile} for compiling [source_code]. *)

  val get_magic_number: unit -> int64
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/import.html#c.PyImport_GetMagicNumber} PyImport_GetMagicNumber} *)

  val get_module_dict: unit -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/import.html#c.PyImport_GetModuleDict} PyImport_GetModuleDict} *)

  val import_frozen_module: string -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/import.html#c.PyImport_ImportFrozenModule} PyImport_ImportFrozenModule} *)

  val import_module: string -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/import.html#c.PyImport_ImportModule} PyImport_ImportModule}
      Note that Python memoizes imported module, so that you will get the same
      object if you import the same module twice.
      ({{:https://github.com/thierry-martinez/pyml/issues/16}GitHub issue #16})

{[let m = Py.Import.import_module "json"
and m' = Py.Import.import_module "json" in
assert (m = m')]} *)

  val import_module_opt: string -> Object.t option
  (** [import_module_opt m] imports the module [m] and
      returns the module object if the import succeeds:.
      in this case, it is equivalent to [Some (import_module m)].
      If the module is not found,
      i.e. if [import_module] raises a Python exception of class
      [ModuleNotFoundError], then [try_import_module] returns [None]. *)

  val try_import_module: string -> Object.t option
  (** Alias for {!import_module_opt}. *)

  val import_module_ex:
      string -> Object.t -> Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/import.html#c.PyImport_ImportModuleEx} PyImport_ImportModuleEx} *)

  val import_module_level:
      string -> Object.t -> Object.t -> Object.t -> int -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/import.html#c.PyImport_ImportModuleLevel} PyImport_ImportModuleLevel} *)

  val reload_module: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/import.html#c.PyImport_ReloadModule} PyImport_ReloadModule} *)
end

val import: string -> Object.t
(** Equivalent to {!Import.import_module}. *)

val import_opt: string -> Object.t option
(** Equivalent to {!Import.import_module_opt}. *)

(** Interface for Python values of type [Iter]. *)
module Iter: sig
  val check: Object.t -> bool
  (** [check o] returns [true] if [o] is an iterator. *)

  val next: Object.t -> Object.t option
  (** [next i] returns the next value from the iteration [i].
      If there are no remaining values, returns [None].
      Wrapper for
      {{:https://docs.python.org/3/c-api/iter.html#c.PyIter_Next} PyIter_Next}. *)

  val iter: (Object.t -> unit) -> Object.t -> unit
  (** [iter f i] iteratively calls [f v] with all the remaining values of the
      iteration [i]. *)

  val to_list: Object.t -> Object.t list
  (** [to_list i] returns the list of all the remaining values from the
      iteration [i]. *)

  val to_list_map: (Object.t -> 'a) -> Object.t -> 'a list
  (** [to_list_map f i] returns the list of the results of [f] applied to all
      the remaining values from the iteration [i].
      [to_list_map f s] is equivalent to [List.map f (to_list s)] but is
      tail-recursive and [f] is applied to the elements of [s] in the reverse
      order. *)

  val of_seq: Object.t Seq.t -> Object.t
  (** [of_seq s] returns an interator that iterates over the values of the
      sequence [s]. *)

  val of_seq_map: ('a -> Object.t) -> 'a Seq.t -> Object.t
  (** [of_seq_map f s] returns an interator that iterates over the results of
      [f] applied to the values of the sequence [s].
      [Py.Iter.of_seq_map f s] is equivalent to
      [Py.Iter.of_seq (Seq.map f s)]. *)

  val to_seq: Object.t -> Object.t Seq.t
  (** [to_seq i] returns the sequence of the values from the iteration [i].
      The Python iteration is consumed while the sequence is browsed.
      Values are memoized, so that the sequence can be browsed many times. *)

  val to_seq_map: (Object.t -> 'a) -> Object.t -> 'a Seq.t
  (** [to_seq_map f i] returns the sequence of the results of [f] applied to the
      values from the iteration [i].
      The Python iteration is consumed while the sequence is browsed.
      Values are memoized, so that the sequence can be browsed many times. *)

  val unsafe_to_seq: Object.t -> Object.t Seq.t
  (** [unsafe_to_seq i] returns the sequence of the values from the iteration
      [i].
      The Python iteration is consumed while the sequence is browsed.
      Warning: values are not memoized, so that the sequence can be browsed
      only once. *)

  val unsafe_to_seq_map: (Object.t -> 'a) -> Object.t -> 'a Seq.t
  (** [unsafe_to_seq_map f i] returns the sequence of the results of [f] applied
      to the values from the iteration [i].
      The Python iteration is consumed while the sequence is browsed.
      Warning: values are not memoized, so that the sequence can be browsed
      only once. *)

  val of_list: Object.t list -> Object.t
  (** [of_list l] returns an interator that iterates over the values of the
      list [l]. *)

  val of_list_map: ('a -> Object.t) -> 'a list -> Object.t
  (** [of_list_map f l] returns an interator that iterates over the results of
      [f] applied to the values of the list [l].
      [Py.Iter.of_list_map f s] is equivalent to
      [Py.Iter.of_list (List.map f s)] but is tail-recursive. *)

  val fold_left: ('a -> Object.t -> 'a) -> 'a -> Object.t -> 'a
  (** [fold_left f v i] returns [(f (...(f v i1)...) in)] where [i1], ..., [in]
      are the remaining values from the iteration [i]. *)

  val fold_right: (Object.t -> 'a -> 'a) -> Object.t -> 'a -> 'a
  (** [fold_right f i v] returns [(f i1 (...(f v in)...)] where [i1], ..., [in]
      are the remaining values from the iteration [i].
      This function is not tail-recursive. *)

  val for_all: (Object.t -> bool) -> Object.t -> bool
  (** [for_all p i] checks if [p] holds for all the remaining values from the
      iteration [i]. *)

  val exists: (Object.t -> bool) -> Object.t -> bool
  (** [exists p i] checks if [p] holds for at least one of the remaining values
      from the iteration [i]. *)

  val create: (unit -> Object.t option) -> Object.t
  (** [create next] returns an iterator that calls [next]. *)

  val seq_iter: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/iterator.html#c.PySeqIter_New} PySeqIter_New} *)

  val call_iter: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/iterator.html#c.PyCallIter_New} PyCallIter_New} *)

  val create_call: (unit -> Object.t option) -> Object.t
  (** [create_call next] returns an iterator that calls [next]. The difference
      with [create] is that this uses [PyCallIter_New] rather than creating
      an object and use the __next__ method. *)
end

(** Interface for Python values of type [List]. *)
module List: sig
  val check: Object.t -> bool
  (** [check v] returns [true] if [v] is a Python list. *)

  val create: int -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/list.html#c.PyList_New} PyList_New} *)

  val get_item: Object.t -> int -> Object.t
  (** Equivalent to {!Sequence.get_item}. *)

  val get: Object.t -> int -> Object.t
  (** Equivalent to {!get_item}. *)

  val set_item: Object.t -> int -> Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/list.html#c.PyList_SetItem} PyList_SetItem} *)

  val set: Object.t -> int -> Object.t -> unit
  (** Equivalent to {!set_item}. *)

  val size: Object.t -> int
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/list.html#c.PyList_Size} PyList_Size} *)

  val length: Object.t -> int
  (** Equivalent to {!size}. *)

  val init: int -> (int -> Object.t) -> Object.t
  (** [init n f] returns the Python list [[f 0, f 1, ..., f (n - 1)]]. *)

  val of_array: Object.t array -> Object.t
  (** [of_array a] returns the Python list with the same elements as [a]. *)

  val of_array_map: ('a -> Object.t) -> 'a array -> Object.t
  (** [of_array_map f a] returns the Python list [(f a0, ..., f ak)] where
      [a0], ..., [ak] are the elements of [a]. *)

  val to_array: Object.t -> Object.t array
  (** Equivalent to {!Sequence.to_array}. *)

  val to_array_map: (Object.t -> 'a) -> Object.t -> 'a array
  (** Equivalent to {!Sequence.to_array_map}. *)

  val of_list: Object.t list -> Object.t
  (** [of_list l] returns the Python list with the same elements as [l]. *)

  val of_list_map: ('a -> Object.t) -> 'a list -> Object.t
  (** [of_list f l] returns the Python list [(f l1, ..., f ln)] where
      [l1], ..., [ln] are the elements of [l].
      [of_list_map f l] is equivalent to [of_list (List.map f l)] but is
      tail-recursive and [f] is applied to the elements of [l] in the reverse
      order. *)

  val to_list: Object.t -> Object.t list
  (** Equivalent to {!Sequence.to_list}. *)

  val to_list_map: (Object.t -> 'a) -> Object.t -> 'a list
  (** Equivalent to {!Sequence.to_list_map}. *)

  val fold_left: ('a -> Object.t -> 'a) -> 'a -> Object.t -> 'a
  (** Equivalent to {!Sequence.fold_left}. *)

  val fold_right: (Object.t -> 'a -> 'a) -> Object.t -> 'a -> 'a
  (** Equivalent to {!Sequence.fold_right}. *)

  val for_all: (Object.t -> bool) -> Object.t -> bool
  (** Equivalent to {!Sequence.for_all}. *)

  val exists: (Object.t -> bool) -> Object.t -> bool
  (** Equivalent to {!Sequence.exists}. *)

  val of_sequence: Object.t -> Object.t
  (** Equivalent to {!Sequence.list}. *)

  val of_seq: Object.t Seq.t -> Object.t
  (** [of_seq s] returns the Python list with the same elements as [s]. *)

  val to_seq: Object.t -> Object.t Seq.t
  (** Equivalent to {!Sequence.to_seq}. *)

  val to_seqi: Object.t -> (int * Object.t) Seq.t
  (** Equivalent to {!Sequence.to_seqi}. *)

  val singleton: Object.t -> Object.t
  (** [singleton o] returns the Python list [[o]]. *)
end

(** Interface for Python values with a [Mapping] interface. *)
module Mapping: sig
  val check: Object.t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/mapping.html#c.PyMapping_Check} PyMapping_Check} *)

  val get_item_string: Object.t -> string -> Object.t option
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/mapping.html#c.PyMapping_GetItemString} PyMapping_GetItemString} *)

  val find_string: Object.t -> string -> Object.t
  (** Equivalent to {!get_item_string} but raises a [Not_found] exception in
      case of failure. *)

  val find_string_opt: Object.t -> string -> Object.t option
  (** Alias for {!get_item_string}. *)

  val has_key: Object.t -> Object.t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/mapping.html#c.PyMapping_HasKey} PyMapping_HasKey} *)

  val has_key_string: Object.t -> string -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/mapping.html#c.PyMapping_HasKeyString} PyMapping_HasKeyString} *)

  val length: Object.t -> int
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/mapping.html#c.PyMapping_Length} PyMapping_Length} *)

  val set_item_string: Object.t -> string -> Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/mapping.html#c.PyMapping_SetItemString} PyMapping_SetItemString} *)

  val size: Object.t -> int
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/mapping.html#c.PyMapping_Size} PyMapping_Size} *)
end

(** Interface for Python values of type [Method]. *)
module Method: sig
  val create: Object.t -> Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/method.html#c.PyMethod_New} PyMethod_New} *)

  val get_function: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/method.html#c.PyMethod_GetFunction} PyMethod_GetFunction} *)

  val self: Object.t -> Object.t option
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/method.html#c.PyMethod_Self} PyMethod_Self} *)
end

type input = Pytypes.input = Single | File | Eval

val string_of_input : input -> string

(** Interface for Python values of type [Module]. *)
module Module: sig
  val check: Object.t -> bool
  (** [check o] returns [true] if [o] is a Python module. *)

  val create: string -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/module.html#c.PyModule_New} PyModule_New} *)

  val get_dict: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/module.html#c.PyModule_GetDict} PyModule_GetDict} *)

  val get_filename: Object.t -> string
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/module.html#c.PyModule_GetFilename} PyModule_GetFilename} *)

  val get_name: Object.t -> string
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/module.html#c.PyModule_GetName} PyModule_GetName} *)

  val get: Object.t -> string -> Object.t
  (** Equivalent to {!Object.find_attr_string}. *)

  val get_opt: Object.t -> string -> Object.t option
  (** Equivalent to {!Object.find_attr_string_opt}. *)

  val get_function: Object.t -> string -> Object.t array -> Object.t
  (** [Py.Module.get_function m name] is equivalent to
      [Py.Callable.to_function (Py.Module.get m name)]. *)

  val get_function_opt: Object.t -> string ->
    (Object.t array -> Object.t) option
  (** [Py.Module.get_function_opt] is equivalent to
      [Py.Module.get_function] but returns [None] in case of failure. *)

  val get_function_with_keywords: Object.t -> string -> Object.t array ->
    (string * Object.t) list -> Object.t
  (** [Py.Module.get_function_with_keywords m name] is equivalent to
      [Py.Callable.to_function_with_keywords (Py.Module.get m name)]. *)

  val get_function_with_keywords_opt: Object.t -> string ->
    (Object.t array -> (string * Object.t) list -> Object.t) option
  (** [Py.Module.get_function_with_keywords_opt] is equivalent to
      [Py.Module.get_function_with_keywords]
      but returns [None] in case of failure. *)

  val set: Object.t -> string -> Object.t -> unit
  (** Equivalent to {!Object.set_attr_string}. *)

  val set_function: Object.t -> string -> (Object.t array -> Object.t) -> unit
  (** [Py.Module.set_function m name f] is equivalent to
      [Py.Module.set m name (Py.Callable.of_function f)]. *)

  val set_function_with_keywords: Object.t -> string ->
    (Object.t array -> Object.t -> Object.t) -> unit
  (** [Py.Module.set_function_with_keywords m name f] is equivalent to
      [Py.Module.set m name (Py.Callable.of_function_with_keywords f)]. *)

  val remove: Object.t -> string -> unit
  (** Equivalent to {!Object.del_attr_string}. *)

  val main: unit -> Object.t
  (** Returns the [__main__] module.
      We have [Py.Module.main () = Py.Module.add_module "__main__"]. *)

  val sys: unit -> Object.t
  (** Returns the [sys] module.
      We have [Py.Module.sys () = Py.Module.import_module "sys"]. *)

  val builtins: unit -> Object.t
  (** Returns the [__builtins__] module.
      We have
[Py.Module.builtins () = Py.Module.find (Py.Module.main ()) "__builtins__"]. *)

  val set_docstring: Object.t -> string -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/module.html#c.PyModule_SetDocString} PyModule_SetDocString} *)

  val compile : source:string -> filename:string -> ?dont_inherit:bool ->
      ?optimize:optimize -> input -> Object.t
  (** [compile ~source ~filename ?dont_inherit ?optimize mode] returns
    the bytecode obtained by compiling ~source. It is a wrapper for
    the built-in function
    {{:https://docs.python.org/3/library/functions.html#compile} compile()}.
 {{:https://github.com/thierry-martinez/pyml/issues/25} GitHub issue #25}*)
end

(** Interface for Python values of type [Number]. *)
module Number: sig
  val absolute: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Absolute} PyNumber_Absolute} *)

  val add: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Add} PyNumber_Add} *)

  val number_and: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_And} PyNumber_And} *)

  val divmod: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Divmod} PyNumber_Divmod} *)

  val float: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Float} PyNumber_Float} *)

  val floor_divide: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_FloorDivide} PyNumber_FloorDivide} *)

  val in_place_add: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlaceAdd} PyNumber_InPlaceAdd} *)

  val in_place_and: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlaceAnd} PyNumber_InPlaceAnd} *)

  val in_place_floor_divide: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlaceFloorDivide} PyNumber_InPlaceFloorDivide} *)

  val in_place_lshift: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlaceLshift} PyNumber_InPlaceLshift} *)

  val in_place_multiply: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlaceMultiply} PyNumber_InPlaceMultiply} *)

  val in_place_or: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlaceOr} PyNumber_InPlaceOr} *)

  val in_place_power: ?modulo:Object.t -> Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlacePower} PyNumber_InPlacePower} *)

  val in_place_remainder: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlaceRemainder} PyNumber_InPlaceRemainder} *)

  val in_place_rshift: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlaceRshift} PyNumber_InPlaceRshift} *)

  val in_place_subtract: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlaceSubtract} PyNumber_InPlaceSubtract} *)

  val in_place_true_divide: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlaceTrueDivide} PyNumber_InPlaceTrueDivide} *)

  val in_place_xor: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_InPlaceXor} PyNumber_InPlaceXor} *)

  val invert: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Invert} PyNumber_Invert} *)

  val lshift: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Lshift} PyNumber_Lshift} *)

  val multiply: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Multiply} PyNumber_Multiply} *)

  val negative: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Negative} PyNumber_Negative} *)

  val number_or: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Or} PyNumber_Or} *)

  val positive: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Positive} PyNumber_Positive} *)

  val power: ?modulo:Object.t -> Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Power} PyNumber_Power} *)

  val remainder: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Remainder} PyNumber_Remainder} *)

  val rshift: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Rshift} PyNumber_Rshift} *)

  val subtract: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Subtract} PyNumber_Subtract} *)

  val true_divide: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_TrueDivide} PyNumber_TrueDivide} *)

  val number_xor: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/number.html#c.PyNumber_Xor} PyNumber_Xor} *)

  val check: Object.t -> bool
  (** [check v] returns [true] if [v] is a Python float or a Python
      integer/long. *)

  val to_float: Object.t -> float
  (** [to_float v] returns the floating-point value equal to the Python integer
      or Python float [v]. Raises a failure ([Failure _]) if [v] is neither a
      float nor an integer. *)

  val of_int: int -> Object.t
  (** Synonym of {!Py.Int.of_int} *)

  val of_int64: int64 -> Object.t
  (** Synonym of {!Py.Int.of_int64} *)

  val of_float: float -> Object.t
  (** Synonym of {!Py.Float.of_float} *)

  val ( + ): Object.t -> Object.t -> Object.t
  (** Synomym of {!add} *)

  val ( - ): Object.t -> Object.t -> Object.t
  (** Synomym of {!subtract} *)

  val ( * ): Object.t -> Object.t -> Object.t
  (** Synomym of {!multiply} *)

  val ( / ): Object.t -> Object.t -> Object.t
  (** Synomym of {!true_divide} *)

  val ( ** ): Object.t -> Object.t -> Object.t
  (** Synomym of {!power} *)

  val ( ~- ): Object.t -> Object.t
  (** Synomym of {!negative} *)

  val ( land ): Object.t -> Object.t -> Object.t
  (** Synomym of {!number_and} *)

  val ( lor ): Object.t -> Object.t -> Object.t
  (** Synomym of {!number_or} *)

  val ( lxor ): Object.t -> Object.t -> Object.t
  (** Synomym of {!number_xor} *)

  val ( lsl ): Object.t -> Object.t -> Object.t
  (** Synomym of {!lshift} *)

  val ( lsr ): Object.t -> Object.t -> Object.t
  (** Synomym of {!rshift} *)
end

(** Interface for Python values of type [Run]. *)
module Run: sig
  val eval: ?start:input -> ?globals:Object.t -> ?locals:Object.t -> string
    -> Object.t
  (** [eval ~start ~globals ~locals e]
      evaluates the Python expression [e] and returns the computed value.
      We have
[Py.Run.eval ~start ~globals ~locals e = Py.Run.string e start globals locals].
      @param start is the initial input mode (default: [Eval]).
      @param globals is the global symbol directory
      (default: [Py.Module.get_dict (Py.Module.main ())]).
      @param locals is the local symbol directory
      (default: [globals]).
   *)

  val load: ?start:input -> ?globals:Object.t -> ?locals:Object.t ->
    in_channel file -> string -> Object.t
  (** [load ~start ~globals ~locals chan filename] loads the contents of the file
      opened in [chan].
      We have
[Py.Run.load ~start ~globals ~locals chan filename = Py.Run.file chan filename start globals locals].
      @param start is the initial input mode (default: [File]).
      @param globals is the global symbol directory
      (default: Module.get_dict (Module.main ())).
      @param locals is the local symbol directory (default: [Dict.create ()]). *)

  val interactive: unit -> unit
  (** Runs the interactive loop.
      We have [Py.Run.interactive () = Py.Run.interactive_loop stdin "<stdin>"].
   *)

  val ipython: ?frame:bool -> unit -> unit
  (** Runs the IPython interactive loop. *)

  val any_file: in_channel file -> string -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/veryhigh.html#c.PyRun_AnyFile} PyRun_AnyFile} *)

  val file: in_channel file -> string -> input -> Object.t -> Object.t
    -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/veryhigh.html#c.PyRun_File} PyRun_File} *)

  val interactive_one: in_channel -> string -> unit
  (**
     Channels suppose that the same C runtime has been used to compile both the
     Python library and the OCaml runtime.
     Warning: using channels is unsafe if runtimes differ (can lead to
     segmentation fault).
      Wrapper for
      {{:https://docs.python.org/3/c-api/veryhigh.html#c.PyRun_InteractiveOne} PyRun_InteractiveOne} *)

  val interactive_loop: in_channel -> string -> unit
  (**
     Channels suppose that the same C runtime has been used to compile both the
     Python library and the OCaml runtime.
     Warning: using channels is unsafe if runtimes differ (can lead to
     segmentation fault).
     Wrapper for
      {{:https://docs.python.org/3/c-api/veryhigh.html#c.PyRun_InteractiveLoop} PyRun_InteractiveLoop} *)

  val simple_file: in_channel file -> string -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/veryhigh.html#c.PyRun_SimpleFile} PyRun_SimpleFile} *)

  val simple_string: string -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/veryhigh.html#c.PyRun_SimpleString} PyRun_SimpleString} *)

  val string: string -> input -> Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/veryhigh.html#c.PyRun_String} PyRun_String} *)

  val frame: ('a -> 'b) -> 'a -> 'b
end

(** Interface for Python values with a [Sequence] interface. *)
module Sequence: sig
  val check: Object.t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_Check} PySequence_Check} *)

  val concat: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_Concat} PySequence_Concat} *)

  val contains: Object.t -> Object.t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_Contains} PySequence_Contains} *)

  val count: Object.t -> Object.t -> int
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_Count} PySequence_Count} *)

  val del_item: Object.t -> int -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_DelItem} PySequence_DelItem} *)

  val fast: Object.t -> string -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_Fast} PySequence_Fast} *)

  val get_item: Object.t -> int -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_GetItem} PySequence_GetItem} *)

  val get: Object.t -> int -> Object.t
  (** Equivalent to {!get_item}. *)

  val get_slice: Object.t -> int -> int -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_GetSlice} PySequence_GetSlice} *)

  val index: Object.t -> Object.t -> int
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_Index} PySequence_Index} *)

  val in_place_concat: Object.t -> Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_InPlaceConcat} PySequence_InPlaceConcat} *)

  val in_place_repeat: Object.t -> int -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_InPlaceRepeat} PySequence_InPlaceRepeat} *)

  val length: Object.t -> int
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_Length} PySequence_Length} *)

  val list: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_List} PySequence_List} *)

  val repeat: Object.t -> int -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_Repeat} PySequence_Repeat} *)

  val set_item: Object.t -> int -> Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_SetItem} PySequence_SetItem} *)

  val set: Object.t -> int -> Object.t -> unit
  (** Equivalent to {!set_item}. *)

  val set_slice: Object.t -> int -> int -> Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_SetSlice} PySequence_SetSlice} *)

  val size: Object.t -> int
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_Size} PySequence_Size} *)

  val tuple: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PySequence_Tuple} PySequence_Tuple} *)

  val to_array: Object.t -> Object.t array
  (** [to_array s] returns the array with the same elements as the Python
      sequence [s]. *)

  val to_array_map: (Object.t -> 'a) -> Object.t -> 'a array
  (** [to_array_map f s] returns the array of the results of [f] applied to
      all the elements of the Python sequence [s]. *)

  val to_list: Object.t -> Object.t list
  (** [to_list s] returns the list with the same elements as the Python
      sequence [s]. *)

  val to_list_map: (Object.t -> 'a) -> Object.t -> 'a list
  (** [to_list_map f s] returns the list of the results of [f] applied to all
      the elements of the Python sequence [s].
      [to_list_map f s] is equivalent to [List.map f (to_list s)] but is
      tail-recursive and [f] is applied to the elements of [s] in the reverse
      order. *)

  val to_seq: Object.t -> Object.t Seq.t
  (** [to_seq s] returns the OCaml sequence of the values from the Python
      sequence [s]. *)

  val to_seqi: Object.t -> (int * Object.t) Seq.t
  (** [to_seqi s] returns the OCaml indexed sequence of the values from the
      Python sequence [s]. *)

  val fold_left: ('a -> Object.t -> 'a) -> 'a -> Object.t -> 'a
  (** [fold_left f v s] returns [(f (...(f v s1)...) sn)] where [s1], ..., [sn]
      are the elements of the Python sequence [s]. *)

  val fold_right: (Object.t -> 'a -> 'a) -> Object.t -> 'a -> 'a
  (** [fold_right f s v] returns [(f s1 (...(f v sn)...)] where [s1], ..., [sn]
      are the elements of the Python sequence [s].
      This function is tail-recursive. *)

  val for_all: (Object.t -> bool) -> Object.t -> bool
  (** [for_all p s] checks if [p] holds for all the elements of the Python
      sequence [s]. *)

  val exists: (Object.t -> bool) -> Object.t -> bool
  (** [exists p s] checks if [p] holds for at least one of the elements of the
      Python sequence [s]. *)
end

type byteorder =
    LittleEndian
  | BigEndian

(** Interface for Python values of type [String], [Bytes] and [Unicode]. *)
module String: sig
  val check: Object.t -> bool
  (** [check o] returns [o] if [o] is a Python string
      (either [Bytes] or [Unicode] with Python 3). *)

  val check_bytes: Object.t -> bool
  (** [check_bytes o] returns [o] if [o] is a Python bytes string. *)

  val check_unicode: Object.t -> bool
  (** [check_unicode o] returns [o] if [o] is a Python unicode string. *)

  val format: Object.t -> Object.t -> Object.t
  (** [format fmt args] returns the formatted Python string from the string
      format [fmt] and the arguments [args].
      This is analogous to [fmt % args].
      With Python 2, if [fmt] is a String, wrapper for
      {{:https://docs.python.org/2/c-api/string.html#c.PyString_Format} PyString_Format}.
      With Python 3 or with Python 2 if [fmt] is Unicode, wrapper for
      {{:https://docs.python.org/3/c-api/unicode.html#c.PyUnicode_Format} PyUnicode_Format}. *)

  val as_UTF8_string: Object.t -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/unicode.html#c.PyUnicode_AsUTF8String} PyUnicode_AsUTF8String} *)

  val decode_UTF8: ?errors:string -> ?size:int -> string -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/unicode.html#c.PyUnicode_DecodeUTF8} PyUnicode_DecodeUTF8}.
      If [size] is omitted, the length of the string is used by default. *)

  val decode_UTF16: ?errors:string -> ?size:int -> ?byteorder:byteorder
    -> string -> Object.t * byteorder
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/unicode.html#c.PyUnicode_DecodeUTF16} PyUnicode_DecodeUTF16}.
      If [size] is omitted, the length of the string is used by default. *)

  val decode_UTF32: ?errors:string -> ?size:int -> ?byteorder:byteorder
    -> string -> Object.t * byteorder
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/unicode.html#c.PyUnicode_DecodeUTF32} PyUnicode_DecodeUTF32}.
      If [size] is omitted, the length of the string is used by default. *)

  val length: Object.t -> int
  (** [length s] returns the length of the Python string [s].
      A failure ([Failure _]) is raised if [s] is neither a [Bytes] value
      nor a [Unicode] value.
      With Python 2,
      if [s] is a String, wrapper for
      {{:https://docs.python.org/2/c-api/string.html#c.PyString_Size} PyString_Size},
      and if [s] is Unicode, wrapper for
      {{:https://docs.python.org/2/c-api/unicode.html#c.PyUnicode_GetSize} PyUnicode_GetSize},
      With Python 3,
      if [s] is Bytes, wrapper for
      {{:https://docs.python.org/2/c-api/bytes.html#c.PyBytes_Size} PyBytes_Size},
      and if [s] is Unicode, wrapper for
      {{:https://docs.python.org/2/c-api/unicode.html#c.PyUnicode_GetLength} PyUnicode_GetLength}. *)

  val of_string: string -> Object.t
  (** [of_string s] returns the Python string with the value [s].
      [s] should be a valid UTF-8 string. *)

  val of_bytes: bytes -> Object.t
  (** Same as [of_string] but with an argument of type [bytes]. *)

  val to_string: Object.t -> string
  (** [to_string o] returns the string contained in the Python value [o].
      A failure ([Failure _]) is raised if [o] is neither a
      [String]/[Bytes] value nor a [Unicode] value. *)

  val to_bytes: Object.t -> bytes
  (** Same as [to_string] but with an a result of type [bytes]. *)

  val of_unicode: ?size:int -> int array -> Object.t
  (** [of_unicode codepoints] returns the Python Unicode string with the
      codepoints [codepoints]. *)

  val to_unicode: Object.t -> int array
  (** [to_unicode s] returns the codepoints of the Python Unicode string
      [s]. *)
end

(** Interface for Python values of type [Bytes].
    With Python 2, aliases for [String]. *)
module Bytes: sig
  val of_string: string -> Object.t
  (** [of_string s] returns the Python byte sequence with the contents of
      [s]. *)

  val of_bytes: bytes -> Object.t
  (** Same as [of_string] but with an argument of type [bytes]. *)

  val to_string: Object.t -> string
  (** [to_string o] returns the string contained in the Python value [o]. *)

  val to_bytes: Object.t -> bytes
  (** Same as [to_string] but with an a result of type [bytes]. *)

  val length: Object.t -> int
  (** [length s] returns the length of the Python byte sequence [s]. *)
end

(** Interface for Python values of type [Tuple]. *)
module Tuple: sig
  val check: Object.t -> bool
  (** [check o] returns [true] if [o] is a Python tuple. *)

  val create: int -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/tuple.html#c.PyTuple_New} PyTuple_New} *)

  val empty: Object.t
  (** The empty tuple [()].
      This value is guaranteed to be the unique value associated to [()]. *)

  val is_empty: Object.t -> bool
  (** [Py.is_empty v] is true if and only if [v] is [()].
      Since [Py.Tuple.empty] is guaranteed to be the unique value associated to
      [()], [Py.is_empty v] is equivalent to [v == Py.empty]. *)

  val get_item: Object.t -> int -> Object.t
  (** Equivalent to {!Sequence.get_item}. *)

  val get: Object.t -> int -> Object.t
  (** Equivalent to {!get_item}. *)

  val set_item: Object.t -> int -> Object.t -> unit
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/sequence.html#c.PyTuple_SetItem} PyTuple_SetItem} *)

  val set: Object.t -> int -> Object.t -> unit
  (** Equivalent to {!set_item}. *)

  val get_slice: Object.t -> int -> int -> Object.t
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/tuple.html#c.PyTuple_GetSlice} PyTuple_GetSlice} *)

  val size: Object.t -> int
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/tuple.html#c.PyTuple_Size} PyTuple_Size} *)

  val init: int -> (int -> Object.t) -> Object.t
  (** [init n f] returns the Python tuple [(f 0, f 1, ..., f (n - 1))]. *)

  val of_array: Object.t array -> Object.t
  (** [of_array a] returns the Python tuple with the same elements as [a]. *)

  val of_array_map: ('a -> Object.t) -> 'a array -> Object.t
  (** [of_array_map f a] returns the Python tuple [(f a0, ..., f ak)] where
      [a0], ..., [ak] are the elements of [a]. *)

  val of_list: Object.t list -> Object.t
  (** [of_list l] returns the Python tuple with the same elements as [l]. *)

  val of_list_map: ('a -> Object.t) -> 'a list -> Object.t
  (** [of_list f l] returns the Python tuple [(f l1, ..., f ln)] where
      [l1], ..., [ln] are the elements of [l].
      [of_list_map f l] is equivalent to [of_list (List.map f l)] but is
      tail-recursive. *)

  val to_array: Object.t -> Object.t array
  (** Equivalent to {!Sequence.to_array}. *)

  val to_array_map: (Object.t -> 'a) -> Object.t -> 'a array
  (** Equivalent to {!Sequence.to_array_map}. *)

  val to_list: Object.t -> Object.t list
  (** Equivalent to {!Sequence.to_list}. *)

  val to_list_map: (Object.t -> 'a) -> Object.t -> 'a list
  (** Equivalent to {!Sequence.to_list_map}. *)

  val of_seq: Object.t Seq.t -> Object.t
  (** [of_seq s] returns the Python tuple with the values of the sequence s. *)

  val to_seq: Object.t -> Object.t Seq.t
  (** Equivalent to {!Sequence.to_seq}. *)

  val to_seqi: Object.t -> (int * Object.t) Seq.t
  (** Equivalent to {!Sequence.to_seqi}. *)

  val fold_left: ('a -> Object.t -> 'a) -> 'a -> Object.t -> 'a
  (** Equivalent to {!Sequence.fold_left}. *)

  val fold_right: (Object.t -> 'a -> 'a) -> Object.t -> 'a -> 'a
  (** Equivalent to {!Sequence.fold_right}. *)

  val for_all: (Object.t -> bool) -> Object.t -> bool
  (** Equivalent to {!Sequence.for_all}. *)

  val exists: (Object.t -> bool) -> Object.t -> bool
  (** Equivalent to {!Sequence.exists}. *)

  val of_sequence: Object.t -> Object.t
  (** Equivalent to {!Sequence.tuple}. *)

  val of_tuple1: Object.t -> Object.t
  (** [of_tuple1 o0] returns the Python tuple [(o0)]. *)

  val of_tuple2: Object.t * Object.t -> Object.t
  (** [of_tuple4 (o0, o1)] returns the Python tuple [(o0, o1)]. *)

  val of_tuple3: Object.t * Object.t * Object.t -> Object.t
  (** [of_tuple4 (o0, o1, o2)] returns the Python tuple [(o0, o1, o2)]. *)

  val of_tuple4: Object.t * Object.t * Object.t * Object.t -> Object.t
  (** [of_tuple4 (o0, o1, o2, o3)] returns the Python tuple
      [(o0, o1, o2, o3)]. *)

  val of_tuple5:
    Object.t * Object.t * Object.t * Object.t * Object.t -> Object.t
  (** [of_tuple5 (o0, o1, o2, o3, o4)] returns the Python tuple
      [(o0, o1, o2, o3, o4)]. *)

  val to_tuple1: Object.t -> Object.t
  (** [to_tuple1 t] returns the value [Py.Tuple.get_item t 0]. *)

  val to_tuple2: Object.t -> Object.t * Object.t
  (** [to_tuple5 t] returns the tuple [(Py.Tuple.get_item t 0,
      Py.Tuple.get_item t 1)]. *)

  val to_tuple3: Object.t -> Object.t * Object.t * Object.t
  (** [to_tuple5 t] returns the tuple [(Py.Tuple.get_item t 0,
      Py.Tuple.get_item t 1, Py.Tuple.get_item t 2)]. *)

  val to_tuple4: Object.t -> Object.t * Object.t * Object.t * Object.t
  (** [to_tuple5 t] returns the tuple [(Py.Tuple.get_item t 0,
      Py.Tuple.get_item t 1, Py.Tuple.get_item t 2, Py.Tuple.get_item t 3)]. *)

  val to_tuple5:
    Object.t -> Object.t * Object.t * Object.t * Object.t * Object.t
  (** [to_tuple5 t] returns the tuple [(Py.Tuple.get_item t 0,
      Py.Tuple.get_item t 1, Py.Tuple.get_item t 2, Py.Tuple.get_item t 3,
      Py.Tuple.get_item t 4)]. *)

  val singleton: Object.t -> Object.t
  (** Equivalent to {!of_tuple1}. *)

  val to_singleton: Object.t -> Object.t
  (** Equivalent to {!to_tuple1}. *)

  val of_pair: Object.t * Object.t -> Object.t
  (** Equivalent to {!of_tuple2}. *)

  val to_pair: Object.t -> Object.t * Object.t
  (** Equivalent to {!to_tuple2}. *)
end

(** Introspection of Python types *)
module Type: sig
  type t =
      Unknown
    | Bool
    | Bytes
    | Callable
    | Capsule
    | Closure
    | Dict
    | Float
    | List
    | Int
    | Long
    | Module
    | None
    | Null
    | Tuple
    | Type
    | Unicode
    | Iter
    | Set
  (** Some types of Python values.
      [Bytes] covers both the [Str] values of Python 2
      and the [Bytes] values of Python 3.
      [Long] covers both the [Int] values of Python 2
      and the [Long] values of Python 3.
      [Capsule] corresponds to the values created with {!Py.Capsule}.
      [Closure] corresponds to the values created with {!Py.Callable}. *)

  val get: Object.t -> t
  (** [get o] returns the type of the Python value [o]. *)

  val is_subtype: Object.t -> Object.t -> bool
  (** Wrapper for
      {{:https://docs.python.org/3/c-api/type.html#c.PyType_IsSubtype} PyType_IsSubtype} *)

  val is_none: Object.t -> bool
  (** [is_none o] returns [true] if the Python object [o] is [None]. *)

  val name: t -> string
  (** [name t] returns a string that represents the type [t]. *)

  val mismatch: string -> Object.t -> 'a
  (** [mismatch ty obj] raises a type mismatch [Failure _] that indicates that
      an object of type [ty] was expected, but [obj] was found. *)

  val create: string -> Object.t list -> (string * Object.t) list -> Object.t
  (** [create classname parents dict] calls Python [type()] function to create
      a new type [classname] deriving from [parents] with the dictionary
      [dict]. *)
end

module Marshal: sig
  val read_object_from_file: in_channel file -> Object.t
  (** [read_object_from_file f] reads one value from [f] and returns it.
      Wrapper for
      {{:https://docs.python.org/3/c-api/marshal.html#c.PyMarshal_ReadObjectFromFile} PyMarshal_ReadObjectFromFile} *)

  val load: in_channel file -> Object.t
  (** Equivalent to {!read_object_from_file}. *)

  val read_last_object_from_file: in_channel file -> Object.t
  (** [read_last_object_from_file f] reads a value from [f] and returns it.
      That value should be the only value remaining to be read from [f] before
      EOF.
      Wrapper for
      {{:https://docs.python.org/3/c-api/marshal.html#c.PyMarshal_ReadLastObjectFromFile} PyMarshal_ReadLastObjectFromFile} *)

  val read_object_from_string: string -> int -> Object.t
  (** [read_object_from_string s len] reads a value from the [len] first
      bytes of [s].
      Wrapper for
      {{:https://docs.python.org/3/c-api/marshal.html#c.PyMarshal_ReadObjectFromString} PyMarshal_ReadObjectFromString} *)

  val loads: string -> Object.t
  (** [Py.Marshal.loads s] is equivalent to
      [Py.Marshal.read_object_from_string s (String.length s)]. *)

  val write_object_to_file: Object.t -> out_channel file -> int -> unit
  (** [write_object_to_file value file version] writes the object [value] to
      [file].
      [version] indicates the file format
      (use {!version} to get the current version).
      Wrapper for
      {{:https://docs.python.org/3/c-api/marshal.html#c.PyMarshal_WriteObjectToFile} PyMarshal_WriteObjectToFile} *)

  val dump: ?version:int -> Object.t -> out_channel file -> unit
  (** [Py.Marshal.dump ?version value file] is equivalent to
      [Py.Marshal.write_object_to_file value file version].
      By default, the version returned by {!version} is used. *)

  val write_object_to_string: Object.t -> int -> Object.t
  (** [write_object_to_file value file version] returns the Python string
      representing the object [value].
      [version] indicates the format
      (use {!version} to get the current version).
      Wrapper for
      {{:https://docs.python.org/3/c-api/marshal.html#c.PyMarshal_WriteObjectToString} PyMarshal_WriteObjectToString} *)

  val dumps: ?version:int -> Object.t -> string
  (** [Py.Marshal.dumps ?version value] is equivalent to
      [Py.String.to_string (Py.Marshal.write_object_to_string value version)].
      By default, the version returned by {!version} is used. *)

  val version: unit -> int
  (** Returns the current file format version number. *)
end

module Array: sig
  val of_indexed_structure:
      (int -> Object.t) -> (int -> Object.t -> unit) -> int -> Object.t
  (** [Py.Array.of_indexed_structure getter setter length] returns a Python
      array-like structure [a] of length [length], such that reading [a[i]]
      returns [getter i] and [a[i] = v] calls [setter i v].
      To make the array-like structure read-only,
      raise an exception in [setter]. *)

  val of_array: ('a -> Object.t) -> (Object.t -> 'a) -> 'a array -> Object.t
  (** [Py.Array.of_array getter setter array] returns a Python
      array-like structure accessing the elements of [array] via [getter]
      and [setter].
      To make the array-like structure read-only,
      raise an exception in [setter]. *)

  val numpy_api: unit -> Object.t
  (** Returns the object which contains the entry points to the Numpy API.
      It is used internally by the following functions and by the {!Numpy}
      module. *)

  val pyarray_type: unit -> Object.t
  (** Returns the type of Numpy arrays. *)

  val numpy: floatarray -> Object.t
  (** [numpy a] returns a Numpy array that shares the same contents than
      the OCaml array [a].
      The array is passed in place (without copy) which relies on the
      unboxed representation of [floatarray] : Python programs can
      change the contents of the array and the changes are visible in
      the OCaml array.
      Note that the {!Numpy} module provides a more general interface
      between Numpy arrays and OCaml bigarrays. *)

  val numpy_get_array: Object.t -> floatarray
  (** [numpy_get_array a] returns the OCaml array from which the Numpy
      array [a] has been converted from. Note that this function fails
      if [a] has not been obtained by calling the {!numpy} function
      above. If you need to convert an arbitrary Numpy array to OCaml,
      you should use bigarrays and the {!Numpy} module. *)
end

module Gil : sig
  type t

  val ensure : unit -> t
  (** [ensure ()] ensures that the current thread holds the global
      interpreter lock and hence can call the Python C API in a safe
      way.
      Wrapper for
      {{::https//docs.python.org/3/c-api/init.html#c.PyGILState_Ensure} PyGILState_Ensure} *)

  val release : t -> unit
  (** [release t] releases any resource acquired by [ensure].
      Wrapper for
      {{::https//docs.python.org/3/c-api/init.html#c.PyGILState_Release} PyGILState_Release} *)

  val check : unit -> bool
  (** [check ()] returns true if the current thread holds the global
      interpreter lock.
      Wrapper for
      {{::https//docs.python.org/3/c-api/init.html#c.PyGILState_Check} PyGILState_Check} *)

  val with_lock : (unit -> 'a) -> 'a
  (** [with_lock f] runs [f] ensuring that we hold the global interpreter
      lock to do so. If the lock needs to be acquired it is released once
      [f] completes or if [f] raises an exception.
  *)
end

val set_argv: string array -> unit
(** [set_argv argv] set Python's [sys.argv]. *)

val last_value: unit -> Object.t
(** [last_value ()] returns the last value that was computed in the
    toplevel.
    We have [Py.last_value = Py.Module.find (Py.Module.builtins ()) "_"]. *)

val exception_printer: exn -> string option
(** This printer pretty-prints [E (ty, value)] exceptions.
    It is automatically registered to [Printexc.register_printer]. *)

val compile: source:string -> filename:string -> ?dont_inherit:bool ->
  ?optimize:[`Default | `Debug | `Normal | `RemoveDocstrings ] ->
  [`Exec | `Eval | `Single] -> Object.t
(** Old interface for {!val:Py.Module.compile}. *)
