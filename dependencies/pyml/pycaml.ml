type pyobject = Py.Object.t

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
  | AnyType

let pytype_name t =
  match t with
  | TupleType -> "Python-Tuple"
  | BytesType -> "Python-Bytes"
  | UnicodeType -> "Python-Unicode"
  | BoolType -> "Python-Bool"
  | IntType -> "Python-Int"
  | FloatType -> "Python-Float"
  | ListType -> "Python-List"
  | NoneType -> "Python-None"
  | CallableType -> "Python-Callable"
  | ModuleType -> "Python-Module"
  | ClassType -> "Python-Class"
  | NullType -> "Python-Null"
  | TypeType -> "Python-Type"
  | DictType -> "Python-Dict"
  | CamlpillType -> "Python-Camlpill"
  | OtherType -> "Python-Other"
  | EitherStringType -> "Python-EitherString"
  | CamlpillSubtype sym -> "Python-Camlpill-" ^ sym
  | AnyType -> "Python-Any"

let _py_type_of_pyobject_type t =
  match t with
  | BoolType -> Py.Type.Bool
  | BytesType -> Py.Type.Bytes
  | CallableType -> Py.Type.Callable
  | CamlpillType
  | CamlpillSubtype _ -> Py.Type.Capsule
  | DictType -> Py.Type.Dict
  | FloatType -> Py.Type.Float
  | ListType -> Py.Type.List
  | IntType -> Py.Type.Long
  | ModuleType -> Py.Type.Module
  | NoneType -> Py.Type.None
  | NullType -> Py.Type.Null
  | TupleType -> Py.Type.Tuple
  | TypeType -> Py.Type.Type
  | EitherStringType
  | UnicodeType -> Py.Type.Unicode
  | AnyType
  | ClassType
  | OtherType -> Py.Type.Unknown

let pyobject_type_of_py_type t =
  match t with
    Py.Type.Unknown | Py.Type.Iter | Py.Type.Set -> OtherType
  | Py.Type.Bool -> BoolType
  | Py.Type.Bytes -> BytesType
  | Py.Type.Callable -> CallableType
  | Py.Type.Capsule -> CamlpillType
  | Py.Type.Closure -> CallableType
  | Py.Type.Dict -> DictType
  | Py.Type.Float -> FloatType
  | Py.Type.List -> ListType
  | Py.Type.Int
  | Py.Type.Long -> IntType
  | Py.Type.Module -> ModuleType
  | Py.Type.None -> NoneType
  | Py.Type.Null -> NullType
  | Py.Type.Tuple -> TupleType
  | Py.Type.Type -> TypeType
  | Py.Type.Unicode -> UnicodeType


type pyerror_type =
    Pyerr_Exception
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

include Pywrappers.Pycaml

exception Pycaml_exn of (pyerror_type * string)

let make_pill_wrapping name _instance = Py.Capsule.make name

let py_false () = Py.Bool.f

let py_finalize = Py.finalize

let py_initialize () = Py.initialize ()

let py_is_true = Py.Object.is_true

let py_isinitialized () =
  if Py.is_initialized () then 1
  else 0

let py_setprogramname = Py.set_program_name

let py_setpythonhome = Py.set_python_home

let py_getprogramname = Py.get_program_name

let py_getpythonhome = Py.get_python_home

let py_getprogramfullpath = Py.get_program_full_path

let py_getprefix = Py.get_prefix

let py_getexecprefix = Py.get_exec_prefix

let py_getpath = Py.get_path

let py_true () = Py.Bool.t

let _pycaml_seterror error msg =
  let error' =
    match error with
      Pyerr_Exception -> Py.Err.Exception
    | Pyerr_StandardError -> Py.Err.StandardError
    | Pyerr_ArithmeticError -> Py.Err.ArithmeticError
    | Pyerr_LookupError -> Py.Err.LookupError
    | Pyerr_AssertionError -> Py.Err.AssertionError
    | Pyerr_AttributeError -> Py.Err.AttributeError
    | Pyerr_EOFError -> Py.Err.EOFError
    | Pyerr_EnvironmentError -> Py.Err.EnvironmentError
    | Pyerr_FloatingPointError -> Py.Err.FloatingPointError
    | Pyerr_IOError -> Py.Err.IOError
    | Pyerr_ImportError -> Py.Err.ImportError
    | Pyerr_IndexError -> Py.Err.IndexError
    | Pyerr_KeyError -> Py.Err.KeyError
    | Pyerr_KeyboardInterrupt -> Py.Err.KeyboardInterrupt
    | Pyerr_MemoryError -> Py.Err.MemoryError
    | Pyerr_NameError -> Py.Err.NameError
    | Pyerr_NotImplementedError -> Py.Err.NotImplementedError
    | Pyerr_OSError -> Py.Err.OSError
    | Pyerr_OverflowError -> Py.Err.OverflowError
    | Pyerr_ReferenceError -> Py.Err.ReferenceError
    | Pyerr_RuntimeError -> Py.Err.RuntimeError
    | Pyerr_SyntaxError -> Py.Err.SyntaxError
    | Pyerr_SystemExit -> Py.Err.SystemExit
    | Pyerr_TypeError -> Py.Err.TypeError
    | Pyerr_ValueError -> Py.Err.ValueError
    | Pyerr_ZeroDivisionError -> Py.Err.ZeroDivisionError in
  Py.Err.set_error error' msg

let int_of_bool b =
  if b then -1
  else 0

let _pybytes_check v = int_of_bool (Py.Type.get v = Py.Type.Bytes)

let pybytes_asstring = Py.String.to_string

let pybytes_format (fmt, args) = Py.String.format fmt args

let pyiter_check v = int_of_bool (Py.Type.get v = Py.Type.Iter)

let pymodule_getfilename = Py.Module.get_filename

let pymodule_getname = Py.Module.get_name

let _pyunicode_check v = int_of_bool (Py.Type.get v = Py.Type.Unicode)

let pyerr_fetch _ =
  match Py.Err.fetch () with
    None -> (Py.null, Py.null, Py.null)
  | Some e -> e

let pyerr_normalizeexception e = e

let pylist_toarray = Py.Sequence.to_array

let pyint_asint = Py.Long.to_int

let pyint_fromint = Py.Long.of_int

let pynone () = Py.none

let pynull () = Py.null

let pystring_asstring = Py.String.to_string

let pystring_fromstring = Py.String.of_string

let pytuple_fromarray = Py.Tuple.of_array

let pytuple_fromsingle = Py.Tuple.singleton

let pytuple_toarray = Py.Tuple.to_array

let pytype v = pyobject_type_of_py_type (Py.Type.get v)

let register_ocamlpill_types _array = ()

let pyeval_callobject (func, arg) =
  Pywrappers.pyeval_callobjectwithkeywords func arg Py.null

let pyimport_execcodemodule (obj, s) =
  Pywrappers.pyimport_execcodemodule s obj

let pyimport_importmoduleex (name, globals, locals, fromlist) =
  Pywrappers.pyimport_importmodulelevel name globals locals fromlist (-1)

let py_compilestringflags (str, filename, start, flags) =
  if Py.version_major () <= 2 then
    py_compilestringflags (str, filename, start, flags)
  else
    py_compilestringexflags (str, filename, start, flags, -1)

let py_compilestring (str, filename, start) =
  py_compilestringflags (str, filename, start, None)

let pyrun_anyfile (fd, filename) =
  pyrun_anyfileexflags (fd, filename, 0, None)

let pyrun_anyfileex (fd, filename, closeit) =
  pyrun_anyfileexflags (fd, filename, closeit, None)

let pyrun_file (fd, filename, start, globals, locals) =
  pyrun_fileexflags (fd, filename, start, globals, locals, 0, None)

let pyrun_fileex (fd, filename, start, globals, locals, closeit) =
  pyrun_fileexflags (fd, filename, start, globals, locals, closeit, None)

let pyrun_interactiveone (fd, filename) =
  pyrun_interactiveoneflags (fd, filename, None)

let pyrun_interactiveloop (fd, filename) =
  pyrun_interactiveloopflags (fd, filename, None)

let pyrun_simplefile (fd, filename) =
  pyrun_simplefileexflags (fd, filename, 0, None)

let pyrun_simplefileex (fd, filename, closeit) =
  pyrun_simplefileexflags (fd, filename, closeit, None)

let pyrun_simplestring s = pyrun_simplestringflags (s, None)

let pyrun_string (s, start, globals, locals) =
  pyrun_stringflags (s, start, globals, locals, None)

let pywrap_closure f = Py.Callable.of_function_as_tuple f

let pytuple_empty = Py.Tuple.empty

let pytuple2 = Py.Tuple.of_tuple2

let pytuple3 = Py.Tuple.of_tuple3

let pytuple4 = Py.Tuple.of_tuple4

let pytuple5 = Py.Tuple.of_tuple5

let set_python_argv = Py.set_argv

let py_optionally unwrapper py_value =
  if not (Py.List.check py_value) then
    Py.Type.mismatch "List" py_value;
  match Py.List.size py_value with
    0 -> None
  | 1 -> Some (unwrapper (Py.List.get_item py_value 0))
  | _ -> Py.Type.mismatch "List of size 0 or 1" py_value

let pycallable_asfun = Py.Callable.to_function

let guarded_pytuple_toarray = Py.Tuple.to_array

let guarded_pylist_toarray = Py.List.to_array

let guarded_pybytes_asstring = Py.String.to_string

let guarded_pynumber_asfloat = Py.Number.to_float

let guarded_pyfloat_asfloat = Py.Float.to_float

let guarded_pyint_asint = Py.Long.to_int

let ocamlpill_hard_unwrap v = snd (Py.Capsule.unsafe_unwrap_value v)

let python_eval = pyrun_simplestring

let python_load filename =
  ignore (Py.Run.load (Py.Filename filename) filename)

let pybytes_asstringandsize = Py.String.to_string

let pystring_asstringandsize = Py.String.to_string

let pyunicode_decodeutf8 (s, errors) = Py.String.decode_UTF8 s ?errors

let byteorder_of_int_option byteorder_int =
  match byteorder_int with
    None -> None
  | Some (-1) -> Some Py.LittleEndian
  | Some 1 -> Some Py.BigEndian
  | _ -> failwith "pyunicode_decodeutf: invalid byteorder"

let pyunicode_decodeutf16 (s, errors, byteorder_int) =
  let byteorder = byteorder_of_int_option byteorder_int in
  fst (Py.String.decode_UTF16 s ?errors ?byteorder)

let pyunicode_decodeutf32 (s, errors, byteorder_int) =
  let byteorder = byteorder_of_int_option byteorder_int in
  fst (Py.String.decode_UTF32 s ?errors ?byteorder)

let pyunicode_fromunicode f size = Py.String.of_unicode (Array.init size f)

let pyunicode_asunicode = Py.String.to_unicode

let pyunicode_getsize = Py.String.length

let pyobject_ascharbuffer = Py.Object.as_char_buffer

let pyobject_asreadbuffer = Py.Object.as_read_buffer

let pyobject_aswritebuffer = Py.Object.as_write_buffer

let python () =
  Py.Run.interactive ();
  0

let ipython () =
  Py.Run.ipython ();
  0

let make_ocamlpill_wrapper_unwrapper = make_pill_wrapping

let ocamlpill_type_of = Py.Capsule.type_of

let type_mismatch_exception type_wanted type_here pos exn_name =
  Pycaml_exn
    (Pyerr_TypeError,
     (Printf.sprintf "Argument %d: Type wanted: %s -- Type provided: %s%s."
        (pos + 1) (pytype_name type_wanted) (pytype_name type_here)
        exn_name))

let pill_type_mismatch_exception ?position ?exn_name wanted gotten =
  let arg_no =
    match position with
      None -> ""
    | Some _p -> "Argument %d: " in
  let en =
    match exn_name with
      None -> ""
    | Some n -> n in
  Pycaml_exn
    (Pyerr_TypeError,
     Printf.sprintf
       "%sPython-Ocaml Pill Type mismatch: wanted: '%s' - got: '%s'%s"
       arg_no wanted gotten en)

let check_pill_type ?position ?exn_name wanted pill =
  let gotten = ocamlpill_type_of pill in
  if not (gotten = wanted) then
      raise
      (pill_type_mismatch_exception
         ?position:position ?exn_name:exn_name wanted gotten)

let unpythonizing_function ?name ?(catch_weird_exceptions = true) ?extra_guards
    ?(expect_tuple = false) wanted_types function_body =
  ignore catch_weird_exceptions;
  let exn_name =
    match name with
      None -> ""
    | Some s -> Printf.sprintf " (%s)" s in
  let work_fun python_args =
    let body () =
      let nr_args_given =
        if expect_tuple then pytuple_size python_args
        else 1 in
      let nr_args_wanted = Array.length wanted_types in
      let () =
        if nr_args_given <> nr_args_wanted then
          raise
            (Pycaml_exn
               (Pyerr_IndexError,
                (Printf.sprintf
                   "Args given: %d Wanted: %d%s"
                   nr_args_given nr_args_wanted exn_name)))
      in
      let arr_args =
        if expect_tuple then
          Py.Tuple.to_array python_args
        else
          [| python_args |]
      in
      let rec check_types pos =
        if pos = nr_args_given then
          function_body arr_args
        else
          let arg = arr_args.(pos) in
          let type_wanted = wanted_types.(pos) in
          let () =
            match type_wanted with
              | AnyType -> ()
              | EitherStringType ->
                  if not (Py.String.check arg) then
                    raise
                      (type_mismatch_exception
                         type_wanted (pytype arg) pos exn_name)
              | CamlpillSubtype sym ->
                  check_pill_type ~position:pos ~exn_name:exn_name sym arg
              | _ ->
                  let type_here = pytype arg in
                  if type_here <> type_wanted then
                    raise
                      (type_mismatch_exception type_wanted type_here pos
                         exn_name) in
          begin
            match extra_guards with
              None -> ()
            | Some guards ->
                let guard = guards.(pos) in
                let guard_error = guard arr_args.(pos) in
                match guard_error with
                  None -> ()
                | Some msg ->
                    raise (Pycaml_exn
                             (Pyerr_TypeError,
                              (Printf.sprintf
                                 "Check for argument %d failed: %s%s"
                                 (pos + 1) msg exn_name)))
          end;
          check_types (pos+1) in
      check_types 0 in
    body () in
  work_fun

let py_profiling_active = ref false

let py_profile_hash = Lazy.from_fun (fun () -> Hashtbl.create 100)

let py_activate_profiling () =
  let old_value = !py_profiling_active in
  py_profiling_active := true;
  old_value

let py_deactivate_profiling () =
  let old_value = !py_profiling_active in
  py_profiling_active := false;
  old_value

let py_profile_report () =
  let add_entry name time_and_calls list =
    (name, time_and_calls.(0), time_and_calls.(1)) :: list in
  let items = Hashtbl.fold add_entry (Lazy.force py_profile_hash) [] in
  let array = Array.of_list items in
  let order (_, time_a, _) (_, time_b, _) = compare time_b time_a in
  Array.sort order array;
  array

let py_profile_reset () =
  Hashtbl.clear (Lazy.force py_profile_hash)

let python_interfaced_function ?name ?(catch_weird_exceptions = true) ?doc
    ?extra_guards wanted_types function_body =
  let exn_name =
    match name with
      None -> ""
    | Some s -> Printf.sprintf " (%s)" s in
  let closure =
    unpythonizing_function ?name ~catch_weird_exceptions ?extra_guards
      wanted_types function_body in
  let closure' args =
    try
      closure args
    with
      Not_found ->
        let msg = Printf.sprintf "OCaml exception 'Not_found'%s" exn_name in
        raise (Py.Err (Py.Err.LookupError, msg))
    | Division_by_zero ->
        let msg =
          Printf.sprintf "OCaml exception 'Division_by_zero'%s" exn_name in
        raise (Py.Err (Py.Err.ZeroDivisionError, msg))
    | Failure s ->
        let msg = Printf.sprintf "OCaml exception 'Failure: %s'%s" s exn_name in
        raise (Py.Err (Py.Err.StandardError, msg))
    | Invalid_argument s ->
        let msg =
          Printf.sprintf
            "OCaml exception 'Invalid_argument: %s'%s" s exn_name in
        raise (Py.Err (Py.Err.StandardError, msg))
    | Out_of_memory ->
        let msg = Printf.sprintf "OCaml exception 'Out_of_memory'%s" exn_name in
        raise (Py.Err (Py.Err.MemoryError, msg))
    | Stack_overflow ->
        let msg =
          Printf.sprintf "OCaml exception 'Stack_overflow'%s" exn_name in
        raise (Py.Err (Py.Err.OverflowError, msg))
    | Sys_error s ->
        let msg =
          Printf.sprintf "OCaml exception 'Sys_error: %s'%s" s exn_name in
        raise (Py.Err (Py.Err.StandardError, msg))
    | End_of_file ->
        let msg = Printf.sprintf "OCaml exception 'End_of_file'%s" exn_name in
        raise (Py.Err (Py.Err.IOError, msg))
    | Match_failure (filename, line, column) ->
        let msg =
          Printf.sprintf
            "OCaml exception 'Match_faiure file=%s line=%d(c. %d)'%s"
            filename line column exn_name in
        raise (Py.Err (Py.Err.StandardError, msg))
    | Assert_failure (filename, line, column) ->
        let msg =
          Printf.sprintf
            "OCaml exception 'Assert_faiure file=%s line=%d(c. %d)'%s"
            filename line column exn_name in
        raise (Py.Err (Py.Err.StandardError, msg))
    | Py.E (_, _) | Py.Err (_, _) as e -> raise e
    | something_else when catch_weird_exceptions ->
        let msg =
          Printf.sprintf "OCaml weird low-level exception '%s'%s"
            (Printexc.to_string something_else) exn_name in
        raise (Py.Err (Py.Err.StandardError, msg)) in
  let closure'' =
    match name with
      Some s when !py_profiling_active ->
        let closure'' args =
          let t0 = Unix.gettimeofday () in
          let stop_timer () =
            let t1 = Unix.gettimeofday () in
            let time_and_calls =
              let py_profile_hash' = Lazy.force py_profile_hash in
              try
                Hashtbl.find py_profile_hash' s
              with Not_found ->
                let x = [| 0.; 0. |] in
                Hashtbl.add py_profile_hash' s x;
                x in
            time_and_calls.(0) <- time_and_calls.(0) +. t1 -. t0;
            time_and_calls.(1) <- time_and_calls.(1) +. 1. in
          try
            let result = closure' args in
            stop_timer ();
            result
          with e ->
            stop_timer ();
            raise e in
        closure''
    | _ -> closure' in
  Py.Callable.of_function_as_tuple ?docstring:doc closure''

let python_pre_interfaced_function ?catch_weird_exceptions ?doc ?extra_guards
    wanted_types function_body name =
  python_interfaced_function ~name ?catch_weird_exceptions ?doc ?extra_guards
    wanted_types function_body

let pythonize_string = Py.String.of_string

let unpythonize_string = Py.String.to_string

let py_homogeneous_list_as_array ?error_label ?length type_name type_checker
    unwrapper list =
  let the_error_label =
    match error_label with
      None -> ""
    | Some x -> Printf.sprintf "%s: " x in
  let list_size = Py.List.size list in
  let () =
    match length with
      None -> ()
    | Some length' ->
        if list_size <> length' then
          raise
            (Pycaml_exn
               (Pyerr_TypeError,
                (Printf.sprintf "%sExpected list of length %d, got length: %d"
                   the_error_label length' list_size))) in
  for i = 0 to list_size - 1 do
    let item = Py.List.get list i in
    if not (type_checker item) then
      raise
        (Pycaml_exn
           (Pyerr_TypeError,
            Printf.sprintf
              "%sExpected homogeneous list of %s. Entry %d is of type %s (%s)!"
              the_error_label type_name (1 + i)
              (Py.Type.name (Py.Type.get item))
              (Py.Object.string_of_repr item)))
  done;
  Py.List.to_array_map unwrapper list

let py_float_list_as_array ?error_label ?length arr =
  py_homogeneous_list_as_array
    ?error_label ?length
    "float" Py.Float.check pyfloat_asdouble arr

let py_int_list_as_array ?error_label ?length arr =
  py_homogeneous_list_as_array
    ?error_label ?length
    "int" Py.Long.check pyint_asint arr

let py_number_list_as_float_array ?error_label ?length arr =
  py_homogeneous_list_as_array
    ?error_label ?length
    "number" Py.Number.check Py.Number.to_float arr

let py_string_list_as_array ?error_label ?length arr =
  py_homogeneous_list_as_array
    ?error_label ?length
    "string" Py.String.check Py.String.to_string arr

let py_list_list_as_array_map ?error_label ?length map arr =
  py_homogeneous_list_as_array
    ?error_label ?length
    "<Python List>" Py.List.check map arr

let py_list_list_as_array ?error_label ?length arr =
  py_list_list_as_array_map ?error_label ?length (fun x -> x) arr

let py_list_list_as_array2 ?error_label ?length arr =
  py_list_list_as_array_map ?error_label ?length Py.List.to_array arr

let py_float_list_list_as_array ?error_label ?length_outer ?length_inner arr =
  py_list_list_as_array_map ?error_label ?length:length_outer
    (py_float_list_as_array ?error_label ?length:length_inner)
    arr

let py_number_list_list_as_float_array ?error_label ?length_outer
    ?length_inner arr =
  py_list_list_as_array_map ?error_label ?length:length_outer
    (py_number_list_as_float_array ?error_label ?length:length_inner)
    arr

let py_int_list_list_as_array ?error_label ?length_outer ?length_inner arr =
  py_list_list_as_array_map ?error_label ?length:length_outer
    (py_int_list_as_array ?error_label ?length:length_inner)
    arr

let py_string_list_list_as_array ?error_label ?length_outer ?length_inner arr =
  py_list_list_as_array_map ?error_label ?length:length_outer
    (py_string_list_as_array ?error_label ?length:length_inner)
    arr

let py_float_tensor ?(init=(fun _ -> 0.0)) index_ranges =
  let nr_indices = Array.length index_ranges in
  let v_indices = Array.make nr_indices 0 in
  if nr_indices = 0 then
    (pyfloat_fromdouble (init v_indices),
     fun _ -> failwith "Cannot set rank-0 python float tensor!")
  else
    let rec build pos =
      let range = index_ranges.(pos) in
      Py.List.init range
        (fun ix_here ->
          let () = v_indices.(pos) <- ix_here in
          if pos = nr_indices-1 then
            pyfloat_fromdouble (init v_indices)
          else
            build (succ pos)) in
    let structure = build 0 in
    let setter indices value =
      let rec walk sub_structure pos =
        let i = indices.(pos) in
        if pos = nr_indices-1 then
          Py.List.set sub_structure i value
        else
          walk (Py.List.get sub_structure i) (succ pos) in
      walk structure 0 in
    (structure,setter)

let int_array_to_python = Py.List.of_array_map Py.Long.of_int

let float_array_to_python = Py.List.of_array_map Py.Float.of_float

let register_for_python stuff =
  let ocaml_module = Py.Import.add_module "ocaml" in
  let register (python_name, value) =
    Py.Object.set_attr_string ocaml_module python_name value in
  Array.iter register stuff

let register_pre_functions_for_python stuff =
  let prepare (python_name, pre_fun) = (python_name, pre_fun python_name) in
  register_for_python (Array.map prepare stuff)

let python_last_value = Py.last_value

let pywrap_closure_docstring docstring f =
  Py.Callable.of_function_as_tuple ~docstring f

let pyrefcount = Py.Object.reference_count

let pylist_get = Py.List.get

let pylist_set = Py.List.set

let pylist_fromarray = Py.List.of_array

let py_repr obj = Py.Object.string_of_repr obj

let pyunwrap_value = Py.Capsule.unsafe_unwrap_value

let pywrap_value = Py.Capsule.unsafe_wrap_value

type funcptr
type funcent = funcptr * int * int * bool

type pymodule_func = {
  pyml_name : string ;
  pyml_func : (pyobject -> pyobject) ;
  pyml_flags : int ;
  pyml_doc : string;
}
