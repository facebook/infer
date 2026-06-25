external pyarray_of_bigarray: Py.Object.t -> Py.Object.t
  -> ('a, 'b, 'c) Bigarray.Genarray.t
  -> Py.Object.t = "pyarray_of_bigarray_wrapper"

external bigarray_of_pyarray: Py.Object.t -> Py.Object.t
  -> ('a, 'b) Bigarray.kind * 'c Bigarray.layout * ('a, 'b, 'c) Bigarray.Genarray.t
  = "bigarray_of_pyarray_wrapper"

let pyarray_subtype_ref = ref None

let () = Py.on_finalize (fun () -> pyarray_subtype_ref := None)

let pyarray_subtype () =
  match !pyarray_subtype_ref with
    Some pyarray_subtype -> pyarray_subtype
  | None ->
     let pyarray_type = Py.Array.pyarray_type () in
     let pyarray_subtype =
       Py.Type.create "ocamlbigarray" [pyarray_type]
         [("ocamlbigarray", Py.none)] in
     pyarray_subtype_ref := Some pyarray_subtype;
     pyarray_subtype

let of_bigarray bigarray =
  let result =
    pyarray_of_bigarray (Py.Array.numpy_api ()) (pyarray_subtype ()) bigarray in
  let result = Py.check_not_null result in
  let capsule = Py.Capsule.unsafe_wrap_value bigarray in
  Py.Object.set_attr_string result "ocamlbigarray" capsule;
  result

(* written with equalities to support OCaml pre-GADT *)
let string_of_kind kind =
  if kind = Obj.magic Bigarray.float32 then
    "float32/NPY_FLOAT"
  else if kind = Obj.magic Bigarray.float64 then
    "float64/NPY_DOUBLE"
  else if kind = Obj.magic Bigarray.int8_signed then
    "int8_signed/NPY_BYTE"
  else if kind = Obj.magic Bigarray.int8_unsigned then
    "int8_unsigned/NPY_UBYTE"
  else if kind = Obj.magic Bigarray.int16_signed then
    "int16_signed/NPY_SHORT"
  else if kind = Obj.magic Bigarray.int16_unsigned then
    "int16_unsigned/NPY_USHORT"
  else if kind = Obj.magic Bigarray.int32 then
    "int32/NPY_INT"
  else if kind = Obj.magic Bigarray.int64 then
    "int64/NPY_LONGLONG"
  else if kind = Obj.magic Bigarray.int then
    "int"
  else if kind = Obj.magic Bigarray.nativeint then
    "nativeint/NPY_LONG"
  else if kind = Obj.magic Bigarray.complex32 then
    "complex32/NPY_CFLOAT"
  else if kind = Obj.magic Bigarray.complex64 then
    "complex64/NPY_CDOUBLE"
  else if kind = Obj.magic Bigarray.char then
    "char/NPY_CHAR"
  else
    "unknown kind"

let string_of_layout layout =
  if layout = Obj.magic Bigarray.c_layout then
    "C"
  else if layout = Obj.magic Bigarray.fortran_layout then
    "Fortran"
  else
    "unknown layout"

let to_bigarray kind layout t =
  if not (Py.Object.is_instance t (Py.Array.pyarray_type ())) then
    invalid_arg "Numpy.to_bigarray";
  let kind', layout', array = bigarray_of_pyarray (Py.Array.numpy_api ()) t in
  if kind <> kind' then
    invalid_arg (Printf.sprintf
      "Numpy.to_bigarray: Numpy array has elements of kind %s, but to_bigarray expected %s"
      (string_of_kind kind') (string_of_kind kind));
  if layout <> layout' then
    invalid_arg (Printf.sprintf
      "Numpy.to_bigarray: Numpy array has %s layout, but to_bigarray expected %s"
      (string_of_layout layout') (string_of_layout layout));
  array

type ('a, 'b, 'c) to_bigarray =
  { kind : ('a, 'b) Bigarray.kind
  ; layout : 'c Bigarray.layout
  ; array : ('a, 'b, 'c) Bigarray.Genarray.t
  }

type 'r to_bigarray_k =
  { f : 'a 'b 'c . ('a, 'b, 'c) to_bigarray -> 'r }

let to_bigarray_k (k : 'r to_bigarray_k) t : 'r =
  if not (Py.Object.is_instance t (Py.Array.pyarray_type ())) then
    invalid_arg "Numpy.to_bigarray";
  let kind, layout, array = bigarray_of_pyarray (Py.Array.numpy_api ()) t in
  k.f { kind; layout; array }

external compare_kind :
  ('a, 'b) Bigarray.kind -> ('c, 'd) Bigarray.kind -> int = "%compare"

external compare_layout :
  'a Bigarray.layout -> 'b Bigarray.layout -> int = "%compare"

let check_kind_and_layout (kind : ('a, 'b) Bigarray.kind)
      (layout : 'c Bigarray.layout) t :
      ('a, 'b, 'c) Bigarray.Genarray.t option =
  if compare_kind kind (Bigarray.Genarray.kind t) = 0 &&
       compare_layout layout (Bigarray.Genarray.layout t) = 0 then
    Some (Obj.magic t)
  else
    None
