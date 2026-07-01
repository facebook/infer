(** OCaml Interface for Numpy. *)

(** Arrays are passed in place (without copy): Python and OCaml programs
    can change the contents of the array and the changes are visible in
    the other language. *)

(** The following table gives the correspondence between bigarray kinds
    and Numpy element types.
 {ul
 {li [float32] / [NPY_FLOAT]}
 {li [float64] / [NPY_DOUBLE]}
 {li [int8_signed] / [NPY_BYTE]}
 {li [int8_unsigned] / [NPY_UBYTE]}
 {li [int16_signed] / [NPY_SHORT]}
 {li [int16_unsigned] / [NPY_USHORT]}
 {li [int32] / [NPY_INT]}
 {li [int64] / [NPY_LONGLONG]}
 {li [nativeint] / [NPY_LONG]}
 {li [complex32] / [NPY_CFLOAT]}
 {li [complex64] / [NPY_CDOUBLE]}
 {li [char] / [NPY_CHAR]}}
 Other kinds/element types are not supported. In particular, OCaml
 integer kind, [int], has no equivalent type in Numpy. *)

val of_bigarray:
  ('a, 'b, 'c) Bigarray.Genarray.t -> Py.Object.t
(** [of_bigarray a] returns a Numpy array that shares the same contents
    than the OCaml array [a]. *)

val to_bigarray:
  ('a, 'b) Bigarray.kind -> 'c Bigarray.layout -> Py.Object.t ->
    ('a, 'b, 'c) Bigarray.Genarray.t
(** [to_bigarray kind layout a] returns a bigarray that shares the same
    contents than the Numpy array [a].
    If `kind` and/or `layout` are unknown, you may use {!val:to_bigarray_k}. *)

type ('a, 'b, 'c) to_bigarray =
  { kind : ('a, 'b) Bigarray.kind
  ; layout : 'c Bigarray.layout
  ; array : ('a, 'b, 'c) Bigarray.Genarray.t
  }

type 'r to_bigarray_k =
  { f : 'a 'b 'c . ('a, 'b, 'c) to_bigarray -> 'r }

val to_bigarray_k : 'r to_bigarray_k -> Py.Object.t -> 'r
(** [to_bigarray_k k a] calls [k.f] with the contents of the Numpy array [a].
    [k.f] has to be polymorphic in the kind and the layout of the bigarray:
    functions {!val:compare_kind}, {!val:compare_layout} and
    {!val:check_kind_and_layout} can be used to introspect the bigarray
    polymorphically. *)

val compare_kind : ('a, 'b) Bigarray.kind -> ('c, 'd) Bigarray.kind -> int
(** [compare_kind] provides a total order on {!val:Bigarray.kind}.
    As opposed to generic [compare] of OCaml standard libary,
    [compare_kind] is polymorphic in the kind of the bigarray. *)

val compare_layout : 'a Bigarray.layout -> 'b Bigarray.layout -> int
(** [compare_layout] provides a total order on {!val:Bigarray.layout}.
    As opposed to generic [compare] of OCaml standard libary,
    [compare_kind] is polymorphic in the layout of the bigarray. *)

val check_kind_and_layout :
  ('a, 'b) Bigarray.kind -> 'c Bigarray.layout ->
    ('d, 'e, 'f) Bigarray.Genarray.t ->
    ('a, 'b, 'c) Bigarray.Genarray.t option
(** [check_kind_and_layout kind layout a] returns [Some a] if [a] has the given
    [kind] and [layout] (that is to say, if we have the following type
    equalities, ['a = 'd], ['b = 'e] and ['c = 'f]).
    This function allows the callback of {!val:to_bigarray_k} to be polymorphic
    in the kind of the array. *)
