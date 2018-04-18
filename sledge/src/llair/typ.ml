(* Copyright (c) 2018 - present Facebook, Inc. All rights reserved.

   This source code is licensed under the BSD style license found in the
   LICENSE file in the root directory of this source tree. An additional
   grant of patent rights can be found in the PATENTS file in the same
   directory. *)

(** Types *)

type t =
  | Function of {return: t option; args: t vector}
  | Integer of {bits: int}
  | Float of {bits: int; enc: [`IEEE | `Extended | `Pair]}
  | Pointer of {elt: t}
  | Array of {elt: t; len: int}
  | Tuple of {elts: t vector; packed: bool}
  | Struct of
      { name: string
      ; elts: t vector [@compare.ignore] (* possibly cyclic, name unique *)
      ; packed: bool }
  | Opaque of {name: string}
  | Bytes
[@@deriving compare, sexp]

let equal x y = compare x y = 0

let rec fmt ff typ =
  match typ with
  | Function {return; args} ->
      Format.fprintf ff "@[%a@ (@[%a@])@]" (option_fmt "%a" fmt) return fmts
        args
  | Integer {bits} -> Format.fprintf ff "@[i%i@]" bits
  | Float {bits; enc} ->
      let fmt_enc ff = function
        | `IEEE -> ()
        | `Extended -> Format.pp_print_string ff "extend"
        | `Pair -> Format.pp_print_string ff "pair"
      in
      Format.fprintf ff "@[f%i%a@]" bits fmt_enc enc
  | Pointer {elt} -> Format.fprintf ff "@[%a*@]" fmt elt
  | Array {elt; len} -> Format.fprintf ff "@[[%i x %a]@]" len fmt elt
  | Tuple {elts; packed} ->
      let opn, cls = if packed then ("<{", "}>") else ("{", "}") in
      Format.fprintf ff "@[%s @[%a@] %s@]" opn fmts elts cls
  | Struct {name} | Opaque {name} -> Format.fprintf ff "@[%%%s@]" name
  | Bytes -> Format.fprintf ff "bytes"

and fmts ff typs = vector_fmt ",@ " fmt ff typs

let fmt_defn ff = function
  | Struct {name; elts; packed} ->
      let opn, cls = if packed then ("<{", "}>") else ("{", "}") in
      Format.fprintf ff "@[<2>%%%s =@ @[%s %a@] %s@]" name opn fmts elts cls
  | Opaque {name} -> Format.fprintf ff "@[<2>%%%s =@ opaque@]" name
  | typ -> fmt ff typ

let is_sized = function
  | Function _ | Bytes -> false
  | Integer _ | Float _ | Pointer _ | Array _ | Tuple _ | Struct _ -> true
  | Opaque _ ->
      (* This is optimisic since sizedness of Opaque types is indeterminate,
         as they are not sized but may become sized through linking. *)
      true

let rec prim_bit_size_of = function
  | Integer {bits} | Float {bits} -> Some bits
  | Array {elt; len} ->
      Option.map (prim_bit_size_of elt) ~f:(fun n -> n * len)
  | Opaque _ | Function _ | Pointer _ | Tuple _ | Struct _ | Bytes -> None

let rec compatible t0 t1 =
  match (t0, t1, prim_bit_size_of t0, prim_bit_size_of t1) with
  | ( (Integer _ | Float _ | Pointer _)
    , (Integer _ | Float _ | Pointer _)
    , _
    , _ ) ->
      true
  | Array {elt= t; len= m}, Array {elt= u; len= n}, _, _
    when m = n && compatible t u ->
      true
  | ( (Integer _ | Float _ | Pointer _ | Array _)
    , (Integer _ | Float _ | Pointer _ | Array _)
    , Some s0
    , Some s1 )
    when s0 = s1 ->
      true
  | _ -> false

let mkFunction ~return ~args =
  assert (
    Option.for_all ~f:is_sized return && Vector.for_all ~f:is_sized args ) ;
  Function {return; args}

let mkInteger ~bits = Integer {bits}

let mkFloat ~bits ~enc = Float {bits; enc}

let mkPointer ~elt = Pointer {elt}

let mkArray ~elt ~len =
  assert (is_sized elt) ;
  Array {elt; len}

let defns : (string, t) Hashtbl.t = Hashtbl.create (module String) ()

let mkTuple ~packed elts =
  assert (Vector.for_all ~f:is_sized elts) ;
  Tuple {elts; packed}

let mkStruct ~name ~packed elt_thks =
  match Hashtbl.find defns name with
  | Some typ -> typ
  | None ->
      (* Add placeholder defn to prevent computing [elts] in calls to
         [mkStruct] from [elts] for recursive occurrences of [name]. *)
      let elts =
        Array.create ~len:(Vector.length elt_thks) (mkInteger ~bits:0)
      in
      let typ = Struct {name; elts= Vector.of_array elts; packed} in
      Hashtbl.set defns ~key:name ~data:typ ;
      Vector.iteri elt_thks ~f:(fun i elt_thk ->
          let elt = Lazy.force elt_thk in
          assert (is_sized elt) ;
          elts.(i) <- elt ) ;
      typ

let mkOpaque ~name = Opaque {name}

let mkBytes = Bytes

let i1 = mkInteger ~bits:1

let i8p = mkPointer ~elt:(mkInteger ~bits:8)
