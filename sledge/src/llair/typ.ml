(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
      ; elts: t vector (* possibly cyclic, name unique *)
            [@compare.ignore] [@equal.ignore] [@sexp_drop_if fun _ -> true]
      ; packed: bool }
  | Opaque of {name: string}
[@@deriving compare, equal, hash, sexp]

let rec pp fs typ =
  let pf pp =
    Format.pp_open_box fs 2 ;
    Format.kfprintf (fun fs -> Format.pp_close_box fs ()) fs pp
  in
  match typ with
  | Function {return; args} ->
      pf "%a@ (@[%a@])" (Option.pp "%a" pp) return pps args
  | Integer {bits} -> pf "i%i" bits
  | Float {bits; enc} ->
      let enc_str =
        match enc with
        | `IEEE -> ""
        | `Extended -> "extend"
        | `Pair -> "pair"
      in
      pf "f%i%s" bits enc_str
  | Pointer {elt} -> pf "%a*" pp elt
  | Array {elt; len} -> pf "[%i x %a]" len pp elt
  | Tuple {elts; packed} ->
      let opn, cls = if packed then ("<{", "}>") else ("{", "}") in
      pf "%s @[%a@] %s" opn pps elts cls
  | Struct {name} | Opaque {name} -> pf "%%%s" name

and pps fs typs = Vector.pp ",@ " pp fs typs

let pp_defn fs = function
  | Struct {name; elts; packed} ->
      let opn, cls = if packed then ("<{", "}>") else ("{", "}") in
      Format.fprintf fs "@[<2>%%%s =@ @[%s %a@] %s@]" name opn pps elts cls
  | Opaque {name} -> Format.fprintf fs "@[<2>%%%s =@ opaque@]" name
  | typ -> pp fs typ

(** Invariants *)

let is_sized = function
  | Function _ -> false
  | Integer _ | Float _ | Pointer _ | Array _ | Tuple _ | Struct _ -> true
  | Opaque _ -> (* optimistically assume linking will make it sized *) true

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t]
  @@ fun () ->
  match t with
  | Function {return; args} ->
      assert (Option.for_all ~f:is_sized return) ;
      assert (Vector.for_all ~f:is_sized args)
  | Array {elt} -> assert (is_sized elt)
  | Tuple {elts} | Struct {elts} -> assert (Vector.for_all ~f:is_sized elts)
  | Integer {bits} | Float {bits} -> assert (bits > 0)
  | Pointer _ | Opaque _ -> assert true

(** Constructors *)

let function_ ~return ~args = Function {return; args} |> check invariant
let integer ~bits = Integer {bits} |> check invariant
let float ~bits ~enc = Float {bits; enc} |> check invariant
let pointer ~elt = Pointer {elt} |> check invariant
let array ~elt ~len = Array {elt; len} |> check invariant
let tuple elts ~packed = Tuple {elts; packed} |> check invariant
let opaque ~name = Opaque {name} |> check invariant

let struct_ =
  let defns : (string, t) Hashtbl.t = Hashtbl.create (module String) in
  let dummy_typ = Opaque {name= "dummy"} in
  fun ~name ~packed elt_thks ->
    match Hashtbl.find defns name with
    | Some typ -> typ
    | None ->
        (* Add placeholder defn to prevent computing [elts] in calls to
           [struct] from [elts] for recursive occurrences of [name]. *)
        let elts = Array.create ~len:(Vector.length elt_thks) dummy_typ in
        let typ = Struct {name; elts= Vector.of_array elts; packed} in
        Hashtbl.set defns ~key:name ~data:typ ;
        Vector.iteri elt_thks ~f:(fun i (lazy elt) -> elts.(i) <- elt) ;
        typ |> check invariant

(** Constants *)

let bool = integer ~bits:1
let byt = integer ~bits:8
let int = integer ~bits:32
let siz = integer ~bits:64

(** [ptr] is semantically equivalent to [siz], but has a distinct
    representation because the element type is important for [Global]s *)
let ptr = pointer ~elt:byt

(** Queries *)

let rec prim_bit_size_of = function
  | Integer {bits} | Float {bits} -> Some bits
  | Pointer _ -> prim_bit_size_of siz
  | Array {elt; len} ->
      Option.map (prim_bit_size_of elt) ~f:(fun n -> n * len)
  | Function _ | Tuple _ | Struct _ | Opaque _ -> None

let rec equivalent t0 t1 =
  match (t0, t1) with
  | (Pointer _ | Integer _), (Pointer _ | Integer _) -> (
    match (prim_bit_size_of t0, prim_bit_size_of t1) with
    | Some n0, Some n1 -> n0 = n1
    | _ -> false )
  | Array {elt= t; len= m}, Array {elt= u; len= n} ->
      m = n && equivalent t u
  | _ -> equal t0 t1

let castable t0 t1 =
  match (t0, t1) with
  | ( (Pointer _ | Integer _ | Float _ | Array _)
    , (Pointer _ | Integer _ | Float _ | Array _) ) -> (
    match (prim_bit_size_of t0, prim_bit_size_of t1) with
    | Some n0, Some n1 -> n0 = n1
    | _ -> false )
  | _ -> equal t0 t1

let rec convertible t0 t1 =
  castable t0 t1
  ||
  match (t0, t1) with
  | (Integer _ | Float _ | Pointer _), (Integer _ | Float _ | Pointer _) ->
      true
  | Array {elt= t; len= m}, Array {elt= u; len= n} ->
      m = n && convertible t u
  | _ -> false
