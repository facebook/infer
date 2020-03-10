(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Types *)

type t =
  | Function of {return: t option; args: t vector}
  | Integer of {bits: int; byts: int}
  | Float of {bits: int; byts: int; enc: [`IEEE | `Extended | `Pair]}
  | Pointer of {elt: t}
  | Array of {elt: t; len: int; bits: int; byts: int}
  | Tuple of {elts: t vector; bits: int; byts: int; packed: bool}
  | Struct of
      { name: string
      ; elts: t vector (* possibly cyclic, name unique *)
            [@compare.ignore] [@equal.ignore] [@sexp_drop_if fun _ -> true]
      ; bits: int
      ; byts: int
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
let integer ~bits ~byts = Integer {bits; byts} |> check invariant
let float ~bits ~byts ~enc = Float {bits; byts; enc} |> check invariant
let pointer ~elt = Pointer {elt} |> check invariant

let array ~elt ~len ~bits ~byts =
  Array {elt; len; bits; byts} |> check invariant

let tuple elts ~bits ~byts ~packed =
  Tuple {elts; bits; byts; packed} |> check invariant

let opaque ~name = Opaque {name} |> check invariant

let struct_ =
  let defns : (string, t) Hashtbl.t = Hashtbl.create (module String) in
  let dummy_typ = Opaque {name= "dummy"} in
  fun ~name ~bits ~byts ~packed elt_thks ->
    match Hashtbl.find defns name with
    | Some typ -> typ
    | None ->
        (* Add placeholder defn to prevent computing [elts] in calls to
           [struct] from [elts] for recursive occurrences of [name]. *)
        let elts = Array.create ~len:(Vector.length elt_thks) dummy_typ in
        let typ =
          Struct {name; elts= Vector.of_array elts; bits; byts; packed}
        in
        Hashtbl.set defns ~key:name ~data:typ ;
        Vector.iteri elt_thks ~f:(fun i (lazy elt) -> elts.(i) <- elt) ;
        typ |> check invariant

(** Constants *)

let bool = integer ~bits:1 ~byts:1
let byt = integer ~bits:8 ~byts:1
let int = integer ~bits:32 ~byts:4
let siz = integer ~bits:64 ~byts:8

(** [ptr] is semantically equivalent to [siz], but has a distinct
    representation because the element type is important for [Global]s *)
let ptr = pointer ~elt:byt

(** Queries *)

let bit_size_of = function
  | (Function _ | Opaque _) as t ->
      fail "bit_size_of requires is_sized: %a" pp t ()
  | Integer {bits; _}
   |Float {bits; _}
   |Array {bits; _}
   |Tuple {bits; _}
   |Struct {bits; _} ->
      bits
  | Pointer _ -> 64

let size_of = function
  | (Function _ | Opaque _) as t ->
      fail "size_of requires is_sized: %a" pp t ()
  | Integer {byts; _}
   |Float {byts; _}
   |Array {byts; _}
   |Tuple {byts; _}
   |Struct {byts; _} ->
      byts
  | Pointer _ -> 8

let rec equivalent t0 t1 =
  match (t0, t1) with
  | (Pointer _ | Integer _), (Pointer _ | Integer _) ->
      bit_size_of t0 = bit_size_of t1
  | Array {elt= t; len= m}, Array {elt= u; len= n} ->
      m = n && equivalent t u
  | _ -> equal t0 t1

let castable t0 t1 =
  (is_sized t0 && is_sized t1 && bit_size_of t0 = bit_size_of t1)
  || equal t0 t1

let rec convertible t0 t1 =
  castable t0 t1
  ||
  match (t0, t1) with
  | (Integer _ | Float _ | Pointer _), (Integer _ | Float _ | Pointer _) ->
      true
  | Array {elt= t; len= m}, Array {elt= u; len= n} ->
      m = n && convertible t u
  | _ -> false
