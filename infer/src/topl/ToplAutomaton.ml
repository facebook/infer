(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let tt = ToplUtils.debug

module Vname = struct
  module T = struct
    type t = ToplAst.property_name * ToplAst.vertex [@@deriving compare, hash, sexp]
  end

  include T
  include Hashable.Make (T)
end

type vname = Vname.t

type vindex = int

type tindex = int

type transition = {source: vindex; target: vindex; label: ToplAst.label}

(** INV1: Array.length states = Array.length outgoing
    INV2: each index of [transitions] occurs exactly once in one of [outgoing]'s lists
    INV3: max_args is the maximum length of the arguments list in a label on a transition *)
type t =
  { states: vname array
  ; transitions: transition array
  ; outgoing: tindex list array
  ; vindex: vname -> vindex
  ; max_args: int (* redundant; cached for speed *) }

(** [index_in H a x] is the (last) index of [x] in array [a]. *)
let index_in (type k) (module H : Hashtbl_intf.S with type key = k) (a : k array) : k -> int =
  let h = H.create ~size:(2 * Array.length a) () in
  let f i x = H.set h ~key:x ~data:i in
  Array.iteri ~f a ; H.find_exn h


let make properties =
  let states : vname array =
    let open ToplAst in
    let f p =
      let f {source; target; _} = [(p.name, source); (p.name, target)] in
      List.concat_map ~f p.transitions
    in
    Array.of_list (List.dedup_and_sort ~compare:Vname.compare (List.concat_map ~f properties))
  in
  if Config.trace_topl then Array.iteri ~f:(fun i (p, v) -> tt "state[%d]=(%s,%s)@\n" i p v) states ;
  let vindex = index_in (module Vname.Table) states in
  let transitions : transition array =
    let f p =
      let prefix_pname pname =
        "^\\(\\|" ^ String.concat ~sep:"\\|" p.ToplAst.prefixes ^ "\\)\\." ^ pname ^ "("
      in
      let f t =
        let source = vindex ToplAst.(p.name, t.source) in
        let target = vindex ToplAst.(p.name, t.target) in
        let procedure_name = prefix_pname ToplAst.(t.label.procedure_name) in
        let label = {t.ToplAst.label with procedure_name} in
        {source; target; label}
      in
      List.map ~f p.ToplAst.transitions
    in
    Array.of_list (List.concat_map ~f properties)
  in
  if Config.trace_topl then
    Array.iteri transitions ~f:(fun i {source; target; label} ->
        tt "transition%d %d -> %d on %s@\n" i source target label.ToplAst.procedure_name ) ;
  let outgoing : tindex list array =
    let vcount = Array.length states in
    let a = Array.create ~len:vcount [] in
    let f i t = a.(t.source) <- i :: a.(t.source) in
    Array.iteri ~f transitions ; a
  in
  let max_args =
    let f x t =
      let y = Option.value_map ~default:0 ~f:List.length t.label.ToplAst.arguments in
      Int.max x y
    in
    Array.fold ~init:0 ~f transitions
  in
  {states; transitions; outgoing; vindex; max_args}


let outgoing a i = a.outgoing.(i)

let vname a i = a.states.(i)

let vcount a = Array.length a.states

let transition a i = a.transitions.(i)

let tcount a = Array.length a.transitions

let max_args a = a.max_args

let get_start_error_pairs a =
  let starts = String.Table.create () ~size:(2 * vcount a) in
  let errors = String.Table.create () ~size:(2 * vcount a) in
  let record dict keep index (property, name) =
    if String.equal keep name then Hashtbl.add_exn dict ~key:property ~data:index
  in
  Array.iteri ~f:(record starts "start") a.states ;
  Array.iteri ~f:(record errors "error") a.states ;
  let f ~key:_ = function `Both (x, y) -> Some (x, y) | _ -> None in
  let pairs = Hashtbl.merge starts errors ~f in
  Hashtbl.data pairs
