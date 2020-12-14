(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let tt = ToplUtils.debug

module Vname = struct
  module T = struct
    type t = ToplAst.property_name * ToplAst.vertex [@@deriving compare, hash, sexp]
  end

  include T
  include Hashable.Make (T)
end

type vname = Vname.t

type vindex = int [@@deriving compare]

type tindex = int

type transition = {source: vindex; target: vindex; label: ToplAst.label option}

(** - INV1: Array.length states = Array.length outgoing = Array.length nondets
    - INV2: Array.length transitions = Array.length skips
    - INV3: each index of [transitions] occurs exactly once in one of [outgoing]'s lists
    - INV4: max_args is the maximum length of the arguments list in a label on a transition

    The fields marked as redundant are computed from the others (when the automaton is built), and
    are cached for speed. *)
type t =
  { states: vname array
  ; nondets: bool array (* redundant *)
  ; transitions: transition array
  ; skips: bool array (* redundant *)
  ; outgoing: tindex list array
  ; vindex: vname -> vindex
  ; max_args: int (* redundant *) }

(** [index_in H a] returns a pair of functions [(opt, err)] that lookup the (last) index of an
    element in [a]. The difference is that [opt x] returns an option, while [err msg x] makes Infer
    die, mentioning [msg].*)
let index_in (type k) (module H : Hashtbl_intf.S with type key = k) (a : k array) :
    (k -> int option) * (string -> k -> int) =
  let h = H.create ~size:(2 * Array.length a) () in
  let f i x = H.set h ~key:x ~data:i in
  Array.iteri ~f a ;
  let opt = H.find h in
  let err msg x =
    match opt x with
    | Some x ->
        x
    | None ->
        L.die InternalError "ToplAutomaton.index_in out of bounds (%s)" msg
  in
  (opt, err)


let make properties =
  let states : vname array =
    let open ToplAst in
    let f p =
      let f {source; target; _} = [(p.name, source); (p.name, target)] in
      List.concat_map ~f p.transitions
    in
    Array.of_list (List.dedup_and_sort ~compare:Vname.compare (List.concat_map ~f properties))
  in
  Array.iteri ~f:(fun i (p, v) -> tt "state[%d]=(%s,%s)@\n" i p v) states ;
  let vindex_opt, vindex = index_in (module Vname.Table) states in
  let vindex = vindex "vertex" in
  let transitions : transition array =
    let f p =
      let prefix_pname pname =
        if String.equal ".*" pname then pname
        else
          let ps = List.map ~f:(fun p -> "\\|" ^ p ^ "\\.") p.ToplAst.prefixes in
          "^\\(" ^ String.concat ps ^ "\\)" ^ pname ^ "("
      in
      let prefix_pattern =
        ToplAst.(
          function
          | ProcedureNamePattern pname -> ProcedureNamePattern (prefix_pname pname) | p -> p)
      in
      let prefix_label label = ToplAst.{label with pattern= prefix_pattern label.pattern} in
      let f t =
        let source = vindex ToplAst.(p.name, t.source) in
        let target = vindex ToplAst.(p.name, t.target) in
        let label = Option.map ~f:prefix_label t.ToplAst.label in
        {source; target; label}
      in
      List.map ~f p.ToplAst.transitions
    in
    Array.of_list (List.concat_map ~f properties)
  in
  Array.iteri transitions ~f:(fun i {source; target; label} ->
      tt "transition%d %d -> %d on %a@\n" i source target ToplAstOps.pp_label label ) ;
  let outgoing : tindex list array =
    let vcount = Array.length states in
    let a = Array.create ~len:vcount [] in
    let f i t = a.(t.source) <- i :: a.(t.source) in
    Array.iteri ~f transitions ;
    a
  in
  let max_args =
    let llen l = Option.value_map ~default:0 ~f:List.length l.ToplAst.arguments in
    let tlen t = Option.value_map ~default:0 ~f:llen t.label in
    transitions |> Array.map ~f:tlen
    |> Array.max_elt ~compare:Int.compare
    |> Option.value ~default:0 |> succ
  in
  let nondets : bool array =
    let vcount = Array.length states in
    let a = Array.create ~len:vcount false in
    let f ToplAst.{nondet; name; _} =
      let set_nondet state =
        match vindex_opt (name, state) with
        | Some i ->
            a.(i) <- true
        | None ->
            L.user_warning
              "TOPL: %s declared as nondet, but it appears in no transition of property %s" state
              name
      in
      List.iter ~f:set_nondet nondet
    in
    List.iter ~f properties ;
    a
  in
  let skips : bool array =
    (* TODO(rgrigore): Rename "anys"? *)
    let is_skip {label} = Option.is_none label in
    Array.map ~f:is_skip transitions
  in
  {states; nondets; transitions; skips; outgoing; vindex; max_args}


let outgoing a i = a.outgoing.(i)

let vname a i = a.states.(i)

let is_nondet a i = a.nondets.(i)

let vcount a = Array.length a.states

let transition a i = a.transitions.(i)

let is_skip a i = a.skips.(i)

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


let registers a =
  (* TODO(rgrigore): cache *)
  let do_assignment acc (r, _v) = String.Set.add acc r in
  let do_action acc = List.fold ~init:acc ~f:do_assignment in
  let do_value acc = ToplAst.(function Register r -> String.Set.add acc r | _ -> acc) in
  let do_predicate acc =
    ToplAst.(function Binop (_op, l, r) -> do_value (do_value acc l) r | _ -> acc)
  in
  let do_condition acc = List.fold ~init:acc ~f:do_predicate in
  let do_label acc {ToplAst.action; condition} = do_action (do_condition acc condition) action in
  let do_label_opt acc = Option.fold ~init:acc ~f:do_label in
  let do_transition acc {label} = do_label_opt acc label in
  String.Set.to_list (Array.fold ~init:String.Set.empty ~f:do_transition a.transitions)


let pp_message_of_state fmt (a, i) =
  let property, state = vname a i in
  Format.fprintf fmt "property %s reaches state %s" property state


let tfilter_map a ~f = Array.to_list (Array.filter_map ~f a.transitions)

let pp_transition f {source; target; label} =
  Format.fprintf f "@[%d -> %d:@,%a@]" source target ToplAstOps.pp_label label


let has_name n a i =
  let _property, name = vname a i in
  String.equal name n


let is_start = has_name "start"

let is_error = has_name "error"
