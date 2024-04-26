(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let tt fmt =
  let mode = if Config.trace_topl then Logging.Quiet else Logging.Verbose in
  Logging.debug Analysis mode "ToplTrace: " ;
  Logging.debug Analysis mode fmt


type pindex = int [@@deriving compare, hash, sexp]

type pname = ToplAst.property_name

module Vname = struct
  module T = struct
    type t = pindex * ToplAst.vertex [@@deriving compare, hash, sexp]
  end

  include T
  include Hashable.Make (T)
end

type vname = Vname.t

type vindex = int [@@deriving compare, equal]

type tindex = int

type transition = {source: vindex; target: vindex; label: ToplAst.label option}

(** - INV1: Array.length transitions = Array.length skips
    - INV2: each index of [transitions] occurs exactly once in one of [outgoing]'s lists

    The fields marked as redundant are computed from the others (when the automaton is built), and
    are cached for speed. *)
type t =
  { names: pname array
  ; pindex: pname -> pindex (* redundant *)
  ; messages: string array
  ; states: vname array
  ; transitions: transition array
  ; skips: bool array (* redundant *)
  ; outgoing: tindex list array
  ; vindex: vname -> vindex (* redundant *) }

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
  let names : pname array =
    let f {ToplAst.name} = name in
    Array.of_list (List.map ~f properties)
  in
  let _pindex_opt, pindex = index_in (module String.Table) names in
  let pindex = pindex "property name" in
  let messages : string array =
    let f {ToplAst.name; message} =
      match message with None -> Format.sprintf "property %s fails" name | Some m -> m
    in
    Array.of_list (List.map ~f properties)
  in
  let states : vname array =
    let f index {ToplAst.transitions} =
      let f {ToplAst.source; target; _} = [(index, source); (index, target)] in
      List.concat_map ~f transitions
    in
    Array.of_list (List.dedup_and_sort ~compare:Vname.compare (List.concat_mapi ~f properties))
  in
  Array.iteri ~f:(fun i (p, v) -> tt "state[%d]=(%d,%s)@\n" i p v) states ;
  let _vindex_opt, vindex = index_in (module Vname.Table) states in
  let vindex = vindex "vertex" in
  let transitions : transition array =
    let f pindex p =
      let prefix_pname ({ToplAst.re_text} as old_regex) =
        if String.equal ".*" re_text then old_regex
        else
          let ps = List.map ~f:(fun p -> "\\|" ^ p ^ ".") p.ToplAst.prefixes in
          ToplAst.mk_regex ("^\\(" ^ String.concat ps ^ "\\)" ^ re_text ^ "\\((\\|$\\)")
      in
      let prefix_pattern (label_pattern : ToplAst.label_pattern) =
        match label_pattern with
        | CallPattern call_pattern ->
            ToplAst.CallPattern
              { call_pattern with
                procedure_name_regex= prefix_pname call_pattern.procedure_name_regex }
        | _ ->
            label_pattern
      in
      let prefix_label label = ToplAst.{label with pattern= prefix_pattern label.pattern} in
      let f {ToplAst.source; target; label} =
        let source = vindex (pindex, source) in
        let target = vindex (pindex, target) in
        let label = Option.map ~f:prefix_label label in
        {source; target; label}
      in
      List.map ~f p.ToplAst.transitions
    in
    Array.of_list (List.concat_mapi ~f properties)
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
  let skips : bool array =
    (* TODO(rgrigore): Rename "anys"? *)
    let is_skip {label} = Option.is_none label in
    Array.map ~f:is_skip transitions
  in
  {names; pindex; messages; states; transitions; skips; outgoing; vindex}


let vname a i = a.states.(i)

let vcount a = Array.length a.states

let tcount a = Array.length a.transitions

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


let tfilter_mapi a ~f = Array.to_list (Array.filter_mapi ~f a.transitions)

let pp_vertex a f i =
  let pindex, vertex = vname a i in
  let {names} = a in
  Format.fprintf f "@[%s.%s[%d]@]" names.(pindex) vertex i


let pp_transition a f {source; target; label} =
  Format.fprintf f "@[<v2>%a -> %a:@,%a@]" (pp_vertex a) source (pp_vertex a) target
    ToplAstOps.pp_label label


let pp_tindex a f i = pp_transition a f a.transitions.(i)

let has_name n a i =
  let _property, name = vname a i in
  String.equal name n


let start_name = "start"

let error_name = "error"

let is_start = has_name start_name

let is_error = has_name error_name

let message a i =
  let {messages} = a in
  let pindex, _vertex = vname a i in
  messages.(pindex)
