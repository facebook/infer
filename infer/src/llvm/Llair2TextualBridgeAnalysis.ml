(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Llair

let is_msgsend_callee name =
  String.is_substring name ~substring:"objc_msgSend"
  || String.is_substring name ~substring:"performSelector"


(* The two structural signals an [objc_msgSend]'s downstream CFG can carry,
   either of which lets the Llair->Textual translator recover a [Nullable]
   annotation on the call:

   - [as?]-cast re-bridge: some path from the seed reaches a
     [_bridgeToObjectiveC] call through the standard [Optional<T>] bridge +
     ARC chain. Source-level: [if let s = api.f() as? T { ... }].

   - Optional-passthrough getter: every path from the seed reaches a
     value-returning [Return] through the same transparent chain, AND at
     least one path traverses [_unconditionallyBridgeFromObjectiveC] (so the
     Optional really gets boxed). Source-level:
     [func g() -> T? { return api.f() }] or its trivial let-binding variant.

   The two are mutually exclusive: [_bridgeToObjectiveC] is non-transparent
   for the passthrough check (it means the Optional is being used further,
   not just returned), so a re-bridge match disqualifies passthrough on
   that path. *)

let is_bridge_to_objc_callee name = String.is_substring name ~substring:"bridgeToObjectiveC"

let is_bridge_from_objc_callee name =
  String.is_substring name ~substring:"BridgeFromObjectiveC"
  || String.is_substring name ~substring:"bridgeFromObjectiveC"


(* Calls the walker can pass through without losing either signal: ARC
   retain/release helpers that accompany the [Optional<T>] packaging. The
   bridge-from helpers above are tracked separately because the passthrough
   signal needs to know one was seen. *)
let is_arc_helper_callee name =
  String.is_substring name ~substring:"objc_retain"
  || String.is_substring name ~substring:"objc_release"
  || String.is_substring name ~substring:"swift_retain"
  || String.is_substring name ~substring:"swift_release"


let callee_name (callee : Llair.callee) =
  match callee with
  | Direct {func= {name; _}; _} ->
      Some (FuncName.name name)
  | Indirect _ | Intrinsic _ ->
      None


type outcome =
  { reaches_re_bridge: bool
        (* [_bridgeToObjectiveC] reachable on some path through transparent
           calls -- the [as?]-cast signature *)
  ; passthrough: bool option
        (* [Some saw_bridge_from] if every path from here reaches a
           value-returning [Return] through transparent calls only;
           [saw_bridge_from] is true iff at least one such path traversed
           [_unconditionallyBridgeFromObjectiveC]. [None] if any path stops
           at a non-transparent [Call], a void [Return], a [Throw], or
           similar -- i.e. the seed's result is not a clean Optional
           passthrough. *)
  }

let no_outcome = {reaches_re_bridge= false; passthrough= None}

let combine_passthrough p q =
  match (p, q) with None, _ | _, None -> None | Some a, Some b -> Some (a || b)


(* Single forward CFG walk from [start_block] computing both signals. *)
let classify_msgsend_destiny start_block =
  let visited = Hash_set.create (module String) in
  let rec visit (b : Llair.block) =
    if Hash_set.mem visited b.lbl then
      (* Loops contribute the safe identity to both fields: false to the
         existential bridge check, [Some false] (saw no bridge yet, but the
         path doesn't escape via this back-edge) to the passthrough check. *)
      {reaches_re_bridge= false; passthrough= Some false}
    else (
      Hash_set.add visited b.lbl ;
      match b.term with
      | Return {exp= Some _; _} ->
          {reaches_re_bridge= false; passthrough= Some false}
      | Return {exp= None; _} | Throw _ | Abort _ | Unreachable _ ->
          no_outcome
      | Call {callee; return; _} -> (
        match callee_name callee with
        | Some name when is_bridge_to_objc_callee name ->
            (* Re-bridge succeeds for [as?]-cast; disqualifies passthrough. *)
            {reaches_re_bridge= true; passthrough= None}
        | Some name when is_bridge_from_objc_callee name ->
            let r = visit return.dst in
            { reaches_re_bridge= r.reaches_re_bridge
            ; passthrough= (match r.passthrough with Some _ -> Some true | None -> None) }
        | Some name when is_arc_helper_callee name ->
            visit return.dst
        | _ ->
            no_outcome )
      | Switch {tbl; els; _} ->
          NS.IArray.fold tbl (visit els.dst) ~f:(fun (_, (jump : Llair.jump)) acc ->
              let r = visit jump.dst in
              { reaches_re_bridge= acc.reaches_re_bridge || r.reaches_re_bridge
              ; passthrough= combine_passthrough acc.passthrough r.passthrough } )
      | Iswitch {tbl; _} ->
          NS.IArray.fold tbl {reaches_re_bridge= false; passthrough= Some false}
            ~f:(fun (jump : Llair.jump) acc ->
              let r = visit jump.dst in
              { reaches_re_bridge= acc.reaches_re_bridge || r.reaches_re_bridge
              ; passthrough= combine_passthrough acc.passthrough r.passthrough } ) )
  in
  visit start_block


let has_recoverable_nullability outcome =
  outcome.reaches_re_bridge
  || match outcome.passthrough with Some saw_bridge_from -> saw_bridge_from | None -> false


let find_msgsends_with_recoverable_nullability (func : Llair.func) =
  let result = Hashtbl.create (module Int) in
  let visited = Hash_set.create (module String) in
  let rec visit_block (b : Llair.block) =
    if not (Hash_set.mem visited b.lbl) then (
      Hash_set.add visited b.lbl ;
      ( match b.term with
      | Call {callee; areturn= Some areturn; return; _} -> (
        match callee_name callee with
        | Some name when is_msgsend_callee name ->
            if has_recoverable_nullability (classify_msgsend_destiny return.dst) then
              Hashtbl.set result ~key:(Reg.id areturn) ~data:()
        | _ ->
            () )
      | _ ->
          () ) ;
      match b.term with
      | Switch {tbl; els; _} ->
          NS.IArray.iter tbl ~f:(fun (_, (jump : Llair.jump)) -> visit_block jump.dst) ;
          visit_block els.dst
      | Iswitch {tbl; _} ->
          NS.IArray.iter tbl ~f:(fun (jump : Llair.jump) -> visit_block jump.dst)
      | Call {return; _} ->
          visit_block return.dst
      | Return _ | Throw _ | Abort _ | Unreachable _ ->
          () )
  in
  visit_block func.entry ;
  result
