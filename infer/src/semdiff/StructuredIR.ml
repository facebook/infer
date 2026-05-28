(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module T = Textual

type label = T.NodeName.t

type t =
  | Instrs of {label: label; instrs: T.Instr.t list}
  | Seq of t * t
  | Block of t
  | Loop of {label: label; body: t}
  | If of {label: label; bexp: T.BoolExp.t; then_: t; else_: t}
  | Branch of int
  | Return of {label: label; exp: T.Exp.t}
  | Throw of {label: label; exp: T.Exp.t}

let rec pp fmt = function
  | Instrs {instrs= []; _} ->
      ()
  | Instrs {instrs; _} ->
      List.iter instrs ~f:(fun instr ->
          F.fprintf fmt "%a@;" (T.Instr.pp ~show_location:false) instr )
  | Seq (a, b) ->
      pp fmt a ;
      pp fmt b
  | Block body ->
      F.fprintf fmt "@[<v 2>block@;%a@]@;" pp body
  | Loop {body; _} ->
      F.fprintf fmt "@[<v 2>loop@;%a@]@;" pp body
  | If {bexp; then_; else_; _} ->
      F.fprintf fmt "@[<v 2>if %a then@;%a@]@;" T.BoolExp.pp bexp pp then_ ;
      F.fprintf fmt "@[<v 2>else@;%a@]@;" pp else_
  | Branch depth ->
      F.fprintf fmt "branch %d@;" depth
  | Return {exp; _} ->
      F.fprintf fmt "ret %a@;" T.Exp.pp exp
  | Throw {exp; _} ->
      F.fprintf fmt "throw %a@;" T.Exp.pp exp


(* ---------- Structured IR → Textual CFG ---------- *)

let mk_node ~label ~instrs ~last =
  { T.Node.label
  ; ssa_parameters= []
  ; exn_succs= []
  ; last
  ; instrs
  ; last_loc= T.Location.Unknown
  ; label_loc= T.Location.Unknown }


let mk_jump label = T.Terminator.Jump [{label; ssa_args= []}]

let rec to_cfg_aux (sir : t) ~(exit : label) ~(loop_exits : label list) :
    T.Node.t list * T.NodeName.t =
  match sir with
  | Instrs {label; instrs} ->
      let node = mk_node ~label ~instrs ~last:(mk_jump exit) in
      ([node], label)
  | Seq (a, b) ->
      let b_nodes, b_start = to_cfg_aux b ~exit ~loop_exits in
      let a_nodes, a_start = to_cfg_aux a ~exit:b_start ~loop_exits in
      (a_nodes @ b_nodes, a_start)
  | Block body ->
      to_cfg_aux body ~exit ~loop_exits:(exit :: loop_exits)
  | Loop {label; body} ->
      let body_nodes, body_start = to_cfg_aux body ~exit ~loop_exits:(label :: loop_exits) in
      if T.NodeName.equal body_start label then (body_nodes, label)
      else
        let header = mk_node ~label ~instrs:[] ~last:(mk_jump body_start) in
        (header :: body_nodes, label)
  | If {label; bexp; then_; else_} ->
      let then_nodes, then_start = to_cfg_aux then_ ~exit ~loop_exits:(exit :: loop_exits) in
      let else_nodes, else_start = to_cfg_aux else_ ~exit ~loop_exits:(exit :: loop_exits) in
      let last = T.Terminator.If {bexp; then_= mk_jump then_start; else_= mk_jump else_start} in
      let node = mk_node ~label ~instrs:[] ~last in
      ((node :: then_nodes) @ else_nodes, label)
  | Branch depth -> (
    match List.nth loop_exits depth with
    | Some target ->
        ([], target)
    | None ->
        L.die InternalError "Invalid br depth" )
  | Return {label; exp} ->
      let node = mk_node ~label ~instrs:[] ~last:(T.Terminator.Ret exp) in
      ([node], label)
  | Throw {label; exp} ->
      let node = mk_node ~label ~instrs:[] ~last:(T.Terminator.Throw exp) in
      ([node], label)


let to_cfg sir =
  let exit = T.NodeName.of_string "exit" in
  to_cfg_aux sir ~exit ~loop_exits:[]


(* ---------- Graph utilities for CFG → Structured IR ---------- *)

let successor_labels term =
  let rec collect acc (term : T.Terminator.t) =
    match term with
    | Ret _ | Throw _ | Unreachable ->
        acc
    | Jump targets ->
        List.fold targets ~init:acc ~f:(fun acc {T.Terminator.label} -> label :: acc)
    | If {then_; else_; _} ->
        collect (collect acc then_) else_
  in
  collect [] term |> List.rev


let build_node_map nodes =
  List.fold nodes ~init:T.NodeName.Map.empty ~f:(fun acc (node : T.Node.t) ->
      T.NodeName.Map.add node.label node acc )


let build_pred_map nodes =
  List.fold nodes ~init:T.NodeName.Map.empty ~f:(fun acc (node : T.Node.t) ->
      List.fold (successor_labels node.last) ~init:acc ~f:(fun acc succ ->
          let existing = T.NodeName.Map.find_opt succ acc |> Option.value ~default:[] in
          T.NodeName.Map.add succ (node.label :: existing) acc ) )


let reverse_postorder nodes start =
  let node_map = build_node_map nodes in
  let visited = T.NodeName.HashSet.create 16 in
  let rec dfs acc label =
    if T.NodeName.HashSet.mem visited label then acc
    else (
      T.NodeName.HashSet.add label visited ;
      let acc =
        Option.fold (T.NodeName.Map.find_opt label node_map) ~init:acc
          ~f:(fun acc (node : T.Node.t) -> List.fold (successor_labels node.last) ~init:acc ~f:dfs )
      in
      label :: acc )
  in
  let rpo_list = dfs [] start in
  List.foldi rpo_list ~init:T.NodeName.Map.empty ~f:(fun i acc label ->
      T.NodeName.Map.add label i acc )


(* Cooper, Harvey & Kennedy, "A Simple, Fast Dominance Algorithm", 2001.
   Iterative fixpoint on RPO-sorted nodes. The entry node dominates itself. *)
let compute_idom nodes start =
  let rpo = reverse_postorder nodes start in
  let pred_map = build_pred_map nodes in
  (* all nodes except start, sorted by RPO *)
  let rpo_sorted =
    List.filter_map nodes ~f:(fun (node : T.Node.t) ->
        if T.NodeName.equal node.label start then None
        else if not (T.NodeName.Map.mem node.label rpo) then None
        else Some node.label )
    |> List.sort ~compare:(fun a b ->
           Int.compare (T.NodeName.Map.find a rpo) (T.NodeName.Map.find b rpo) )
  in
  let idom = ref (T.NodeName.Map.singleton start start) in
  (* walk up the dominator tree from b1 and b2 until they meet *)
  let intersect b1 b2 =
    let b1 = ref b1 and b2 = ref b2 in
    while not (T.NodeName.equal !b1 !b2) do
      while T.NodeName.Map.find !b1 rpo > T.NodeName.Map.find !b2 rpo do
        b1 := T.NodeName.Map.find !b1 !idom
      done ;
      while T.NodeName.Map.find !b2 rpo > T.NodeName.Map.find !b1 rpo do
        b2 := T.NodeName.Map.find !b2 !idom
      done
    done ;
    !b1
  in
  (* iterate until fixpoint *)
  let changed = ref true in
  while !changed do
    changed := false ;
    List.iter rpo_sorted ~f:(fun b ->
        let preds = T.NodeName.Map.find_opt b pred_map |> Option.value ~default:[] in
        let processed_preds = List.filter preds ~f:(fun p -> T.NodeName.Map.mem p !idom) in
        match processed_preds with
        | [] ->
            ()
        | first :: rest ->
            let new_idom = List.fold rest ~init:first ~f:(fun acc p -> intersect p acc) in
            let old_idom = T.NodeName.Map.find_opt b !idom in
            if not (Option.exists old_idom ~f:(T.NodeName.equal new_idom)) then (
              idom := T.NodeName.Map.add b new_idom !idom ;
              changed := true ) )
  done ;
  !idom


let dominator_children idom =
  T.NodeName.Map.fold
    (fun child parent acc ->
      if T.NodeName.equal child parent then acc
      else
        let existing = T.NodeName.Map.find_opt parent acc |> Option.value ~default:[] in
        T.NodeName.Map.add parent (child :: existing) acc )
    idom T.NodeName.Map.empty
