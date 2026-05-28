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
