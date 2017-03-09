(*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module L = Logging


let string_widening_limit = 1000
let verbose = false

(* Merge two constant maps by adding keys as necessary *)
let merge_values _ c1_opt c2_opt =
  match c1_opt, c2_opt with
  | Some (Some c1), Some (Some c2) when Const.equal c1 c2 -> Some (Some c1)
  | Some c, None
  | None, Some c -> Some c
  | _ -> Some None

module ConstantMap = Exp.Map

(** Dataflow struct *)
module ConstantFlow = Dataflow.MakeDF(struct
    type t = (Const.t option) ConstantMap.t [@@deriving compare]

    let equal = [%compare.equal : t]

    let pp fmt constants =
      let pp_key fmt = Exp.pp fmt in
      let print_kv k = function
        | Some v -> Format.fprintf fmt "  %a -> %a@." pp_key k (Const.pp Pp.text) v
        | _ -> Format.fprintf fmt "  %a -> None@." pp_key k in
      Format.fprintf fmt "[@.";
      ConstantMap.iter print_kv constants;
      Format.fprintf fmt "]@."

    let join = ConstantMap.merge merge_values

    let proc_throws _ = Dataflow.DontKnow

    let do_node _ node constants =

      let do_instr constants instr =
        try
          let update key value constants =
            ConstantMap.merge
              merge_values
              constants
              (ConstantMap.add key value ConstantMap.empty) in

          let has_class pn name = match pn with
            | Typ.Procname.Java pn_java ->
                String.equal (Typ.Procname.java_get_class_name pn_java) name
            | _ ->
                false in
          let has_method pn name = match pn with
            | Typ.Procname.Java pn_java ->
                String.equal (Typ.Procname.java_get_method pn_java) name
            | _ ->
                false in

          match instr with
          | Sil.Load (i, Exp.Lvar p, _, _) ->        (* tmp = var *)
              update (Exp.Var i) (ConstantMap.find (Exp.Lvar p) constants) constants

          | Sil.Store (Exp.Lvar p, _, Exp.Const c, _) ->   (* var = const *)
              update (Exp.Lvar p) (Some c) constants

          | Sil.Store (Exp.Lvar p, _, Exp.Var i, _) ->     (* var = tmp *)
              update (Exp.Lvar p) (ConstantMap.find (Exp.Var i) constants) constants

          (* Handle propagation of string with StringBuilder. Does not handle null case *)
          | Sil.Call (_, Exp.Const (Const.Cfun pn), (Exp.Var sb, _):: [], _, _)
            when has_class pn "java.lang.StringBuilder"
              && has_method pn "<init>" ->  (* StringBuilder.<init> *)
              update (Exp.Var sb) (Some (Const.Cstr "")) constants

          | Sil.Call (Some (i, _), Exp.Const (Const.Cfun pn), (Exp.Var i1, _):: [], _, _)
            when has_class pn "java.lang.StringBuilder"
              && has_method pn "toString" -> (* StringBuilder.toString *)
              update (Exp.Var i) (ConstantMap.find (Exp.Var i1) constants) constants

          | Sil.Call
              (Some (i, _), Exp.Const (Const.Cfun pn), (Exp.Var i1, _):: (Exp.Var i2, _):: [], _, _)
            when has_class pn "java.lang.StringBuilder"
              && has_method pn "append" -> (* StringBuilder.append *)
              (match
                 ConstantMap.find (Exp.Var i1) constants,
                 ConstantMap.find (Exp.Var i2) constants with
              | Some (Const.Cstr s1), Some (Const.Cstr s2) ->
                  begin
                    let s = s1 ^ s2 in
                    let u =
                      if String.length s < string_widening_limit then
                        Some (Const.Cstr s)
                      else
                        None in
                    update (Exp.Var i) u constants
                  end
              | _ -> constants)

          | _ -> constants
        with Not_found -> constants in

      if verbose then
        begin
          L.stdout "Node %i:" (Procdesc.Node.get_id node :> int);
          L.stdout "%a" pp constants;
          List.iter
            ~f:(fun instr -> L.stdout "%a@." (Sil.pp_instr Pp.text) instr)
            (Procdesc.Node.get_instrs node)
        end;
      let constants =
        List.fold
          ~f:do_instr
          ~init:constants
          (Procdesc.Node.get_instrs node) in
      if verbose then L.stdout "%a\n@." pp constants;
      [constants], [constants]
  end)

let run tenv proc_desc =
  let transitions = ConstantFlow.run tenv proc_desc ConstantMap.empty in
  let get_constants node =
    match transitions node with
    | ConstantFlow.Transition (_, post_states, _) -> ConstantFlow.join post_states ConstantMap.empty
    | ConstantFlow.Dead_state -> ConstantMap.empty in
  get_constants

type const_map = Procdesc.Node.t -> Exp.t -> Const.t option

(** Build a const map lazily. *)
let build_const_map tenv pdesc =
  let const_map = lazy (run tenv pdesc) in
  let f node exp =
    try
      let map = (Lazy.force const_map) node in
      ConstantMap.find exp map
    with Not_found -> None in
  f
