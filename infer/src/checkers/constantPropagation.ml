(*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module L = Logging


let string_widening_limit = 1000
let verbose = false

(* Merge two constant maps by adding keys as necessary *)
let merge_values _ c1_opt c2_opt =
  match c1_opt, c2_opt with
  | Some (Some c1), Some (Some c2) when Sil.const_equal c1 c2 -> Some (Some c1)
  | Some c, None
  | None, Some c -> Some c
  | _ -> Some None

module ConstantMap = Sil.ExpMap

(** Dataflow struct *)
module ConstantFlow = Dataflow.MakeDF(struct
    type t = (Sil.const option) ConstantMap.t

    let pp fmt constants =
      let pp_key fmt = Sil.pp_exp pe_text fmt in
      let print_kv k = function
        | Some v -> Format.fprintf fmt "  %a -> %a@." pp_key k (Sil.pp_const pe_text) v
        | _ -> Format.fprintf fmt "  %a -> None@." pp_key k in
      Format.fprintf fmt "[@.";
      ConstantMap.iter print_kv constants;
      Format.fprintf fmt "]@."

    (* Item - wise equality where values are equal iff
       - both are None
       - both are a constant and equal wrt. Sil.const_equal *)
    let equal m n = ConstantMap.equal (opt_equal Sil.const_equal) m n

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
            | Procname.Java pn_java ->
                Procname.java_get_class_name pn_java = name
            | _ ->
                false in
          let has_method pn name = match pn with
            | Procname.Java pn_java ->
                Procname.java_get_method pn_java = name
            | _ ->
                false in

          match instr with
          | Sil.Letderef (i, Sil.Lvar p, _, _) ->        (* tmp = var *)
              update (Sil.Var i) (ConstantMap.find (Sil.Lvar p) constants) constants

          | Sil.Set (Sil.Lvar p, _, Sil.Const c, _) ->   (* var = const *)
              update (Sil.Lvar p) (Some c) constants

          | Sil.Set (Sil.Lvar p, _, Sil.Var i, _) ->     (* var = tmp *)
              update (Sil.Lvar p) (ConstantMap.find (Sil.Var i) constants) constants

          (* Handle propagation of string with StringBuilder. Does not handle null case *)
          | Sil.Call (_, Sil.Const (Sil.Cfun pn), (Sil.Var sb, _):: [], _, _)
            when has_class pn "java.lang.StringBuilder"
              && has_method pn "<init>" ->  (* StringBuilder.<init> *)
              update (Sil.Var sb) (Some (Sil.Cstr "")) constants

          | Sil.Call (i:: [], Sil.Const (Sil.Cfun pn), (Sil.Var i1, _):: [], _, _)
            when has_class pn "java.lang.StringBuilder"
              && has_method pn "toString" -> (* StringBuilder.toString *)
              update (Sil.Var i) (ConstantMap.find (Sil.Var i1) constants) constants

          | Sil.Call (i:: [], Sil.Const (Sil.Cfun pn), (Sil.Var i1, _):: (Sil.Var i2, _):: [], _, _)
            when has_class pn "java.lang.StringBuilder"
              && has_method pn "append" -> (* StringBuilder.append *)
              (match
                 ConstantMap.find (Sil.Var i1) constants,
                 ConstantMap.find (Sil.Var i2) constants with
              | Some (Sil.Cstr s1), Some (Sil.Cstr s2) ->
                  begin
                    let s = s1 ^ s2 in
                    let u =
                      if String.length s < string_widening_limit then
                        Some (Sil.Cstr s)
                      else
                        None in
                    update (Sil.Var i) u constants
                  end
              | _ -> constants)

          | _ -> constants
        with Not_found -> constants in

      if verbose then
        begin
          L.stdout "Node %i:" (Cfg.Node.get_id node :> int);
          L.stdout "%a" pp constants;
          IList.iter
            (fun instr -> L.stdout "%a@." (Sil.pp_instr pe_text) instr)
            (Cfg.Node.get_instrs node)
        end;
      let constants =
        IList.fold_left
          do_instr
          constants
          (Cfg.Node.get_instrs node) in
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

type const_map = Cfg.Node.t -> Sil.exp -> Sil.const option

(** Build a const map lazily. *)
let build_const_map tenv pdesc =
  let const_map = lazy (run tenv pdesc) in
  let f node exp =
    try
      let map = (Lazy.force const_map) node in
      ConstantMap.find exp map
    with Not_found -> None in
  f
