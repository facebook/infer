(*
* Copyright (c) 2014 - Facebook. All rights reserved.
*)

open Utils

module L = Logging

type t

module ConstantMap = Map.Make(struct
  type t = string
  let compare = string_compare
end)

let string_widening_limit = 1000
let verbose = false

(* Merge two constant maps by adding keys as necessary *)
let merge_values key c1_opt c2_opt =
  match c1_opt, c2_opt with
  | Some (Some c1), Some (Some c2) when Sil.const_equal c1 c2 -> Some (Some c1)
  | Some c, None
  | None, Some c -> Some c
  | _ -> Some None

(** Dataflow struct *)
module ConstantFlow = Dataflow.MakeDF(struct
  type t = (Sil.const option) ConstantMap.t

  let pp fmt constants =
    let print_kv k = function
      | Some v -> Format.fprintf fmt "  %s -> %a@." k (Sil.pp_const pe_text) v
      | _ -> Format.fprintf fmt "  %s -> None@." k in
    Format.fprintf fmt "[@.";
    ConstantMap.iter print_kv constants;
    Format.fprintf fmt "]@."

  (* Item-wise equality where values are equal iff
    - both are None
    - both are a constant and equal wrt. Sil.const_equal *)
  let equal m n = ConstantMap.equal (opt_equal Sil.const_equal) m n

  let join = ConstantMap.merge merge_values

  let proc_throws pn = Dataflow.DontKnow

  let do_node node constants =

    let do_instr constants instr =
      try
        let update key value constants =
          ConstantMap.merge
            merge_values
            constants
            (ConstantMap.add key value ConstantMap.empty) in

        match instr with
        | Sil.Letderef (i, Sil.Lvar p, _, _) ->        (* tmp = var *)
            let lvar = Ident.to_string i in
            let rvar = Sil.pvar_to_string p in
            update lvar (ConstantMap.find rvar constants) constants

        | Sil.Set (Sil.Lvar p, _, Sil.Const c, _) ->   (* var = const *)
            update (Sil.pvar_to_string p) (Some c) constants

        | Sil.Set (Sil.Lvar p, _, Sil.Var i, _) ->     (* var = tmp *)
            let lvar = Sil.pvar_to_string p in
            let rvar = Ident.to_string i in
            update lvar (ConstantMap.find rvar constants) constants

        (* Handle propagation of string with StringBuilder. Does not handle null case *)
        | Sil.Call (_, Sil.Const (Sil.Cfun pn), (Sil.Var sb, _):: [], _, _)
        when Procname.java_get_class pn = "java.lang.StringBuilder"
        && Procname.java_get_method pn = "<init>" ->  (* StringBuilder.<init> *)
            update (Ident.to_string sb) (Some (Sil.Cstr "")) constants

        | Sil.Call (i:: [], Sil.Const (Sil.Cfun pn), (Sil.Var i1, _):: [], _, _)
        when Procname.java_get_class pn = "java.lang.StringBuilder"
        && Procname.java_get_method pn = "toString" -> (* StringBuilder.toString *)
            let lvar = Ident.to_string i in
            let rvar = Ident.to_string i1 in
            update lvar (ConstantMap.find rvar constants) constants

        | Sil.Call (i:: [], Sil.Const (Sil.Cfun pn), (Sil.Var i1, _):: (Sil.Var i2, _):: [], _, _)
        when Procname.java_get_class pn = "java.lang.StringBuilder"
        && Procname.java_get_method pn = "append" -> (* StringBuilder.append *)
            let lvar = Ident.to_string i in
            let rvar1 = Ident.to_string i1 in
            let rvar2 = Ident.to_string i2 in
            (match ConstantMap.find rvar1 constants, ConstantMap.find rvar2 constants with
            | Some (Sil.Cstr s1), Some (Sil.Cstr s2) ->
              begin
                let s = s1 ^ s2 in
                let u =
                  if String.length s < string_widening_limit then
                    Some (Sil.Cstr s)
                  else
                    None in
                update lvar u constants
              end
            | _ -> constants)

        | _ -> constants
      with Not_found -> constants in

      if verbose then
        begin
          L.stdout "Node %i:" (Cfg.Node.get_id node);
          L.stdout "%a" pp constants;
          list_iter
            (fun instr -> L.stdout "%a@." (Sil.pp_instr pe_text) instr)
            (Cfg.Node.get_instrs node)
        end;
      let constants =
        list_fold_left
          do_instr
          constants
          (Cfg.Node.get_instrs node) in
      if verbose then L.stdout "%a\n@." pp constants;
      [constants], [constants]
end)

let run proc_desc =
  let transitions = ConstantFlow.run proc_desc ConstantMap.empty in
  let get_constants node =
    match transitions node with
    | ConstantFlow.Transition (_, post_states, _) -> ConstantFlow.join post_states ConstantMap.empty
    | ConstantFlow.Dead_state -> ConstantMap.empty in
  get_constants
