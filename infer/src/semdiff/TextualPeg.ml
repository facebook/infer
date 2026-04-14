(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module CC = CongruenceClosureSolver
module T = Textual

(* ---------- Equations trace ---------- *)

module Equations = struct
  type entry = {name: string; atom: CC.Atom.t; origin: string}

  type t = {mutable entries: entry list}

  let empty () = {entries= []}

  let add t ~name ~atom ~origin = t.entries <- {name; atom; origin} :: t.entries

  let entries t = List.rev t.entries

  let pp cc fmt t =
    List.iter (entries t) ~f:(fun {name; atom; origin} ->
        F.fprintf fmt "@[<h>%-6s = %a  [%s]@]@." name (CC.pp_nested_term cc) atom origin )
end

(* ---------- Locals map ---------- *)

module StringMap = IString.Map

module LocalsMap = struct
  type t = CC.Atom.t StringMap.t

  let empty = StringMap.empty

end

(* ---------- Environment ---------- *)

module Env = struct
  type t =
    { cc: CC.t
    ; ident_map: CC.Atom.t T.Ident.Map.t
    ; locals: LocalsMap.t
    ; state: CC.Atom.t
    ; equations: Equations.t
    ; node_map: T.Node.t T.NodeName.Map.t }

  let init cc ~params =
    let equations = Equations.empty () in
    let locals =
      List.fold params ~init:LocalsMap.empty ~f:(fun acc name ->
          let atom = CC.mk_term cc (CC.mk_header cc ("@param:" ^ name)) [] in
          Equations.add equations ~name ~atom ~origin:"param" ;
          StringMap.add name atom acc )
    in
    let state = CC.mk_term cc (CC.mk_header cc "@state0") [] in
    {cc; ident_map= T.Ident.Map.empty; locals; state; equations; node_map= T.NodeName.Map.empty}


  let bind_ident t id atom =
    let name = F.asprintf "%a" T.Ident.pp id in
    Equations.add t.equations ~name ~atom ~origin:"let" ;
    {t with ident_map= T.Ident.Map.add id atom t.ident_map}


  let lookup_ident t id = T.Ident.Map.find_opt id t.ident_map

  let update_state t state = {t with state}
end

(* ---------- Helpers ---------- *)

let mk_const cc name = CC.mk_term cc (CC.mk_header cc name) []

let mk_term cc name args = CC.mk_term cc (CC.mk_header cc name) args

let qualified_procname_to_string proc = F.asprintf "%a" T.QualifiedProcName.pp proc

(* ---------- Expression conversion ---------- *)

let rec convert_exp (env : Env.t) (exp : T.Exp.t) : CC.Atom.t =
  let cc = env.cc in
  match exp with
  | Var id -> (
    match Env.lookup_ident env id with
    | Some atom ->
        atom
    | None ->
        (* unknown ident -create a fresh named atom *)
        mk_const cc (F.asprintf "?%a" T.Ident.pp id) )
  | Lvar vname ->
      mk_term cc "@lvar" [mk_const cc (T.VarName.to_string vname)]
  | Const (Int z) ->
      mk_const cc (Z.to_string z)
  | Const Null ->
      mk_const cc "@null"
  | Const (Str s) ->
      mk_term cc "@str" [mk_const cc s]
  | Const (Float f) ->
      mk_const cc (Float.to_string f)
  | Load {exp= inner_exp} ->
      mk_term cc "@load" [convert_exp env inner_exp]
  | Field {exp= inner_exp; field} ->
      let field_name = T.FieldName.to_string field.name in
      mk_term cc "@field" [convert_exp env inner_exp; mk_const cc field_name]
  | Index (e1, e2) ->
      mk_term cc "@index" [convert_exp env e1; convert_exp env e2]
  | Call {proc; args} ->
      let header = qualified_procname_to_string proc in
      mk_term cc header (List.map args ~f:(convert_exp env))
  | Closure {proc; captured} ->
      let name = qualified_procname_to_string proc in
      mk_term cc "@closure" (mk_const cc name :: List.map captured ~f:(convert_exp env))
  | Apply {closure; args} ->
      mk_term cc "@apply" (convert_exp env closure :: List.map args ~f:(convert_exp env))
  | If {cond; then_; else_} ->
      let c = convert_boolexp env cond in
      let t = convert_exp env then_ in
      let e = convert_exp env else_ in
      mk_term cc "@phi" [c; t; e]
  | Typ _ ->
      mk_const cc "@typ"


and convert_boolexp (env : Env.t) (bexp : T.BoolExp.t) : CC.Atom.t =
  let cc = env.cc in
  match bexp with
  | Exp e ->
      convert_exp env e
  | Not b ->
      mk_term cc "@not" [convert_boolexp env b]
  | And (b1, b2) ->
      mk_term cc "@and" [convert_boolexp env b1; convert_boolexp env b2]
  | Or (b1, b2) ->
      mk_term cc "@or" [convert_boolexp env b1; convert_boolexp env b2]


(* ---------- Recognized Python builtins for locals tracking ---------- *)

(* ---------- Instruction conversion ---------- *)

let convert_instr (env : Env.t) (instr : T.Instr.t) : Env.t =
  let cc = env.cc in
  match instr with
  | Let {id; exp} -> (
      let atom = convert_exp env exp in
      let env = match id with Some id -> Env.bind_ident env id atom | None -> env in
      (* side-effectful calls: thread state *)
      match exp with
      | Call _ ->
          let new_state = mk_term cc "@seq" [env.state; atom] in
          Env.update_state env new_state
      | _ ->
          env )
  | Load {id; exp} ->
      let atom = mk_term cc "@deref" [convert_exp env exp] in
      Env.bind_ident env id atom
  | Store {exp1; exp2} ->
      let addr = convert_exp env exp1 in
      let value = convert_exp env exp2 in
      let eff = mk_term cc "@store" [addr; value] in
      let new_state = mk_term cc "@seq" [env.state; eff] in
      Env.update_state env new_state
  | Prune {exp} ->
      let cond = convert_exp env exp in
      let eff = mk_term cc "@prune" [cond] in
      let new_state = mk_term cc "@seq" [env.state; eff] in
      Env.update_state env new_state


(* ---------- Node / terminator conversion ---------- *)

let convert_instrs env instrs = List.fold instrs ~init:env ~f:convert_instr

(** Process a node's instructions, then recursively process its terminator. Returns the final
    e-graph atom for the result and the updated environment. *)
let rec convert_node (env : Env.t) (node : T.Node.t) : CC.Atom.t =
  let env = convert_instrs env node.instrs in
  convert_terminator env node.last


and convert_terminator (env : Env.t) (term : T.Terminator.t) : CC.Atom.t =
  let cc = env.cc in
  match term with
  | Ret exp ->
      let value = convert_exp env exp in
      let result = mk_term cc "@ret" [env.state; value] in
      Equations.add env.equations ~name:"RET" ~atom:result ~origin:"ret" ;
      result
  | Throw exp ->
      let value = convert_exp env exp in
      let result = mk_term cc "@throw" [env.state; value] in
      Equations.add env.equations ~name:"THROW" ~atom:result ~origin:"throw" ;
      result
  | Unreachable ->
      mk_const cc "@unreachable"
  | Jump [{label; ssa_args}] ->
      convert_jump env ~label ~ssa_args
  | Jump _ ->
      (* multi-target jump without If -unsupported in Phase 1 *)
      mk_const cc "@unsupported_jump"
  | If {bexp; then_; else_} ->
      convert_if env ~bexp ~then_ ~else_


and convert_jump (env : Env.t) ~label ~ssa_args : CC.Atom.t =
  match T.NodeName.Map.find_opt label env.node_map with
  | None ->
      mk_const env.cc "@unknown_label"
  | Some target_node ->
      (* bind SSA parameters of target node *)
      let env =
        List.fold2_exn target_node.ssa_parameters ssa_args ~init:env
          ~f:(fun env (id, _typ) arg_exp ->
            let atom = convert_exp env arg_exp in
            Env.bind_ident env id atom )
      in
      convert_node env target_node


and convert_if (env : Env.t) ~bexp:_ ~then_:_ ~else_:_ : CC.Atom.t =
  mk_const env.cc "@unsupported_if"


(* ---------- Terminator label iteration ---------- *)

let rec iter_terminator_labels (term : T.Terminator.t) ~f =
  match term with
  | Ret _ | Throw _ | Unreachable ->
      ()
  | Jump targets ->
      List.iter targets ~f:(fun {T.Terminator.label} -> f label)
  | If {then_; else_} ->
      iter_terminator_labels then_ ~f ;
      iter_terminator_labels else_ ~f


(* ---------- Main entry point ---------- *)

let convert_proc cc (proc : T.ProcDesc.t) : (CC.Atom.t * Equations.t, string) result =
  (* Extract parameter names from .args annotation *)
  let params =
    List.find_map proc.procdecl.attributes ~f:T.Attr.find_python_args |> Option.value ~default:[]
  in
  (* Build node map *)
  let node_map =
    List.fold proc.nodes ~init:T.NodeName.Map.empty ~f:(fun acc (node : T.Node.t) ->
        T.NodeName.Map.add node.label node acc )
  in
  (* Check for back-edges via simple visited-set DFS *)
  let has_back_edge =
    let visited = T.NodeName.HashSet.create 16 in
    let in_stack = T.NodeName.HashSet.create 16 in
    let found_back_edge = ref false in
    let rec dfs label =
      if T.NodeName.HashSet.mem in_stack label then found_back_edge := true
      else if not (T.NodeName.HashSet.mem visited label) then (
        T.NodeName.HashSet.add label visited ;
        T.NodeName.HashSet.add label in_stack ;
        ( match T.NodeName.Map.find_opt label node_map with
        | Some node ->
            iter_terminator_labels node.last ~f:dfs
        | None ->
            () ) ;
        T.NodeName.HashSet.remove label in_stack )
    in
    dfs proc.start ;
    !found_back_edge
  in
  if has_back_edge then Error "procedure contains loops (back-edges detected)"
  else
    let env = Env.init cc ~params in
    let env = {env with node_map} in
    match T.NodeName.Map.find_opt proc.start node_map with
    | None ->
        Error "start node not found"
    | Some start_node ->
        let result = convert_node env start_node in
        Ok (result, env.equations)
