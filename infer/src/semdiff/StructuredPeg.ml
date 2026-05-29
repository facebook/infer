(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module CC = CongruenceClosureSolver
module T = Textual
module S = StructuredIR

(* ---------- Duplicated helpers from TextualPeg ---------- *)

module StringMap = IString.Map

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

module LocalsMap = struct
  type t = CC.Atom.t StringMap.t

  let empty = StringMap.empty

  let store t ~name ~atom = StringMap.add name atom t

  let load t ~name = StringMap.find_opt name t
end

module Env = struct
  type t =
    { cc: CC.t
    ; ident_map: CC.Atom.t T.Ident.Map.t
    ; locals: LocalsMap.t
    ; state: CC.Atom.t
    ; equations: Equations.t
    ; theta_counter: int ref
    ; loop_counter: int ref }

  let init cc ~params =
    let equations = Equations.empty () in
    let locals =
      List.fold params ~init:LocalsMap.empty ~f:(fun acc name ->
          let atom = CC.mk_term cc (CC.mk_header cc ("@param:" ^ name)) [] in
          Equations.add equations ~name ~atom ~origin:"param" ;
          LocalsMap.store acc ~name ~atom )
    in
    let state = CC.mk_term cc (CC.mk_header cc "@state0") [] in
    { cc
    ; ident_map= T.Ident.Map.empty
    ; locals
    ; state
    ; equations
    ; theta_counter= ref 0
    ; loop_counter= ref 0 }


  let bind_ident t id atom =
    let name = F.asprintf "%a" T.Ident.pp id in
    Equations.add t.equations ~name ~atom ~origin:"let" ;
    {t with ident_map= T.Ident.Map.add id atom t.ident_map}


  let lookup_ident t id = T.Ident.Map.find_opt id t.ident_map
end

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
        L.die InternalError "StructuredPeg: unknown ident %a" T.Ident.pp id )
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


(* ---------- Call classification ---------- *)

let extract_varname_from_args (args : T.Exp.t list) : string option =
  match args with Const (Str name) :: _ -> Some name | _ -> None


let is_py_builtin proc name =
  let s = qualified_procname_to_string proc in
  String.is_suffix s ~suffix:name


let pure_builtin_prefixes =
  ["py_make_"; "py_binary_"; "py_unary_"; "py_compare_"; "py_inplace_"; "py_build_tuple"]


type call_effect = Pure | Reader | Writer

let is_pure_call proc =
  let s = qualified_procname_to_string proc in
  List.exists pure_builtin_prefixes ~f:(fun prefix -> String.is_substring s ~substring:prefix)
  || is_py_builtin proc "py_bool" || is_py_builtin proc "py_bool_true"
  || is_py_builtin proc "py_bool_false"


let is_reader_call proc =
  is_py_builtin proc "py_load_global"
  || is_py_builtin proc "py_get_iter" || is_py_builtin proc "py_next_iter"
  || is_py_builtin proc "py_has_next_iter"


let try_simplify_py_call (env : Env.t) (args : T.Exp.t list) : CC.Atom.t option =
  match args with
  | Var callee_id :: _ -> (
    match Env.lookup_ident env callee_id with
    | Some callee_atom ->
        let s = F.asprintf "%a" (CC.pp_nested_term env.cc) callee_atom in
        if String.is_substring s ~substring:"(@str enumerate)" then
          let actual_args = match args with _ :: _ :: rest -> rest | _ -> [] in
          Some (mk_term env.cc "@enumerate" (List.map actual_args ~f:(convert_exp env)))
        else None
    | None ->
        None )
  | _ ->
      None


let dict_reader_methods = ["items"; "keys"; "values"]

let try_simplify_py_call_method (env : Env.t) (args : T.Exp.t list) : CC.Atom.t option =
  match args with
  | Const (Str method_name) :: obj :: _
    when List.mem dict_reader_methods method_name ~equal:String.equal ->
      let obj_atom = convert_exp env obj in
      Some (mk_term env.cc ("@dict_" ^ method_name) [env.state; obj_atom])
  | _ ->
      None


let classify_call (env : Env.t) proc (args : T.Exp.t list) =
  if is_pure_call proc then Pure
  else if is_reader_call proc then Reader
  else if is_py_builtin proc "py_call" && Option.is_some (try_simplify_py_call env args) then Pure
  else if
    is_py_builtin proc "py_call_method" && Option.is_some (try_simplify_py_call_method env args)
  then Reader
  else Writer


(* ---------- Instruction conversion ---------- *)

let convert_instr (env : Env.t) (instr : T.Instr.t) : Env.t =
  let cc = env.cc in
  match instr with
  | Let {id= Some id; exp= Call {proc; args}} when is_py_builtin proc "py_load_fast" -> (
    match extract_varname_from_args args with
    | Some name -> (
      match LocalsMap.load env.locals ~name with
      | Some atom ->
          let id_name = F.asprintf "%a" T.Ident.pp id in
          Equations.add env.equations ~name:id_name ~atom ~origin:"load_fast: locals" ;
          {env with ident_map= T.Ident.Map.add id atom env.ident_map}
      | None ->
          L.die InternalError "StructuredPeg: py_load_fast: variable %s not found in locals map"
            name )
    | None ->
        L.die InternalError "StructuredPeg: py_load_fast: could not extract variable name" )
  | Let {id= None; exp= Call {proc; args}} when is_py_builtin proc "py_store_fast" -> (
    match args with
    | Const (Str name) :: _locals :: value :: _ ->
        let atom = convert_exp env value in
        let locals = LocalsMap.store env.locals ~name ~atom in
        Equations.add env.equations ~name ~atom ~origin:"store_fast: locals" ;
        {env with locals}
    | _ ->
        L.die InternalError "StructuredPeg: py_store_fast: malformed arguments" )
  | Let {id; exp= Call {proc}} when is_py_builtin proc "py_make_none" -> (
      let atom = mk_const cc "@None" in
      match id with Some id -> Env.bind_ident env id atom | None -> env )
  | Let {id= _; exp= Call {proc}} when is_py_builtin proc "py_nullify_locals" ->
      env
  | Let {id; exp} -> (
      let eff = match exp with Call {proc; args} -> classify_call env proc args | _ -> Pure in
      let atom =
        match (exp, eff) with
        | Call {proc; args}, Writer ->
            let header = qualified_procname_to_string proc in
            let op = mk_term cc header (env.state :: List.map args ~f:(convert_exp env)) in
            mk_term cc "@eval" [op]
        | Call {proc; args}, Reader -> (
          match
            if is_py_builtin proc "py_call_method" then try_simplify_py_call_method env args
            else None
          with
          | Some atom ->
              atom
          | None ->
              let header = qualified_procname_to_string proc in
              mk_term cc header (env.state :: List.map args ~f:(convert_exp env)) )
        | Call {proc; args}, Pure when is_py_builtin proc "py_call" -> (
          match try_simplify_py_call env args with Some atom -> atom | None -> convert_exp env exp )
        | _ ->
            convert_exp env exp
      in
      let env = match id with Some id -> Env.bind_ident env id atom | None -> env in
      match eff with
      | Writer ->
          let op =
            match CC.get_enode cc atom with
            | Some {children= [op]; _} ->
                op
            | _ ->
                L.die InternalError "StructuredPeg: expected @eval node for writer call"
          in
          {env with state= mk_term cc "@heap" [op]}
      | Reader | Pure ->
          env )
  | Load {id; exp} ->
      let atom = mk_term cc "@deref" [convert_exp env exp] in
      Env.bind_ident env id atom
  | Store {exp1; exp2} ->
      let addr = convert_exp env exp1 in
      let value = convert_exp env exp2 in
      let eff = mk_term cc "@store" [env.state; addr; value] in
      {env with state= mk_term cc "@heap" [eff]}
  | Prune _ ->
      L.die InternalError "StructuredPeg: Prune instructions are not supported"


let convert_instrs env instrs = List.fold instrs ~init:env ~f:convert_instr

(* ---------- Phi merging ---------- *)

let merge_envs (env : Env.t) ~cond ~(env_then : Env.t) ~(env_else : Env.t) =
  let cc = env.cc in
  let state =
    if CC.is_equiv cc env_then.state env_else.state then env_then.state
    else mk_term cc "@phi" [cond; env_then.state; env_else.state]
  in
  let locals =
    StringMap.merge
      (fun _name v_then v_else ->
        match (v_then, v_else) with
        | Some a_then, Some a_else ->
            if CC.is_equiv cc a_then a_else then Some a_then
            else Some (mk_term cc "@phi" [cond; a_then; a_else])
        | Some a, None | None, Some a ->
            Some a
        | None, None ->
            None )
      env_then.locals env_else.locals
  in
  {env with Env.state; locals}


(* ---------- StructuredIR → PEG conversion ---------- *)

type convert_result = Normal of CC.Atom.t * Env.t | Exit of int * Env.t

let rec convert (env : Env.t) (sir : S.t) : convert_result =
  let cc = env.cc in
  match sir with
  | Instrs {instrs; _} ->
      let env = convert_instrs env instrs in
      Normal (mk_const cc "@unit", env)
  | Seq (a, b) -> (
    match convert env a with Normal (_, env_a) -> convert env_a b | Exit _ as exit -> exit )
  | Return {exp; _} ->
      let value = convert_exp env exp in
      let result = mk_term cc "@ret" [env.state; value] in
      Normal (result, env)
  | Throw {exp; _} ->
      let value = convert_exp env exp in
      let result = mk_term cc "@throw" [env.state; value] in
      Normal (result, env)
  | Branch depth ->
      Exit (depth, env)
  | If {bexp; then_; else_; _} ->
      convert_if env ~bexp ~then_ ~else_
  | Block body ->
      convert_block env body
  | Loop _ ->
      L.die InternalError "StructuredPeg: Loop not yet supported"


and convert_if (env : Env.t) ~bexp ~then_ ~else_ : convert_result =
  let cc = env.cc in
  let cond = convert_boolexp env bexp in
  let result_then = convert env then_ in
  let result_else = convert env else_ in
  match (result_then, result_else) with
  | Normal (atom_then, env_then), Normal (atom_else, env_else) ->
      let result = mk_term cc "@phi" [cond; atom_then; atom_else] in
      let merged = merge_envs env ~cond ~env_then ~env_else in
      Normal (result, merged)
  | Exit (n1, env_then), Exit (n2, env_else) when Int.equal n1 n2 ->
      let merged = merge_envs env ~cond ~env_then ~env_else in
      Exit (n1, merged)
  | Normal (_atom, _env_normal), Exit (n, env_exit) ->
      Exit (n, env_exit)
  | Exit (n, env_exit), Normal (_atom, _env_normal) ->
      Exit (n, env_exit)
  | Exit (n1, _), Exit (n2, _) ->
      Exit (Int.min n1 n2, env)


and convert_block (env : Env.t) (body : S.t) : convert_result =
  match convert env body with
  | Normal _ as result ->
      result
  | Exit (0, env_exit) ->
      Normal (mk_const env.cc "@unit", env_exit)
  | Exit (n, env_exit) ->
      Exit (n - 1, env_exit)


(* ---------- Main entry point ---------- *)

let convert_proc ?(theta_counter = ref 0) cc (proc : T.ProcDesc.t) :
    (CC.Atom.t * Equations.t * int, string) result =
  let params =
    List.find_map proc.procdecl.attributes ~f:T.Attr.find_python_args |> Option.value ~default:[]
  in
  let env = Env.init cc ~params in
  let env = {env with theta_counter} in
  let sir = S.of_cfg proc.nodes proc.start in
  match convert env sir with
  | Normal (result, env) ->
      Ok (result, env.equations, !(env.loop_counter))
  | Exit _ ->
      Error "StructuredPeg: top-level Branch (should not happen)"
