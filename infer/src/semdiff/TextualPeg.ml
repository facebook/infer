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

  let store t ~name ~atom = StringMap.add name atom t

  let load t ~name = StringMap.find_opt name t
end

(* ---------- Environment ---------- *)

module Env = struct
  type t =
    { cc: CC.t
    ; ident_map: CC.Atom.t T.Ident.Map.t
    ; locals: LocalsMap.t
    ; state: CC.Atom.t
    ; equations: Equations.t
    ; node_map: T.Node.t T.NodeName.Map.t
    ; loop_headers: T.NodeName.Set.t
    ; active_loops: T.NodeName.Set.t
    ; theta_counter: int ref }

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
    ; node_map= T.NodeName.Map.empty
    ; loop_headers= T.NodeName.Set.empty
    ; active_loops= T.NodeName.Set.empty
    ; theta_counter= ref 0 }


  let bind_ident t id atom =
    let name = F.asprintf "%a" T.Ident.pp id in
    Equations.add t.equations ~name ~atom ~origin:"let" ;
    {t with ident_map= T.Ident.Map.add id atom t.ident_map}


  let lookup_ident t id = T.Ident.Map.find_opt id t.ident_map

  let update_locals t locals = {t with locals}

  let update_state t state = {t with state}
end

(* ---------- ASCII tree printer ---------- *)

let pp_tree ?(depth = 32) cc fmt atom =
  let rec pp depth prefix is_last fmt atom =
    if depth <= 0 then F.fprintf fmt "%s%s...@." prefix (if is_last then "└── " else "├── ")
    else
      let connector = if is_last then "└── " else "├── " in
      let child_prefix = prefix ^ if is_last then "    " else "│   " in
      match CC.get_enode cc atom with
      | Some {head; children= []} ->
          F.fprintf fmt "%s%s%a@." prefix connector (CC.pp_nested_term cc) head
      | Some {head; children} ->
          F.fprintf fmt "%s%s%a@." prefix connector (CC.pp_nested_term cc) head ;
          let n = List.length children in
          List.iteri children ~f:(fun i child ->
              pp (depth - 1) child_prefix (Int.equal i (n - 1)) fmt child )
      | None ->
          F.fprintf fmt "%s%s%a@." prefix connector CC.Atom.pp atom
  in
  match CC.get_enode cc atom with
  | Some {head; children= []} ->
      F.fprintf fmt "%a@." (CC.pp_nested_term cc) head
  | Some {head; children} ->
      F.fprintf fmt "%a@." (CC.pp_nested_term cc) head ;
      let n = List.length children in
      List.iteri children ~f:(fun i child -> pp (depth - 1) "" (Int.equal i (n - 1)) fmt child)
  | None ->
      F.fprintf fmt "%a@." CC.Atom.pp atom


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
        L.die InternalError "TextualPeg: unknown ident %a" T.Ident.pp id )
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

(** Try to extract the variable name from a py_store_fast / py_load_fast call. These calls have the
    form: [$builtins.py_store_fast("name", locals, value)] or
    [$builtins.py_load_fast("name", locals)]. The first argument is always a string constant with
    the variable name. *)
let extract_varname_from_args (args : T.Exp.t list) : string option =
  match args with Const (Str name) :: _ -> Some name | _ -> None


let is_py_builtin proc name =
  let s = qualified_procname_to_string proc in
  String.is_suffix s ~suffix:name


let pure_builtin_prefixes =
  ["py_make_"; "py_binary_"; "py_unary_"; "py_compare_"; "py_inplace_"; "py_build_tuple"]


let is_pure_call proc =
  let s = qualified_procname_to_string proc in
  List.exists pure_builtin_prefixes ~f:(fun prefix -> String.is_substring s ~substring:prefix)
  || is_py_builtin proc "py_bool" || is_py_builtin proc "py_bool_true"
  || is_py_builtin proc "py_bool_false"


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
          L.die InternalError "TextualPeg: py_load_fast: variable %s not found in locals map" name )
    | None ->
        L.die InternalError "TextualPeg: py_load_fast: could not extract variable name" )
  | Let {id= None; exp= Call {proc; args}} when is_py_builtin proc "py_store_fast" -> (
    match args with
    | Const (Str name) :: _locals :: value :: _ ->
        let atom = convert_exp env value in
        let locals = LocalsMap.store env.locals ~name ~atom in
        Equations.add env.equations ~name ~atom ~origin:"store_fast: locals" ;
        Env.update_locals env locals
    | _ ->
        L.die InternalError "TextualPeg: py_store_fast: malformed arguments" )
  | Let {id; exp= Call {proc}} when is_py_builtin proc "py_make_none" -> (
      let atom = mk_const cc "@None" in
      match id with Some id -> Env.bind_ident env id atom | None -> env )
  | Let {id= _; exp= Call {proc}} when is_py_builtin proc "py_nullify_locals" ->
      (* py_nullify_locals is a compiler artifact, not an observable effect — skip it *)
      env
  | Let {id; exp} -> (
      let atom = convert_exp env exp in
      let env = match id with Some id -> Env.bind_ident env id atom | None -> env in
      match exp with
      | Call {proc} when is_pure_call proc ->
          env
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
  | Prune _ ->
      L.die InternalError "TextualPeg: Prune instructions are not supported"


(* ---------- Node / terminator conversion ---------- *)

let convert_instrs env instrs = List.fold instrs ~init:env ~f:convert_instr

(** Process a node's instructions, then recursively process its terminator. Returns the final
    e-graph atom for the result and the updated environment. *)
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


(* ---------- Loop body analysis ---------- *)

(** Collect variable names modified by [py_store_fast] in the loop body. The loop body consists of
    all nodes reachable from the header's then-branch up to (but not including) the header itself.
*)
let collect_loop_stores (env : Env.t) (header_label : T.NodeName.t) : IString.Set.t =
  let stores = ref IString.Set.empty in
  let visited = T.NodeName.HashSet.create 16 in
  let rec walk label =
    if T.NodeName.equal label header_label || T.NodeName.HashSet.mem visited label then ()
    else (
      T.NodeName.HashSet.add label visited ;
      match T.NodeName.Map.find_opt label env.node_map with
      | None ->
          ()
      | Some node ->
          List.iter node.instrs ~f:(fun (instr : T.Instr.t) ->
              match instr with
              | Let {id= None; exp= Call {proc; args}} when is_py_builtin proc "py_store_fast" -> (
                match extract_varname_from_args args with
                | Some name ->
                    stores := IString.Set.add name !stores
                | None ->
                    () )
              | _ ->
                  () ) ;
          iter_terminator_labels node.last ~f:walk )
  in
  (* Start from the header's successors (the body path) *)
  ( match T.NodeName.Map.find_opt header_label env.node_map with
  | Some header_node ->
      iter_terminator_labels header_node.last ~f:walk
  | None ->
      () ) ;
  !stores


(* ---------- Node / terminator conversion ---------- *)

let rec convert_node (env : Env.t) (node : T.Node.t) : CC.Atom.t * Env.t =
  let env = convert_instrs env node.instrs in
  convert_terminator env node.last


and convert_terminator (env : Env.t) (term : T.Terminator.t) : CC.Atom.t * Env.t =
  let cc = env.cc in
  match term with
  | Ret exp ->
      let value = convert_exp env exp in
      let result = mk_term cc "@ret" [env.state; value] in
      Equations.add env.equations ~name:"RET" ~atom:result ~origin:"ret" ;
      (result, env)
  | Throw exp ->
      let value = convert_exp env exp in
      let result = mk_term cc "@throw" [env.state; value] in
      Equations.add env.equations ~name:"THROW" ~atom:result ~origin:"throw" ;
      (result, env)
  | Unreachable ->
      (mk_const cc "@unreachable", env)
  | Jump [{label; ssa_args}] ->
      convert_jump env ~label ~ssa_args
  | Jump _ ->
      L.die InternalError "TextualPeg: multi-target jump is not supported"
  | If {bexp; then_; else_} ->
      convert_if env ~bexp ~then_ ~else_


and convert_jump (env : Env.t) ~label ~ssa_args : CC.Atom.t * Env.t =
  if T.NodeName.Set.mem label env.active_loops then
    (* Back-edge: do not recurse into the header again. Return a sentinel;
       the caller (convert_loop) will read state/locals from the returned env. *)
    (mk_const env.cc "@back_edge", env)
  else if T.NodeName.Set.mem label env.loop_headers then convert_loop env label ~ssa_args
  else
    match T.NodeName.Map.find_opt label env.node_map with
    | None ->
        L.die InternalError "TextualPeg: unknown label %a" T.NodeName.pp label
    | Some target_node ->
        (* bind SSA parameters of target node *)
        let env =
          List.fold2_exn target_node.ssa_parameters ssa_args ~init:env
            ~f:(fun env (id, _typ) arg_exp ->
              let atom = convert_exp env arg_exp in
              Env.bind_ident env id atom )
        in
        convert_node env target_node


and convert_if (env : Env.t) ~bexp ~then_ ~else_ : CC.Atom.t * Env.t =
  let cc = env.cc in
  let cond = convert_boolexp env bexp in
  let result_then, _env_then = convert_terminator env then_ in
  let result_else, _env_else = convert_terminator env else_ in
  let result = mk_term cc "@phi" [cond; result_then; result_else] in
  Equations.add env.equations ~name:"PHI" ~atom:result ~origin:"if" ;
  (result, env)


and convert_loop (env : Env.t) (header_label : T.NodeName.t) ~ssa_args : CC.Atom.t * Env.t =
  let cc = env.cc in
  let header_node =
    match T.NodeName.Map.find_opt header_label env.node_map with
    | Some n ->
        n
    | None ->
        L.die InternalError "TextualPeg: unknown loop header %a" T.NodeName.pp header_label
  in
  (* Bind SSA parameters of header node *)
  let env =
    List.fold2_exn header_node.ssa_parameters ssa_args ~init:env ~f:(fun env (id, _typ) arg_exp ->
        let atom = convert_exp env arg_exp in
        Env.bind_ident env id atom )
  in
  (* 1. Collect variables modified in the loop body *)
  let modified_vars = collect_loop_stores env header_label in
  (* 2. Create theta placeholders *)
  let n = !(env.theta_counter) in
  env.theta_counter := n + 1 ;
  let theta_state_placeholder = mk_const cc (F.asprintf "@theta:state:%d" n) in
  let init_locals = env.locals in
  let init_state = env.state in
  let theta_locals, theta_var_placeholders =
    IString.Set.fold
      (fun name (locals, placeholders) ->
        let placeholder = mk_const cc (F.asprintf "@theta:%s:%d" name n) in
        (LocalsMap.store locals ~name ~atom:placeholder, (name, placeholder) :: placeholders) )
      modified_vars (env.locals, [])
  in
  (* 3. Set up loop env with theta placeholders *)
  let loop_env =
    { env with
      state= theta_state_placeholder
    ; locals= theta_locals
    ; active_loops= T.NodeName.Set.add header_label env.active_loops }
  in
  (* 4. Process header instructions and terminator *)
  let loop_env = convert_instrs loop_env header_node.instrs in
  match header_node.last with
  | If {bexp; then_; else_} ->
      let _cond = convert_boolexp loop_env bexp in
      (* Process body branch — will hit the back-edge and return *)
      let _body_result, body_env = convert_terminator loop_env then_ in
      (* 5. Close theta nodes: merge placeholders with @theta(init, body) terms *)
      let theta_state_term = mk_term cc "@theta" [init_state; body_env.state] in
      CC.merge cc theta_state_placeholder (CC.Atom theta_state_term) ;
      Equations.add env.equations
        ~name:(F.asprintf "\xCE\xB8_state_%d" n)
        ~atom:theta_state_term ~origin:"theta_close" ;
      List.iter theta_var_placeholders ~f:(fun (name, placeholder) ->
          let init_val =
            match LocalsMap.load init_locals ~name with
            | Some atom ->
                atom
            | None ->
                mk_const cc "@undef"
          in
          let body_val =
            match LocalsMap.load body_env.locals ~name with
            | Some atom ->
                atom
            | None ->
                L.die InternalError "TextualPeg: theta variable %s not found after body" name
          in
          let theta_term = mk_term cc "@theta" [init_val; body_val] in
          CC.merge cc placeholder (CC.Atom theta_term) ;
          Equations.add env.equations
            ~name:(F.asprintf "\xCE\xB8_%s_%d" name n)
            ~atom:theta_term ~origin:"theta_close" ) ;
      (* 6. Process exit branch with theta bindings in effect *)
      let exit_env = {loop_env with active_loops= env.active_loops} in
      convert_terminator exit_env else_
  | _ ->
      L.die InternalError "TextualPeg: loop header %a must have If terminator" T.NodeName.pp
        header_label


(* ---------- Main entry point ---------- *)

let convert_proc ?(theta_counter = ref 0) cc (proc : T.ProcDesc.t) :
    (CC.Atom.t * Equations.t, string) result =
  (* Extract parameter names from .args annotation *)
  let params =
    List.find_map proc.procdecl.attributes ~f:T.Attr.find_python_args |> Option.value ~default:[]
  in
  (* Build node map *)
  let node_map =
    List.fold proc.nodes ~init:T.NodeName.Map.empty ~f:(fun acc (node : T.Node.t) ->
        T.NodeName.Map.add node.label node acc )
  in
  (* Detect loop headers via DFS: a node is a loop header if it is the target of a back-edge *)
  let loop_headers =
    let visited = T.NodeName.HashSet.create 16 in
    let in_stack = T.NodeName.HashSet.create 16 in
    let headers = ref T.NodeName.Set.empty in
    let rec dfs label =
      if T.NodeName.HashSet.mem in_stack label then headers := T.NodeName.Set.add label !headers
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
    !headers
  in
  let env = Env.init cc ~params in
  let env = {env with node_map; loop_headers; theta_counter} in
  match T.NodeName.Map.find_opt proc.start node_map with
  | None ->
      Error "start node not found"
  | Some start_node ->
      let result, _env = convert_node env start_node in
      Ok (result, env.equations)
