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

(* ---------- Helpers ---------- *)

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


  let pp_thetas cc fmt t =
    List.iter (entries t) ~f:(fun {name; atom; _} ->
        if String.is_prefix name ~prefix:"\xCE\xB8" then
          F.fprintf fmt "@[<h>%-6s = %a@]@." name (CC.pp_nested_term cc) atom )
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

(* An empty container constructor -- set()/dict()/list()/tuple() -- builds the same value as the
   corresponding literal ({}, [], ()) ; the only difference is allocation identity, deliberately
   ignored for B006. Model it as the pure empty-container builtin so that a default constructed in
   the module body (recovered as prev's default) and the same value rebuilt in the migrated guard
   body (curr) are equal regardless of their state context -- and so that dict() ~ {} and list() ~
   []. [callee_repr] is the printed callee atom, which contains e.g. [(@str set)]. *)
let empty_constructor_builtin callee_repr =
  let is name = String.is_substring callee_repr ~substring:(Printf.sprintf "(@str %s)" name) in
  if is "set" then Some "$builtins.py_build_set"
  else if is "dict" then Some "$builtins.py_build_map"
  else if is "list" then Some "$builtins.py_build_list"
  else if is "tuple" then Some "$builtins.py_build_tuple"
  else None


(* Depth used when printing a callee atom to recover its shallow [(@str name)]. A small cap keeps the
   print cheap even when the callee carries a deep, hash-consed state-threading DAG -- a full print
   expands that DAG as a tree (see pp_nested_term) and dominates conversion time on large functions. *)
let callee_name_depth = 6


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
  | Call {proc} when String.is_suffix (qualified_procname_to_string proc) ~suffix:"py_make_none" ->
      mk_const cc "@None"
  | Load {exp= inner_exp} ->
      mk_term cc "@load" [convert_exp env inner_exp]
  | Field {exp= inner_exp; field} ->
      let field_name = T.FieldName.to_string field.name in
      mk_term cc "@field" [convert_exp env inner_exp; mk_const cc field_name]
  | Index (e1, e2) ->
      mk_term cc "@index" [convert_exp env e1; convert_exp env e2]
  | Call {proc; args}
    when String.is_suffix (qualified_procname_to_string proc) ~suffix:"py_call" -> (
      let generic () =
        mk_term cc (qualified_procname_to_string proc) (List.map args ~f:(convert_exp env))
      in
      let callee_is s callee =
        String.is_substring
          (F.asprintf "%a" (CC.pp_nested_term ~depth:callee_name_depth cc) (convert_exp env callee))
          ~substring:(Printf.sprintf "(@str %s)" s)
      in
      match args with
      | callee :: T.Exp.Call {proc= tproc; args= kwnames} :: vals
        when String.is_suffix (qualified_procname_to_string tproc) ~suffix:"py_build_tuple"
             && (not (List.is_empty vals))
             && Int.equal (List.length kwnames) (List.length vals)
             && List.for_all kwnames ~f:(function
                  | T.Exp.Call {proc= p} ->
                      String.is_suffix (qualified_procname_to_string p) ~suffix:"py_make_string"
                  | _ ->
                      false )
             && callee_is "dict" callee ->
          (* A keyword dict constructor dict(k1=v1, ..., kn=vn) is equivalent to the literal
             {k1: v1, ..., kn: vn}. It is compiled as py_call(dict, py_build_tuple(k1,...,kn),
             v1,...,vn); rewrite it to the same pure py_build_map(k1, v1, ..., kn, vn) the literal
             produces, so a codemod that turns a dict(...) default into a {...} literal is equivalent. *)
          let pairs =
            List.concat_map (List.zip_exn kwnames vals) ~f:(fun (k, v) ->
                [convert_exp env k; convert_exp env v] )
          in
          mk_term cc "$builtins.py_build_map" pairs
      | callee :: rest
        when List.for_all rest ~f:(function
               | T.Exp.Call {proc= p} ->
                   String.is_suffix (qualified_procname_to_string p) ~suffix:"py_make_none"
               | _ ->
                   false ) -> (
          (* An empty container constructor call set()/dict()/list()/tuple() (no positional args, only
             an optional None kwargs placeholder). Canonicalise to the pure empty-container builtin,
             matching how the statement-level try_simplify_py_call handles the guard body. *)
          match
            empty_constructor_builtin
              (F.asprintf "%a" (CC.pp_nested_term ~depth:callee_name_depth cc) (convert_exp env callee))
          with
          | Some builtin ->
              mk_term cc builtin []
          | None ->
              generic () )
      | _ ->
          generic () )
  | Call {proc; args}
    when String.is_suffix (qualified_procname_to_string proc) ~suffix:"py_make_function" -> (
      (* A B006 codemod rewrites a parameter's default value (e.g. [] -> None) and its type
         annotation (T -> Optional[T]). Both are baked into py_make_function's arguments
         (default_values, default_values_kw, annotations) at the definition site, i.e. the enclosing
         module body or class body. The default's actual effect is modelled inside the callee via
         @phi(@is_default, ...), so these definition-site arguments are redundant here and would only
         make the enclosing proc diverge spuriously (the class body of a migrated method is not the
         skipped __module_body__). Keep the closure (function identity) and the captured cells; drop
         the defaults/kwdefaults/annotations. *)
      let header = qualified_procname_to_string proc in
      match args with
      | closure :: _default_values :: _default_values_kw :: _annotations :: cells ->
          mk_term cc header (convert_exp env closure :: List.map cells ~f:(convert_exp env))
      | _ ->
          mk_term cc header (List.map args ~f:(convert_exp env)) )
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


(* Empty/literal container construction is modelled as pure: the only impurity is allocation
   identity (a fresh object each call), which is exactly the cross-call aliasing the B006 "surprise"
   relies on — and which we deliberately ignore. *)
let pure_builtin_prefixes =
  [ "py_make_"
  ; "py_binary_"
  ; "py_unary_"
  ; "py_compare_"
  ; "py_inplace_"
  ; "py_build_tuple"
  ; "py_build_list"
  ; "py_build_map"
  ; "py_build_set" ]


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


(* CPython compiles a list/set/dict display of >=3 constant elements to an empty BUILD_x followed by
   an in-place X_EXTEND / X_UPDATE of a constant tuple/map. These in-place updates mutate the fresh
   container, so their effect belongs to the container's value, not to the heap. We model them as
   pure so a container built this way has its elements in its value term (see convert_instr). *)
let container_update_ops = ["py_list_extend"; "py_set_update"; "py_dict_update"; "py_dict_merge"]

let is_container_update proc = List.exists container_update_ops ~f:(is_py_builtin proc)

let try_simplify_py_call (env : Env.t) (args : T.Exp.t list) : CC.Atom.t option =
  match args with
  | Var callee_id :: _ -> (
    match Env.lookup_ident env callee_id with
    | Some callee_atom ->
        (* Only the shallow callee name [(@str X)] matters here. Cap the print depth: a full print
           would exponentially expand the deep, hash-consed state-threading DAG carried by the
           callee (e.g. py_load_global's state argument), which dominates conversion time on large
           functions. *)
        let s = F.asprintf "%a" (CC.pp_nested_term ~depth:callee_name_depth env.cc) callee_atom in
        let actual_args = match args with _ :: _ :: rest -> rest | _ -> [] in
        if String.is_substring s ~substring:"(@str enumerate)" then
          Some (mk_term env.cc "@enumerate" (List.map actual_args ~f:(convert_exp env)))
        else if List.is_empty actual_args then
          Option.map (empty_constructor_builtin s) ~f:(fun builtin -> mk_term env.cc builtin [])
        else (
          (* A keyword dict constructor dict(k1=v1, ..., kn=vn) == the literal {k1: v1, ...}: it is
             py_call(dict, py_build_tuple(k1,...,kn), v1,...,vn). Model it as the same pure
             py_build_map the literal produces, so it matches whether the codemod keeps dict(...) or
             rewrites it to {...}. (Mirrors the convert_exp case, for the statement path.) *)
          match args with
          | _ :: T.Exp.Call {proc= tproc; args= kwnames} :: vals
            when String.is_substring s ~substring:"(@str dict)"
                 && String.is_suffix (qualified_procname_to_string tproc) ~suffix:"py_build_tuple"
                 && Int.equal (List.length kwnames) (List.length vals)
                 && List.for_all kwnames ~f:(function
                      | T.Exp.Call {proc= p} ->
                          String.is_suffix (qualified_procname_to_string p) ~suffix:"py_make_string"
                      | _ ->
                          false ) ->
              let pairs =
                List.concat_map (List.zip_exn kwnames vals) ~f:(fun (k, v) ->
                    [convert_exp env k; convert_exp env v] )
              in
              Some (mk_term env.cc "$builtins.py_build_map" pairs)
          | _ ->
              None )
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
          (* Defensive: a name may be loaded without being seeded in the locals map -- e.g. *args /
             **kwargs (not listed in python_args), or a name read on a path before its first store.
             Rather than abort the whole file, model it as an opaque parameter-like value and seed it,
             so the analysis degrades gracefully. It is the same on both sides, so the equivalence
             verdict is preserved. *)
          let atom = mk_term cc ("@param:" ^ name) [] in
          let id_name = F.asprintf "%a" T.Ident.pp id in
          Equations.add env.equations ~name:id_name ~atom ~origin:"load_fast: opaque (unseeded)" ;
          let locals = LocalsMap.store env.locals ~name ~atom in
          {env with locals; ident_map= T.Ident.Map.add id atom env.ident_map} )
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
  | Let {id; exp= Call {proc; args= [Var target; iterable]}} when is_container_update proc ->
      (* In-place X_EXTEND / X_UPDATE of a freshly built container (the >=3-element literal idiom).
         Rebind the container ident to a pure <update>(container, iterable) value term, without
         threading state, so its elements live in the value -- matching a default recovered as the
         same expression (see resolve_exp / build_extend_map). Allocation identity is ignored for
         B006. The update returns None, but binding [id] to the updated container is harmless and
         convenient. *)
      let header = qualified_procname_to_string proc in
      let container_atom = convert_exp env (Var target) in
      let iterable_atom = convert_exp env iterable in
      let atom = mk_term cc header [container_atom; iterable_atom] in
      let env = Env.bind_ident env target atom in
      Option.value_map id ~default:env ~f:(fun id -> Env.bind_ident env id atom)
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
  (* SSA idents (incl. desugared block parameters) bound differently on each branch reconverge as a
     @phi, exactly like named locals. Without this the join would drop one branch's binding and a
     use after the merge would fail with "unknown ident". *)
  let ident_map =
    T.Ident.Map.merge
      (fun _id a b ->
        match (a, b) with
        | Some x, Some y ->
            if CC.is_equiv cc x y then Some x else Some (mk_term cc "@phi" [cond; x; y])
        | (Some _ as x), None | None, (Some _ as x) ->
            x
        | None, None ->
            None )
      env_then.ident_map env_else.ident_map
  in
  {env with Env.state; locals; ident_map}


(* ---------- StructuredIR → PEG conversion ---------- *)

type exit_info = {depth: int; atom: CC.Atom.t; cond_stack: CC.Atom.t list; env: Env.t}

type convert_result = Normal of CC.Atom.t * Env.t | Exit of exit_info

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
      Exit {depth= Int.max_value; atom= result; cond_stack= []; env}
  | Throw {exp; _} ->
      let value = convert_exp env exp in
      let result = mk_term cc "@throw" [env.state; value] in
      Exit {depth= Int.max_value; atom= result; cond_stack= []; env}
  | Branch depth ->
      Exit {depth; atom= mk_const env.cc "@back_edge"; cond_stack= []; env}
  | If {bexp; then_; else_; _} ->
      convert_if env ~bexp ~then_ ~else_
  | Block body ->
      convert_block env body
  | Loop {body; _} ->
      convert_loop env body


and convert_if (env : Env.t) ~bexp ~then_ ~else_ : convert_result =
  let cc = env.cc in
  let cond = convert_boolexp env bexp in
  let result_then = convert env then_ in
  let result_else = convert env else_ in
  (* If creates a nesting level: decrement Exit depths *)
  let decrement = function
    | Normal _ as r ->
        r
    | Exit ({depth= 0; _} as e) ->
        Normal (e.atom, e.env)
    | Exit e ->
        Exit {e with depth= e.depth - 1}
  in
  let result_then = decrement result_then in
  let result_else = decrement result_else in
  let back_edge = mk_const cc "@back_edge" in
  let is_back_edge atom = CC.is_equiv cc atom back_edge in
  match (result_then, result_else) with
  | Normal (atom_then, env_then), Normal (atom_else, env_else) ->
      let result =
        if is_back_edge atom_then then atom_else
        else if is_back_edge atom_else then atom_then
        else mk_term cc "@phi" [cond; atom_then; atom_else]
      in
      let merged = merge_envs env ~cond ~env_then ~env_else in
      Normal (result, merged)
  | Exit e1, Exit e2 when Int.equal e1.depth e2.depth ->
      (* Both branches exit to the same target, so they reconverge there: merge their envs.
         A back-edge sentinel carries no value, so collapse it rather than building a
         @phi over @back_edge (which would leak the sentinel into the result). *)
      let result =
        if is_back_edge e1.atom then e2.atom
        else if is_back_edge e2.atom then e1.atom
        else mk_term cc "@phi" [cond; e1.atom; e2.atom]
      in
      let merged = merge_envs env ~cond ~env_then:e1.env ~env_else:e2.env in
      Exit {depth= e1.depth; atom= result; cond_stack= e1.cond_stack; env= merged}
  | Normal (_atom_normal, env_normal), Exit e ->
      let not_cond = mk_term cc "@not" [cond] in
      let merged = merge_envs env ~cond ~env_then:env_normal ~env_else:e.env in
      Exit {depth= e.depth; atom= e.atom; cond_stack= not_cond :: e.cond_stack; env= merged}
  | Exit e, Normal (_atom_normal, env_normal) ->
      let merged = merge_envs env ~cond ~env_then:e.env ~env_else:env_normal in
      Exit {depth= e.depth; atom= e.atom; cond_stack= cond :: e.cond_stack; env= merged}
  | Exit e1, Exit e2 ->
      let result =
        if is_back_edge e1.atom then e2.atom
        else if is_back_edge e2.atom then e1.atom
        else mk_term cc "@phi" [cond; e1.atom; e2.atom]
      in
      let min_exit, max_exit = if e1.depth <= e2.depth then (e1, e2) else (e2, e1) in
      (* A back-edge branch is the loop's continue path: its env defines the loop recurrence
         (the theta step), so it must propagate verbatim. The other branch's exit value is
         already captured in [result], so its env is not needed here. Only when neither branch
         is a back-edge does control genuinely reconverge — then merge. *)
      let merged =
        if is_back_edge e1.atom then e1.env
        else if is_back_edge e2.atom then e2.env
        else merge_envs env ~cond ~env_then:e1.env ~env_else:e2.env
      in
      let cond_stack =
        if is_back_edge min_exit.atom then
          (if Int.equal min_exit.depth e1.depth then mk_term cc "@not" [cond] else cond)
          :: max_exit.cond_stack
        else max_exit.cond_stack
      in
      Exit {depth= min_exit.depth; atom= result; cond_stack; env= merged}


and convert_block (env : Env.t) (body : S.t) : convert_result =
  match convert env body with
  | Normal _ as result ->
      result
  | Exit {depth= 0; atom; env= env_exit; _} ->
      Normal (atom, env_exit)
  | Exit e ->
      Exit {e with depth= e.depth - 1}


and convert_loop (env : Env.t) (body : S.t) : convert_result =
  let cc = env.cc in
  (* step 1: collect variables modified in the loop body *)
  let modified_vars = collect_modified_vars body in
  (* step 2: create theta placeholders *)
  let fresh = !(env.theta_counter) in
  env.theta_counter := fresh + 1 ;
  let loop_idx = !(env.loop_counter) in
  env.loop_counter := loop_idx + 1 ;
  let theta_state_placeholder = mk_const cc (F.asprintf "@theta:state:%d" fresh) in
  let init_locals = env.locals in
  let init_state = env.state in
  let theta_locals, theta_var_placeholders =
    IString.Set.fold
      (fun name (locals, placeholders) ->
        let placeholder = mk_const cc (F.asprintf "@theta:%s:%d" name fresh) in
        (LocalsMap.store locals ~name ~atom:placeholder, (name, placeholder) :: placeholders) )
      modified_vars (env.locals, [])
  in
  (* step 3: process body with theta placeholders *)
  let loop_env = {env with state= theta_state_placeholder; locals= theta_locals} in
  let body_result = convert loop_env body in
  (* step 4: close theta nodes *)
  let theta_header = F.asprintf "@theta_%d" loop_idx in
  let close_thetas (body_env : Env.t) =
    let theta_state_term = mk_term cc theta_header [init_state; body_env.state] in
    CC.merge cc theta_state_placeholder (CC.Atom theta_state_term) ;
    Equations.add env.equations
      ~name:(F.asprintf "\xCE\xB8_state_%d" fresh)
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
              L.die InternalError "StructuredPeg: theta variable %s not found after body" name
        in
        let theta_term = mk_term cc theta_header [init_val; body_val] in
        CC.merge cc placeholder (CC.Atom theta_term) ;
        Equations.add env.equations
          ~name:(F.asprintf "\xCE\xB8_%s_%d" name fresh)
          ~atom:theta_term ~origin:"theta_close" )
  in
  (* step 5: build exit condition from cond_stack and create @exit_value(θ, cond) *)
  let close_and_exit ~cond_stack body_env =
    close_thetas body_env ;
    let exit_cond =
      match cond_stack with
      | [] ->
          None
      | [c] ->
          Some c
      | first :: rest ->
          Some (List.fold rest ~init:first ~f:(fun acc c -> mk_term cc "@and" [acc; c]))
    in
    let mk_exit_value placeholder =
      match exit_cond with
      | Some cond ->
          mk_term cc "@exit_value" [placeholder; cond]
      | None ->
          mk_term cc "@exit_value" [placeholder]
    in
    let exit_state = mk_exit_value theta_state_placeholder in
    let exit_locals =
      List.fold theta_var_placeholders ~init:loop_env.locals ~f:(fun locals (name, placeholder) ->
          LocalsMap.store locals ~name ~atom:(mk_exit_value placeholder) )
    in
    {loop_env with Env.state= exit_state; locals= exit_locals}
  in
  match body_result with
  | Exit {depth= 0; atom; cond_stack; env= body_env} ->
      let exit_env = close_and_exit ~cond_stack body_env in
      Normal (atom, exit_env)
  | Exit ({depth; _} as e) ->
      let exit_env = close_and_exit ~cond_stack:e.cond_stack e.env in
      Exit {depth= depth - 1; atom= e.atom; cond_stack= []; env= exit_env}
  | Normal (atom, body_env) ->
      let exit_env = close_and_exit ~cond_stack:[] body_env in
      Normal (atom, exit_env)


(* Walk StructuredIR to collect py_store_fast variable names *)
and collect_modified_vars (sir : S.t) : IString.Set.t =
  match sir with
  | Instrs {instrs; _} ->
      List.fold instrs ~init:IString.Set.empty ~f:(fun acc (instr : T.Instr.t) ->
          match instr with
          | Let {id= None; exp= Call {proc; args}} when is_py_builtin proc "py_store_fast" -> (
            match extract_varname_from_args args with
            | Some name ->
                IString.Set.add name acc
            | None ->
                acc )
          | _ ->
              acc )
  | Seq (a, b) ->
      IString.Set.union (collect_modified_vars a) (collect_modified_vars b)
  | Block body | Loop {body; _} ->
      collect_modified_vars body
  | If {then_; else_; _} ->
      IString.Set.union (collect_modified_vars then_) (collect_modified_vars else_)
  | Return _ | Throw _ | Branch _ ->
      IString.Set.empty


(* ---------- Default-argument extraction (for B006) ---------- *)

(* In CPython, default argument values are evaluated at the function-definition site, not in the
   function body. The frontend emits them in the enclosing scope (the __module_body__ for a
   top-level function, or the class body for a method) as the second argument of py_make_function.
   To reason about defaults we recover, for each parameter that has one, the (self-contained)
   default expression. *)

(* Map each SSA ident defined in [proc] to its defining expression. *)
let build_ident_def_map (proc : T.ProcDesc.t) : T.Exp.t T.Ident.Map.t =
  List.fold proc.nodes ~init:T.Ident.Map.empty ~f:(fun acc (node : T.Node.t) ->
      List.fold node.instrs ~init:acc ~f:(fun acc (instr : T.Instr.t) ->
          match instr with Let {id= Some id; exp; _} -> T.Ident.Map.add id exp acc | _ -> acc ) )


(* Map each container ident to the in-place X_EXTEND / X_UPDATE calls applied to it, in order. Used
   to recover the elements of a >=3-element list/set/dict literal, whose elements are added by such
   updates rather than being part of the BUILD_x expression (see resolve_exp). *)
let build_extend_map (proc : T.ProcDesc.t) : T.Exp.t list T.Ident.Map.t =
  List.fold proc.nodes ~init:T.Ident.Map.empty ~f:(fun acc (node : T.Node.t) ->
      List.fold node.instrs ~init:acc ~f:(fun acc (instr : T.Instr.t) ->
          match instr with
          | Let {exp= Call {proc= p; args= Var target :: _} as call; _} when is_container_update p
            ->
              let prev = Option.value (T.Ident.Map.find_opt target acc) ~default:[] in
              T.Ident.Map.add target (prev @ [call]) acc
          | _ ->
              acc ) )


(* Inline SSA idents to obtain a self-contained expression (module body is acyclic SSA). When an
   ident denotes a container that is then updated in place (the >=3-element literal idiom), fold the
   recorded updates back into the expression so the recovered value carries its elements. *)
let rec resolve_exp ?(extends = T.Ident.Map.empty) (defs : T.Exp.t T.Ident.Map.t) (exp : T.Exp.t) :
    T.Exp.t =
  match exp with
  | Var id -> (
      let base =
        match T.Ident.Map.find_opt id defs with
        | Some e ->
            resolve_exp ~extends defs e
        | None ->
            exp
      in
      match T.Ident.Map.find_opt id extends with
      | Some updates ->
          List.fold updates ~init:base ~f:(fun acc update ->
              match update with
              | T.Exp.Call {proc; args= _container :: rest; kind; caller_ret_annots} ->
                  T.Exp.Call
                    { proc
                    ; args= acc :: List.map rest ~f:(resolve_exp ~extends defs)
                    ; kind
                    ; caller_ret_annots }
              | _ ->
                  acc )
      | None ->
          base )
  | Load {exp; typ} ->
      Load {exp= resolve_exp ~extends defs exp; typ}
  | Field {exp; field} ->
      Field {exp= resolve_exp ~extends defs exp; field}
  | Index (e1, e2) ->
      Index (resolve_exp ~extends defs e1, resolve_exp ~extends defs e2)
  | Call {proc; args; kind; caller_ret_annots} ->
      Call {proc; args= List.map args ~f:(resolve_exp ~extends defs); kind; caller_ret_annots}
  | Apply {closure; args} ->
      Apply
        { closure= resolve_exp ~extends defs closure
        ; args= List.map args ~f:(resolve_exp ~extends defs) }
  | If {cond; then_; else_} ->
      If {cond; then_= resolve_exp ~extends defs then_; else_= resolve_exp ~extends defs else_}
  | Closure _ | Const _ | Lvar _ | Typ _ ->
      exp


(* Find the positional defaults tuple and the keyword-only defaults map passed to py_make_function
   for [target] in the enclosing scope. Returns (positional default exprs, keyword-only (name,
   default) pairs). The kwonly defaults come from py_make_function's third argument (kwdefaults),
   shaped as py_build_map(py_make_string k1, v1, py_make_string k2, v2, ...). *)
let find_make_function_defaults ?(extends = T.Ident.Map.empty) (module_body : T.ProcDesc.t)
    (target : T.QualifiedProcName.t) (defs : T.Exp.t T.Ident.Map.t) :
    (T.Exp.t list * (string * T.Exp.t) list) option =
  let parse_kwdefaults e =
    match e with
    | T.Exp.Call {proc; args} when is_py_builtin proc "py_build_map" ->
        let rec pairs = function
          | k :: v :: rest -> (
            match k with
            | T.Exp.Call {proc= p; args= [T.Exp.Const (Str name)]}
              when is_py_builtin p "py_make_string" ->
                (name, v) :: pairs rest
            | _ ->
                pairs rest )
          | _ ->
              []
        in
        pairs args
    | _ ->
        []
  in
  List.find_map module_body.nodes ~f:(fun (node : T.Node.t) ->
      List.find_map node.instrs ~f:(fun (instr : T.Instr.t) ->
          match instr with
          | Let {exp= Call {proc; args}; _} when is_py_builtin proc "py_make_function" -> (
            match args with
            | closure_arg :: defaults_arg :: rest -> (
              match resolve_exp ~extends defs closure_arg with
              | Closure {proc= cproc} when T.QualifiedProcName.equal cproc target ->
                  let pos =
                    match resolve_exp ~extends defs defaults_arg with
                    | Call {proc= tproc; args= elems} when is_py_builtin tproc "py_build_tuple" ->
                        elems
                    | _ ->
                        []
                  in
                  let kw =
                    match rest with
                    | kwd :: _ ->
                        parse_kwdefaults (resolve_exp ~extends defs kwd)
                    | _ ->
                        []
                  in
                  Some (pos, kw)
              | _ ->
                  None )
            | _ ->
                None )
          | _ ->
              None ) )


(* For procedure [proc] in [module_], return the map (parameter name -> default expression),
   restricted to parameters that actually have a default. Positional defaults bind to the last
   parameters. *)
let extract_defaults (module_ : T.Module.t) (proc : T.ProcDesc.t) : T.Exp.t StringMap.t =
  let params =
    List.find_map proc.procdecl.attributes ~f:T.Attr.find_python_args |> Option.value ~default:[]
  in
  let target = proc.procdecl.qualified_name in
  (* The py_make_function that bakes in [target]'s defaults lives in the enclosing scope: the
     __module_body__ for a top-level function, or the class body proc for a method. Scan every proc
     to find it (the class body is itself a RegularFunction, not the skipped __module_body__). *)
  let defaults =
    List.find_map module_.decls ~f:(function
      | T.Module.Proc p ->
          let defs = build_ident_def_map p in
          let extends = build_extend_map p in
          find_make_function_defaults ~extends p target defs
      | _ ->
          None )
  in
  match defaults with
  | None ->
      StringMap.empty
  | Some (pos, kw) ->
      (* Positional defaults bind to the last [k] positional parameters. *)
      let acc =
        let n = List.length params and k = List.length pos in
        if Int.equal k 0 || k > n then StringMap.empty
        else
          List.fold2_exn (List.drop params (n - k)) pos ~init:StringMap.empty
            ~f:(fun acc name d -> StringMap.add name d acc)
      in
      (* Keyword-only defaults are keyed by name (these params are not in [python_args], which lists
         only the positional co_argcount ones — hence they must be seeded here or a load_fast of them
         would miss). *)
      List.fold kw ~init:acc ~f:(fun acc (name, d) -> StringMap.add name d acc)


(* A class body proc is a definition-site scope, just like __module_body__: it constructs the class
   namespace, baking in the methods' default values and parameter type annotations (and
   __qualname__/__module__). A B006 codemod legitimately perturbs those (default [] -> None,
   annotation T -> Optional[T]), so comparing the class body would diverge spuriously. Identify class
   bodies as the py_make_function closure target passed as the first argument of py_build_class. *)
let find_class_body_procs (module_ : T.Module.t) : T.QualifiedProcName.t list =
  List.fold module_.decls ~init:[] ~f:(fun acc decl ->
      match decl with
      | T.Module.Proc p ->
          let defs = build_ident_def_map p in
          List.fold p.nodes ~init:acc ~f:(fun acc (node : T.Node.t) ->
              List.fold node.instrs ~init:acc ~f:(fun acc (instr : T.Instr.t) ->
                  match instr with
                  | Let {exp= Call {proc; args}; _} when is_py_builtin proc "py_build_class" -> (
                    match args with
                    | class_fn :: _ -> (
                      match resolve_exp defs class_fn with
                      | Call {proc= mfp; args= Closure {proc= cproc} :: _}
                        when is_py_builtin mfp "py_make_function" ->
                          cproc :: acc
                      | Closure {proc= cproc} ->
                          cproc :: acc
                      | _ ->
                          acc )
                    | _ ->
                        acc )
                  | _ ->
                      acc ) )
      | _ ->
          acc )


(* ---------- Main entry point ---------- *)

(* Model a parameter [p] that has a default expression [d] as
     @phi(@is_default(p), <d>, @arg(p))
   where @arg(p) is the explicitly-passed argument and @is_default(p) is true when the caller
   omitted the argument. This makes the calling convention explicit so that two functions differing
   only by their defaults (e.g. a B006 codemod) can be related. *)
let model_default_params (env : Env.t) (defaults : T.Exp.t StringMap.t) : Env.t =
  let cc = env.cc in
  StringMap.fold
    (fun name d (env : Env.t) ->
      let default_atom = convert_exp env d in
      let arg = mk_term cc "@arg" [mk_const cc name] in
      let is_default = mk_term cc "@is_default" [mk_const cc name] in
      let atom = mk_term cc "@phi" [is_default; default_atom; arg] in
      Equations.add env.equations ~name ~atom ~origin:"param-default" ;
      let locals = LocalsMap.store env.locals ~name ~atom in
      {env with locals} )
    defaults env


let convert_proc ?(theta_counter = ref 0) ?(defaults = StringMap.empty) cc (proc : T.ProcDesc.t) :
    (CC.Atom.t * Equations.t * int, string) result =
  let params =
    List.find_map proc.procdecl.attributes ~f:T.Attr.find_python_args |> Option.value ~default:[]
  in
  let env = Env.init cc ~params in
  let env = {env with theta_counter} in
  let env = model_default_params env defaults in
  let sir = S.of_cfg proc.nodes proc.start in
  match convert env sir with
  | Normal (result, env) ->
      Ok (result, env.equations, !(env.loop_counter))
  | Exit {atom= result; env; _} ->
      Ok (result, env.equations, !(env.loop_counter))
