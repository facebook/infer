(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
open PyIR

let todo msg = L.die InternalError "TODO: %s" msg

module GenericUnsafeHashtbl (H : Stdlib.Hashtbl.S) = struct
  let get hashtbl mk_error_msg key =
    match H.find_opt hashtbl key with
    | Some v ->
        v
    | None ->
        L.die L.InternalError "%s" (mk_error_msg key)


  let get_opt hashtbl key = H.find_opt hashtbl key

  let set hashtbl key v = H.replace hashtbl key v

  let bindings hashtbl = H.to_seq hashtbl |> ListLabels.of_seq

  type 'a t =
    { get: H.key -> 'a
    ; get_opt: H.key -> 'a option
    ; set: H.key -> 'a -> unit
    ; bindings: unit -> (H.key * 'a) list }

  let create ?mk_error_msg () =
    let hashtbl = H.create 17 in
    let mk_error_msg =
      Option.value mk_error_msg ~default:(fun _ident -> "key not found in anonymous dictionnary")
    in
    let get = get hashtbl mk_error_msg in
    let get_opt = get_opt hashtbl in
    let set = set hashtbl in
    let bindings () = bindings hashtbl in
    {get; get_opt; set; bindings}
end

module SSAEnv = GenericUnsafeHashtbl (SSA.Hashtbl)
module Dict = GenericUnsafeHashtbl (Ident.Hashtbl)
(* Note: Python dicts can have non-string keys. We simplify here *)

type pval =
  (* very simple for now *)
  | None
  | Bool of bool
  | Int of Z.t
  | String of string
  | Closure of (?locals:pval Dict.t -> pval list -> pval)
  | Dict of pval Dict.t
  | Tuple of pval list

let pp_pval fmt pval =
  let rec aux ~inside_collection fmt = function
    | None ->
        F.pp_print_string fmt "None"
    | Bool true ->
        F.pp_print_string fmt "True"
    | Bool false ->
        F.pp_print_string fmt "False"
    | Int i ->
        F.pp_print_string fmt (Z.to_string i)
    | String s when inside_collection ->
        F.fprintf fmt "'%s'" s
    | String s ->
        F.pp_print_string fmt s
    | Closure _ ->
        F.pp_print_string fmt "Closure"
    | Dict {bindings} ->
        (* Note: we do our best to mimic Python output, but this is fragile *)
        F.fprintf fmt "{%a}"
          (Pp.seq ~sep:", " (fun fmt (key, v) ->
               F.fprintf fmt "'%a': %a" Ident.pp key (aux ~inside_collection:true) v ) )
          (bindings () |> List.sort ~compare:(fun (key1, _) (key2, _) -> Ident.compare key1 key2))
    | Tuple l ->
        F.fprintf fmt "(%a)" (Pp.seq ~sep:", " (aux ~inside_collection:true)) l
  in
  aux ~inside_collection:false fmt pval


let expect_int ~who ?how = function
  | Int i ->
      i
  | v ->
      L.die InternalError "%s expects an integer%a and received %a" who
        (Pp.option (fun fmt how -> F.fprintf fmt " as %s" how))
        how pp_pval v


let expect_string ~who ?how = function
  | String s ->
      s
  | v ->
      L.die InternalError "%s expects an string%a and received %a" who
        (Pp.option (fun fmt how -> F.fprintf fmt " as %s" how))
        how pp_pval v


let expect_dict ~who ?how = function
  | Dict d ->
      d
  | v ->
      L.die InternalError "%s expects a dictionnary%a and received %a" who
        (Pp.option (fun fmt how -> F.fprintf fmt " as %s" how))
        how pp_pval v


let expect_tuple ~who ?how = function
  | Tuple l ->
      l
  | v ->
      L.die InternalError "%s expects a tuple%a and received %a" who
        (Pp.option (fun fmt how -> F.fprintf fmt " as %s" how))
        how pp_pval v


let expect_bool ~who ?how = function
  | Bool b ->
      b
  | v ->
      L.die InternalError "%s expects a bool%a and received %a" who
        (Pp.option (fun fmt how -> F.fprintf fmt " as %s" how))
        how pp_pval v


let expect_2_args ~who = function
  | [arg1; arg2] ->
      (arg1, arg2)
  | args ->
      L.die InternalError "%s expects 2 args and reveiced [%a]" who (Pp.comma_seq Exp.pp) args


let expect_at_least_2_args ~who = function
  | arg1 :: arg2 :: args ->
      (arg1, arg2, args)
  | args ->
      L.die InternalError "%s expects at least 2 args and reveiced [%a]" who (Pp.comma_seq Exp.pp)
        args


let expect_closure ~who = function
  | Closure f ->
      f
  | v ->
      L.die InternalError "%s expects a closure and received %a" who pp_pval v


module Builtin = struct
  let print ?locals:_ args =
    F.printf "%a@\n" (Pp.seq ~sep:" " pp_pval) args ;
    None


  let mk_builtins_getter () =
    let mk_error_msg ident = F.asprintf "builtin %a not found" Ident.pp ident in
    let {Dict.get; set} = Dict.create () ~mk_error_msg in
    set Ident.Special.print (Closure print) ;
    get
end

module Globals = struct
  let mk_globals ~name =
    let mk_error_msg ident =
      F.asprintf "in module %a, global variable %a is not bind to any value" Ident.pp name Ident.pp
        ident
    in
    Dict.create ~mk_error_msg ()
end

module Locals = struct
  let mk_locals ~co_name ~co_varnames ~co_argcount args =
    let ({Dict.set} as dict) =
      let mk_error_msg ident =
        F.asprintf "in cfg %a, local variable %a is not bind to any value" Ident.pp co_name Ident.pp
          ident
      in
      Dict.create ~mk_error_msg ()
    in
    let args_length = List.length args in
    if not (Int.equal args_length co_argcount) then
      (* TODO: deal with optionals arguments *)
      L.die InternalError "In cfg %a, %d arguments are expected but %d are given" Ident.pp co_name
        args_length co_argcount ;
    List.iteri args ~f:(fun i arg ->
        let arg_name = co_varnames.(i) in
        set arg_name arg ) ;
    dict


  let mk_raw_object name =
    let ({Dict.set} as dict) =
      let mk_error_msg ident =
        F.asprintf "key %a is not bind to any value in object %a" Ident.pp ident pp_pval name
      in
      Dict.create ~mk_error_msg ()
    in
    set Ident.Special.name name ;
    dict
end

module Modules = struct
  type status = NotImportedYet of CFG.t | AlreadyImported of pval

  type t =
    { get_module_status: Ident.t -> status
    ; mark_as_initialized: Ident.t -> pval -> unit
    ; get_cfg: QualName.t -> CFG.t }

  let mk units =
    let bodies = Ident.Hashtbl.create 17 in
    let cfgs =
      List.fold units ~init:QualName.Map.empty ~f:(fun cfgs {Module.name; toplevel; functions} ->
          Ident.Hashtbl.replace bodies name (NotImportedYet toplevel) ;
          QualName.Map.union
            (fun qual_name _ _ ->
              L.die InternalError "function %a is implemented twice" QualName.pp qual_name )
            cfgs functions )
    in
    let get_cfg qual_name =
      QualName.Map.find_opt qual_name cfgs
      |> Option.value_or_thunk ~default:(fun () ->
             L.die InternalError "no cfg with name %a" QualName.pp qual_name )
    in
    let get_module_status module_name =
      Ident.Hashtbl.find_opt bodies module_name
      |> Option.value_or_thunk ~default:(fun () ->
             L.die InternalError "import: no module body with name %a" Ident.pp module_name )
    in
    let mark_as_initialized module_name value =
      if not (Ident.Hashtbl.mem bodies module_name) then
        L.die InternalError "module %a can not be marked as initialized without any body" Ident.pp
          module_name ;
      Ident.Hashtbl.replace bodies module_name (AlreadyImported value)
    in
    {get_module_status; get_cfg; mark_as_initialized}
end

let run_files modules =
  let builtins_get = Builtin.mk_builtins_getter () in
  let {Modules.get_cfg; get_module_status; mark_as_initialized} = Modules.mk modules in
  let rec exec_module name =
    let ({Dict.get_opt= globals_get_opt; set= globals_set} as globals) = Globals.mk_globals ~name in
    let rec exec_cfg {CFG.entry; nodes; code_info} ?locals args =
      let {CodeInfo.co_name; co_varnames; co_argcount} = code_info in
      let get_node node_name =
        NodeName.Map.find_opt node_name nodes
        |> Option.value_or_thunk ~default:(fun () ->
               L.die InternalError "exec_cfg: in cfg %a, no node with name %a" Ident.pp co_name
                 NodeName.pp node_name )
      in
      let entry_node = get_node entry in
      let {SSAEnv.get= ssa_get; set= ssa_set} =
        let mk_error_msg ssa =
          F.asprintf "in cfg %a, SSA variable %a is not bind to any value" Ident.pp co_name SSA.pp
            ssa
        in
        SSAEnv.create ~mk_error_msg ()
      in
      let {Dict.get_opt= locals_get_opt; get= locals_get; set= locals_set} =
        Option.value_or_thunk locals ~default:(fun () ->
            Locals.mk_locals ~co_name ~co_varnames ~co_argcount args )
      in
      let eval_const const =
        match (const : Const.t) with
        | None ->
            None
        | Bool b ->
            Bool b
        | Int i ->
            Int i
        | String s ->
            String s
        | Float _ | Complex _ | InvalidUnicode | Bytes _ ->
            (* I don't think it makes sense to deal with this kind of constant in the interpreter *)
            todo "eval_const"
      in
      let rec eval_exp exp =
        match (exp : Exp.t) with
        | Const const ->
            eval_const const
        | Var {scope= Fast; ident} ->
            locals_get ident
        | Var {scope= Global; ident} ->
            globals_get_opt ident |> Option.value_or_thunk ~default:(fun () -> builtins_get ident)
        | Var {scope= Name; ident} ->
            locals_get_opt ident
            |> Option.value_or_thunk ~default:(fun () ->
                   globals_get_opt ident
                   |> Option.value_or_thunk ~default:(fun () -> builtins_get ident) )
        | Temp ssa ->
            ssa_get ssa
        | ImportName {name} ->
            exec_module name
        | ImportFrom {name; exp} ->
            let who = "$BuiltinCall.ImportFrom" in
            let {Dict.get} = eval_exp exp |> expect_dict ~who in
            get name
        | GetAttr {exp; attr} ->
            (* TODO: implement more realistic attribute lookup *)
            let {Dict.get} = eval_exp exp |> expect_dict ~who:"GetAttr" in
            get attr
        | Collection {kind= Tuple; values} ->
            Tuple (List.map ~f:eval_exp values)
        | Collection {kind= Map; values} ->
            let ({Dict.set} as dict) = Dict.create () in
            let values = List.map ~f:eval_exp values in
            if Int.equal (List.length values mod 2) 1 then
              L.die InternalError "$BuildMap expects a even number of arguments" ;
            let bindings = List.chunks_of ~length:2 values in
            let who = "$BuildMap" in
            List.iter bindings ~f:(function
              | [key; arg] ->
                  let key = expect_string ~who ~how:"as key" key |> Ident.mk in
                  set key arg
              | _ ->
                  () ) ;
            Dict dict
        | Function {qual_name} ->
            let cfg = get_cfg qual_name in
            let eval = exec_cfg cfg in
            Closure eval
        | Subscript {exp; index} ->
            let who = "Subscript" in
            let {Dict.get} = eval_exp exp |> expect_dict ~who ~how:"as 1st argument" in
            (* Note: Python dictionnaries may have other type of keys *)
            let index = eval_exp index |> expect_string ~who ~how:"as index" |> Ident.mk in
            get index
        | AssertionError
        | BuildSlice _
        | BuildString _
        | BuildFrozenSet _
        | MatchClass _
        | BoolOfMatchClass _
        | AttributesOfMatchClass _
        | MatchSequence _
        | GetLen _
        | Collection _
        | LoadClosure _
        | LoadDeref _
        | LoadClassDeref _
        | LoadFastCheck _
        | LoadFastAndClear _
        | LoadLocals
        | LoadFromDictOrDeref _
        | LoadSuperAttr _ ->
            todo "eval_exp"
      in
      let exec_stmt stmt =
        match (stmt : Stmt.t) with
        | Let {lhs; rhs} ->
            ssa_set lhs (eval_exp rhs)
        | Store {lhs= {scope= Fast; ident}; rhs} ->
            locals_set ident (eval_exp rhs)
        | Store {lhs= {scope= Name; ident}; rhs} ->
            (* Note 1: inside module body, globals = locals.
               See https://tenthousandmeters.com/blog/python-behind-the-scenes-5-how-variables-are-implemented-in-cpython/
               >> module's f_locals and module's f_globals is the same thing *)
            (* Note 2: inside class body, locals = class object under construction *)
            locals_set ident (eval_exp rhs)
        | Store {lhs= {scope= Global; ident}; rhs} ->
            globals_set ident (eval_exp rhs)
        | Call {lhs; exp; args} ->
            let f = expect_closure ~who:"Call" (eval_exp exp) in
            let args = List.map ~f:eval_exp args in
            ssa_set lhs (f args)
        | BuiltinCall {lhs; call= BuildClass; args} ->
            let who = "$BuildClass" in
            let body, name, _args = expect_at_least_2_args ~who args in
            let body = eval_exp body |> expect_closure ~who in
            let name = eval_exp name in
            let locals = Locals.mk_raw_object name in
            body ~locals [] |> ignore ;
            ssa_set lhs (Dict locals)
        | BuiltinCall {lhs; call= BuildConstKeyMap; args} ->
            let who = "$BuildConstKeyMap" in
            let keys, arg0, args = expect_at_least_2_args ~who args in
            let args = List.map (arg0 :: args) ~f:eval_exp in
            let keys = eval_exp keys |> expect_tuple ~who ~how:"as first argument" in
            if not (Int.equal (List.length args) (List.length keys)) then
              L.die InternalError "$BuildConstKeyMap keys and values shouds have the same length" ;
            let ({Dict.set} as dict) = Dict.create () in
            List.iter2_exn keys args ~f:(fun key arg ->
                let key = expect_string ~who ~how:"as key" key |> Ident.mk in
                set key arg ) ;
            ssa_set lhs (Dict dict)
        | BuiltinCall {lhs; call= Inplace Add; args} ->
            let who = "$BuiltinCall.Inplace.Add" in
            let arg1, arg2 = expect_2_args ~who args in
            let i1 = eval_exp arg1 |> expect_int ~who ~how:"as first argument" in
            let i2 = eval_exp arg2 |> expect_int ~who ~how:"as second argument" in
            ssa_set lhs (Int (Z.add i1 i2))
        | BuiltinCall {lhs; call= Compare Le; args} ->
            let who = "$BuiltinCall.Compare.Le" in
            let arg1, arg2 = expect_2_args ~who args in
            let i1 = eval_exp arg1 |> expect_int ~who ~how:"as first argument" in
            let i2 = eval_exp arg2 |> expect_int ~who ~how:"as second argument" in
            ssa_set lhs (Bool (Z.leq i1 i2))
        | BuiltinCall {lhs; call= Binary Subtract; args} ->
            let who = "$BuiltinCall.Binary.Subtract" in
            let arg1, arg2 = expect_2_args ~who args in
            let i1 = eval_exp arg1 |> expect_int ~who ~how:"as first argument" in
            let i2 = eval_exp arg2 |> expect_int ~who ~how:"as second argument" in
            ssa_set lhs (Int (Z.sub i1 i2))
        | BuiltinCall {lhs; call= Binary Multiply; args} ->
            let who = "$BuiltinCall.Binary.Subtract" in
            let arg1, arg2 = expect_2_args ~who args in
            let i1 = eval_exp arg1 |> expect_int ~who ~how:"as first argument" in
            let i2 = eval_exp arg2 |> expect_int ~who ~how:"as second argument" in
            ssa_set lhs (Int (Z.mul i1 i2))
        | SetAttr {lhs; attr; rhs} ->
            let {Dict.set} = eval_exp lhs |> expect_dict ~who:"SetAttr" in
            set attr (eval_exp rhs)
        | CallMethod {lhs; name; self_if_needed; args} ->
            let {Dict.get} = eval_exp self_if_needed |> expect_dict ~who:"CallMethod" in
            (* TODO: bind the self parameter for bounded methods *)
            let f = expect_closure ~who:"CallMethod" (get name) in
            let args = List.map ~f:eval_exp args in
            ssa_set lhs (f args)
        | StoreSubscript {lhs; index; rhs} ->
            let who = "StoreSubscript" in
            let {Dict.set} = eval_exp lhs |> expect_dict ~who ~how:"as 1st argument" in
            let key = eval_exp index |> expect_string ~who ~how:"as 2nd argument" |> Ident.mk in
            set key (eval_exp rhs)
        | BuiltinCall _
        | CallEx _
        | Delete _
        | DeleteDeref _
        | DeleteAttr _
        | StoreDeref _
        | StoreSlice _
        | SetupAnnotations
        | ImportStar _
        | GenStart _
        | MakeCell _
        | CopyFreeVars _
        | Yield _ ->
            todo "exec_stmt"
      in
      let rec exec_terminator terminator =
        match (terminator : Terminator.t) with
        | Return exp ->
            eval_exp exp
        | If {exp; then_; else_} ->
            let test = eval_exp exp |> expect_bool ~who:"If terminator" in
            if test then exec_node_call then_ else exec_node_call else_
        | Jump node_call ->
            exec_node_call node_call
        | _ ->
            todo "exec_terminator"
      and exec_node_call {Terminator.label; ssa_args} =
        let node = get_node label in
        let args = List.map ~f:eval_exp ssa_args in
        exec_node node args
      and exec_node {Node.ssa_parameters; stmts; last} args =
        List.iter2_exn ssa_parameters args ~f:(fun ssa v -> ssa_set ssa v) ;
        List.iter stmts ~f:(fun (_loc, stmt) -> exec_stmt stmt) ;
        exec_terminator last
      in
      exec_node entry_node []
    in
    match get_module_status name with
    | NotImportedYet cfg ->
        let result = Dict globals in
        mark_as_initialized name result ;
        exec_cfg ~locals:globals cfg [] |> ignore ;
        result
    | AlreadyImported v ->
        v
  in
  match modules with
  | [] ->
      F.printf "nothing to execute@\n"
  | {Module.name} :: _ ->
      exec_module name |> ignore


let run module_ = run_files [module_]
