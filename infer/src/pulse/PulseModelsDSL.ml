(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import
open PulseModelsImport

type astate = AbductiveDomain.t

type 'a result = 'a AccessResult.t

type aval = AbstractValue.t * ValueHistory.t

(* we work on a disjunction of executions *)
type 'a model_monad = model_data -> astate -> 'a execution result list

and 'a execution =
  | ContinueProgram of 'a * astate
  | Other of ExecutionDomain.t (* should never contain a ExecutionDomain.ContinueProgram *)

module Syntax = struct
  let ret (a : 'a) : 'a model_monad = fun _data astate -> [Ok (ContinueProgram (a, astate))]

  let unreachable : 'a model_monad = fun _ _ -> []

  let bind (x : 'a model_monad) (f : 'a -> 'b model_monad) : 'b model_monad =
   fun data astate ->
    List.concat_map (x data astate) ~f:(function
      | Ok (ContinueProgram (a, astate)) ->
          f a data astate
      | Recoverable (ContinueProgram (a, astate), err) ->
          f a data astate |> List.map ~f:(PulseResult.append_errors err)
      | Ok (Other (ContinueProgram _)) | Recoverable (Other (ContinueProgram _), _) ->
          L.die InternalError "DSL.Other should never contains ContinueProgram"
      | Ok (Other exec) ->
          [Ok (Other exec)]
      | Recoverable (Other exec, err) ->
          [Recoverable (Other exec, err)]
      | FatalError _ as err ->
          [err] )


  let ( let* ) a f = bind a f

  let list_fold (list : 'a list) ~(init : 'accum) ~(f : 'accum -> 'a -> 'accum model_monad) :
      'accum model_monad =
    List.fold list ~init:(ret init) ~f:(fun monad a ->
        let* acc = monad in
        f acc a )


  let list_iter (list : 'a list) ~(f : 'a -> unit model_monad) : unit model_monad =
    list_fold list ~init:() ~f:(fun () a -> f a)


  let list_filter_map (list : 'a list) ~(f : 'a -> 'b option model_monad) : 'b list model_monad =
    let* rev_res =
      list_fold list ~init:[] ~f:(fun acc a ->
          let* opt = f a in
          match opt with None -> ret acc | Some b -> b :: acc |> ret )
    in
    List.rev rev_res |> ret


  let option_iter (o : 'a option) ~(f : 'a -> unit model_monad) : unit model_monad =
    Option.value_map o ~default:(ret ()) ~f


  let get_data : model_data model_monad = fun data astate -> ret data data astate

  let ok x = Ok x

  let sat x = Sat x

  let ( >> ) f g x = f x |> g

  let start_model (monad : unit model_monad) : model =
   fun data astate ->
    List.map (monad data astate)
      ~f:
        (PulseResult.map ~f:(function
          | ContinueProgram ((), astate) ->
              ExecutionDomain.ContinueProgram astate
          | Other exec ->
              exec ) )


  let lift_to_monad (model : model) : unit model_monad =
   fun data astate ->
    List.map (model data astate)
      ~f:
        (PulseResult.map ~f:(function
          | ExecutionDomain.ContinueProgram astate ->
              ContinueProgram ((), astate)
          | exec ->
              Other exec ) )


  let disjuncts (list : 'a model_monad list) : 'a model_monad =
   fun data astate -> List.concat_map list ~f:(fun m -> m data astate)


  let exec_partial_command (f : astate -> astate PulseOperationResult.t) : unit model_monad =
   fun _data astate ->
    match f astate with
    | Unsat ->
        []
    | Sat res ->
        [PulseResult.map res ~f:(fun astate -> ContinueProgram ((), astate))]


  let exec_command (f : astate -> astate) : unit model_monad =
   fun _data astate -> [ok (ContinueProgram ((), f astate))]


  let exec_partial_operation (f : astate -> (astate * 'a) PulseOperationResult.t) : 'a model_monad =
   fun _data astate ->
    match f astate with
    | Unsat ->
        []
    | Sat res ->
        [PulseResult.map res ~f:(fun (astate, a) -> ContinueProgram (a, astate))]


  let exec_operation (f : astate -> 'a * astate) : 'a model_monad =
   fun data astate ->
    let a, astate = f astate in
    ret a data astate


  let exec_pure_operation (f : astate -> 'a) : 'a model_monad =
   fun data astate ->
    let a = f astate in
    ret a data astate


  let get_known_constant_opt (v, _) : Q.t option model_monad =
   fun data astate ->
    let phi = astate.path_condition in
    ret (Formula.get_known_constant_opt phi v) data astate


  let get_known_fields (v, _) =
    exec_pure_operation (fun astate ->
        let res =
          AbductiveDomain.Memory.fold_edges v astate ~init:[] ~f:(fun l (access, _) -> access :: l)
        in
        res )


  let assign_ret aval : unit model_monad =
    let* {ret= ret_id, _} = get_data in
    PulseOperations.write_id ret_id aval |> exec_command


  let read_ret : aval option model_monad =
    let* {ret= ret_id, _} = get_data in
    PulseOperations.read_id ret_id |> exec_pure_operation


  let remove_ret_binding : unit model_monad =
    let* {ret= ret_id, _} = get_data in
    AbductiveDomain.Stack.remove_vars [Var.of_id ret_id] |> exec_command


  let lift_to_monad_and_get_result (model : model) : aval model_monad =
    let* () = lift_to_monad model in
    let* res = read_ret in
    let* () = remove_ret_binding in
    match res with
    | None ->
        L.die InternalError "call_model_and_get_result: the model did not assign ret_id"
    | Some aval ->
        ret aval


  let eval_deref_access access_mode aval access : aval model_monad =
    let* {path; location} = get_data in
    PulseOperations.eval_deref_access path access_mode location aval access
    >> sat |> exec_partial_operation


  let add_dynamic_type typ (addr, _) : unit model_monad =
    PulseOperations.add_dynamic_type typ addr |> exec_command


  let get_dynamic_type ~ask_specialization (addr, _) :
      Attribute.dynamic_type_data option model_monad =
   fun data astate ->
    let res = AbductiveDomain.AddressAttributes.get_dynamic_type addr astate in
    let astate =
      if ask_specialization && Option.is_none res then
        AbductiveDomain.add_need_dynamic_type_specialization addr astate
      else astate
    in
    ret res data astate


  let and_eq_int (size_addr, _) i : unit model_monad =
    PulseArithmetic.and_eq_int size_addr i |> exec_partial_command


  (* and_eq v and_equal inconsistent naming *)
  let and_eq (x, _) (y, _) : unit model_monad =
    PulseArithmetic.and_equal (PulseArithmetic.AbstractValueOperand x)
      (PulseArithmetic.AbstractValueOperand y)
    |> exec_partial_command


  let and_positive (addr, _) : unit model_monad =
    PulseArithmetic.and_positive addr |> exec_partial_command


  let add_static_type typ_name (addr, _) : unit model_monad =
   fun ({analysis_data= {tenv}} as data) astate ->
    let astate = AbductiveDomain.AddressAttributes.add_static_type tenv typ_name addr astate in
    ret () data astate


  let get_const_string (addr, _) : string option model_monad =
    AddressAttributes.get_const_string addr |> exec_pure_operation


  let tenv_resolve_field_info typ_name field_name : Struct.field_info option model_monad =
   fun ({analysis_data= {tenv}} as data) astate ->
    let info = Tenv.resolve_field_info tenv typ_name field_name in
    ret info data astate


  let tenv_resolve_fieldname typ_name name : Fieldname.t option model_monad =
   fun ({analysis_data= {tenv}} as data) astate ->
    let field_name = Tenv.resolve_fieldname tenv typ_name name in
    ret field_name data astate


  let eval_read exp : aval model_monad =
    let* {path; location} = get_data in
    PulseOperations.eval path Read location exp |> exec_partial_operation


  let eval_to_value_origin exp : ValueOrigin.t model_monad =
    let* {path; location} = get_data in
    PulseOperations.eval_to_value_origin path Read location exp |> exec_partial_operation


  let allocation attr (addr, _) : unit model_monad =
    let* {location} = get_data in
    PulseOperations.allocate attr location addr |> exec_command


  let mk_fresh ~model_desc ?more () : aval model_monad =
    let* {path; location} = get_data in
    let addr = AbstractValue.mk_fresh () in
    let hist = Hist.single_call path location model_desc ?more in
    ret (addr, hist)


  let write_deref_field ~ref ~obj field : unit model_monad =
    let* {path; location} = get_data in
    PulseOperations.write_deref_field path location ~ref ~obj field >> sat |> exec_partial_command


  let write_deref ~ref ~obj : unit model_monad =
    let* {path; location} = get_data in
    PulseOperations.write_deref path location ~ref ~obj >> sat |> exec_partial_command


  (* slightly clumsy refactoring here: we unwrap a model to get a nice monadic type in the interface of this module
     but wrap that back up as a model in PulseModelsCpp *)
  let internal_new_ type_name : model =
   fun model_data astate ->
    let<++> astate =
      (* Java, Hack and C++ [new] share the same builtin (note that ObjC gets its own [objc_alloc_no_fail]
         builtin for [\[Class new\]]) *)
      let proc_name = Procdesc.get_proc_name model_data.analysis_data.proc_desc in
      if
        Procname.is_java proc_name || Procname.is_csharp proc_name || Procname.is_hack proc_name
        || Procname.is_python proc_name
      then
        Basic.alloc_no_leak_not_null ~initialize:true (Some type_name) ~desc:"new" model_data astate
      else
        (* C++ *)
        Basic.alloc_not_null ~initialize:true ~desc:"new" CppNew (Some type_name) model_data astate
    in
    astate


  let new_ type_name = lift_to_monad_and_get_result (internal_new_ type_name)

  let constructor type_name fields : aval model_monad =
    let exp =
      Exp.Sizeof
        {typ= Typ.mk_struct type_name; nbytes= None; dynamic_length= None; subtype= Subtype.exact}
    in
    let* new_obj = new_ exp in
    let* () =
      list_iter fields ~f:(fun (fieldname, obj) ->
          let field = Fieldname.make type_name fieldname in
          write_deref_field ~ref:new_obj field ~obj )
    in
    ret new_obj


  let deep_copy ?depth_max source : aval model_monad =
    let* {path; location} = get_data in
    PulseOperations.deep_copy ?depth_max path location source >> sat |> exec_partial_operation


  let aval_operand (addr, _) = PulseArithmetic.AbstractValueOperand addr

  let eval_binop binop (addr1, hist1) (addr2, hist2) : aval model_monad =
    let* {path} = get_data in
    let addr_res = AbstractValue.mk_fresh () in
    let hist_res = Hist.binop path binop hist1 hist2 in
    let* addr_res =
      PulseArithmetic.eval_binop addr_res binop (AbstractValueOperand addr1)
        (AbstractValueOperand addr2)
      |> exec_partial_operation
    in
    ret (addr_res, hist_res)


  let eval_binop_int binop (arg, hist) i : aval model_monad =
    let addr_res = AbstractValue.mk_fresh () in
    let* addr_res =
      PulseArithmetic.eval_binop addr_res binop (AbstractValueOperand arg)
        (PulseArithmetic.ConstOperand (Cint i))
      |> exec_partial_operation
    in
    ret (addr_res, hist)


  let prune_binop ~negated binop operand1 operand2 =
    PulseArithmetic.prune_binop ~negated binop operand1 operand2 |> exec_partial_command


  let prune_eq_int arg i : unit model_monad =
    prune_binop ~negated:false Binop.Eq (aval_operand arg) (PulseArithmetic.ConstOperand (Cint i))


  let prune_eq_zero (addr, _) : unit model_monad =
    PulseArithmetic.prune_eq_zero addr |> exec_partial_command


  let prune_positive (addr, _) : unit model_monad =
    PulseArithmetic.prune_positive addr |> exec_partial_command


  let prune_ne_int arg i : unit model_monad =
    prune_binop ~negated:true Binop.Eq (aval_operand arg) (PulseArithmetic.ConstOperand (Cint i))


  let prune_ne_zero (addr, _) : unit model_monad =
    PulseArithmetic.prune_ne_zero addr |> exec_partial_command


  let prune_lt arg1 arg2 : unit model_monad =
    prune_binop ~negated:false Binop.Lt (aval_operand arg1) (aval_operand arg2)


  let prune_lt_int arg1 i : unit model_monad =
    prune_binop ~negated:false Binop.Lt (aval_operand arg1) (PulseArithmetic.ConstOperand (Cint i))


  let prune_ge arg1 arg2 : unit model_monad =
    prune_binop ~negated:true Binop.Lt (aval_operand arg1) (aval_operand arg2)


  let prune_ge_int arg1 i : unit model_monad =
    prune_binop ~negated:true Binop.Lt (aval_operand arg1) (PulseArithmetic.ConstOperand (Cint i))


  let prune_gt arg1 arg2 = prune_lt arg2 arg1

  let prune_le arg1 arg2 = prune_ge arg2 arg1

  let prune_eq arg1 arg2 : unit model_monad =
    prune_binop ~negated:false Binop.Eq (aval_operand arg1) (aval_operand arg2)


  let prune_ne arg1 arg2 : unit model_monad =
    prune_binop ~negated:true Binop.Eq (aval_operand arg1) (aval_operand arg2)


  let dynamic_dispatch ~(cases : (Typ.name * (unit -> 'a model_monad)) list)
      ?(default : (unit -> 'a model_monad) option) aval : 'a model_monad =
    let* opt_dynamic_type_data = get_dynamic_type ~ask_specialization:true aval in
    match opt_dynamic_type_data with
    | Some {Attribute.typ= {Typ.desc= Tstruct type_name}} -> (
      match (List.find cases ~f:(fun case -> fst case |> Typ.Name.equal type_name), default) with
      | Some (_, case_fun), _ ->
          Logging.d_printfln
            "[ocaml model] dynamic_dispatch: executing case for type %a on value %a" Typ.Name.pp
            type_name AbstractValue.pp (fst aval) ;
          case_fun ()
      | None, Some default ->
          default ()
      | None, None ->
          Logging.d_printfln "[ocaml model] dynamic_dispatch: no case for type %a" Typ.Name.pp
            type_name ;
          unreachable )
    | _ ->
        Logging.d_printfln "[ocaml model] No dynamic type found for value %a!" AbstractValue.pp
          (fst aval) ;
        Option.value_map default ~default:unreachable ~f:(fun f -> f ())


  let dispatch_call ret pname actuals func_args : unit model_monad =
    lift_to_monad
    @@ fun {analysis_data; dispatch_call_eval_args; path; location} astate ->
    dispatch_call_eval_args analysis_data path ret (Const (Cfun pname)) actuals func_args location
      CallFlags.default astate (Some pname)


  module Basic = struct
    (* See internal_new_. We do some crafty unboxing to make the external API nicer *)
    let alloc_not_null ?desc allocator size ~initialize : unit model_monad =
      let model_ model_data astate =
        let<++> astate = Basic.alloc_not_null ?desc allocator size ~initialize model_data astate in
        astate
      in
      lift_to_monad model_
  end
end

let unsafe_to_astate_transformer (monad : 'a model_monad) :
    model_data -> astate -> ('a * astate) sat_unsat_t =
 fun data astate ->
  match monad data astate with
  | [res] ->
      PulseResult.ok res
      |> Option.value_map ~default:Unsat ~f:(function
           | ContinueProgram (a, astate) ->
               Sat (a, astate)
           | _ ->
               Unsat )
  | [] ->
      Unsat
  | _ ->
      Unsat
