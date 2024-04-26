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

type non_disj = NonDisjDomain.t

type 'a result = 'a AccessResult.t

type aval = AbstractValue.t * ValueHistory.t

(* we work on a disjunction of executions *)
type 'a model_monad = model_data -> astate -> non_disj -> 'a execution result list * non_disj

and 'a execution =
  | ContinueProgram of 'a * astate
  | Other of ExecutionDomain.t (* should never contain a ExecutionDomain.ContinueProgram *)

module Syntax = struct
  module ModeledField = PulseOperations.ModeledField

  let ret (a : 'a) : 'a model_monad =
   fun _data astate non_disj -> ([Ok (ContinueProgram (a, astate))], non_disj)


  let throw : unit model_monad =
   fun _data astate non_disj -> ([Ok (Other (ExceptionRaised astate))], non_disj)


  let unreachable : 'a model_monad = fun _ _ _ -> ([], NonDisjDomain.bottom)

  let bind (x : 'a model_monad) (f : 'a -> 'b model_monad) : 'b model_monad =
   fun data astate non_disj ->
    x data astate non_disj
    |> NonDisjDomain.bind ~f:(fun exec non_disj ->
           match exec with
           | Ok (ContinueProgram (a, astate)) ->
               f a data astate non_disj
           | Recoverable (ContinueProgram (a, astate), err) ->
               let execs, non_disj = f a data astate non_disj in
               let execs = List.map ~f:(PulseResult.append_errors err) execs in
               (execs, non_disj)
           | Ok (Other (ContinueProgram _)) | Recoverable (Other (ContinueProgram _), _) ->
               L.die InternalError "DSL.Other should never contains ContinueProgram"
           | Ok (Other exec) ->
               ([Ok (Other exec)], non_disj)
           | Recoverable (Other exec, err) ->
               ([Recoverable (Other exec, err)], non_disj)
           | FatalError _ as err ->
               ([err], non_disj) )


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


  (* TODO: this isn't quite as general as one might like, could put a functor in here *)
  let absvalue_set_fold (s : AbstractValue.Set.t) ~(init : 'accum)
      ~(f : 'accum -> AbstractValue.t -> 'accum model_monad) : 'accum model_monad =
    AbstractValue.Set.fold
      (fun a comp ->
        let* acc = comp in
        f acc a )
      s (ret init)


  let absvalue_set_iter (s : AbstractValue.Set.t) ~(f : AbstractValue.t -> unit model_monad) :
      unit model_monad =
    absvalue_set_fold s ~init:() ~f:(fun () a -> f a)


  let ignore (m : 'a model_monad) : unit model_monad =
    let* _ = m in
    ret ()


  let get_data : model_data model_monad = fun data astate -> ret data data astate

  let ok x = Ok x

  let sat x = Sat x

  let ( >> ) f g x = f x |> g

  let start_model (monad : unit model_monad) : model =
   fun data astate non_disj ->
    let execs, non_disj = monad data astate non_disj in
    ( List.map execs
        ~f:
          (PulseResult.map ~f:(function
            | ContinueProgram ((), astate) ->
                ExecutionDomain.ContinueProgram astate
            | Other exec ->
                exec ) )
    , non_disj )


  let lift_to_monad (model : model) : unit model_monad =
   fun data astate non_disj ->
    let execs, non_disj = model data astate non_disj in
    ( List.map execs
        ~f:
          (PulseResult.map ~f:(function
            | ExecutionDomain.ContinueProgram astate ->
                ContinueProgram ((), astate)
            | exec ->
                Other exec ) )
    , non_disj )


  let disjuncts (list : 'a model_monad list) : 'a model_monad =
   fun data astate non_disj ->
    NonDisjDomain.bind (list, non_disj) ~f:(fun monad non_disj -> monad data astate non_disj)


  let exec_partial_command (f : astate -> astate PulseOperationResult.t) : unit model_monad =
   fun _data astate non_disj ->
    match f astate with
    | Unsat ->
        ([], non_disj)
    | Sat res ->
        ([PulseResult.map res ~f:(fun astate -> ContinueProgram ((), astate))], non_disj)


  let exec_command (f : astate -> astate) : unit model_monad =
   fun _data astate non_disj -> ([ok (ContinueProgram ((), f astate))], non_disj)


  let exec_partial_operation (f : astate -> (astate * 'a) PulseOperationResult.t) : 'a model_monad =
   fun _data astate non_disj ->
    match f astate with
    | Unsat ->
        ([], non_disj)
    | Sat res ->
        ([PulseResult.map res ~f:(fun (astate, a) -> ContinueProgram (a, astate))], non_disj)


  let exec_operation (f : astate -> 'a * astate) : 'a model_monad =
   fun data astate ->
    let a, astate = f astate in
    ret a data astate


  let exec_pure_operation (f : astate -> 'a) : 'a model_monad =
   fun data astate ->
    let a = f astate in
    ret a data astate


  let as_constant_q (v, _) : Q.t option model_monad =
   fun data astate ->
    let phi = astate.path_condition in
    ret (Formula.as_constant_q phi v) data astate


  let as_constant_int v : int option model_monad =
    let* n_q_opt = as_constant_q v in
    let n = Option.bind n_q_opt ~f:QSafeCapped.to_int in
    ret n


  let as_constant_string (v, _) : string option model_monad =
   fun data astate ->
    let phi = astate.path_condition in
    ret (Formula.as_constant_string phi v) data astate


  let aval_of_int hist i : aval model_monad =
   fun data astate ->
    let astate, v = PulseArithmetic.absval_of_int astate (IntLit.of_int i) in
    ret (v, hist) data astate


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


  let eval_access ?desc access_mode aval access : aval model_monad =
    let* {path; location} = get_data in
    let* addr, hist =
      PulseOperations.eval_access path access_mode location aval access
      >> sat |> exec_partial_operation
    in
    let hist =
      Option.value_map desc ~default:hist ~f:(fun desc -> Hist.add_call path location desc hist)
    in
    ret (addr, hist)


  let eval_deref_access access_mode aval access : aval model_monad =
    let* {path; location} = get_data in
    PulseOperations.eval_deref_access path access_mode location aval access
    >> sat |> exec_partial_operation


  let add_dict_contain_const_keys (addr, _) : unit model_monad =
    PulseOperations.add_dict_contain_const_keys addr |> exec_command


  let add_dict_read_const_key (addr, history) key : unit model_monad =
    let* {path= {timestamp}; location} = get_data in
    PulseOperations.add_dict_read_const_key timestamp (Immediate {location; history}) addr key
    >> sat |> exec_partial_command


  let remove_dict_contain_const_keys (addr, _) : unit model_monad =
    PulseOperations.remove_dict_contain_const_keys addr |> exec_command


  let and_dynamic_type_is (v, _) t : unit model_monad =
    PulseArithmetic.and_dynamic_type_is v t |> exec_partial_command


  let get_dynamic_type ~ask_specialization (addr, _) : Formula.dynamic_type_data option model_monad
      =
   fun data astate ->
    let res = PulseArithmetic.get_dynamic_type addr astate in
    let astate =
      if ask_specialization && Option.is_none res then
        AbductiveDomain.add_need_dynamic_type_specialization addr astate
      else astate
    in
    ret res data astate


  let and_eq_int (v, _) i : unit model_monad =
    PulseArithmetic.and_eq_int v i |> exec_partial_command


  (* and_eq v and_equal inconsistent naming *)
  let and_eq (x, _) (y, _) : unit model_monad =
    PulseArithmetic.and_equal (AbstractValueOperand x) (AbstractValueOperand y)
    |> exec_partial_command


  let and_equal_instanceof (res, _) (obj, _) ty : unit model_monad =
    PulseArithmetic.and_equal_instanceof res obj ty |> exec_partial_command


  let and_positive (addr, _) : unit model_monad =
    PulseArithmetic.and_positive addr |> exec_partial_command


  let add_static_type typ_name (addr, _) : unit model_monad =
   fun ({analysis_data= {tenv}; location} as data) astate ->
    let astate =
      AbductiveDomain.AddressAttributes.add_static_type tenv typ_name addr location astate
    in
    ret () data astate


  let get_const_string (v, _) : string option model_monad =
   fun data astate -> ret (PulseArithmetic.as_constant_string astate v) data astate


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


  let eval_const_int i : aval model_monad = eval_read (Const (Cint (IntLit.of_int i)))

  let eval_const_string str : aval model_monad = eval_read (Const (Cstr str))

  let eval_string_concat (v1, hist1) (v2, hist2) : aval model_monad =
    let v = AbstractValue.mk_fresh () in
    let hist = ValueHistory.binary_op (PlusA None) hist1 hist2 in
    let* () =
      exec_partial_command (fun astate ->
          PulseArithmetic.and_equal_string_concat v (AbstractValueOperand v1)
            (AbstractValueOperand v2) astate )
    in
    ret (v, hist)


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


  let write_field ~ref ~obj field : unit model_monad =
    let* {path; location} = get_data in
    PulseOperations.write_field path location ~ref ~obj field >> sat |> exec_partial_command


  let write_deref_field ~ref ~obj field : unit model_monad =
    let* {path; location} = get_data in
    PulseOperations.write_deref_field path location ~ref ~obj field >> sat |> exec_partial_command


  let write_deref ~ref ~obj : unit model_monad =
    let* {path; location} = get_data in
    PulseOperations.write_deref path location ~ref ~obj >> sat |> exec_partial_command


  (* slightly clumsy refactoring here: we unwrap a model to get a nice monadic type in the interface of this module
     but wrap that back up as a model in PulseModelsCpp *)
  let internal_new_ type_name : model =
    (fun model_data astate ->
      let<++> astate =
        (* Java, Hack and C++ [new] share the same builtin (note that ObjC gets its own [objc_alloc_no_fail]
           builtin for [\[Class new\]]) *)
        let proc_name = Procdesc.get_proc_name model_data.analysis_data.proc_desc in
        if
          Procname.is_java proc_name || Procname.is_csharp proc_name || Procname.is_hack proc_name
          || Procname.is_python proc_name
        then
          Basic.alloc_no_leak_not_null ~initialize:true (Some type_name) ~desc:"new" model_data
            astate
        else
          (* C++ *)
          Basic.alloc_not_null ~initialize:true ~desc:"new" CppNew (Some type_name) model_data
            astate
      in
      astate )
    |> lift_model


  let new_ type_name_exp =
    let* new_obj = lift_to_monad_and_get_result (internal_new_ type_name_exp) in
    match type_name_exp with
    | Exp.Sizeof {typ} ->
        let* () = and_dynamic_type_is new_obj typ in
        ret new_obj
    | _ ->
        unreachable


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
    prune_binop ~negated:false Eq (aval_operand arg) (PulseArithmetic.ConstOperand (Cint i))


  let prune_eq_string (v, _) s : unit model_monad =
    PulseArithmetic.prune_binop ~negated:false Eq (AbstractValueOperand v) (ConstOperand (Cstr s))
    |> exec_partial_command


  let prune_ne_string (v, _) s : unit model_monad =
    PulseArithmetic.prune_binop ~negated:false Ne (AbstractValueOperand v) (ConstOperand (Cstr s))
    |> exec_partial_command


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


  let invalidate_access cause ref_addr_hist access : unit model_monad =
    let* {path; location} = get_data in
    PulseOperations.invalidate_access path location cause ref_addr_hist access |> exec_command


  let dynamic_dispatch ~(cases : (Typ.name * (unit -> 'a model_monad)) list)
      ?(default : (unit -> 'a model_monad) option) aval : 'a model_monad =
    let* opt_dynamic_type_data = get_dynamic_type ~ask_specialization:true aval in
    match opt_dynamic_type_data with
    | Some {Formula.typ= {Typ.desc= Tstruct type_name}} -> (
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
    @@ fun {analysis_data; dispatch_call_eval_args; path; location} astate non_disj ->
    dispatch_call_eval_args analysis_data path ret (Const (Cfun pname)) actuals func_args location
      CallFlags.default astate non_disj (Some pname)


  module Basic = struct
    (* See internal_new_. We do some crafty unboxing to make the external API nicer *)
    let alloc_not_null ?desc allocator size ~initialize : unit model_monad =
      let model_ model_data astate =
        let<++> astate = Basic.alloc_not_null ?desc allocator size ~initialize model_data astate in
        astate
      in
      lift_to_monad (lift_model model_)
  end
end

let unsafe_to_astate_transformer (monad : 'a model_monad) :
    model_data -> astate -> ('a * astate) sat_unsat_t =
 fun data astate ->
  (* warning: we currently ignore the non-disjunctive state *)
  match monad data astate NonDisjDomain.bottom |> fst with
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
