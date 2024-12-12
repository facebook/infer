(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
module IRAttributes = Attributes
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import
open PulseModelsImport

type astate = AbductiveDomain.t

type non_disj = NonDisjDomain.t

type 'a result = 'a AccessResult.t

type aval = AbstractValue.t * ValueHistory.t

type 'a execution =
  | ContinueProgram of 'a * astate
  | Other of ExecutionDomain.t (* should never contain a ExecutionDomain.ContinueProgram *)

(* we work on a disjunction of executions *)
type 'a model_monad =
  CallEvent.t * model_data -> astate -> non_disj -> 'a execution result list * non_disj

module Syntax = struct
  module ModeledField = PulseOperations.ModeledField

  let to_aval = ValueOrigin.addr_hist

  let ret (a : 'a) : 'a model_monad =
   fun _data astate non_disj -> ([Ok (ContinueProgram (a, astate))], non_disj)


  let throw : unit model_monad =
   fun _data astate non_disj -> ([Ok (Other (ExceptionRaised astate))], non_disj)


  let unreachable : 'a model_monad = fun _ _ _ -> ([], NonDisjDomain.bottom)

  let report diagnostic : unit model_monad =
   fun _data astate non_disj ->
    ([Recoverable (ContinueProgram ((), astate), [ReportableError {astate; diagnostic}])], non_disj)


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

  let ( >>= ) a f = bind a f

  let ( @= ) f a = bind a f

  let ( @@> ) f g =
    let* () = f in
    g


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


  let option_value_map o ~default ~f = match o with Some v -> f v | None -> ret default

  let option_bind o ~f = option_value_map o ~default:None ~f

  let option_exists o ~f = option_value_map o ~default:false ~f

  let option_iter o ~f = option_value_map o ~default:() ~f

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


  let get_data : model_data model_monad = fun (desc, data) astate -> ret data (desc, data) astate

  let get_desc : CallEvent.t model_monad = fun (desc, data) astate -> ret desc (desc, data) astate

  let event ?more (desc, data) =
    let desc =
      match more with
      | None ->
          desc
      | Some more ->
          CallEvent.Model
            (Format.asprintf "%a %s" (CallEvent.pp_name_only ~with_class:true) desc more)
    in
    ValueHistory.Call
      {f= desc; location= data.location; in_call= ValueHistory.epoch; timestamp= data.path.timestamp}


  let get_event ?more () : ValueHistory.event model_monad =
    let* data = get_data in
    let* desc = get_desc in
    ret (event ?more (desc, data))


  let ok x = Ok x

  let sat x = Sat x

  let ( >> ) f g x = f x |> g

  let lift_to_monad (model : model) : unit model_monad =
   fun (_desc, data) astate non_disj ->
    let execs, non_disj = model data astate non_disj in
    ( List.map execs
        ~f:
          (PulseResult.map ~f:(function
            | ExecutionDomain.ContinueProgram astate ->
                ContinueProgram ((), astate)
            | exec ->
                Other exec ) )
    , non_disj )


  let start_model_ desc (monad : unit -> unit model_monad) : model =
   fun data astate non_disj ->
    let execs, non_disj = monad () (desc, data) astate non_disj in
    ( List.map execs
        ~f:
          (PulseResult.map ~f:(function
            | ContinueProgram ((), astate) ->
                ExecutionDomain.ContinueProgram astate
            | Other exec ->
                exec ) )
    , non_disj )


  let start_named_model desc monad data astate non_disj =
    start_model_ (Model desc) monad data astate non_disj


  let start_model monad data astate non_disj =
    start_model_ (ModelName data.callee_procname) monad data astate non_disj


  let compose1 model1 model2 arg =
    start_model @@ fun () -> lift_to_monad (model1 arg) @@> lift_to_monad (model2 arg)


  let compose2 model1 model2 arg1 arg2 =
    start_model @@ fun () -> lift_to_monad (model1 arg1 arg2) @@> lift_to_monad (model2 arg1 arg2)


  let disj (list : 'a model_monad list) : 'a model_monad =
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


  let add_model_call hist =
    let* event = get_event () in
    ret (Hist.add_event event hist)


  let as_constant_q (v, _) : Q.t option model_monad =
   fun data astate ->
    let phi = astate.path_condition in
    ret (Formula.as_constant_q phi v) data astate


  let as_constant_int v : int option model_monad =
    let* n_q_opt = as_constant_q v in
    let n = Option.bind n_q_opt ~f:QSafeCapped.to_int in
    ret n


  let as_constant_bool v : bool option model_monad =
    let* n_opt = as_constant_int v in
    let b_opt = Option.map n_opt ~f:(fun (n : int) -> not Int.(n = 0)) in
    ret b_opt


  let as_constant_string (v, _) : string option model_monad =
   fun data astate ->
    let phi = astate.path_condition in
    ret (Formula.as_constant_string phi v) data astate


  let int ?hist i : aval model_monad =
   fun (desc, data) astate ->
    let astate, v = PulseArithmetic.absval_of_int astate (IntLit.of_int i) in
    let hist =
      match hist with
      | Some hist ->
          hist
      | None ->
          let event = event (desc, data) in
          Hist.single_event event
    in
    ret (v, hist) (desc, data) astate


  let null : aval model_monad =
    let* {path; location} = get_data in
    let* v, hist = int 0 in
    let null_deref = Invalidation.ConstantDereference IntLit.zero in
    let aval =
      (v, ValueHistory.sequence (Invalidated (null_deref, location, path.timestamp)) hist)
    in
    let* () =
      PulseOperations.invalidate path UntraceableAccess location null_deref aval |> exec_command
    in
    ret aval


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


  let read_ret_origin : ValueOrigin.t model_monad =
    let* {path; location; ret= ret_id, _} = get_data in
    PulseOperations.eval_to_value_origin path NoAccess location (Var ret_id)
    |> exec_partial_operation


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


  let check_valid ?must_be_valid_reason vo : unit model_monad =
    let* {path; location} = get_data in
    PulseOperations.check_addr_access path ?must_be_valid_reason NoAccess location
      (ValueOrigin.addr_hist vo)
    >> sat |> exec_partial_command


  let access access_mode aval access : aval model_monad =
    let* {path; location} = get_data in
    let* addr, hist =
      PulseOperations.eval_access path access_mode location aval access
      >> sat |> exec_partial_operation
    in
    let* event = get_event () in
    let hist = Hist.add_event event hist in
    ret (addr, hist)


  let load_exp exp : aval model_monad =
    let* {path; location} = get_data in
    PulseOperations.eval_deref path location exp |> exec_partial_operation


  let load_access ?(no_access = false) ?(deref = true) aval access : aval model_monad =
    let* {path; location} = get_data in
    let mode = if no_access then NoAccess else Read in
    let eval_access =
      if deref then PulseOperations.eval_deref_access else PulseOperations.eval_access
    in
    eval_access path mode location aval access >> sat |> exec_partial_operation


  let load x = access Read x Dereference

  let _field x f = access Read x (FieldAccess f)

  let add_dict_contain_const_keys (addr, _) : unit model_monad =
    PulseOperations.add_dict_contain_const_keys addr |> exec_command


  let find_decompiler_expr addr : DecompilerExpr.t model_monad =
    (fun {decompiler} -> PulseDecompiler.find addr decompiler) |> exec_pure_operation


  let is_dict_missing_key_var_block_list s =
    Option.exists Config.dict_missing_key_var_block_list ~f:(fun regexp ->
        Str.string_match regexp s 0 )


  let get_pvar_deref_typ formals pvar : Typ.name option model_monad =
    if is_dict_missing_key_var_block_list (Pvar.to_string pvar) then ret None
    else
      match List.Assoc.find formals ~equal:Pvar.equal pvar with
      | Some typ ->
          ret (Typ.name (Typ.strip_ptr typ))
      | None ->
          let* addr, _ = load_exp (Lvar pvar) in
          AddressAttributes.get_static_type addr |> exec_pure_operation


  let resolve_field_info typ fld : Struct.field_info option model_monad =
    if is_dict_missing_key_var_block_list (Fieldname.get_field_name fld) then ret None
    else
      let* {analysis_data= {tenv}} = get_data in
      Tenv.resolve_field_info tenv typ fld |> ret


  let is_dict_non_alias formals addr : bool model_monad =
    let rec get_field_typ typ accesses : Typ.name option model_monad =
      match (accesses : DecompilerExpr.access list) with
      | FieldAccess fld :: Dereference :: accesses ->
          let* field_info = resolve_field_info typ fld in
          option_bind field_info ~f:(fun {Struct.typ} ->
              option_bind (Typ.name (Typ.strip_ptr typ)) ~f:(fun typ -> get_field_typ typ accesses) )
      | [] ->
          ret (Some typ)
      | _ ->
          ret None
    in
    let is_field_dict_typ base_typ accesses =
      let* typ = get_field_typ base_typ accesses in
      ret (Option.exists typ ~f:(Typ.Name.equal TextualSil.hack_dict_type_name))
    in
    let* decompiled = find_decompiler_expr addr in
    match (decompiled : DecompilerExpr.t) with
    | SourceExpr ((PVar pvar, rev_accesses), _) -> (
      (* NOTE: The [access] in [DecompilerExpr.source_expr] has the reverse order by default,
         e.g. the access of [x->f->g] is [\[*; g; *; f; *\]]. *)
      match List.rev rev_accesses with
      | Dereference :: accesses ->
          let* base_typ = get_pvar_deref_typ formals pvar in
          option_exists base_typ ~f:(fun base_typ -> is_field_dict_typ base_typ accesses)
      | accesses when Pvar.is_static_companion pvar ->
          let base_typ = Typ.HackClass (HackClassName.make (Pvar.to_string pvar)) in
          is_field_dict_typ base_typ accesses
      | _ ->
          ret false )
    | _ ->
        ret false


  let add_dict_read_const_key (addr, history) key : unit model_monad =
    let* {analysis_data= {proc_desc}; path= {timestamp}; location} = get_data in
    let* is_dict_non_alias = is_dict_non_alias (Procdesc.get_pvar_formals proc_desc) addr in
    if is_dict_non_alias then
      PulseOperations.add_dict_read_const_key timestamp (Immediate {location; history}) addr key
      >> sat |> exec_partial_command
    else ret ()


  let remove_dict_contain_const_keys (addr, _) : unit model_monad =
    PulseOperations.remove_dict_contain_const_keys addr |> exec_command


  let is_hack_constinit_called (addr, _) : bool model_monad =
    AddressAttributes.is_hack_constinit_called addr |> exec_pure_operation


  let set_hack_constinit_called (addr, _) : unit model_monad =
    AddressAttributes.add_one addr HackConstinitCalled |> exec_command


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


  let and_equal_instanceof (res, _) (obj, _) ty ~nullable : unit model_monad =
    PulseArithmetic.and_equal_instanceof res obj ty ~nullable |> exec_partial_command


  let and_positive (addr, _) : unit model_monad =
    PulseArithmetic.and_positive addr |> exec_partial_command


  let add_static_type typ_name (addr, _) : unit model_monad =
   fun ((_, {analysis_data= {tenv}; location}) as data) astate ->
    let astate =
      AbductiveDomain.AddressAttributes.add_static_type tenv typ_name addr location astate
    in
    ret () data astate


  let get_static_type (addr, _) = AddressAttributes.get_static_type addr |> exec_pure_operation

  let tenv_resolve_field_info typ_name field_name : Struct.field_info option model_monad =
   fun ((_desc, {analysis_data= {tenv}}) as data) astate ->
    let info = Tenv.resolve_field_info tenv typ_name field_name in
    ret info data astate


  let tenv_resolve_fieldname typ_name name :
      (Fieldname.t option * Tenv.unresolved_reason option) model_monad =
   fun ((_desc, {analysis_data= {tenv}}) as data) astate ->
    (* warning: we skipped missed capture informations here *)
    let field_name, missed_capture_types = Tenv.resolve_fieldname tenv typ_name name in
    let unresolved_reason =
      if Typ.Name.Set.is_empty missed_capture_types then None
      else Some Tenv.MaybeMissingDueToMissedCapture
    in
    ret (field_name, unresolved_reason) data astate


  let tenv_resolve_method typ_name proc_name : Procname.t option model_monad =
   fun ((_desc, {analysis_data= {tenv}}) as data) astate ->
    (* Remark: this is a simplified resolution that will work well for closure resolution,
       but does not implement all the steps proposed for regular virtual calls in Pulse.ml *)
    let method_exists proc_name methods = List.mem ~equal:Procname.equal methods proc_name in
    let opt_info = Tenv.resolve_method ~method_exists tenv typ_name proc_name |> Result.ok in
    (* warning: we skipped missed capture informations here *)
    let opt_resolved_proc_name = Option.map opt_info ~f:Tenv.MethodInfo.get_proc_name in
    ret opt_resolved_proc_name data astate


  let read exp : aval model_monad =
    let* {path; location} = get_data in
    PulseOperations.eval path Read location exp |> exec_partial_operation


  let string str : aval model_monad = read (Const (Cstr str))

  let string_concat (v1, hist1) (v2, hist2) : aval model_monad =
    let v = AbstractValue.mk_fresh () in
    let hist = ValueHistory.binary_op (PlusA None) hist1 hist2 in
    let* () =
      exec_partial_command (fun astate ->
          PulseArithmetic.and_equal_string_concat v (AbstractValueOperand v1)
            (AbstractValueOperand v2) astate )
    in
    ret (v, hist)


  let allocation attr (addr, _) : unit model_monad =
    let* {location} = get_data in
    PulseOperations.allocate attr location addr |> exec_command


  let fresh_ var_type ?more () : aval model_monad =
    let addr =
      match var_type with
      | `Unrestricted ->
          AbstractValue.mk_fresh ()
      | `NonNegative ->
          AbstractValue.mk_fresh_restricted ()
    in
    let* event = get_event ?more () in
    let hist = Hist.single_event event in
    ret (addr, hist)


  let fresh ?more () = fresh_ `Unrestricted ?more ()

  let fresh_nonneg ?more () = fresh_ `NonNegative ?more ()

  let write_field ~ref field obj : unit model_monad =
    let* {path; location} = get_data in
    PulseOperations.write_field path location ~ref ~obj field >> sat |> exec_partial_command


  let store_field ?(deref = true) ~ref field obj : unit model_monad =
    let* {path; location} = get_data in
    let write = if deref then PulseOperations.write_deref_field else PulseOperations.write_field in
    write path location ~ref ~obj field >> sat |> exec_partial_command


  let store ~ref obj : unit model_monad =
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


  module HackBuilder = struct
    let is_hack_builder tenv hacktypname builder_class_name =
      (* TODO: deal with namespaces properly! *)
      let builder_class_name = Typ.HackClass (HackClassName.make builder_class_name) in
      PatternMatch.is_subtype tenv (HackClass hacktypname) builder_class_name


    let is_hack_builder_in_config tenv hacktypname =
      List.exists Config.hack_builder_patterns ~f:(fun Config.{class_name} ->
          is_hack_builder tenv hacktypname class_name )


    let is_hack_builder_immediately_non_discardable tenv hacktypname =
      List.exists Config.hack_builder_patterns ~f:(fun Config.{immediately_non_discardable_class} ->
          Option.exists immediately_non_discardable_class
            ~f:(fun immediately_non_discardable_class ->
              is_hack_builder tenv hacktypname immediately_non_discardable_class ) )
  end

  let new_ type_name_exp =
    let* {analysis_data= {tenv}} = get_data in
    let* new_obj = lift_to_monad_and_get_result (internal_new_ type_name_exp) in
    match type_name_exp with
    | Exp.Sizeof {typ} ->
        (* TODO: pass a nullable parameter to and_dynamic_type_is *)
        let* () = and_dynamic_type_is new_obj typ in
        let* () =
          match Typ.name typ with
          | Some (HackClass hacktypname) when HackBuilder.is_hack_builder_in_config tenv hacktypname
            ->
              let* () = allocation (Attribute.HackBuilderResource hacktypname) new_obj in
              (* While it makes sense to set initial builder state to NonDiscardable we'd like
                  to delay that until the first method call to avoid FPs caused by reporting
                  on builders without any method calls unless builder is explicitly configured
                 as non discardable *)
              let builder_state =
                if HackBuilder.is_hack_builder_immediately_non_discardable tenv hacktypname then
                  Attribute.Builder.NonDiscardable
                else Attribute.Builder.Discardable
              in
              AddressAttributes.set_hack_builder (fst new_obj) builder_state |> exec_command
          | _ ->
              ret ()
        in
        ret new_obj
    | _ ->
        unreachable


  let constructor ?(deref = true) type_name fields : aval model_monad =
    let exp =
      Exp.Sizeof
        { typ= Typ.mk_struct type_name
        ; nbytes= None
        ; dynamic_length= None
        ; subtype= Subtype.exact
        ; nullable= false }
    in
    let* new_obj = new_ exp in
    let* () =
      list_iter fields ~f:(fun (fieldname, obj) ->
          let field = Fieldname.make type_name fieldname in
          store_field ~deref ~ref:new_obj field obj )
    in
    ret new_obj


  let remove_hack_builder_attributes bv : unit model_monad =
    AddressAttributes.remove_hack_builder (fst bv) |> exec_command


  let deep_copy ?depth_max source : aval model_monad =
    let* {path; location} = get_data in
    PulseOperations.deep_copy ?depth_max path location source >> sat |> exec_partial_operation


  let aval_operand (addr, _) = PulseArithmetic.AbstractValueOperand addr

  let binop binop (addr1, hist1) (addr2, hist2) : aval model_monad =
    let addr_res = AbstractValue.mk_fresh () in
    let hist_res = Hist.binop binop hist1 hist2 in
    let* addr_res =
      PulseArithmetic.eval_binop addr_res binop (AbstractValueOperand addr1)
        (AbstractValueOperand addr2)
      |> exec_partial_operation
    in
    ret (addr_res, hist_res)


  let binop_int binop (arg, hist) i : aval model_monad =
    let addr_res = AbstractValue.mk_fresh () in
    let* addr_res =
      PulseArithmetic.eval_binop addr_res binop (AbstractValueOperand arg)
        (PulseArithmetic.ConstOperand (Cint i))
      |> exec_partial_operation
    in
    ret (addr_res, hist)


  let unop unop (addr, hist) : aval model_monad =
    let addr_res = AbstractValue.mk_fresh () in
    let* addr_res = PulseArithmetic.eval_unop addr_res unop addr |> exec_partial_operation in
    ret (addr_res, hist)


  let data_dependency dest sources =
    let* {path; location} = get_data in
    let* desc = get_desc in
    PulseTaintOperations.propagate_to path location InternalModel dest sources desc |> exec_command


  let data_dependency_to_ret sources =
    let* ret = read_ret_origin in
    data_dependency ret sources


  let prune_binop binop operand1 operand2 =
    PulseArithmetic.prune_binop ~negated:false binop operand1 operand2 |> exec_partial_command


  let prune_eq_int arg i : unit model_monad =
    prune_binop Eq (aval_operand arg) (ConstOperand (Cint i))


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
    prune_binop Ne (aval_operand arg) (ConstOperand (Cint i))


  let prune_ne_zero (addr, _) : unit model_monad =
    PulseArithmetic.prune_ne_zero addr |> exec_partial_command


  let prune_lt arg1 arg2 : unit model_monad = prune_binop Lt (aval_operand arg1) (aval_operand arg2)

  let prune_lt_int arg1 i : unit model_monad =
    prune_binop Lt (aval_operand arg1) (ConstOperand (Cint i))


  let prune_ge arg1 arg2 : unit model_monad = prune_binop Ge (aval_operand arg1) (aval_operand arg2)

  let prune_ge_int arg1 i : unit model_monad =
    prune_binop Ge (aval_operand arg1) (ConstOperand (Cint i))


  let prune_gt arg1 arg2 = prune_lt arg2 arg1

  let prune_gt_int arg1 i : unit model_monad =
    prune_binop Gt (aval_operand arg1) (ConstOperand (Cint i))


  let prune_le arg1 arg2 = prune_ge arg2 arg1

  let prune_eq arg1 arg2 : unit model_monad = prune_binop Eq (aval_operand arg1) (aval_operand arg2)

  let prune_ne arg1 arg2 : unit model_monad = prune_binop Ne (aval_operand arg1) (aval_operand arg2)

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
          L.d_printfln "[ocaml model] dynamic_dispatch: executing case for type %a on value %a"
            Typ.Name.pp type_name AbstractValue.pp (fst aval) ;
          case_fun ()
      | None, Some default ->
          default ()
      | None, None ->
          L.d_printfln "[ocaml model] dynamic_dispatch: no case for type %a" Typ.Name.pp type_name ;
          unreachable )
    | _ ->
        L.d_printfln "[ocaml model] No dynamic type found for value %a!" AbstractValue.pp (fst aval) ;
        Option.value_map default ~default:unreachable ~f:(fun f -> f ())


  let dispatch_call ret pname func_args : unit model_monad =
    lift_to_monad
    @@ fun {analysis_data; dispatch_call_eval_args; path; location} astate non_disj ->
    dispatch_call_eval_args analysis_data path ret (Const (Cfun pname)) func_args location
      CallFlags.default astate non_disj (Some pname)


  let register_class_object_for_value (aval, _) (class_object, _) : unit model_monad =
    let f =
      Formula.Procname
        (Procname.make_hack ~class_name:None ~function_name:"hack_get_static_class" ~arity:(Some 1))
    in
    PulseArithmetic.and_equal (AbstractValueOperand class_object)
      (FunctionApplicationOperand {f; actuals= [aval]})
    |> exec_partial_command


  type closure_args =
    | Regular of aval list
    | FromAttributes of (ProcAttributes.t option -> aval list model_monad)

  let mixed_type_name lang =
    match (lang : Textual.Lang.t) with
    | Hack ->
        TextualSil.hack_mixed_type_name
    | Python ->
        TextualSil.python_mixed_type_name
    | Java ->
        L.die InternalError "DSL.call not supported on Java"


  let call lang proc_name named_args =
    let mixed_type_name = mixed_type_name lang in
    let typ = Typ.mk_ptr (Typ.mk_struct mixed_type_name) in
    let ret_id = Ident.create_none () in
    let call_args =
      List.map named_args ~f:(fun (str, arg) : ValueOrigin.t ProcnameDispatcher.Call.FuncArg.t ->
          let pvar = Pvar.mk (Mangled.from_string str) proc_name in
          let exp = Exp.Lvar pvar in
          let arg_payload = ValueOrigin.OnStack {var= Var.of_pvar pvar; addr_hist= arg} in
          {exp; typ; arg_payload} )
    in
    let* () = dispatch_call (ret_id, typ) proc_name call_args in
    read (Exp.Var ret_id)


  let python_call = call Python

  let unknown_call ?(force_pure = false) lang skip_reason args : aval model_monad =
    let mixed_type_name = mixed_type_name lang in
    let typ = Typ.mk_ptr (Typ.mk_struct mixed_type_name) in
    let actuals = List.map args ~f:(fun arg -> (arg, typ)) in
    lift_model (Basic.unknown_call_without_formals ~force_pure skip_reason actuals)
    |> lift_to_monad_and_get_result


  let apply_closure lang (closure : aval) unresolved_pname closure_args : aval model_monad =
    let* opt_dynamic_type_data = get_dynamic_type ~ask_specialization:true closure in
    match opt_dynamic_type_data with
    | Some {Formula.typ= {Typ.desc= Tstruct type_name}} -> (
        let* opt_resolved_pname = tenv_resolve_method type_name unresolved_pname in
        match opt_resolved_pname with
        | None ->
            L.d_printfln "[ocaml model] Closure dynamic type is %a but no implementation was found!"
              Typ.Name.pp type_name ;
            let* unknown_res = fresh () in
            ret unknown_res
        | Some resolved_pname ->
            L.d_printfln "[ocaml model] Closure resolved to a call to %a" Procname.pp resolved_pname ;
            let* closure_args =
              match closure_args with
              | Regular closure_args ->
                  ret closure_args
              | FromAttributes gen_closure_args ->
                  gen_closure_args (IRAttributes.load resolved_pname)
            in
            let args = closure :: closure_args in
            let named_args =
              List.mapi args ~f:(fun i arg -> (Printf.sprintf "CLOSURE_ARG%d" i, arg))
            in
            let* res = call lang resolved_pname named_args in
            L.d_printfln "[ocaml model] Closure return value is %a." AbstractValue.pp (fst res) ;
            ret res )
    | _ ->
        L.d_printfln "[ocaml model] Closure dynamic type is unknown." ;
        let unresolved_str = Format.asprintf "%a" Procname.pp unresolved_pname in
        let* args =
          match closure_args with
          | Regular closure_args ->
              ret closure_args
          | FromAttributes gen_closure_args ->
              gen_closure_args None
        in
        unknown_call lang unresolved_str args


  let apply_hack_closure closure closure_args =
    let unresolved_pname =
      Procname.make_hack ~class_name:(Some HackClassName.wildcard) ~function_name:"__invoke"
        ~arity:(Some (1 + List.length closure_args))
    in
    apply_closure Hack closure unresolved_pname (Regular closure_args)


  let apply_python_closure closure gen_closure_args =
    let unresolved_pname =
      Procname.make_python ~class_name:(Some PythonClassName.wildcard) ~function_name:"call"
    in
    apply_closure Python closure unresolved_pname (FromAttributes gen_closure_args)


  module Basic = struct
    (* See internal_new_. We do some crafty unboxing to make the external API nicer *)
    let return_alloc_not_null allocator size ~initialize : unit model_monad =
      let* desc = get_desc in
      let model_ model_data astate =
        let<++> astate =
          Basic.alloc_not_null ~desc:(CallEvent.to_name_only desc) allocator size ~initialize
            model_data astate
        in
        astate
      in
      lift_to_monad (lift_model model_)


    let free invalidation func_arg : unit model_monad =
      Basic.free_or_delete `Free invalidation func_arg |> lift_to_monad


    let early_exit = Basic.early_exit |> lift_model
  end
end

let unsafe_to_astate_transformer (monad : 'a model_monad) :
    CallEvent.t * model_data -> astate -> ('a * astate) sat_unsat_t =
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
