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

let internal_value = Fieldname.make PulseOperations.pulse_model_type "backing_value"

let internal_value_access = HilExp.Access.FieldAccess internal_value

let to_internal_value path mode location optional astate =
  PulseOperations.eval_access path mode location optional internal_value_access astate


let to_internal_value_deref path mode location optional astate =
  let* astate, pointer = to_internal_value path Read location optional astate in
  PulseOperations.eval_access path mode location pointer Dereference astate


let write_value path location this ~value ~desc astate =
  let* astate, value_field = to_internal_value path Read location this astate in
  let value_hist = (fst value, Hist.add_call path location desc (snd value)) in
  let+ astate = PulseOperations.write_deref path location ~ref:value_field ~obj:value_hist astate in
  (astate, (value_field, value_hist))


let assign_value_fresh path location this ~desc astate =
  write_value path location this ~value:(AbstractValue.mk_fresh (), ValueHistory.epoch) ~desc astate


let assign_none this ~desc : model =
 fun {path; location} astate ->
  let<*> astate, (pointer, value) = assign_value_fresh path location this ~desc astate in
  let<**> astate = PulseArithmetic.and_eq_int (fst value) IntLit.zero astate in
  let<+> astate =
    PulseOperations.invalidate path
      (MemoryAccess {pointer; access= Dereference; hist_obj_default= snd value})
      location OptionalEmpty value astate
  in
  astate


let assign_non_empty_value ProcnameDispatcher.Call.FuncArg.{arg_payload= this} ~desc : model =
 fun {path; location} astate ->
  (* This model marks the optional object to be non-empty *)
  let<*> astate, (_, value) = assign_value_fresh path location this ~desc astate in
  let<++> astate = PulseArithmetic.and_positive (fst value) astate in
  astate


let get_template_arg typ =
  match (Typ.strip_ptr typ).desc with
  | Tstruct (CppClass {template_spec_info= Template {args= [TType typ]}}) ->
      Some typ
  | _ ->
      L.d_printfln "Template argument type is not found." ;
      None


let assign_precise_value (ProcnameDispatcher.Call.FuncArg.{typ; arg_payload= this_payload} as this)
    (ProcnameDispatcher.Call.FuncArg.{arg_payload= other_payload} as other) ~desc : model =
 (* This model marks the optional object to be non-empty by storing value. *)
 fun ({callee_procname; path; location} as model_data) astate ->
  match (get_template_arg typ, IRAttributes.load_formal_types callee_procname |> List.last) with
  | Some ({desc= Tstruct class_name} as typ), Some actual ->
      (* assign the value pointer to the field of the shared_ptr *)
      let<**> astate, value_address = Basic.alloc_value_address ~desc typ model_data astate in
      let<*> astate, _ = write_value path location this_payload ~value:value_address ~desc astate in
      let typ = Typ.mk (Tptr (typ, Pk_pointer)) in
      (* We need an expression corresponding to the value of the argument we pass to
         the constructor. *)
      let fake_exp = Exp.Var (Ident.create_fresh Ident.kprimed) in
      let args : (AbstractValue.t * ValueHistory.t) PulseAliasSpecialization.FuncArg.t list =
        {typ; exp= fake_exp; arg_payload= value_address} :: [other]
      in
      (* create the list of types of the actual arguments of the constructor *)
      let actuals = [typ; actual] in
      Basic.call_constructor class_name actuals args fake_exp model_data astate
  | Some _, Some _ ->
      L.d_printfln "Class not found" ;
      let<**> astate, address = Basic.deep_copy path location ~value:other_payload ~desc astate in
      let<+> astate, _ = write_value path location this_payload ~value:address ~desc astate in
      astate
  | _, _ ->
      (* if the model cannot find a template argument and/or the formal parameters,
         it just marks the object non-empty *)
      assign_non_empty_value this
        ~desc:(desc ^ " (cannot find template argument and/or formal parameters)")
        model_data astate


let assign_value (args : (AbstractValue.t * ValueHistory.t) PulseAliasSpecialization.FuncArg.t list)
    ~desc : model =
  match args with
  | [this; value] ->
      assign_precise_value this value ~desc:(desc ^ " (precise value)")
  | this :: _ ->
      assign_non_empty_value this ~desc:(desc ^ " (non-empty value)")
  | _ ->
      L.internal_error "Not enough arguments to call the constructor for Optional" ;
      Basic.skip


let copy_assignment (ProcnameDispatcher.Call.FuncArg.{arg_payload= this_payload} as this)
    ProcnameDispatcher.Call.FuncArg.{typ; arg_payload= other} ~desc : model =
 fun ({path; location} as model_data) astate ->
  let<*> astate, ((other_addr, _) as other) =
    to_internal_value_deref path Read location other astate
  in
  match get_template_arg typ with
  | Some typ ->
      let assign_none =
        let<**> astate = PulseArithmetic.prune_eq_zero other_addr astate in
        assign_none this_payload ~desc model_data astate
      in
      let assign_value =
        let<**> astate = PulseArithmetic.prune_positive other_addr astate in
        assign_precise_value this
          {exp= Var (Ident.create_none ()); typ; arg_payload= other}
          ~desc model_data astate
      in
      assign_none @ assign_value
  | None ->
      Basic.ok_continue astate


let emplace optional ~desc : model =
 (* TODO: destroy current object and call move constructor *)
 fun {path; location} astate ->
  let<+> astate, _ = assign_value_fresh path location optional ~desc astate in
  astate


let value optional ~desc : model =
 fun {path; location; ret= ret_id, _} astate ->
  let<*> astate, ((value_addr, value_hist) as value) =
    to_internal_value_deref path Write location optional astate
  in
  (* Check dereference to show an error at the callsite of `value()` *)
  let<*> astate, _ = PulseOperations.eval_access path Write location value Dereference astate in
  PulseOperations.write_id ret_id (value_addr, Hist.add_call path location desc value_hist) astate
  |> Basic.ok_continue


let has_value this ~desc : model =
 fun {path; location; ret= ret_id, _} astate ->
  let<+> astate, (value_addr, _) = to_internal_value_deref path Write location this astate in
  PulseOperations.write_id ret_id (value_addr, Hist.single_call path location desc) astate


let get_pointer optional ~desc : model =
 fun {path; location; ret= ret_id, _} astate ->
  let<*> astate, value_addr = to_internal_value_deref path Read location optional astate in
  let value_update_hist =
    (fst value_addr, Hist.add_call path location desc ~more:"non-empty case" (snd value_addr))
  in
  let astate_value_addr =
    PulseOperations.write_id ret_id value_update_hist astate
    |> PulseArithmetic.prune_positive (fst value_addr)
    >>|| ExecutionDomain.continue
  in
  let nullptr =
    (AbstractValue.mk_fresh (), Hist.single_call path location desc ~more:"empty case")
  in
  let astate_null =
    PulseOperations.write_id ret_id nullptr astate
    |> PulseArithmetic.prune_eq_zero (fst value_addr)
    >>== PulseArithmetic.and_eq_int (fst nullptr) IntLit.zero
    >>|= PulseOperations.invalidate path
           (StackAddress (Var.of_id ret_id, snd nullptr))
           location (ConstantDereference IntLit.zero) nullptr
    >>|| ExecutionDomain.continue
  in
  SatUnsat.to_list astate_value_addr @ SatUnsat.to_list astate_null


let value_or optional default ~desc : model =
 fun {path; location; ret= ret_id, _} astate ->
  let<*> astate, value_addr = to_internal_value_deref path Read location optional astate in
  let astate_non_empty =
    let++ astate_non_empty, value =
      PulseArithmetic.prune_positive (fst value_addr) astate
      >>|= PulseOperations.eval_access path Read location value_addr Dereference
    in
    let value_update_hist =
      (fst value, Hist.add_call path location desc ~more:"non-empty case" (snd value))
    in
    PulseOperations.write_id ret_id value_update_hist astate_non_empty |> Basic.continue
  in
  let astate_default =
    let=* astate, (default_val, default_hist) =
      PulseOperations.eval_access path Read location default Dereference astate
    in
    let default_value_hist =
      (default_val, Hist.add_call path location desc ~more:"empty case" default_hist)
    in
    PulseArithmetic.prune_eq_zero (fst value_addr) astate
    >>|| PulseOperations.write_id ret_id default_value_hist
    >>|| ExecutionDomain.continue
  in
  SatUnsat.to_list astate_non_empty @ SatUnsat.to_list astate_default


let destruct ProcnameDispatcher.Call.FuncArg.{arg_payload= this; typ} ~desc : model =
 fun ({path; location} as model_data) astate ->
  match get_template_arg typ with
  | Some typ ->
      (* note: We do dereference the value address with [NoAccess], to avoid a null dereference
         issue reported when [None] is given as an optional value. *)
      let<*> astate, (value_addr, value_hist) =
        to_internal_value_deref path NoAccess location this astate
      in
      let value_hist = Hist.add_call path location desc value_hist in
      let deleted_arg =
        ProcnameDispatcher.Call.FuncArg.
          { arg_payload= (value_addr, value_hist)
          ; exp= Var (Ident.create_fresh Ident.kprimed)
          ; typ= {desc= Tptr (typ, Pk_pointer); quals= Typ.mk_type_quals ()} }
      in
      Basic.free_or_delete `Delete CppDelete deleted_arg model_data astate
  | None ->
      Basic.ok_continue astate


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [ -"folly" &:: "Optional" &:: "Optional" <>$ capt_arg_payload
    $+ any_arg_of_typ (-"folly" &:: "None")
    $--> assign_none ~desc:"folly::Optional::Optional(=None)"
  ; -"folly" &:: "Optional" &:: "Optional" <>$ capt_arg_payload
    $--> assign_none ~desc:"folly::Optional::Optional()"
  ; -"folly" &:: "Optional" &:: "Optional" <>$ capt_arg
    $+ capt_arg_of_typ (-"folly" &:: "Optional")
    $--> copy_assignment ~desc:"folly::Optional::Optional(folly::Optional<Value> arg)"
  ; -"folly" &:: "Optional" &:: "Optional"
    &++> assign_value ~desc:"folly::Optional::Optional(Value arg)"
  ; -"folly" &:: "Optional" &:: "assign" <>$ capt_arg_payload
    $+ any_arg_of_typ (-"folly" &:: "None")
    $--> assign_none ~desc:"folly::Optional::assign(=None)"
  ; -"folly" &:: "Optional" &:: "assign" <>$ capt_arg
    $+ capt_arg_of_typ (-"folly" &:: "Optional")
    $--> copy_assignment ~desc:"folly::Optional::assign(folly::Optional<Value> arg)"
  ; -"folly" &:: "Optional" &:: "assign"
    &++> assign_value ~desc:"folly::Optional::assign(Value arg)"
  ; -"folly" &:: "Optional" &:: "emplace<>" $ capt_arg_payload
    $+...$--> emplace ~desc:"folly::Optional::emplace()"
  ; -"folly" &:: "Optional" &:: "emplace" $ capt_arg_payload
    $+...$--> emplace ~desc:"folly::Optional::emplace()"
  ; -"folly" &:: "Optional" &:: "has_value" <>$ capt_arg_payload
    $+...$--> has_value ~desc:"folly::Optional::has_value()"
  ; -"folly" &:: "Optional" &:: "reset" <>$ capt_arg_payload
    $+...$--> assign_none ~desc:"folly::Optional::reset()"
  ; -"folly" &:: "Optional" &:: "value" <>$ capt_arg_payload
    $+...$--> value ~desc:"folly::Optional::value()"
  ; -"folly" &:: "Optional" &:: "operator*" <>$ capt_arg_payload
    $+...$--> value ~desc:"folly::Optional::operator*()"
  ; -"folly" &:: "Optional" &:: "operator->" <>$ capt_arg_payload
    $+...$--> value ~desc:"folly::Optional::operator->()"
  ; -"folly" &:: "Optional" &:: "get_pointer" $ capt_arg_payload
    $+...$--> get_pointer ~desc:"folly::Optional::get_pointer()"
  ; -"folly" &:: "Optional" &:: "value_or" $ capt_arg_payload $+ capt_arg_payload
    $+...$--> value_or ~desc:"folly::Optional::value_or()"
  ; -"folly" &:: "Optional" &:: "~Optional" $ capt_arg
    $--> destruct ~desc:"folly::Optional::~Optional()"
  ; -"std" &:: "optional" &:: "optional" $ capt_arg_payload
    $+ any_arg_of_typ (-"std" &:: "nullopt_t")
    $--> assign_none ~desc:"std::optional::optional(=nullopt)"
  ; -"std" &:: "optional" &:: "optional" $ capt_arg_payload
    $--> assign_none ~desc:"std::optional::optional()"
  ; -"std" &:: "optional" &:: "optional" $ capt_arg
    $+ capt_arg_of_typ (-"std" &:: "optional")
    $--> copy_assignment ~desc:"std::optional::optional(std::optional<Value> arg)"
  ; -"std" &:: "optional" &:: "optional"
    &++> assign_value ~desc:"std::optional::optional(Value arg)"
  ; -"std" &:: "optional" &:: "operator=" $ capt_arg_payload
    $+ any_arg_of_typ (-"std" &:: "nullopt_t")
    $--> assign_none ~desc:"std::optional::operator=(None)"
  ; -"std" &:: "optional" &:: "operator=" $ capt_arg
    $+ capt_arg_of_typ (-"std" &:: "optional")
    $--> copy_assignment ~desc:"std::optional::operator=(std::optional<Value> arg)"
  ; -"std" &:: "optional" &:: "operator="
    &++> assign_value ~desc:"std::optional::operator=(Value arg)"
  ; -"std" &:: "optional" &:: "emplace<>" $ capt_arg_payload
    $+...$--> emplace ~desc:"std::optional::emplace()"
  ; -"std" &:: "optional" &:: "emplace" $ capt_arg_payload
    $+...$--> emplace ~desc:"std::optional::emplace()"
  ; -"std" &:: "optional" &:: "has_value" <>$ capt_arg_payload
    $+...$--> has_value ~desc:"std::optional::has_value()"
  ; -"std" &:: "__optional_storage_base" &:: "has_value" $ capt_arg_payload
    $+...$--> has_value ~desc:"std::optional::has_value()"
  ; -"std" &:: "optional" &:: "operator_bool" <>$ capt_arg_payload
    $+...$--> has_value ~desc:"std::optional::operator_bool()"
  ; -"std" &:: "optional" &:: "reset" <>$ capt_arg_payload
    $+...$--> assign_none ~desc:"std::optional::reset()"
  ; -"std" &:: "optional" &:: "value" <>$ capt_arg_payload
    $+...$--> value ~desc:"std::optional::value()"
  ; -"std" &:: "optional" &:: "operator*" <>$ capt_arg_payload
    $+...$--> value ~desc:"std::optional::operator*()"
  ; -"std" &:: "optional" &:: "operator->" <>$ capt_arg_payload
    $+...$--> value ~desc:"std::optional::operator->()"
  ; -"std" &:: "optional" &:: "value_or" $ capt_arg_payload $+ capt_arg_payload
    $+...$--> value_or ~desc:"std::optional::value_or()"
  ; -"std" &:: "optional" &:: "~optional" $ capt_arg
    $--> destruct ~desc:"std::optional::~optional()" ]
