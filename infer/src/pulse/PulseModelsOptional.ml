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

(* An optional is modelled with two fields:
   - [__infer_has_value] is 1 when the optional is engaged and 0 when it is empty;
   - [__infer_backing_value] points to the contained object when the optional is engaged, and to a
     marker value invalidated with [OptionalEmpty] when it is empty.

   The marker must never be equal to a constant: values equal to the same constant are merged
   together with their attributes, so the invalidation would also apply to every null pointer on the
   path. *)
let internal_value = Fieldname.make PulseOperations.pulse_model_type "__infer_backing_value"

let internal_value_access = Access.FieldAccess internal_value

let has_value_field = Fieldname.make PulseOperations.pulse_model_type "__infer_has_value"

let has_value_access = Access.FieldAccess has_value_field

let to_internal_value path mode location optional astate =
  PulseOperations.eval_access path mode location optional internal_value_access astate


let to_internal_value_deref path mode location optional astate =
  let* astate, pointer = to_internal_value path Read location optional astate in
  PulseOperations.eval_access path mode location pointer Dereference astate


let to_has_value_deref path mode location optional astate =
  let* astate, field =
    PulseOperations.eval_access path Read location optional has_value_access astate
  in
  PulseOperations.eval_access path mode location field Dereference astate


(* The two fields are always read together: an unknown call only havocs the edges that already
   exist, so a field read for the first time after an unknown call would still be taken from the
   precondition while the other one has been havoced. *)
let read_fields path location ~value_mode ~flag_mode optional astate =
  let* astate, value = to_internal_value_deref path value_mode location optional astate in
  let+ astate, flag = to_has_value_deref path flag_mode location optional astate in
  (astate, value, flag)


let write_has_value path location this ~engaged ~hist astate =
  let* astate, field =
    PulseOperations.eval_access path Read location this has_value_access astate
  in
  let astate, flag =
    PulseArithmetic.absval_of_int astate (if engaged then IntLit.one else IntLit.zero)
  in
  PulseOperations.write_deref path location ~ref:field ~obj:(flag, hist) astate


(* Do not record [flag > 0] as a test made by the program: in callers, it would make every later
   issue latent, as [is_manifest] only ignores such tests on allocated values. *)
let prune_engaged ~flag ~pointer astate =
  PulseArithmetic.and_positive flag astate >>== PulseArithmetic.prune_positive pointer


let is_known_engaged flag astate =
  match PulseArithmetic.prune_eq_zero flag astate with Unsat _ -> true | Sat _ -> false


let must_be_valid_in_pre value astate =
  AddressAttributes.find_opt `Pre value astate |> Option.bind ~f:Attributes.get_must_be_valid


let is_accessed value astate =
  let has_edge pre_or_post = Memory.exists_edge ~pre_or_post value astate ~f:(fun _ -> true) in
  has_edge `Post || has_edge `Pre


let is_empty_marker value astate =
  AddressAttributes.find_opt `Post value astate
  |> Option.bind ~f:Attributes.get_invalid
  |> Option.exists ~f:(fun (invalidation, _) -> Invalidation.equal invalidation OptionalEmpty)


(* The contained value is only accessed when the optional is engaged, so a path where it has been
   accessed but the optional is empty is infeasible, unless the optional comes from the precondition
   and is empty in the caller. Stop with a latent issue on the access then, as Pulse does for pointers
   that are compared to null after being dereferenced. *)
let empty_after_access value astate =
  match must_be_valid_in_pre value astate with
  | Some (_, trace, reason) ->
      Sat
        (AccessResult.of_abductive_result
           (Error (`PotentialInvalidAccess (astate, value, (trace, reason)))) )
  | None ->
      Unsat
        { reason= (fun () -> "the contained value of an empty optional was accessed")
        ; source= __POS__ }


let prune_empty ~flag ~value astate =
  let** astate = PulseArithmetic.prune_eq_zero flag astate in
  if is_accessed value astate then empty_after_access value astate else Sat (Ok astate)


(* Record in the post that an optional whose value has been accessed is engaged. Constraining the
   flag of the precondition instead would make the summary inapplicable to callers that pass an
   empty optional, where the access is reported. Bypass [WrittenTo], as this is not a write by the
   program and must not count as a modification of a copy or of a parameter; marking the cell
   initialized is enough for callers to take the flag from the post. *)
let set_engaged_in_post path location optional ~hist astate =
  let+ astate, (field, _) =
    PulseOperations.eval_access path Read location optional has_value_access astate
  in
  let astate, one = PulseArithmetic.absval_of_int astate IntLit.one in
  AbductiveDomain.set_post_edges field
    (UnsafeMemory.Edges.add Dereference (one, hist) UnsafeMemory.Edges.empty)
    astate
  |> AddressAttributes.initialize field


let write_value path location this ~value ~desc astate =
  let* astate, value_field = to_internal_value path Read location this astate in
  let value_hist = (fst value, Hist.add_call path location desc (snd value)) in
  let+ astate = PulseOperations.write_deref path location ~ref:value_field ~obj:value_hist astate in
  (astate, (value_field, value_hist))


let write_engaged_value path location this ~value ~desc astate =
  let* astate, ((_, (_, hist)) as res) = write_value path location this ~value ~desc astate in
  let+ astate = write_has_value path location this ~engaged:true ~hist astate in
  (astate, res)


let write_empty path location this ~marker ~desc astate =
  let* astate, ((_, (_, hist)) as res) =
    write_value path location this ~value:marker ~desc astate
  in
  let+ astate = write_has_value path location this ~engaged:false ~hist astate in
  (astate, res)


let assign_none ?marker history this ~desc : model_no_non_disj =
 fun {path; location} astate ->
  let this = ValueOrigin.addr_hist this in
  let marker = Option.value_or_thunk marker ~default:AbstractValue.mk_fresh in
  let<+> astate, (pointer, value) =
    write_empty path location this ~marker:(marker, history) ~desc astate
  in
  PulseOperations.invalidate path
    (MemoryAccess {pointer; access= Dereference; hist_obj_default= snd value})
    location OptionalEmpty value astate


let assign_non_empty_value history this ~desc : model_no_non_disj =
 fun {path; location} astate ->
  (* This model marks the optional object to be non-empty *)
  let<*> astate, (_, value) =
    write_engaged_value path location (ValueOrigin.addr_hist this)
      ~value:(AbstractValue.mk_fresh (), history)
      ~desc astate
  in
  let<++> astate = PulseArithmetic.and_positive (fst value) astate in
  astate


let get_template_arg typ =
  match (Typ.strip_ptr typ).desc with
  | Tstruct (CppClass {template_spec_info= Template {args= [TType typ]}}) ->
      Some typ
  | _ ->
      L.d_printfln "Template argument type is not found." ;
      None


let assign_precise_value FuncArg.{typ; arg_payload= this_payload}
    (FuncArg.{arg_payload= other_payload} as other) ~desc : model =
 (* This model marks the optional object to be non-empty by storing value. *)
 fun ({callee_procname; path; location} as model_data) astate non_disj ->
  let ( let<*> ) x f = bind_sat_result non_disj (Sat x) f in
  let ( let<**> ) x f = bind_sat_result non_disj x f in
  match (get_template_arg typ, IRAttributes.load_formal_types callee_procname |> List.last) with
  | Some ({desc= Tstruct class_name} as typ), Some actual ->
      (* assign the value pointer to the field of the shared_ptr *)
      let<**> astate, value_address = Basic.alloc_value_address ~desc typ model_data astate in
      let<*> astate, _ =
        write_engaged_value path location
          (ValueOrigin.addr_hist this_payload)
          ~value:value_address ~desc astate
      in
      let typ = Typ.mk (Tptr (typ, Pk_pointer)) in
      (* We need an expression corresponding to the value of the argument we pass to
         the constructor. *)
      let fake_exp = Exp.Var (Ident.create_fresh Ident.kprimed) in
      let args : ValueOrigin.t FuncArg.t list =
        {typ; exp= fake_exp; arg_payload= ValueOrigin.unknown value_address} :: [other]
      in
      (* create the list of types of the actual arguments of the constructor *)
      let actuals = [typ; actual] in
      Basic.call_constructor class_name actuals args fake_exp model_data astate non_disj
  | Some _, Some _ ->
      L.d_printfln "Class not found" ;
      let<**> astate, address =
        Basic.deep_copy path location ~value:(ValueOrigin.addr_hist other_payload) ~desc astate
      in
      let<*> astate, _ =
        write_engaged_value path location
          (ValueOrigin.addr_hist this_payload)
          ~value:address ~desc astate
      in
      (Basic.ok_continue astate, non_disj)
  | _, _ ->
      (* if the model cannot find a template argument and/or the formal parameters,
         it just marks the object non-empty *)
      ( assign_non_empty_value
          (snd @@ ValueOrigin.addr_hist other_payload)
          this_payload
          ~desc:(desc ^ " (cannot find template argument and/or formal parameters)")
          model_data astate
      , non_disj )


let assign_value args ~desc : model =
  match args with
  | [this; value] ->
      assign_precise_value this value ~desc:(desc ^ " (precise value)")
  | {FuncArg.arg_payload= this} :: _ ->
      assign_non_empty_value ValueHistory.epoch this ~desc:(desc ^ " (non-empty value)")
      |> lift_model
  | _ ->
      L.internal_error "Not enough arguments to call the constructor for Optional@\n" ;
      Basic.skip |> lift_model


let copy_assignment (FuncArg.{arg_payload= this_payload} as this)
    FuncArg.{typ; arg_payload= other_payload} ~desc : model =
 fun ({path; location} as model_data) astate non_disj ->
  let ( let<*> ) x f = bind_sat_result non_disj (Sat x) f in
  let ( let<**> ) x f = bind_sat_result non_disj x f in
  let other_payload = ValueOrigin.addr_hist other_payload in
  let<*> astate, ((other_addr, other_hist) as other), (other_flag, _) =
    read_fields path location ~value_mode:Read ~flag_mode:Read other_payload astate
  in
  match get_template_arg typ with
  | Some typ ->
      let assign_none, non_disj =
        let<**> astate = prune_empty ~flag:other_flag ~value:other_addr astate in
        (* share the value of [other] so that accesses to the copy, often a temporary, are reported
           in terms of [other]; only invalidate it when it is already the marker of an empty
           optional, as it may be the contained value of an engaged optional in callers *)
        if is_empty_marker other_addr astate then
          (assign_none ~marker:other_addr other_hist this_payload ~desc model_data astate, non_disj)
        else
          let<*> astate, _ =
            write_empty path location
              (ValueOrigin.addr_hist this_payload)
              ~marker:other ~desc astate
          in
          (Basic.ok_continue astate, non_disj)
      in
      let assign_value, non_disj =
        let<**> astate = prune_engaged ~flag:other_flag ~pointer:other_addr astate in
        assign_precise_value this
          {exp= Var (Ident.create_none ()); typ; arg_payload= ValueOrigin.unknown other}
          ~desc model_data astate non_disj
      in
      (assign_none @ assign_value, non_disj)
  | None ->
      (Basic.ok_continue astate, non_disj)


let emplace optional ~desc : model_no_non_disj =
  (* TODO: destroy current object and call move constructor *)
  assign_non_empty_value ValueHistory.epoch optional ~desc


let value optional ~desc : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let optional = ValueOrigin.addr_hist optional in
  let<*> astate, ((value_addr, value_hist) as value), (flag, _) =
    read_fields path location ~value_mode:Write ~flag_mode:NoAccess optional astate
  in
  (* Check dereference to show an error at the callsite of `value()` *)
  let<*> astate, _ = PulseOperations.eval_access path Write location value Dereference astate in
  let astate =
    PulseOperations.write_id ret_id (value_addr, Hist.add_call path location desc value_hist) astate
  in
  if PulseArithmetic.is_known_zero astate flag then
    let<**> astate = empty_after_access value_addr astate in
    Basic.ok_continue astate
  else if is_known_engaged flag astate then Basic.ok_continue astate
  else if Option.is_some (must_be_valid_in_pre value_addr astate) then
    let<+> astate = set_engaged_in_post path location optional ~hist:value_hist astate in
    astate
  else
    let<++> astate = PulseArithmetic.prune_positive flag astate in
    astate


let has_value this ~desc : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let this = ValueOrigin.addr_hist this in
  let<*> astate, (value_addr, _), (flag, _) =
    read_fields path location ~value_mode:NoAccess ~flag_mode:Write this astate
  in
  let hist = Hist.single_call path location desc in
  let empty =
    PulseOperations.write_id ret_id (flag, hist) astate
    |> prune_empty ~flag ~value:value_addr
    >>|| ExecutionDomain.continue
  in
  let engaged =
    (* a constant makes the program's own test of the result trivial, see [prune_engaged] *)
    let astate, one = PulseArithmetic.absval_of_int astate IntLit.one in
    PulseOperations.write_id ret_id (one, hist) astate
    |> PulseArithmetic.and_positive flag >>|| ExecutionDomain.continue
  in
  SatUnsat.to_list empty @ SatUnsat.to_list engaged


let get_pointer optional ~desc : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let optional = ValueOrigin.addr_hist optional in
  let<*> astate, value_addr, (flag, _) =
    read_fields path location ~value_mode:Read ~flag_mode:Read optional astate
  in
  let value_update_hist =
    (fst value_addr, Hist.add_call path location desc ~more:"non-empty case" (snd value_addr))
  in
  let astate_value_addr =
    PulseOperations.write_id ret_id value_update_hist astate
    |> prune_engaged ~flag ~pointer:(fst value_addr)
    >>|| ExecutionDomain.continue
  in
  let nullptr =
    (AbstractValue.mk_fresh (), Hist.single_call path location desc ~more:"empty case")
  in
  let astate_null =
    PulseOperations.write_id ret_id nullptr astate
    |> prune_empty ~flag ~value:(fst value_addr)
    >>== PulseArithmetic.and_eq_int (fst nullptr) IntLit.zero
    >>|| PulseOperations.invalidate path
           (StackAddress (Var.of_id ret_id, snd nullptr))
           location (ConstantDereference IntLit.zero) nullptr
    >>|| ExecutionDomain.continue
  in
  SatUnsat.to_list astate_value_addr @ SatUnsat.to_list astate_null


let value_or_common ~assign_ret optional default ~desc : model_no_non_disj =
 fun {path; location} astate ->
  let optional = ValueOrigin.addr_hist optional in
  let default = ValueOrigin.addr_hist default in
  let<*> astate, value_addr, (flag, _) =
    read_fields path location ~value_mode:Read ~flag_mode:Read optional astate
  in
  let astate_non_empty =
    let** astate_non_empty, value =
      prune_engaged ~flag ~pointer:(fst value_addr) astate
      >>|= PulseOperations.eval_access path Read location value_addr Dereference
    in
    let value_update_hist =
      (fst value, Hist.add_call path location desc ~more:"non-empty case" (snd value))
    in
    assign_ret value_update_hist astate_non_empty >>|| Basic.continue
  in
  let astate_default =
    let=* astate, (default_val, default_hist) =
      PulseOperations.eval_access path Read location default Dereference astate
    in
    let default_value_hist =
      (default_val, Hist.add_call path location desc ~more:"empty case" default_hist)
    in
    prune_empty ~flag ~value:(fst value_addr) astate
    >>== assign_ret default_value_hist >>|| Basic.continue
  in
  SatUnsat.to_list astate_non_empty @ SatUnsat.to_list astate_default


let value_or optional default ~desc : model_no_non_disj =
 fun ({ret= ret_id, _} as model_data) astate ->
  value_or_common
    ~assign_ret:(fun value_hist astate ->
      Sat (Ok (PulseOperations.write_id ret_id value_hist astate)) )
    optional default ~desc model_data astate


let value_or_obj optional default return_param ~desc : model_no_non_disj =
 fun ({analysis_data= {tenv}; path; callee_procname; location; ret} as model_data) astate ->
  value_or_common
    ~assign_ret:(fun _value_hist astate ->
      let {FuncArg.typ; arg_payload} = return_param in
      let addr_hist = ValueOrigin.addr_hist arg_payload in
      (* note: It is ideal to find and run a copy constructor like we do in
         [assign_precise_value]. *)
      PulseCallOperations.unknown_call tenv path location (Call callee_procname)
        (Some callee_procname) ~ret
        ~actuals:[(addr_hist, typ)]
        ~formals_opt:None astate )
    optional default ~desc model_data astate


let destruct FuncArg.{arg_payload= this; typ} ~desc : model =
 fun ({path; location} as model_data) astate non_disj ->
  let this = ValueOrigin.addr_hist this in
  match get_template_arg typ with
  | Some typ ->
      let ( let<*> ) x f = bind_sat_result non_disj (Sat x) f in
      let ( let<**> ) x f = bind_sat_result non_disj x f in
      let<*> astate, (value_addr, value_hist), (flag, _) =
        read_fields path location ~value_mode:NoAccess ~flag_mode:NoAccess this astate
      in
      let empty, non_disj =
        let<**> astate = prune_empty ~flag ~value:value_addr astate in
        (Basic.ok_continue astate, non_disj)
      in
      let engaged, non_disj =
        let<**> astate = prune_engaged ~flag ~pointer:value_addr astate in
        let value_hist = Hist.add_call path location desc value_hist in
        let deleted_arg =
          { FuncArg.arg_payload= ValueOrigin.Unknown (value_addr, value_hist)
          ; exp= Var (Ident.create_fresh Ident.kprimed)
          ; typ= {desc= Tptr (typ, Pk_pointer); quals= Typ.mk_type_quals ()} }
        in
        Basic.free_or_delete `Delete CppDelete deleted_arg model_data astate non_disj
      in
      (empty @ engaged, non_disj)
  | None ->
      (Basic.ok_continue astate, non_disj)


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [ -"boost" &:: "optional" &:: "optional" <>$ capt_arg_payload
    $+ any_arg_of_typ (-"boost" &:: "none_t")
    $--> assign_none ValueHistory.epoch ~desc:"boost::optional::optional(=none_t)"
    |> with_non_disj
  ; -"boost" &:: "optional" &:: "optional" <>$ capt_arg_payload
    $--> assign_none ValueHistory.epoch ~desc:"boost::optional::optional()"
    |> with_non_disj
  ; -"boost" &:: "optional" &:: "optional" <>$ capt_arg
    $+ capt_arg_of_typ (-"boost" &:: "optional")
    $--> copy_assignment ~desc:"boost::optional::optional(boost::optional<Value> arg)"
  ; -"boost" &:: "optional" &:: "optional"
    &++> assign_value ~desc:"boost::optional::optional(Value arg)"
  ; -"boost" &:: "optional" &:: "operator=" $ capt_arg_payload
    $+ any_arg_of_typ (-"boost" &:: "none_t")
    $--> assign_none ValueHistory.epoch ~desc:"boost::optional::operator=(none_t)"
    |> with_non_disj
  ; -"boost" &:: "optional" &:: "operator=" $ capt_arg
    $+ capt_arg_of_typ (-"boost" &:: "optional")
    $--> copy_assignment ~desc:"boost::optional::operator=(boost::optional<Value> arg)"
  ; -"boost" &:: "optional" &:: "operator="
    &++> assign_value ~desc:"boost::optional::operator=(Value arg)"
  ; -"boost" &:: "optional" &:: "is_initialized" <>$ capt_arg_payload
    $+...$--> has_value ~desc:"boost::optional::is_initialized()"
    |> with_non_disj
  ; -"boost" &:: "optional" &:: "operator_bool" <>$ capt_arg_payload
    $+...$--> has_value ~desc:"boost::optional::operator_bool()"
    |> with_non_disj
  ; -"boost" &:: "optional" &:: "reset" <>$ capt_arg_payload
    $--> assign_none ValueHistory.epoch ~desc:"boost::optional::reset()"
    |> with_non_disj
  ; -"boost" &:: "optional" &:: "reset" &++> assign_value ~desc:"boost::optional::reset(Value arg)"
  ; -"boost" &:: "optional" &:: "get" <>$ capt_arg_payload
    $+...$--> value ~desc:"boost::optional::get()"
    |> with_non_disj
  ; -"boost" &:: "optional" &:: "operator*" <>$ capt_arg_payload
    $+...$--> value ~desc:"boost::optional::operator*()"
    |> with_non_disj
  ; -"boost" &:: "optional" &:: "operator->" <>$ capt_arg_payload
    $+...$--> value ~desc:"boost::optional::operator->()"
    |> with_non_disj
  ; -"boost" &:: "optional" &:: "get_ptr" $ capt_arg_payload
    $+...$--> get_pointer ~desc:"boost::optional::get_ptr()"
    |> with_non_disj
  ; -"boost" &:: "optional" &:: "get_value_or" $ capt_arg_payload $+ capt_arg_payload
    $+...$--> value_or ~desc:"boost::optional::get_value_or()"
    |> with_non_disj
  ; -"boost" &:: "optional" &:: "~optional" $ capt_arg
    $--> destruct ~desc:"boost::optional::~optional()"
  ; -"folly" &:: "Optional" &:: "Optional" <>$ capt_arg_payload
    $+ any_arg_of_typ (-"folly" &:: "None")
    $--> assign_none ValueHistory.epoch ~desc:"folly::Optional::Optional(=None)"
    |> with_non_disj
  ; -"folly" &:: "Optional" &:: "Optional" <>$ capt_arg_payload
    $--> assign_none ValueHistory.epoch ~desc:"folly::Optional::Optional()"
    |> with_non_disj
  ; -"folly" &:: "Optional" &:: "Optional" <>$ capt_arg
    $+ capt_arg_of_typ (-"folly" &:: "Optional")
    $--> copy_assignment ~desc:"folly::Optional::Optional(folly::Optional<Value> arg)"
  ; -"folly" &:: "Optional" &:: "Optional"
    &++> assign_value ~desc:"folly::Optional::Optional(Value arg)"
  ; -"folly" &:: "Optional" &:: "assign" <>$ capt_arg_payload
    $+ any_arg_of_typ (-"folly" &:: "None")
    $--> assign_none ValueHistory.epoch ~desc:"folly::Optional::assign(=None)"
    |> with_non_disj
  ; -"folly" &:: "Optional" &:: "assign" <>$ capt_arg
    $+ capt_arg_of_typ (-"folly" &:: "Optional")
    $--> copy_assignment ~desc:"folly::Optional::assign(folly::Optional<Value> arg)"
  ; -"folly" &:: "Optional" &:: "assign"
    &++> assign_value ~desc:"folly::Optional::assign(Value arg)"
  ; -"folly" &:: "Optional" &:: "emplace<>" $ capt_arg_payload
    $+...$--> emplace ~desc:"folly::Optional::emplace()"
    |> with_non_disj
  ; -"folly" &:: "Optional" &:: "emplace" $ capt_arg_payload
    $+...$--> emplace ~desc:"folly::Optional::emplace()"
    |> with_non_disj
  ; -"folly" &:: "Optional" &:: "has_value" <>$ capt_arg_payload
    $+...$--> has_value ~desc:"folly::Optional::has_value()"
    |> with_non_disj
  ; -"folly" &:: "Optional" &:: "reset" <>$ capt_arg_payload
    $+...$--> assign_none ValueHistory.epoch ~desc:"folly::Optional::reset()"
    |> with_non_disj
  ; -"folly" &:: "Optional" &:: "value" <>$ capt_arg_payload
    $+...$--> value ~desc:"folly::Optional::value()"
    |> with_non_disj
  ; -"folly" &:: "Optional" &:: "operator*" <>$ capt_arg_payload
    $+...$--> value ~desc:"folly::Optional::operator*()"
    |> with_non_disj
  ; -"folly" &:: "Optional" &:: "operator->" <>$ capt_arg_payload
    $+...$--> value ~desc:"folly::Optional::operator->()"
    |> with_non_disj
  ; -"folly" &:: "Optional" &:: "get_pointer" $ capt_arg_payload
    $+...$--> get_pointer ~desc:"folly::Optional::get_pointer()"
    |> with_non_disj
  ; -"folly" &:: "Optional" &:: "value_or" $ capt_arg_payload $+ capt_arg_payload
    $+...$--> value_or ~desc:"folly::Optional::value_or()"
    |> with_non_disj
  ; -"folly" &:: "Optional" &:: "~Optional" $ capt_arg
    $--> destruct ~desc:"folly::Optional::~Optional()"
  ; -"std" &:: "optional" &:: "optional" $ capt_arg_payload
    $+ any_arg_of_typ (-"std" &:: "nullopt_t")
    $--> assign_none ValueHistory.epoch ~desc:"std::optional::optional(=nullopt)"
    |> with_non_disj
  ; -"std" &:: "optional" &:: "optional" $ capt_arg_payload
    $--> assign_none ValueHistory.epoch ~desc:"std::optional::optional()"
    |> with_non_disj
  ; -"std" &:: "optional" &:: "optional" $ capt_arg
    $+ capt_arg_of_typ (-"std" &:: "optional")
    $--> copy_assignment ~desc:"std::optional::optional(std::optional<Value> arg)"
  ; -"std" &:: "optional" &:: "optional"
    &++> assign_value ~desc:"std::optional::optional(Value arg)"
  ; -"std" &:: "optional" &:: "operator=" $ capt_arg_payload
    $+ any_arg_of_typ (-"std" &:: "nullopt_t")
    $--> assign_none ValueHistory.epoch ~desc:"std::optional::operator=(nullopt_t)"
    |> with_non_disj
  ; -"std" &:: "optional" &:: "operator=" $ capt_arg
    $+ capt_arg_of_typ (-"std" &:: "optional")
    $--> copy_assignment ~desc:"std::optional::operator=(std::optional<Value> arg)"
  ; -"std" &:: "optional" &:: "operator="
    &++> assign_value ~desc:"std::optional::operator=(Value arg)"
  ; -"std" &:: "optional" &:: "emplace<>" $ capt_arg_payload
    $+...$--> emplace ~desc:"std::optional::emplace()"
    |> with_non_disj
  ; -"std" &:: "optional" &:: "emplace" $ capt_arg_payload
    $+...$--> emplace ~desc:"std::optional::emplace()"
    |> with_non_disj
  ; -"std" &:: "optional" &:: "has_value" <>$ capt_arg_payload
    $+...$--> has_value ~desc:"std::optional::has_value()"
    |> with_non_disj
  ; -"std" &:: "__optional_storage_base" &:: "has_value" $ capt_arg_payload
    $+...$--> has_value ~desc:"std::optional::has_value()"
    |> with_non_disj
  ; -"std" &:: "optional" &:: "operator_bool" <>$ capt_arg_payload
    $+...$--> has_value ~desc:"std::optional::operator_bool()"
    |> with_non_disj
  ; -"std" &:: "optional" &:: "reset" <>$ capt_arg_payload
    $+...$--> assign_none ValueHistory.epoch ~desc:"std::optional::reset()"
    |> with_non_disj
  ; -"std" &:: "optional" &:: "value" <>$ capt_arg_payload
    $+...$--> value ~desc:"std::optional::value()"
    |> with_non_disj
  ; -"std" &:: "optional" &:: "operator*" <>$ capt_arg_payload
    $+...$--> value ~desc:"std::optional::operator*()"
    |> with_non_disj
  ; -"std" &:: "optional" &:: "operator->" <>$ capt_arg_payload
    $+...$--> value ~desc:"std::optional::operator->()"
    |> with_non_disj
  ; -"std" &:: "optional" &:: "value_or" $ capt_arg_payload $+ capt_arg_payload
    $--> value_or ~desc:"std::optional::value_or()"
    |> with_non_disj
  ; -"std" &:: "optional" &:: "value_or" $ capt_arg_payload $+ capt_arg_payload $+ capt_arg
    $--> value_or_obj ~desc:"std::optional::value_or()"
    |> with_non_disj
  ; -"std" &:: "optional" &:: "~optional" $ capt_arg
    $--> destruct ~desc:"std::optional::~optional()" ]
