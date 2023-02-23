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
module CheapCopyTypes = PulseCheapCopyTypes

type origin =
  | Copy  (** the copied value *)
  | Source  (** the original source value that has been copied *)
  | Parameter  (** the parameter value that is checked for modifications *)
[@@deriving show {with_path= false}]

let is_param = function Parameter -> true | Source | Copy -> false

let get_modeled_as_returning_copy_opt proc_name =
  Option.value_map ~default:None Config.pulse_model_returns_copy_pattern ~f:(fun r ->
      let s = Procname.to_string proc_name in
      if Str.string_match r s 0 then Some Attribute.CopyOrigin.CopyCtor else None )


let get_copy_origin pname =
  let open IOption.Let_syntax in
  let* attrs = IRAttributes.load pname in
  if attrs.ProcAttributes.is_cpp_copy_ctor then Some Attribute.CopyOrigin.CopyCtor
  else if attrs.ProcAttributes.is_cpp_copy_assignment then Some Attribute.CopyOrigin.CopyAssignment
  else None


let to_arg_payloads actuals =
  List.map actuals ~f:(fun (exp, typ) ->
      ProcnameDispatcher.Call.FuncArg.{exp; typ; arg_payload= ()} )


let is_optional_copy_constructor_with_arg_payloads =
  let dispatch : (unit, unit, unit) ProcnameDispatcher.Call.dispatcher =
    let open ProcnameDispatcher.Call in
    make_dispatcher
      [ -"boost" &:: "optional" &:: "optional" $ any_arg
        $+ any_arg_of_typ (-"boost" &:: "optional")
        $--> ()
      ; -"folly" &:: "Optional" &:: "Optional" $ any_arg
        $+ any_arg_of_typ (-"folly" &:: "Optional")
        $--> ()
      ; -"std" &:: "optional" &:: "optional" $ any_arg
        $+ any_arg_of_typ (-"std" &:: "optional")
        $--> () ]
  in
  fun pname arg_payloads ->
    dispatch () pname arg_payloads |> Option.value_map ~default:false ~f:(fun () -> true)


let is_optional_copy_constructor pname actuals =
  is_optional_copy_constructor_with_arg_payloads pname (to_arg_payloads actuals)


let get_element_copy_by_optional =
  let dispatch : (unit, bool, unit) ProcnameDispatcher.Call.dispatcher =
    let open ProcnameDispatcher.Call in
    make_dispatcher
      [ -"boost" &:: "optional" &:: "optional" $ any_arg
        $+ any_arg_of_typ (-"boost" &:: "none_t")
        $--> false
      ; -"boost" &:: "optional" &:: "optional" $ any_arg $+ any_arg $--> true
      ; -"folly" &:: "Optional" &:: "Optional" $ any_arg
        $+ any_arg_of_typ (-"folly" &:: "None")
        $--> false
      ; -"folly" &:: "Optional" &:: "Optional" $ any_arg $+ any_arg $--> true
      ; -"std" &:: "optional" &:: "optional" $ any_arg
        $+ any_arg_of_typ (-"std" &:: "nullopt_t")
        $--> false
      ; -"std" &:: "optional" &:: "optional" $ any_arg $+ any_arg $--> true ]
  in
  fun pname actuals ->
    let arg_payloads = to_arg_payloads actuals in
    if is_optional_copy_constructor_with_arg_payloads pname arg_payloads then None
    else
      dispatch () pname arg_payloads
      |> Option.bind ~f:(fun matched -> Option.some_if matched Attribute.CopyOrigin.CopyToOptional)


let try_eval path location e astate =
  match PulseOperations.eval path NoAccess location e astate with
  | Sat (Ok (astate, (v, _))) ->
      Some (astate, v)
  | Sat (Recoverable _ | FatalError _) | Unsat ->
      None


let get_copied_and_source ({PathContext.timestamp} as path) rest_args location from
    (astate : AbductiveDomain.t) =
  let heap = (astate.post :> BaseDomain.t).heap in
  let copied source_typ =
    NonDisjDomain.Copied {heap; source_typ; location; copied_location= None; from; timestamp}
  in
  let astate, source_addr_typ_opt =
    match rest_args with
    | (source_arg, source_typ) :: _ -> (
      match try_eval path location source_arg astate with
      | Some (astate, source_addr) ->
          let source_expr = Decompiler.find source_addr astate in
          (astate, Some (source_addr, source_expr, source_typ))
      | None ->
          (astate, None) )
    | _ ->
        (astate, None)
  in
  (copied (Option.map ~f:trd3 source_addr_typ_opt), astate, source_addr_typ_opt)


let is_modeled_as_cheap_to_copy tenv actual_typ =
  match actual_typ with
  | {Typ.desc= Tptr ({desc= Tstruct actual_name}, _)} ->
      Option.exists Config.pulse_model_cheap_copy_type ~f:(fun cheap_modeled ->
          PatternMatch.supertype_exists tenv
            (fun type_name _struct -> Str.string_match cheap_modeled (Typ.Name.name type_name) 0)
            actual_name )
  | _ ->
      false


let is_known_cheap_copy typ =
  match typ.Typ.desc with
  | Tptr ({desc= Tstruct typename}, _) ->
      CheapCopyTypes.is_known_cheap_copy typename
  | _ ->
      false


let is_cheap_to_copy tenv typ =
  (Typ.is_pointer typ && Typ.is_trivially_copyable (Typ.strip_ptr typ).quals)
  || is_modeled_as_cheap_to_copy tenv typ
  || is_known_cheap_copy typ


let has_copy_in str = String.is_substring (String.lowercase str) ~substring:"copy"

let has_copy_in_name pname = has_copy_in (Procname.get_method pname)

let get_return_param pdesc =
  let attrs = Procdesc.get_attributes pdesc in
  if attrs.ProcAttributes.has_added_return_param then
    Some (Pvar.get_ret_param_pvar (Procdesc.get_proc_name pdesc))
  else None


let is_copy_into_local copied_var =
  Var.appears_in_source_code copied_var && not (Var.is_global copied_var)


let is_local_variable = function Exp.Lvar pvar -> Pvar.is_local pvar | _ -> false

let continue_bind exec_state ~f =
  match (exec_state : ExecutionDomain.t) with
  | ContinueProgram astate ->
      f astate
  | ExceptionRaised _
  | AbortProgram _
  | ExitProgram _
  | LatentAbortProgram _
  | LatentInvalidAccess _ ->
      None


let continue_map exec_state ~f = continue_bind exec_state ~f:(fun astate -> Some (f astate))

let continue_fold astates ~init ~f =
  List.fold astates ~init ~f:(fun acc exec_state ->
      continue_map exec_state ~f:(fun astate -> f acc astate) |> Option.value ~default:acc )


let continue_fold_map astates ~init ~f =
  List.fold_map astates ~init ~f:(fun acc exec_state ->
      continue_bind exec_state ~f:(fun astate -> f acc astate)
      |> Option.value_map ~default:(acc, exec_state) ~f:(fun (acc, astate) ->
             (acc, ExecutionDomain.continue astate) ) )


let is_copy_assigned_from_this ~from source_addr_typ_opt =
  Attribute.CopyOrigin.equal from CopyAssignment
  && Option.exists source_addr_typ_opt ~f:(fun (_, source_expr, _) ->
         match source_expr with
         | DecompilerExpr.SourceExpr ((PVar pvar, _), _) ->
             Pvar.is_this pvar
         | _ ->
             false )


let add_copies_to_pvar_or_field tenv path location from args (astate_n, astate) =
  let open IOption.Let_syntax in
  match (args : (Exp.t * Typ.t) list) with
  | ((Lvar copy_pvar | Lindex (Lvar copy_pvar, _)), copy_type) :: rest_args
    when (not (is_cheap_to_copy tenv copy_type)) && not (NonDisjDomain.is_locked astate_n) ->
      let copied_var = Var.of_pvar copy_pvar in
      let copied, astate, source_addr_typ_opt =
        get_copied_and_source path rest_args location from astate
      in
      let* _, source_expr, _ = source_addr_typ_opt in
      let copy_into_opt : Attribute.CopiedInto.t option =
        if is_copy_assigned_from_this ~from source_addr_typ_opt then
          (* If source is copy assigned from a member field, we cannot suggest move as other procedures might access it. *)
          None
        else
          (* order matters here  *)
          match (source_expr : DecompilerExpr.t) with
          | SourceExpr (source_expr, _) when is_copy_into_local copied_var ->
              (* case 1: we copy into a local variable that occurs in the code with a known source  *)
              Some (IntoVar {copied_var; source_opt= Some source_expr})
          | SourceExpr (((PVar pvar, _) as source_expr), _)
            when not (Pvar.is_frontend_tmp pvar || Pvar.is_this pvar) ->
              (* case 2: we copy into an intermediate that is not a field member/frontend temp and source is known. This is the case for intermediate copies of the pass by value arguments. *)
              Some (IntoIntermediate {copied_var; source_opt= Some source_expr})
          | Unknown _ when is_copy_into_local copied_var ->
              (* case 3: analogous to case 1 but source is an unknown call that is know no create a copy *)
              Some (IntoVar {copied_var; source_opt= None})
          | Unknown _ ->
              (* case 4: analogous to case 2 but source is an unknown call that is know no create a copy *)
              Some (IntoIntermediate {copied_var; source_opt= None})
          | _ ->
              None
      in
      Option.map copy_into_opt ~f:(fun copy_into ->
          let copy_addr, _ = Option.value_exn (Stack.find_opt copied_var astate) in
          let astate' =
            Option.value_map source_addr_typ_opt ~default:astate
              ~f:(fun (source_addr, _, source_typ) ->
                AddressAttributes.add_one source_addr (CopiedInto copy_into) astate
                |> AddressAttributes.add_one copy_addr
                     (SourceOriginOfCopy
                        {source= source_addr; is_const_ref= Typ.is_const_reference source_typ} ) )
          in
          ( NonDisjDomain.add_var copy_into
              ~source_addr_opt:(Option.map source_addr_typ_opt ~f:fst3)
              copied astate_n
          , astate' ) )
  | ((Lfield (_, field, _) as exp), copy_type) :: ((_, source_typ) :: _ as rest_args)
    when not (is_cheap_to_copy tenv copy_type) ->
      let copied, astate, source_addr_typ_opt =
        get_copied_and_source path rest_args location from astate
      in
      let+ astate, copy_addr = try_eval path location exp astate in
      let astate' =
        Option.value_map source_addr_typ_opt ~default:astate
          ~f:(fun (source_addr, source_expr, _) ->
            AddressAttributes.add_one source_addr
              (CopiedInto (IntoField {field; source_opt= Some source_expr}))
              astate
            |> AddressAttributes.add_one copy_addr
                 (SourceOriginOfCopy
                    {source= source_addr; is_const_ref= Typ.is_const_reference source_typ} ) )
      in
      ( NonDisjDomain.add_field field
          ~source_opt:(Option.map source_addr_typ_opt ~f:snd3)
          copied astate_n
      , astate' )
  | _ ->
      None


let get_copied_return_addr proc_desc path location copy_var astate =
  let open IOption.Let_syntax in
  let* return_param = get_return_param proc_desc in
  let astate, (return_param_addr, _) = PulseOperations.eval_var path location return_param astate in
  let* copy_addr, _ = PulseOperations.read_id copy_var astate in
  let* return_addr, _ = Memory.find_edge_opt return_param_addr Dereference astate in
  if AbstractValue.equal copy_addr return_addr then Some (copy_addr, astate) else None


let add_copies_to_return tenv proc_desc path location from args (astate_n, astate) =
  let open IOption.Let_syntax in
  match (args : (Exp.t * Typ.t) list) with
  | (Var copy_var, copy_type) :: (source_exp, source_typ) :: _
    when (not (is_cheap_to_copy tenv copy_type))
         && (not (Typ.is_pointer_to_smart_pointer copy_type))
         && (not (has_copy_in_name (Procdesc.get_proc_name proc_desc)))
         && (not (NonDisjDomain.is_locked astate_n))
         && not (is_local_variable source_exp) ->
      let* copy_addr, astate = get_copied_return_addr proc_desc path location copy_var astate in
      let+ astate, source = try_eval path location source_exp astate in
      let astate =
        AddressAttributes.add_copied_return copy_addr ~source
          ~is_const_ref:(Typ.is_const_reference source_typ)
          from location astate
      in
      (astate_n, astate)
  | _ ->
      None


let remove_optional_copies_to_return proc_desc path location pname args (astate_n, astate) =
  let open IOption.Let_syntax in
  match (args : (Exp.t * Typ.t) list) with
  | (Var copy_var, _) :: (Lvar source_var, _) :: _ when is_optional_copy_constructor pname args ->
      let+ _, astate = get_copied_return_addr proc_desc path location copy_var astate in
      let astate_n = NonDisjDomain.remove_var (Var.of_pvar source_var) astate_n in
      (astate_n, astate)
  | _ ->
      None


let add_copies tenv proc_desc path location pname actuals default =
  let aux copy_check_fn args_map_fn default =
    Option.bind (copy_check_fn pname) ~f:(fun from ->
        let ( |-> ) = IOption.continue ~default in
        let args = args_map_fn actuals in
        add_copies_to_pvar_or_field tenv path location from args default
        |-> add_copies_to_return tenv proc_desc path location from args
        (* Ignore optional copies to return value to avoid false positives w.r.t. RVO/NRVO *)
        |-> remove_optional_copies_to_return proc_desc path location pname args )
  in
  let ( |-> ) = IOption.continue ~default in
  aux get_copy_origin Fn.id default
  (* For functions that return a copy, the last argument is the assigned copy *)
  |-> aux get_modeled_as_returning_copy_opt List.rev
  (* Record a copy of element in optional constructors *)
  |-> aux (fun pname -> get_element_copy_by_optional pname actuals) Fn.id


let is_lock pname =
  let method_name = Procname.get_method pname in
  String.equal method_name "lock" || String.equal method_name "rlock"


let get_copied_into copied_var source_addr_opt astate : Attribute.CopiedInto.t =
  let source_opt =
    Option.bind source_addr_opt ~f:(fun source_addr ->
        match Decompiler.find source_addr astate with
        | SourceExpr (source_expr, _) ->
            Some source_expr
        | _ ->
            None )
  in
  if is_copy_into_local copied_var then IntoVar {copied_var; source_opt}
  else IntoIntermediate {copied_var; source_opt}


let add_copied_return path location pname actuals (astate_n, astate) =
  let open IOption.Let_syntax in
  if is_lock pname then Some (NonDisjDomain.set_locked astate_n, astate)
  else if not (NonDisjDomain.is_locked astate_n) then
    match (IRAttributes.load pname, List.last actuals) with
    | Some attrs, Some ((Exp.Lvar ret_pvar as ret), copy_type) when attrs.has_added_return_param ->
        let copied_var = Var.of_pvar ret_pvar in
        if is_copy_into_local copied_var then
          let* astate, ret_addr = try_eval path location ret astate in
          let+ source, is_const_ref, from, copied_location =
            AddressAttributes.get_copied_return ret_addr astate
          in
          let into = get_copied_into copied_var (Some source) astate in
          let astate =
            AddressAttributes.remove_copied_return ret_addr astate
            |> AddressAttributes.add_one source (CopiedInto into)
            |> AddressAttributes.add_one ret_addr (SourceOriginOfCopy {source; is_const_ref})
          in
          let astate_n =
            NonDisjDomain.add_var into ~source_addr_opt:(Some source)
              (Copied
                 { heap= (astate.post :> BaseDomain.t).heap
                 ; source_typ= Some (Typ.strip_ptr copy_type)
                 ; location
                 ; copied_location= Some (pname, copied_location)
                 ; from
                 ; timestamp= path.PathContext.timestamp } )
              astate_n
          in
          (astate_n, astate)
        else None
    | _, _ ->
        None
  else None


let call tenv proc_desc path loc ~call_exp ~actuals astates astate_n =
  match (call_exp : Exp.t) with
  | Const (Cfun pname) | Closure {name= pname} ->
      continue_fold_map astates ~init:astate_n ~f:(fun astate_n astate ->
          let default = (astate_n, astate) in
          let ( |-> ) = IOption.continue ~default in
          add_copies tenv proc_desc path loc pname actuals default
          |-> add_copied_return path loc pname actuals )
  | _ ->
      (astate_n, astates)


let is_folly_coro_task =
  let matcher = QualifiedCppName.Match.of_fuzzy_qual_names ["folly::coro::Task"] in
  fun typ ->
    match typ.Typ.desc with
    | Tptr ({desc= Tstruct (CppClass {name})}, _) ->
        QualifiedCppName.Match.match_qualifiers matcher name
    | _ ->
        false


let init_const_refable_parameters procdesc tenv astates astate_n =
  if
    Option.exists (Procdesc.get_ret_param_type procdesc) ~f:is_folly_coro_task
    || Procname.is_lambda_or_block (Procdesc.get_proc_name procdesc)
  then astate_n
  else
    let proc_parameters = Procdesc.get_passed_by_value_formals procdesc in
    let location = Procdesc.get_loc procdesc in
    continue_fold astates ~init:astate_n ~f:(fun astate_n astate ->
        List.fold proc_parameters ~init:astate_n ~f:(fun astate_n (pvar, typ) ->
            let var = Var.of_pvar pvar in
            if
              Var.appears_in_source_code var && Typ.is_reference typ
              && (not (is_cheap_to_copy tenv typ))
              && (not (Var.is_cpp_unnamed_param var))
              && (* [unique_ptr] is ignored since it is not copied. This condition can be removed if
                    we can distinguish whether a class has a copy constructor or not. *)
              not (Typ.is_pointer_to_unique_pointer typ)
            then
              (* [&] is added by the frontend and type is pass-by-value anyways so strip it *)
              NonDisjDomain.add_parameter var
                (Unmodified
                   {heap= (astate.post :> BaseDomain.t).heap; typ= Typ.strip_ptr typ; location} )
                astate_n
            else astate_n ) )


let is_matching_edges ~get_repr ~edges_curr ~edges_orig =
  Option.value_map edges_orig ~default:true ~f:(fun edges_orig ->
      BaseMemory.Edges.for_all edges_curr ~f:(fun (access_curr, (addr_curr, _)) ->
          match BaseMemory.Edges.find_opt access_curr edges_orig with
          | Some (addr_orig, _) ->
              (* check matching for the addresses on the copy and the current heap. *)
              AbstractValue.equal (get_repr addr_curr) (get_repr addr_orig)
          | None ->
              (* address only occurs on the current heap, most likely it has been read since the
                 copy.  Continue exploring the rest of the edges...*)
              true ) )


let is_modified_since_detected addr ~is_param ~get_repr ~current_heap ~current_attrs ~copy_heap
    ~(copy_timestamp : Timestamp.t) ~source_addr_opt =
  let is_written_after_copy addr =
    BaseAddressAttributes.get_written_to addr current_attrs
    |> Option.exists ~f:(fun ((timestamp : Timestamp.t), _) ->
           (copy_timestamp :> int) < (timestamp :> int) )
  in
  let rec aux ~addr_to_explore ~visited =
    match addr_to_explore with
    | [] ->
        false
    | addr :: addr_to_explore -> (
        if AbstractValue.Set.mem addr visited then aux ~addr_to_explore ~visited
        else
          let visited = AbstractValue.Set.add addr visited in
          let is_moved =
            (is_param || BaseAddressAttributes.is_copied_from_const_ref addr current_attrs)
            && BaseAddressAttributes.is_std_moved addr current_attrs
          in
          is_moved || is_written_after_copy addr
          ||
          match BaseMemory.find_opt addr current_heap with
          | None ->
              aux ~addr_to_explore ~visited
          | Some edges_curr ->
              (not
                 (is_matching_edges ~get_repr ~edges_curr
                    ~edges_orig:(BaseMemory.find_opt addr copy_heap) ) )
              ||
              let addr_to_explore =
                BaseMemory.Edges.fold edges_curr ~init:addr_to_explore ~f:(fun acc (_, (addr, _)) ->
                    addr :: acc )
              in
              aux ~addr_to_explore ~visited )
  in
  (* check for modifications to values coming from addresses
     that is returned from unknown calls *)
  let addr_to_explore_opt =
    let open IOption.Let_syntax in
    let* source_addr = source_addr_opt in
    let+ return = BaseAddressAttributes.get_returned_from_unknown source_addr current_attrs in
    addr :: return
  in
  let addr_to_explore = Option.value addr_to_explore_opt ~default:[addr] in
  aux ~addr_to_explore ~visited:AbstractValue.Set.empty


let is_modified origin ~source_addr_opt address astate copy_heap copy_timestamp =
  let get_repr x = Formula.get_var_repr astate.AbductiveDomain.path_condition x in
  let current_heap = (astate.AbductiveDomain.post :> BaseDomain.t).heap in
  let current_attrs = (astate.AbductiveDomain.post :> BaseDomain.t).attrs in
  if Config.debug_mode then (
    let reachable_addresses_from_copy =
      BaseDomain.reachable_addresses_from (Caml.List.to_seq [address])
        (astate.AbductiveDomain.post :> BaseDomain.t)
    in
    let reachable_from heap =
      BaseMemory.filter
        (fun address _ -> AbstractValue.Set.mem address reachable_addresses_from_copy)
        heap
    in
    L.d_printfln_escaped "Current reachable heap %a" BaseMemory.pp (reachable_from current_heap) ;
    L.d_printfln_escaped "%a reachable heap %a" pp_origin origin BaseMemory.pp
      (reachable_from copy_heap) ) ;
  is_modified_since_detected address ~is_param:(is_param origin) ~get_repr ~current_heap
    ~current_attrs ~copy_heap ~copy_timestamp ~source_addr_opt


let mark_modified_address_at ~address ~source_addr_opt origin ~copied_into astate
    (astate_n : NonDisjDomain.t) : NonDisjDomain.t =
  NonDisjDomain.mark_copy_as_modified ~copied_into ~source_addr_opt astate_n
    ~is_modified:(is_modified origin ~source_addr_opt address astate)


let mark_modified_parameter_at ~address ~var astate (astate_n : NonDisjDomain.t) : NonDisjDomain.t =
  NonDisjDomain.mark_parameter_as_modified ~var astate_n
    ~is_modified:(is_modified Parameter ~source_addr_opt:None address astate)


let mark_modified_copies_and_parameters_on_abductive vars astate astate_n =
  let mark_modified_copy var default =
    Stack.find_opt var astate
    |> Option.value_map ~default ~f:(fun (address, _history) ->
           let source_addr_opt = AddressAttributes.get_source_origin_of_copy address astate in
           let copied_into = get_copied_into var source_addr_opt astate in
           mark_modified_address_at ~address ~source_addr_opt ~copied_into Copy astate default )
  in
  let mark_modified_parameter var default =
    Stack.find_opt var astate
    |> Option.value_map ~default ~f:(fun (address, _history) ->
           mark_modified_parameter_at ~address ~var astate default )
  in
  List.fold vars ~init:astate_n ~f:(fun astate_n var ->
      let astate_n = mark_modified_parameter var astate_n in
      (* mark modified copy when [var] is used as source *)
      let astate_n =
        (let open IOption.Let_syntax in
        let* source_addr, _ = Stack.find_opt var astate in
        let+ copied_into = AddressAttributes.get_copied_into source_addr astate in
        mark_modified_address_at ~address:source_addr ~source_addr_opt:(Some source_addr) Source
          ~copied_into astate astate_n)
        |> Option.value ~default:astate_n
      in
      (* mark modified copy when [var] is used as target *)
      mark_modified_copy var astate_n )


let mark_modified_copies_and_parameters vars astates astate_n =
  let unchecked_vars =
    List.filter vars ~f:(fun var -> not (NonDisjDomain.is_checked_via_dtor var astate_n))
  in
  continue_fold astates ~init:astate_n ~f:(fun astate_n astate ->
      mark_modified_copies_and_parameters_on_abductive unchecked_vars astate astate_n )
