(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
open PulseDomainInterface
open PulseBasicInterface
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


let try_eval path location e disjunct =
  match PulseOperations.eval path NoAccess location e disjunct with
  | Sat (Ok (disjunct, (v, _))) ->
      Some (disjunct, v)
  | Sat (Recoverable _ | FatalError _) | Unsat ->
      None


let get_copied_and_source copy_type ({PathContext.timestamp} as path) rest_args location from
    (disjunct : AbductiveDomain.t) =
  let heap = (disjunct.post :> BaseDomain.t).heap in
  let copied =
    NonDisjDomain.Copied
      {heap; typ= Typ.strip_ptr copy_type; location; copied_location= None; from; timestamp}
  in
  let disjunct, source_addr_typ_opt =
    match rest_args with
    | (source_arg, source_typ) :: _ -> (
      match try_eval path location source_arg disjunct with
      | Some (disjunct, source_addr) ->
          let source_expr = PulseDecompiler.find source_addr disjunct.decompiler in
          (disjunct, Some (source_addr, source_expr, source_typ))
      | None ->
          (disjunct, None) )
    | _ ->
        (disjunct, None)
  in
  (copied, disjunct, source_addr_typ_opt)


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


let is_copy_legit copied_var =
  Var.appears_in_source_code copied_var && not (Var.is_global copied_var)


let is_local_variable = function Exp.Lvar pvar -> Pvar.is_local pvar | _ -> false

let continue_bind exec_state ~f =
  match (exec_state : ExecutionDomain.t) with
  | ContinueProgram disjunct ->
      f disjunct
  | ExceptionRaised _
  | ISLLatentMemoryError _
  | AbortProgram _
  | ExitProgram _
  | LatentAbortProgram _
  | LatentInvalidAccess _ ->
      None


let continue_map exec_state ~f = continue_bind exec_state ~f:(fun disjunct -> Some (f disjunct))

let continue_fold astates ~init ~f =
  List.fold astates ~init ~f:(fun acc exec_state ->
      continue_map exec_state ~f:(fun disjunct -> f acc disjunct) |> Option.value ~default:acc )


let continue_fold_map astates ~init ~f =
  List.fold_map astates ~init ~f:(fun acc exec_state ->
      continue_bind exec_state ~f:(fun disjunct -> f acc disjunct)
      |> Option.value_map ~default:(acc, exec_state) ~f:(fun (acc, disjunct) ->
             (acc, ExecutionDomain.continue disjunct) ) )


let add_copies tenv proc_desc path location call_exp actuals astates astate_non_disj =
  let open IOption.Let_syntax in
  let aux (copy_check_fn, args_map_fn) init astates =
    continue_fold_map astates ~init ~f:(fun astate_non_disj disjunct ->
        match ((call_exp : Exp.t), args_map_fn actuals) with
        | ( (Const (Cfun procname) | Closure {name= procname})
          , ((Exp.Lvar copy_pvar | Exp.Lindex (Exp.Lvar copy_pvar, _)), copy_type) :: rest_args )
          when (not (is_cheap_to_copy tenv copy_type))
               && not (NonDisjDomain.is_locked astate_non_disj) ->
            let* from = copy_check_fn procname in
            let copied_var = Var.of_pvar copy_pvar in
            let copied, disjunct, source_addr_typ_opt =
              get_copied_and_source copy_type path rest_args location from disjunct
            in
            let is_copy_legit = is_copy_legit copied_var in
            let source_opt =
              let* _, source_expr, _ = source_addr_typ_opt in
              match source_expr with
              | DecompilerExpr.SourceExpr ((PVar pvar, _), _)
                when (not (Pvar.is_frontend_tmp pvar)) && not is_copy_legit ->
                  Some pvar
              | _ ->
                  None
            in
            if Option.is_some source_opt || is_copy_legit then
              let copy_addr, _ = Option.value_exn (Stack.find_opt copied_var disjunct) in
              let disjunct' =
                Option.value_map source_addr_typ_opt ~default:disjunct
                  ~f:(fun (source_addr, _, source_typ) ->
                    AddressAttributes.add_one source_addr
                      (CopiedInto (Attribute.CopiedInto.IntoVar {copied_var; source_opt}))
                      disjunct
                    |> AddressAttributes.add_one copy_addr
                         (SourceOriginOfCopy
                            {source= source_addr; is_const_ref= Typ.is_const_reference source_typ}
                         ) )
              in
              Some
                ( NonDisjDomain.add_var copied_var ~source_opt
                    ~source_addr_opt:(Option.map source_addr_typ_opt ~f:fst3)
                    copied astate_non_disj
                , disjunct' )
            else None
        | ( (Const (Cfun procname) | Closure {name= procname})
          , ((Exp.Lfield (_, field, _) as exp), copy_type) :: ((_, source_typ) :: _ as rest_args) )
          when Typ.is_rvalue_reference source_typ && not (is_cheap_to_copy tenv copy_type) ->
            let* from = copy_check_fn procname in
            let copied, disjunct, source_addr_typ_opt =
              get_copied_and_source copy_type path rest_args location from disjunct
            in
            let+ disjunct, copy_addr = try_eval path location exp disjunct in
            let disjunct' =
              Option.value_map source_addr_typ_opt ~default:disjunct
                ~f:(fun (source_addr, source_expr, _) ->
                  AddressAttributes.add_one source_addr
                    (CopiedInto (IntoField {field; source_opt= Some source_expr}))
                    disjunct
                  |> AddressAttributes.add_one copy_addr
                       (SourceOriginOfCopy
                          {source= source_addr; is_const_ref= Typ.is_const_reference source_typ} ) )
            in
            ( NonDisjDomain.add_field field
                ~source_opt:(Option.map source_addr_typ_opt ~f:snd3)
                copied astate_non_disj
            , disjunct' )
        | ( (Const (Cfun procname) | Closure {name= procname})
          , (Exp.Var copy_var, copy_type) :: (source_exp, source_typ) :: _ )
          when (not (is_cheap_to_copy tenv copy_type))
               && (not (Typ.is_pointer_to_smart_pointer copy_type))
               && (not (has_copy_in_name (Procdesc.get_proc_name proc_desc)))
               && (not (NonDisjDomain.is_locked astate_non_disj))
               && not (is_local_variable source_exp) ->
            let* from = copy_check_fn procname in
            let* return_param = get_return_param proc_desc in
            let disjunct, (return_param_addr, _) =
              PulseOperations.eval_var path location return_param disjunct
            in
            let* copy_addr, _ = PulseOperations.read_id copy_var disjunct in
            let* return_addr, _ = Memory.find_edge_opt return_param_addr Dereference disjunct in
            if AbstractValue.equal copy_addr return_addr then
              let+ disjunct, source = try_eval path location source_exp disjunct in
              let disjunct =
                AddressAttributes.add_copied_return copy_addr ~source
                  ~is_const_ref:(Typ.is_const_reference source_typ)
                  from location disjunct
              in
              (astate_non_disj, disjunct)
            else None
        | _, _ ->
            None )
  in
  let copy_from_fn pname =
    let open Attribute.CopyOrigin in
    if Procname.is_copy_ctor pname then Some CopyCtor
    else if Procname.is_copy_assignment pname then Some CopyAssignment
    else None
  in
  let astate_n, astates = aux (copy_from_fn, Fn.id) astate_non_disj astates in
  (* For functions that return a copy, the last argument is the assigned copy *)
  aux (get_modeled_as_returning_copy_opt, List.rev) astate_n astates


let is_lock pname = String.equal (Procname.get_method pname) "lock"

let add_copied_return path location call_exp actuals astates astate_non_disj =
  let open IOption.Let_syntax in
  let default = (astate_non_disj, astates) in
  match (call_exp : Exp.t) with
  | (Const (Cfun procname) | Closure {name= procname}) when is_lock procname ->
      (NonDisjDomain.set_locked astate_non_disj, astates)
  | (Const (Cfun procname) | Closure {name= procname})
    when not (NonDisjDomain.is_locked astate_non_disj) -> (
    match (Option.map (Procdesc.load procname) ~f:Procdesc.get_attributes, List.last actuals) with
    | Some attrs, Some ((Exp.Lvar ret_pvar as ret), copy_type) when attrs.has_added_return_param ->
        let copied_var = Var.of_pvar ret_pvar in
        if is_copy_legit copied_var then
          continue_fold_map astates ~init:astate_non_disj ~f:(fun astate_non_disj disjunct ->
              let* disjunct, ret_addr = try_eval path location ret disjunct in
              let+ source, is_const_ref, from, copied_location =
                AddressAttributes.get_copied_return ret_addr disjunct
              in
              let disjunct =
                AddressAttributes.remove_copied_return ret_addr disjunct
                |> AddressAttributes.add_one source
                     (CopiedInto (IntoVar {copied_var; source_opt= None}))
                |> AddressAttributes.add_one ret_addr (SourceOriginOfCopy {source; is_const_ref})
              in
              let astate_non_disj =
                NonDisjDomain.add_var copied_var ~source_opt:None ~source_addr_opt:(Some source)
                  (Copied
                     { heap= (disjunct.post :> BaseDomain.t).heap
                     ; typ= Typ.strip_ptr copy_type
                     ; location
                     ; copied_location= Some (procname, copied_location)
                     ; from
                     ; timestamp= path.PathContext.timestamp } )
                  astate_non_disj
              in
              (astate_non_disj, disjunct) )
        else default
    | _, _ ->
        default )
  | _ ->
      default


let add_const_refable_parameters procdesc tenv astates astate_non_disj =
  let proc_parameters = Procdesc.get_passed_by_value_formals procdesc in
  let location = Procdesc.get_loc procdesc in
  continue_fold (List.map astates ~f:fst) ~init:astate_non_disj ~f:(fun astate_non_disj disjunct ->
      List.fold proc_parameters ~init:astate_non_disj ~f:(fun astate_non_disj (pvar, typ) ->
          let var = Var.of_pvar pvar in
          if
            Var.appears_in_source_code var && Typ.is_reference typ
            && (not (is_cheap_to_copy tenv typ))
            && not (Var.is_cpp_unnamed_param var)
          then
            (* [&] is added by the frontend and type is pass-by-value anyways so strip it *)
            NonDisjDomain.add_parameter var
              (NonDisjDomain.Unmodified
                 {heap= (disjunct.post :> BaseDomain.t).heap; typ= Typ.strip_ptr typ; location} )
              astate_non_disj
          else astate_non_disj ) )


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


let mark_modified_copies_and_parameters_with vars ~astate astate_n =
  let mark_modified_copy var default =
    Stack.find_opt var astate
    |> Option.value_map ~default ~f:(fun (address, _history) ->
           let source_addr_opt = AddressAttributes.get_source_origin_of_copy address astate in
           mark_modified_address_at ~address ~source_addr_opt
             ~copied_into:(Attribute.CopiedInto.IntoVar {copied_var= var; source_opt= None})
             Copy astate default )
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


let mark_modified_copies_and_parameters vars disjuncts astate_n =
  let unchecked_vars =
    List.filter vars ~f:(fun var ->
        not (PulseNonDisjunctiveDomain.is_checked_via_dtor var astate_n) )
  in
  continue_fold disjuncts ~init:astate_n ~f:(fun astate_n astate ->
      mark_modified_copies_and_parameters_with unchecked_vars ~astate astate_n )
