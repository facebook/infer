(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module F = Format
module L = Logging


module Summary = Summary.Make (struct
    type summary = ThreadSafetyDomain.summary

    let update_payload summary payload =
      { payload with Specs.threadsafety = Some summary }

    let read_from_payload payload =
      payload.Specs.threadsafety
  end)

let is_owned access_path attribute_map =
  ThreadSafetyDomain.AttributeMapDomain.has_attribute
    access_path ThreadSafetyDomain.Attribute.unconditionally_owned attribute_map

let container_write_string = "__CONTAINERWRITE__"

let is_container_write_str str =
  String.is_substring ~substring:container_write_string str

let strip_container_write str =
  String.substr_replace_first str ~pattern:container_write_string ~with_:""

let is_container_write_sink sink =
  let _, access_list = fst (ThreadSafetyDomain.TraceElem.kind sink) in
  match List.rev access_list with
  |  FieldAccess (fn) :: _  -> is_container_write_str (Ident.fieldname_to_string fn)
  | _ -> false

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = ThreadSafetyDomain
  type extras = FormalMap.t

  type lock_model =
    | Lock
    | Unlock
    | NoEffect

  let get_lock_model = function
    | Typ.Procname.Java java_pname ->
        begin
          match Typ.Procname.java_get_class_name java_pname, Typ.Procname.java_get_method java_pname with
          | "java.util.concurrent.locks.Lock", "lock" ->
              Lock
          | ("java.util.concurrent.locks.ReentrantLock"
            | "java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock"
            | "java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock"),
            ("lock" | "tryLock" | "lockInterruptibly") ->
              Lock
          | ("java.util.concurrent.locks.Lock"
            |"java.util.concurrent.locks.ReentrantLock"
            | "java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock"
            | "java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock"),
            "unlock" ->
              Unlock
          | _ ->
              NoEffect
        end
    | pname when Typ.Procname.equal pname BuiltinDecl.__set_locked_attribute ->
        Lock
    | pname when Typ.Procname.equal pname BuiltinDecl.__delete_locked_attribute ->
        Unlock
    | _ ->
        NoEffect

  let resolve_id (id_map : IdAccessPathMapDomain.astate) id =
    try Some (IdAccessPathMapDomain.find id id_map)
    with Not_found -> None

  let is_constant = function
    | Exp.Const _ -> true
    | _ -> false

  let add_conditional_ownership_attribute access_path formal_map attribute_map attributes =
    match FormalMap.get_formal_index (fst access_path) formal_map with
    | Some formal_index when not (is_owned access_path attribute_map) ->
        Domain.AttributeSetDomain.add (Domain.Attribute.OwnedIf (Some formal_index)) attributes
    | _ ->
        attributes

  (* if rhs has associated attributes, propagate them to the lhs *)
  let propagate_attributes lhs_access_path rhs_exp rhs_typ ~f_resolve_id attribute_map formal_map =
    let rhs_access_paths = AccessPath.of_exp rhs_exp rhs_typ ~f_resolve_id in
    let rhs_attributes =
      if List.is_empty rhs_access_paths (* only happens when rhs is a constant *)
      then
        (* rhs is a constant, and constants are both owned and functional *)
        Domain.AttributeSetDomain.of_list
          [Domain.Attribute.unconditionally_owned; Domain.Attribute.Functional]
      else
        let propagate_attributes_ acc rhs_access_path =
          (try Domain.AttributeMapDomain.find rhs_access_path attribute_map
           with Not_found -> acc)
          |> add_conditional_ownership_attribute rhs_access_path formal_map attribute_map in
        List.fold
          ~f:propagate_attributes_
          ~init:Domain.AttributeSetDomain.empty
          rhs_access_paths in
    Domain.AttributeMapDomain.add lhs_access_path rhs_attributes attribute_map

  let propagate_return_attributes
      ret_opt ret_attributes actuals attribute_map ~f_resolve_id formal_map =
    match ret_opt with
    | Some (ret_id, ret_typ) ->
        let ownership_attributes, other_attributes =
          Domain.AttributeSetDomain.partition
            (function
              | OwnedIf _ -> true
              | _ -> false)
            ret_attributes in
        let caller_return_attributes =
          match Domain.AttributeSetDomain.elements ownership_attributes with
          | [] -> other_attributes
          | [(OwnedIf None) as unconditionally_owned] ->
              Domain.AttributeSetDomain.add unconditionally_owned other_attributes
          | [OwnedIf (Some formal_index)] ->
              begin
                match List.nth actuals formal_index with
                | Some (actual_exp, actual_typ) ->
                    begin
                      match
                        AccessPath.of_lhs_exp actual_exp actual_typ ~f_resolve_id with
                      | Some actual_ap ->
                          if is_owned actual_ap attribute_map
                          then
                            Domain.AttributeSetDomain.add
                              Domain.Attribute.unconditionally_owned other_attributes
                          else
                            add_conditional_ownership_attribute
                              actual_ap formal_map attribute_map  other_attributes
                      | None ->
                          other_attributes
                    end
                | None ->
                    other_attributes
              end
          | _multiple_ownership_attributes ->
              (* TODO: handle multiple ownership attributes *)
              other_attributes in
        Domain.AttributeMapDomain.add
          (AccessPath.of_id ret_id ret_typ)
          caller_return_attributes
          attribute_map
    | None ->
        attribute_map

  let add_path_to_state access_kind exp typ loc path_state id_map attribute_map tenv =
    (* we don't want to warn on writes to the field if it is (a) thread-confined, or (b) volatile *)
    let is_safe_write access_path tenv =
      let is_thread_safe_write accesses tenv =
        match List.rev accesses,
              AccessPath.Raw.get_typ (AccessPath.Raw.truncate access_path) tenv with
        | AccessPath.FieldAccess fieldname :: _,
          Some (Typ.Tstruct typename | Tptr (Tstruct typename, _)) ->
            begin
              match Tenv.lookup tenv typename with
              | Some struct_typ ->
                  Annotations.struct_typ_has_annot struct_typ Annotations.ia_is_thread_confined ||
                  Annotations.field_has_annot
                    fieldname struct_typ Annotations.ia_is_thread_confined ||
                  Annotations.field_has_annot fieldname struct_typ Annotations.ia_is_volatile
              | None ->
                  false
            end
        | _ ->
            false in
      is_thread_safe_write (snd access_path) tenv in
    let f_resolve_id = resolve_id id_map in

    if is_constant exp
    then
      path_state
    else
      List.fold
        ~f:(fun acc rawpath ->
            if not (is_owned (AccessPath.Raw.truncate rawpath) attribute_map) &&
               not (is_safe_write rawpath tenv)
            then Domain.PathDomain.add_sink (Domain.make_access rawpath access_kind loc) acc
            else acc)
        ~init:path_state
        (AccessPath.of_exp exp typ ~f_resolve_id)

  let is_unprotected is_locked pdesc =
    not is_locked && not (Procdesc.is_java_synchronized pdesc)

  let add_access
      exp
      loc
      access_kind
      (astate : Domain.astate)
      ~f_resolve_id
      (proc_data : FormalMap.t ProcData.t) =
    let get_formal_index exp typ = match AccessPath.of_lhs_exp exp typ ~f_resolve_id with
      | Some (base, _) -> FormalMap.get_formal_index base proc_data.extras
      | None -> None in
    match exp with
    | Exp.Lfield (base_exp, _, typ) ->
        let open Domain in
        if is_unprotected astate.locks proc_data.pdesc
        then
          match get_formal_index base_exp typ with
          | Some formal_index ->
              let pre = AccessPrecondition.ProtectedIf (Some formal_index) in
              let conditional_accesses_for_pre =
                add_path_to_state
                  access_kind
                  exp
                  typ
                  loc
                  (AccessDomain.get_accesses pre astate.accesses)
                  astate.id_map
                  astate.attribute_map
                  proc_data.tenv in
              AccessDomain.add pre conditional_accesses_for_pre astate.accesses
          | None ->
              let unsafe_accesses =
                add_path_to_state
                  access_kind
                  exp
                  typ
                  loc
                  (AccessDomain.get_accesses AccessPrecondition.unprotected astate.accesses)
                  astate.id_map
                  astate.attribute_map
                  proc_data.tenv in
              AccessDomain.add AccessPrecondition.unprotected unsafe_accesses astate.accesses
        else
          let safe_accesses =
            add_path_to_state
              access_kind
              exp
              typ
              loc
              (AccessDomain.get_accesses Protected astate.accesses)
              astate.id_map
              astate.attribute_map
              proc_data.tenv in
          AccessDomain.add Protected safe_accesses astate.accesses
    | _ ->
        astate.accesses

  let analyze_id_assignment lhs_id rhs_exp rhs_typ { Domain.id_map; } =
    let f_resolve_id = resolve_id id_map in
    match AccessPath.of_lhs_exp rhs_exp rhs_typ ~f_resolve_id with
    | Some rhs_access_path -> IdAccessPathMapDomain.add lhs_id rhs_access_path id_map
    | None -> id_map

  let has_return_annot predicate pn =
    Annotations.pname_has_return_annot
      pn
      ~attrs_of_pname:Specs.proc_resolve_attributes
      predicate

  let is_functional pname =
    let is_annotated_functional =
      has_return_annot Annotations.ia_is_functional in
    let is_modeled_functional = function
      | Typ.Procname.Java java_pname ->
          begin
            match Typ.Procname.java_get_class_name java_pname,
                  Typ.Procname.java_get_method java_pname with
            | "android.content.res.Resources", method_name ->
                (* all methods of Resources are considered @Functional except for the ones in this
                     blacklist *)
                let non_functional_resource_methods = [
                  "getAssets";
                  "getConfiguration";
                  "getSystem";
                  "newTheme";
                  "openRawResource";
                  "openRawResourceFd"
                ] in
                not (List.mem non_functional_resource_methods method_name)
            | _ ->
                false
          end
      | _ ->
          false in
    is_annotated_functional pname || is_modeled_functional pname

  let acquires_ownership pname tenv =
    let is_allocation pn =
      Typ.Procname.equal pn BuiltinDecl.__new || Typ.Procname.equal pn BuiltinDecl.__new_array in
    (* identify library functions that maintain ownership invariants behind the scenes *)
    let is_owned_in_library = function
      | Typ.Procname.Java java_pname ->
          begin
            match Typ.Procname.java_get_class_name java_pname,
                  Typ.Procname.java_get_method java_pname with
            | "javax.inject.Provider", "get" ->
                (* in dependency injection, the library allocates fresh values behind the scenes *)
                true
            | "java.lang.ThreadLocal", "get" ->
                (* ThreadLocal prevents sharing between threads behind the scenes *)
                true
            | "android.support.v4.util.Pools$SynchronizedPool", "acquire" ->
                (* a pool should own all of its objects *)
                true
            | _ ->
                false
          end
      | _ ->
          false in
    is_allocation pname ||
    is_owned_in_library pname ||
    PatternMatch.override_exists is_owned_in_library tenv pname

  let exec_instr (astate : Domain.astate) ({ ProcData.pdesc; tenv; extras; } as proc_data) _ =
    let is_container_write pn tenv = match pn with
      | Typ.Procname.Java java_pname ->
          let typename = Typename.Java.from_string (Typ.Procname.java_get_class_name java_pname) in
          let is_container_write_ typename _ =
            match Typename.name typename, Typ.Procname.java_get_method java_pname with
            | "java.util.List", ("add" | "addAll" | "clear" | "remove" | "set") -> true
            | "java.util.Map", ("clear" | "put" | "putAll" | "remove") -> true
            | _ -> false in
          let is_threadsafe_collection typename _ = match Typename.name typename with
            | "java.util.concurrent.ConcurrentMap" | "java.util.concurrent.CopyOnWriteArrayList" ->
                true
            | _ ->
                false in
          PatternMatch.supertype_exists tenv is_container_write_ typename &&
          not (PatternMatch.supertype_exists tenv is_threadsafe_collection typename)
      | _ -> false in
    let add_container_write callee_pname actuals ~f_resolve_id callee_loc =
      match actuals with
      | (receiver_exp, receiver_typ) :: _ ->
          (* create a dummy write that represents mutating the contents of the container *)
          let open Domain in
          let dummy_fieldname =
            Ident.create_fieldname
              (Mangled.from_string
                 (container_write_string ^ (Typ.Procname.get_method callee_pname))) 0 in
          let dummy_access_exp = Exp.Lfield (receiver_exp, dummy_fieldname, receiver_typ) in
          let callee_accesses =
            match AccessPath.of_lhs_exp dummy_access_exp receiver_typ ~f_resolve_id with
            | Some container_ap ->
                AccessDomain.add_access
                  (ProtectedIf (Some 0))
                  (make_access container_ap Write callee_loc)
                  AccessDomain.empty
            | None ->
                AccessDomain.empty in
          Some (false, callee_accesses, AttributeSetDomain.empty)
      | _ ->
          failwithf
            "Call to %a is marked as a container write, but has no receiver"
            Typ.Procname.pp callee_pname in
    let get_summary caller_pdesc callee_pname actuals ~f_resolve_id callee_loc tenv =
      if is_container_write callee_pname tenv
      then
        add_container_write callee_pname actuals ~f_resolve_id callee_loc
      else
        Summary.read_summary caller_pdesc callee_pname in
    (* return true if the given procname boxes a primitive type into a reference type *)
    let is_box = function
      | Typ.Procname.Java java_pname ->
          begin
            match Typ.Procname.java_get_class_name java_pname, Typ.Procname.java_get_method java_pname with
            | ("java.lang.Boolean" |
               "java.lang.Byte" |
               "java.lang.Char" |
               "java.lang.Double" |
               "java.lang.Float" |
               "java.lang.Integer" |
               "java.lang.Long" |
               "java.lang.Short"),
              "valueOf" -> true
            | _ -> false
          end
      | _ ->
          false in
    let f_resolve_id = resolve_id astate.id_map in

    let open Domain in
    function
    | Sil.Call (Some (lhs_id, lhs_typ), Const (Cfun pn), _, _, _) when acquires_ownership pn tenv ->
        begin
          match AccessPath.of_lhs_exp (Exp.Var lhs_id) lhs_typ ~f_resolve_id with
          | Some lhs_access_path ->
              let attribute_map =
                AttributeMapDomain.add_attribute
                  lhs_access_path
                  Attribute.unconditionally_owned
                  astate.attribute_map in
              { astate with attribute_map; }
          | None ->
              astate
        end

    | Sil.Call (Some (ret_id, _), Const (Cfun callee_pname),
                (target_exp, target_typ) :: (Exp.Sizeof (cast_typ, _, _), _) :: _ , _, _)
      when Typ.Procname.equal callee_pname BuiltinDecl.__cast ->
        let lhs_access_path = AccessPath.of_id ret_id (Typ.Tptr (cast_typ, Pk_pointer)) in
        let attribute_map =
          propagate_attributes
            lhs_access_path target_exp target_typ ~f_resolve_id astate.attribute_map extras in
        { astate with attribute_map; }

    | Sil.Call (ret_opt, Const (Cfun callee_pname), actuals, loc, _) ->
        let astate_callee =
          (* assuming that modeled procedures do not have useful summaries *)
          match get_lock_model callee_pname with
          | Lock ->
              { astate with locks = true; }
          | Unlock ->
              { astate with locks = false; }
          | NoEffect ->
              match
                get_summary pdesc callee_pname actuals ~f_resolve_id loc tenv with
              | Some (callee_locks, callee_accesses, return_attributes) ->
                  let call_site = CallSite.make callee_pname loc in
                  let combine_accesses_for_pre pre ~caller_accesses ~callee_accesses =
                    let combined_accesses =
                      PathDomain.with_callsite
                        (AccessDomain.get_accesses pre callee_accesses) call_site
                      |> PathDomain.join (AccessDomain.get_accesses pre caller_accesses) in
                    AccessDomain.add pre combined_accesses caller_accesses in
                  let combined_safe_accesses =
                    combine_accesses_for_pre
                      AccessPrecondition.Protected
                      ~caller_accesses:astate.accesses
                      ~callee_accesses in
                  let locks' = callee_locks || astate.locks in
                  let astate' =
                    if is_unprotected locks' pdesc
                    then
                      let add_conditional_accesses index accesses_acc (actual_exp, actual_typ) =
                        if is_constant actual_exp
                        then
                          (* the actual is a constant, so it's owned in the caller. *)
                          accesses_acc
                        else
                          let callee_conditional_accesses =
                            PathDomain.with_callsite
                              (AccessDomain.get_accesses (ProtectedIf (Some index)) callee_accesses)
                              call_site in
                          begin
                            match AccessPath.of_lhs_exp actual_exp actual_typ ~f_resolve_id with
                            | Some actual_access_path ->
                                if is_owned actual_access_path astate.attribute_map
                                then
                                  (* the actual passed to the current callee is owned. drop all the
                                     conditional accesses for that actual, since they're all safe *)
                                  accesses_acc
                                else
                                  let base = fst actual_access_path in
                                  begin
                                    match FormalMap.get_formal_index base extras with
                                    | Some formal_index ->
                                        (* the actual passed to the current callee is rooted in a
                                           formal. add to conditional accesses *)
                                        PathDomain.Sinks.fold
                                          (AccessDomain.add_access
                                             (ProtectedIf (Some formal_index)))
                                          (PathDomain.sinks callee_conditional_accesses)
                                          accesses_acc
                                    | None ->
                                        begin
                                          match
                                            AttributeMapDomain.get_conditional_ownership_index
                                              actual_access_path
                                              astate.attribute_map
                                          with
                                          |(Some formal_index) ->
                                              (* access path conditionally owned, add to
                                                 protected-if accesses *)
                                              PathDomain.Sinks.fold
                                                (AccessDomain.add_access
                                                   (ProtectedIf (Some formal_index)))
                                                (PathDomain.sinks callee_conditional_accesses)
                                                accesses_acc
                                          | None ->
                                              (* access path not owned and not rooted in a formal.
                                                 add to unprotected accesses *)
                                              PathDomain.Sinks.fold
                                                (AccessDomain.add_access
                                                   AccessPrecondition.unprotected)
                                                (PathDomain.sinks callee_conditional_accesses)
                                                accesses_acc
                                        end
                                  end
                            | None ->
                                PathDomain.Sinks.fold
                                  (AccessDomain.add_access AccessPrecondition.unprotected)
                                  (PathDomain.sinks callee_conditional_accesses)
                                  accesses_acc
                          end in

                      let unsafe_accesses =
                        combine_accesses_for_pre
                          AccessPrecondition.unprotected
                          ~caller_accesses:combined_safe_accesses
                          ~callee_accesses in
                      let accesses =
                        List.foldi ~f:add_conditional_accesses ~init:unsafe_accesses actuals in
                      { astate with accesses; }
                    else
                      { astate with accesses = combined_safe_accesses; } in
                  let attribute_map =
                    propagate_return_attributes
                      ret_opt
                      return_attributes
                      actuals
                      astate.attribute_map
                      ~f_resolve_id
                      extras in
                  { astate' with locks = locks'; attribute_map; }
              | None ->
                  if is_box callee_pname
                  then
                    match ret_opt, actuals with
                    | Some (ret_id, ret_typ), (actual_exp, actual_typ) :: _ ->
                        begin
                          match AccessPath.of_lhs_exp actual_exp actual_typ ~f_resolve_id with
                          | Some ap
                            when AttributeMapDomain.has_attribute
                                ap Functional astate.attribute_map ->
                              let attribute_map =
                                AttributeMapDomain.add_attribute
                                  (AccessPath.of_id ret_id ret_typ)
                                  Functional
                                  astate.attribute_map in
                              { astate with attribute_map; }
                          | _ ->
                              astate
                        end
                    | _ ->
                        astate
                  else if FbThreadSafety.is_graphql_constructor callee_pname
                  then
                    (* assume generated GraphQL code returns ownership *)
                    match ret_opt with
                    | Some (ret_id, ret_typ) ->
                        let attribute_map =
                          AttributeMapDomain.add_attribute
                            (AccessPath.of_id ret_id ret_typ)
                            Attribute.unconditionally_owned
                            astate.attribute_map in
                        { astate with attribute_map; }
                    | None -> astate
                  else
                    astate in
        begin
          match ret_opt with
          | Some (_, (Typ.Tint ILong | Tfloat FDouble)) ->
              (* writes to longs and doubles are not guaranteed to be atomic in Java, so don't
                 bother tracking whether a returned long or float value is functional *)
              astate_callee
          | Some (ret_id, ret_typ) ->
              let add_if_annotated predicate attribute attribute_map =
                if PatternMatch.override_exists predicate tenv callee_pname
                then
                  AttributeMapDomain.add_attribute
                    (AccessPath.of_id ret_id ret_typ) attribute attribute_map
                else attribute_map in
              let attribute_map =
                add_if_annotated is_functional Functional astate_callee.attribute_map
                |> add_if_annotated
                  (has_return_annot Annotations.ia_is_returns_ownership)
                  Domain.Attribute.unconditionally_owned in
              { astate_callee with attribute_map; }
          | _ ->
              astate_callee
        end

    | Sil.Store (Exp.Lvar lhs_pvar, lhs_typ, rhs_exp, _)
      when Pvar.is_frontend_tmp lhs_pvar && not (is_constant rhs_exp) ->
        let id_map' = analyze_id_assignment (Var.of_pvar lhs_pvar) rhs_exp lhs_typ astate in
        { astate with id_map = id_map'; }

    | Sil.Store (lhs_exp, lhs_typ, rhs_exp, loc) ->
        let is_marked_functional exp typ attribute_map =
          match AccessPath.of_lhs_exp exp typ ~f_resolve_id with
          | Some access_path ->
              AttributeMapDomain.has_attribute access_path Functional attribute_map
          | None ->
              false in
        let accesses =
          if is_marked_functional rhs_exp lhs_typ astate.attribute_map
          then
            (* we want to forget about writes to @Functional fields altogether, otherwise we'll
               report spurious read/write races *)
            astate.accesses
          else
            add_access lhs_exp loc Write astate ~f_resolve_id proc_data in
        let attribute_map =
          match AccessPath.of_lhs_exp lhs_exp lhs_typ ~f_resolve_id with
          | Some lhs_access_path ->
              propagate_attributes
                lhs_access_path rhs_exp lhs_typ ~f_resolve_id astate.attribute_map extras
          | None ->
              astate.attribute_map in
        { astate with accesses; attribute_map; }

    | Sil.Load (lhs_id, rhs_exp, rhs_typ, loc) ->
        let id_map = analyze_id_assignment (Var.of_id lhs_id) rhs_exp rhs_typ astate in
        let accesses = add_access rhs_exp loc Read astate ~f_resolve_id proc_data in
        let lhs_access_path = AccessPath.of_id lhs_id rhs_typ in
        let attribute_map =
          propagate_attributes
            lhs_access_path rhs_exp rhs_typ ~f_resolve_id astate.attribute_map extras in
        { astate with accesses; id_map; attribute_map; }

    | Sil.Remove_temps (ids, _) ->
        let id_map =
          List.fold
            ~f:(fun acc id -> IdAccessPathMapDomain.remove (Var.of_id id) acc)
            ~init:astate.id_map
            ids in
        { astate with id_map; }

    |  _  ->
        astate
end

module Analyzer = AbstractInterpreter.Make (ProcCfg.Normal) (TransferFunctions)

module Interprocedural = AbstractInterpreter.Interprocedural (Summary)

(* a results table is a Map where a key is an a procedure environment,
   i.e., something of type Idenv.t * Tenv.t * Typ.Procname.t * Procdesc.t
*)
module ResultsTableType = Caml.Map.Make (struct
    type t = Idenv.t * Tenv.t * Typ.Procname.t * Procdesc.t
    let compare (_, _, pn1, _) (_,_,pn2,_) =  Typ.Procname.compare pn1 pn2
  end)

(* we want to consider Builder classes and other safe immutablility-ensuring patterns as
   thread-safe. we are overly friendly about this for now; any class whose name ends with `Builder`
   is assumed to be thread-safe. in the future, we can ask for builder classes to be annotated with
   @Builder and verify that annotated classes satisfy the expected invariants. *)
let is_builder_class class_name =
  String.is_suffix ~suffix:"Builder" class_name

(* similarly, we assume that immutable classes safely encapsulate their state *)
let is_immutable_collection_class class_name tenv =
  let immutable_collections = [
    "com.google.common.collect.ImmutableCollection";
    "com.google.common.collect.ImmutableMap";
    "com.google.common.collect.ImmutableTable";
  ] in
  PatternMatch.supertype_exists
    tenv
    (fun typename _ ->
       List.mem ~equal:String.equal immutable_collections (Typename.name typename))
    class_name

let is_call_to_builder_class_method = function
  | Typ.Procname.Java java_pname -> is_builder_class (Typ.Procname.java_get_class_name java_pname)
  | _ -> false

let is_call_to_immutable_collection_method tenv = function
  | Typ.Procname.Java java_pname ->
      is_immutable_collection_class (Typ.Procname.java_get_class_type_name java_pname) tenv
  | _ ->
      false

(* Methods in @ThreadConfined classes and methods annotated with @ThreadConfied are assumed to all
   run on the same thread. For the moment we won't warn on accesses resulting from use of such
   methods at all. In future we should account for races between these methods and methods from
   completely different classes that don't necessarily run on the same thread as the confined
   object. *)
let is_thread_confined_method tenv pdesc =
  Annotations.pdesc_return_annot_ends_with pdesc Annotations.thread_confined ||
  PatternMatch.check_current_class_attributes
    Annotations.ia_is_thread_confined tenv (Procdesc.get_proc_name pdesc)

(* we don't want to warn on methods that run on the UI thread because they should always be
   single-threaded *)
let runs_on_ui_thread proc_desc =
  (* assume that methods annotated with @UiThread, @OnEvent, @OnBind, @OnMount, @OnUnbind,
     @OnUnmount always run on the UI thread *)
  Annotations.pdesc_has_return_annot
    proc_desc
    (fun annot -> Annotations.ia_is_ui_thread annot ||
                  Annotations.ia_is_on_bind annot ||
                  Annotations.ia_is_on_event annot ||
                  Annotations.ia_is_on_mount annot ||
                  Annotations.ia_is_on_unbind annot ||
                  Annotations.ia_is_on_unmount annot)

let threadsafe_annotations =
  Annotations.thread_safe ::
  (ThreadSafetyConfig.AnnotationAliases.of_json Config.threadsafe_aliases)

(* returns true if the annotation is @ThreadSafe, @ThreadSafe(enableChecks = true), or is defined
   as an alias of @ThreadSafe in a .inferconfig file. *)
let is_thread_safe item_annot =
  let f ((annot : Annot.t), _) =
    List.exists
      ~f:(fun annot_string ->
          Annotations.annot_ends_with annot annot_string ||
          String.equal annot.class_name annot_string)
      threadsafe_annotations &&
    match annot.Annot.parameters with
    | ["false"] -> false
    | _ -> true in
  List.exists ~f item_annot

(* returns true if the annotation is @ThreadSafe(enableChecks = false) *)
let is_assumed_thread_safe item_annot =
  let f (annot, _) =
    Annotations.annot_ends_with annot Annotations.thread_safe &&
    match annot.Annot.parameters with
    | ["false"] -> true
    | _ -> false in
  List.exists ~f item_annot

let pdesc_is_assumed_thread_safe pdesc tenv =
  is_assumed_thread_safe (Annotations.pdesc_get_return_annot pdesc) ||
  PatternMatch.check_current_class_attributes
    is_assumed_thread_safe tenv (Procdesc.get_proc_name pdesc)

(* return true if we should compute a summary for the procedure. if this returns false, we won't
   analyze the procedure or report any warnings on it *)
(* note: in the future, we will want to analyze the procedures in all of these cases in order to
   find more bugs. this is just a temporary measure to avoid obvious false positives *)
let should_analyze_proc pdesc tenv =
  let pn = Procdesc.get_proc_name pdesc in
  not (Typ.Procname.is_class_initializer pn) &&
  not (FbThreadSafety.is_logging_method pn) &&
  not (is_call_to_builder_class_method pn) &&
  not (is_call_to_immutable_collection_method tenv pn) &&
  not (runs_on_ui_thread pdesc) &&
  not (is_thread_confined_method tenv pdesc) &&
  not (pdesc_is_assumed_thread_safe pdesc tenv)

(* return true if we should report on unprotected accesses during the procedure *)
let should_report_on_proc (_, _, proc_name, proc_desc) =
  not (Typ.Procname.java_is_autogen_method proc_name) &&
  Procdesc.get_access proc_desc <> PredSymb.Private &&
  not (Annotations.pdesc_return_annot_ends_with proc_desc Annotations.visibleForTesting)

let analyze_procedure callback =
  let is_initializer tenv proc_name =
    Typ.Procname.is_constructor proc_name || FbThreadSafety.is_custom_init tenv proc_name in
  let open ThreadSafetyDomain in
  let has_lock = false in
  let return_attrs = AttributeSetDomain.empty in
  let empty = has_lock, AccessDomain.empty, return_attrs in
  (* convert the abstract state to a summary by dropping the id map *)
  let compute_post ({ ProcData.pdesc; tenv; extras; } as proc_data) =
    if should_analyze_proc pdesc tenv
    then
      begin
        if not (Procdesc.did_preanalysis pdesc) then Preanal.do_liveness pdesc tenv;
        let initial =
          if is_initializer tenv (Procdesc.get_proc_name pdesc)
          then
            (* express that the constructor owns [this] *)
            match FormalMap.get_formal_base 0 extras with
            | Some base ->
                let attribute_map =
                  AttributeMapDomain.add_attribute
                    (base, [])
                    Attribute.unconditionally_owned
                    ThreadSafetyDomain.empty.attribute_map in
                { ThreadSafetyDomain.empty with attribute_map; }
            | None -> ThreadSafetyDomain.empty
          else
            ThreadSafetyDomain.empty in

        match Analyzer.compute_post proc_data ~initial with
        | Some { locks; accesses; attribute_map; } ->
            let return_var_ap =
              AccessPath.of_pvar
                (Pvar.get_ret_pvar (Procdesc.get_proc_name pdesc))
                (Procdesc.get_ret_type pdesc) in
            let return_attributes =
              try AttributeMapDomain.find return_var_ap attribute_map
              with Not_found -> AttributeSetDomain.empty in
            Some (locks, accesses, return_attributes)
        | None ->
            None
      end
    else
      Some empty in
  match
    Interprocedural.compute_and_store_post
      ~compute_post
      ~make_extras:FormalMap.make
      callback with
  | Some post -> post
  | None -> empty

let checker callback =
  ignore (analyze_procedure callback)

(* creates a map from proc_envs to postconditions *)
let make_results_table get_proc_desc file_env =
  (* make a Map sending each element e of list l to (f e) *)
  let map_post_computation_over_procs f l =
    List.fold
      ~f:(fun m p -> ResultsTableType.add p (f p) m)
      ~init:ResultsTableType.empty
      l in
  let compute_post_for_procedure = (* takes proc_env as arg *)
    fun (idenv, tenv, proc_name, proc_desc) ->
      match Summary.read_summary proc_desc proc_name with
      | Some summ -> summ
      | None ->
          let callback_arg =
            let get_procs_in_file _ = [] in
            { Callbacks.get_proc_desc; get_procs_in_file; idenv; tenv; proc_name; proc_desc } in
          analyze_procedure callback_arg in
  map_post_computation_over_procs compute_post_for_procedure file_env

let get_current_class_and_threadsafe_superclasses tenv pname =
  match pname with
  | Typ.Procname.Java java_pname ->
      let current_class = Typ.Procname.java_get_class_type_name java_pname in
      let thread_safe_annotated_classes =
        PatternMatch.find_superclasses_with_attributes
          is_thread_safe tenv current_class
      in
      Some (current_class,thread_safe_annotated_classes)
  | _ -> None  (*shouldn't happen*)

(** The addendum message says that a superclass is marked @ThreadSafe,
    when the current class is not so marked*)
let calculate_addendum_message tenv pname =
  match get_current_class_and_threadsafe_superclasses tenv pname with
  | Some (current_class,thread_safe_annotated_classes) ->
      if not (List.mem ~equal:Typename.equal thread_safe_annotated_classes current_class) then
        match thread_safe_annotated_classes with
        | hd::_ -> F.asprintf "\n Note: Superclass %a is marked @ThreadSafe." Typename.pp hd
        | [] -> ""
      else ""
  | _ -> ""

(* keep only the accesses of the given kind *)
let filter_by_kind access_kind trace =
  let open ThreadSafetyDomain in
  PathDomain.Sinks.filter
    (fun sink -> phys_equal access_kind (snd (TraceElem.kind sink)))
    (PathDomain.sinks trace)
  |> PathDomain.update_sinks trace

(* get all of the unprotected accesses of the given kind *)
let get_possibly_unsafe_accesses access_kind accesses =
  let open ThreadSafetyDomain in
  AccessDomain.fold
    (fun pre trace acc -> match pre with
       | ProtectedIf _ -> PathDomain.join (filter_by_kind access_kind trace) acc
       | Protected -> acc)
    accesses
    PathDomain.empty

let get_possibly_unsafe_reads = get_possibly_unsafe_accesses Read

let get_possibly_unsafe_writes = get_possibly_unsafe_accesses Write

(* get all accesses of the given kind *)
let get_all_accesses access_kind accesses =
  let open ThreadSafetyDomain in
  AccessDomain.fold
    (fun _ trace acc ->  PathDomain.join (filter_by_kind access_kind trace) acc)
    accesses
    PathDomain.empty

let equal_locs (sink1 : ThreadSafetyDomain.TraceElem.t) (sink2 : ThreadSafetyDomain.TraceElem.t) =
  Location.equal
    (CallSite.loc (ThreadSafetyDomain.TraceElem.call_site sink1))
    (CallSite.loc (ThreadSafetyDomain.TraceElem.call_site sink2))

let equal_accesses (sink1 : ThreadSafetyDomain.TraceElem.t)
    (sink2 : ThreadSafetyDomain.TraceElem.t) =
  AccessPath.equal_access_list
    (snd (fst (ThreadSafetyDomain.TraceElem.kind sink1)))
    (snd (fst (ThreadSafetyDomain.TraceElem.kind sink2)))

(* For now equal-access and conflicting-access are equivalent.
   But that will change when we (soon) consider conficting accesses
   that are not via assignment, such as add and get for containers*)
let conflicting_accesses (sink1 : ThreadSafetyDomain.TraceElem.t)
    (sink2 : ThreadSafetyDomain.TraceElem.t) =
  equal_accesses sink1 sink2

(* trace is really reads or writes set. Fix terminology later *)
let filter_conflicting_sinks sink trace =
  let conflicts =
    ThreadSafetyDomain.PathDomain.Sinks.filter
      (fun sink2 -> conflicting_accesses sink sink2)
      (ThreadSafetyDomain.PathDomain.sinks trace) in
  ThreadSafetyDomain.PathDomain.update_sinks trace conflicts

(* Given a sink representing a read access,
   return a list of (proc_env,access-astate) pairs where
   access-astate is a non-empty collection of conflicting
   write accesses*)
let collect_conflicting_writes sink tab =
  let procs_and_writes =
    List.map
      ~f:(fun (key, (_, accesses, _)) ->
          let conflicting_writes =
            filter_conflicting_sinks sink (get_all_accesses Write accesses) in
          key, conflicting_writes
        )
      (ResultsTableType.bindings tab) in
  List.filter
    ~f:(fun (proc_env,writes) ->
        (should_report_on_proc proc_env)
        && not (ThreadSafetyDomain.PathDomain.Sinks.is_empty
                  (ThreadSafetyDomain.PathDomain.sinks writes))
      )
    procs_and_writes

(* keep only the first copy of an access per procedure,
   and keep at most one warning per line (they are usually interprocedural accesses
   to different fields generated by the same call)  *)
let de_dup trace =
  let select_representatives original_sinks predicate =
    let list_of_original_sinks = ThreadSafetyDomain.PathDomain.Sinks.elements original_sinks in
    ThreadSafetyDomain.PathDomain.Sinks.filter
      (fun sink ->
         (* for each sink we will keep one in the equivalence class of those
            satisfying predicate. We select that by using find_exn to get
            the first element equivalent ot sink in a list of sinks. This
            first element is the dedup representative, and it happens to
            typically be the first such access in a method.  *)
         let first_sink =
           List.find_exn
             ~f:(fun sink2 -> predicate sink sink2)
             list_of_original_sinks in
         Int.equal (ThreadSafetyDomain.TraceElem.compare sink first_sink) 0
      )
      original_sinks in
  let de_duped_sinks_by_accesses = select_representatives
      (ThreadSafetyDomain.PathDomain.sinks trace)
      equal_accesses in
  let de_duped_sinks_by_locs_and_accesses = select_representatives
      de_duped_sinks_by_accesses
      equal_locs in
  ThreadSafetyDomain.PathDomain.update_sinks trace de_duped_sinks_by_locs_and_accesses

let strip_reads_that_have_co_located_write reads writes =
  let set_of_read_sinks = ThreadSafetyDomain.PathDomain.sinks reads in
  let set_of_write_sinks = ThreadSafetyDomain.PathDomain.sinks writes in
  let stripped_read_sinks =
    ThreadSafetyDomain.PathDomain.Sinks.filter
      (fun sink -> not (ThreadSafetyDomain.PathDomain.Sinks.exists
                          (fun sink2 -> equal_locs sink sink2)
                          set_of_write_sinks
                       )
      )
      set_of_read_sinks in
  ThreadSafetyDomain.PathDomain.update_sinks reads stripped_read_sinks

(*A helper function used in the error reporting*)
let pp_accesses_sink fmt ~is_write_access sink =
  let access_path, _ = ThreadSafetyDomain.PathDomain.Sink.kind sink in
  let container_write = is_write_access && is_container_write_sink sink in
  F.fprintf fmt
    (if container_write then "container %a" else "%a")
    AccessPath.pp_access_list
    (if container_write
     then snd (AccessPath.Raw.truncate access_path)
     else snd access_path)

(* trace is really a set of accesses*)
let report_thread_safety_violations ( _, tenv, pname, pdesc) make_description trace tab =
  let open ThreadSafetyDomain in
  let trace_of_pname callee_pname =
    match Summary.read_summary pdesc callee_pname with
    | Some (_, accesses, _) -> get_possibly_unsafe_writes accesses
    | _ -> PathDomain.empty in
  let report_one_path ((_, sinks) as path) =
    let initial_sink, _ = List.last_exn sinks in
    let final_sink, _ = List.hd_exn sinks in
    let initial_sink_site = PathDomain.Sink.call_site initial_sink in
    let final_sink_site = PathDomain.Sink.call_site final_sink in
    let desc_of_sink sink =
      if
        CallSite.equal (PathDomain.Sink.call_site sink) final_sink_site
      then
        Format.asprintf "access to %a" (pp_accesses_sink ~is_write_access:true) sink
      else
        Format.asprintf
          "call to %a" Typ.Procname.pp (CallSite.pname (PathDomain.Sink.call_site sink)) in
    let loc = CallSite.loc (PathDomain.Sink.call_site initial_sink) in
    let ltr = PathDomain.to_sink_loc_trace ~desc_of_sink path in
    let msg = Localise.to_string Localise.thread_safety_violation in
    let description = make_description tenv pname final_sink_site
        initial_sink_site final_sink tab in
    let exn = Exceptions.Checkers (msg, Localise.verbatim_desc description) in
    Reporting.log_error pname ~loc ~ltr exn in

  List.iter
    ~f:report_one_path
    (PathDomain.get_reportable_sink_paths (de_dup trace) ~trace_of_pname)


let make_unprotected_write_description
    tenv pname final_sink_site initial_sink_site final_sink _ =
  Format.asprintf
    "Unprotected write. Public method %a%s %s %a outside of synchronization.%s"
    Typ.Procname.pp pname
    (if CallSite.equal final_sink_site initial_sink_site then "" else " indirectly")
    (if is_container_write_sink final_sink then "mutates"  else "writes to field")
    (pp_accesses_sink ~is_write_access:true) final_sink
    (calculate_addendum_message tenv pname)

let make_read_write_race_description tenv pname final_sink_site initial_sink_site final_sink tab =
  let conflicting_proc_envs = List.map
      ~f:fst
      (collect_conflicting_writes final_sink tab) in
  let conflicting_proc_names = List.map
      ~f:(fun (_,_,proc_name,_) -> proc_name)
      conflicting_proc_envs in
  let pp_proc_name_list fmt proc_names =
    let pp_sep _ _ = F.fprintf fmt " , " in
    F.pp_print_list ~pp_sep Typ.Procname.pp fmt proc_names in
  let conflicts_description =
    Format.asprintf "Potentially races with writes in method%s %a."
      (if List.length conflicting_proc_names > 1 then "s" else "")
      pp_proc_name_list conflicting_proc_names in
  Format.asprintf "Read/Write race. Public method %a%s reads from field %a. %s %s"
    Typ.Procname.pp pname
    (if CallSite.equal final_sink_site initial_sink_site then "" else " indirectly")
    (pp_accesses_sink ~is_write_access:false) final_sink
    conflicts_description
    (calculate_addendum_message tenv pname)

(* find those elements of reads which have conflicts
        somewhere else, and report them *)
let report_reads proc_env reads tab =
  let racy_read_sinks =
    ThreadSafetyDomain.PathDomain.Sinks.filter
      (fun sink ->
         (* there exists a postcondition whose write set conflicts with
            sink*)
         not (List.is_empty (collect_conflicting_writes sink tab))
      )
      (ThreadSafetyDomain.PathDomain.sinks reads)
  in
  let racy_reads =
    ThreadSafetyDomain.PathDomain.update_sinks reads racy_read_sinks
  in
  report_thread_safety_violations proc_env
    make_read_write_race_description
    racy_reads
    tab

(* Currently we analyze if there is an @ThreadSafe annotation on at least one of
   the classes in a file. This might be tightened in future or even broadened in future
   based on other criteria *)
let should_report_on_file file_env =
  let current_class_or_super_marked_threadsafe =
    fun (_, tenv, pname, _) ->
      match get_current_class_and_threadsafe_superclasses tenv pname with
      | Some (_, thread_safe_annotated_classes) ->
          not (List.is_empty thread_safe_annotated_classes)
      | _ -> false
  in
  let current_class_marked_not_threadsafe =
    fun (_, tenv, pname, _) ->
      PatternMatch.check_current_class_attributes Annotations.ia_is_not_thread_safe tenv pname
  in
  not (List.exists ~f:current_class_marked_not_threadsafe file_env) &&
  List.exists ~f:current_class_or_super_marked_threadsafe file_env

(* For now, just checks if there is one active element amongst the posts of the analyzed methods.
   This indicates that the method races with itself. To be refined later. *)
let process_results_table file_env tab =
  let should_report_on_all_procs = should_report_on_file file_env in
  (* TODO (t15588153): clean this up *)
  let is_thread_safe_method pdesc tenv =
    PatternMatch.override_exists
      (fun pn ->
         Annotations.pname_has_return_annot
           pn
           ~attrs_of_pname:Specs.proc_resolve_attributes
           is_thread_safe)
      tenv
      (Procdesc.get_proc_name pdesc) in
  let should_report ((_, tenv, _, pdesc) as proc_env) =
    (should_report_on_all_procs || is_thread_safe_method pdesc tenv)
    && should_report_on_proc proc_env in
  ResultsTableType.iter (* report errors for each method *)
    (fun proc_env (_, accesses, _) ->
       if should_report proc_env
       then
         let open ThreadSafetyDomain in
         let reads, writes =
           AccessDomain.fold
             (fun pre accesses (reads_acc, writes_acc) ->
                let read_accesses, write_accesses =
                  PathDomain.Sinks.partition TraceElem.is_read (PathDomain.sinks accesses) in
                AccessDomain.add pre (PathDomain.update_sinks accesses read_accesses) reads_acc,
                AccessDomain.add pre (PathDomain.update_sinks accesses write_accesses) writes_acc)
             accesses
             (AccessDomain.empty, AccessDomain.empty) in
         begin
           let unsafe_writes = get_possibly_unsafe_writes writes in
           let unsafe_reads = get_possibly_unsafe_reads reads in
           let stripped_unsafe_reads = strip_reads_that_have_co_located_write
               unsafe_reads
               unsafe_writes in
           report_thread_safety_violations
             proc_env make_unprotected_write_description unsafe_writes tab;
           report_reads proc_env stripped_unsafe_reads tab
         end
    )
    tab


(* Gathers results by analyzing all the methods in a file, then post-processes the results to check
   an (approximation of) thread safety *)
let file_analysis _ _ get_procdesc file_env =
  process_results_table file_env (make_results_table get_procdesc file_env)
