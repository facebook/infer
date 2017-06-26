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
module MF = MarkupFormatter

module Summary = Summary.Make (struct
    type payload = ThreadSafetyDomain.summary

    let update_payload post (summary : Specs.summary) =
      { summary with payload = { summary.payload with threadsafety = Some post }}

    let read_payload (summary : Specs.summary) =
      summary.payload.threadsafety
  end)

let is_owned access_path attribute_map =
  ThreadSafetyDomain.AttributeMapDomain.has_attribute
    access_path ThreadSafetyDomain.Attribute.unconditionally_owned attribute_map

let container_write_string = "infer.dummy.__CONTAINERWRITE__"

(* return (name of container, name of mutating function call) pair *)
let get_container_write_desc sink =
  let (base_var, _), access_list = fst (ThreadSafetyDomain.TraceElem.kind sink) in
  let get_container_write_desc_ call_name container_name =
    match String.chop_prefix (Fieldname.to_string call_name) ~prefix:container_write_string with
    | Some call_name -> Some (container_name, call_name)
    | None -> None in

  match List.rev access_list with
  |  FieldAccess call_name :: FieldAccess container_name :: _ ->
      get_container_write_desc_ call_name (Fieldname.to_string container_name)
  | [FieldAccess call_name] ->
      get_container_write_desc_ call_name (F.asprintf "%a" Var.pp base_var)
  | _ ->
      None

let is_container_write_sink sink =
  Option.is_some (get_container_write_desc sink)

(*Bit of redundancy with code in is_unprotected, might alter later *)
let make_excluder locks threads =
  if locks && not threads then ThreadSafetyDomain.Excluder.Lock
  else if not locks && threads then ThreadSafetyDomain.Excluder.Thread
  else if locks && threads then ThreadSafetyDomain.Excluder.Both
  else failwithf "called when neither lock nor thread known"

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = ThreadSafetyDomain
  type extras = FormalMap.t

  type lock_model =
    | Lock
    | Unlock
    | LockedIfTrue
    | NoEffect

  type thread_model =
    | Threaded
    | Unknown
    | ThreadedIfTrue

  let is_thread_utils_type java_pname =
    let pn = (Typ.Procname.java_get_class_name java_pname) in
    String.is_suffix ~suffix:"ThreadUtils" pn
    || String.is_suffix ~suffix:"ThreadUtil" pn

  let is_thread_utils_method method_name_str = function
    | Typ.Procname.Java java_pname ->
        is_thread_utils_type  java_pname
        && String.equal (Typ.Procname.java_get_method java_pname) method_name_str
    | _ -> false

  let get_lock_model =
    let is_cpp_lock =
      let matcher = QualifiedCppName.Match.of_fuzzy_qual_names [
          "std::mutex::lock"; "std::lock_guard::lock_guard"] in
      fun pname ->
        QualifiedCppName.Match.match_qualifiers matcher (Typ.Procname.get_qualifiers pname)
    and is_std_mutex_unlock =
      let matcher = QualifiedCppName.Match.of_fuzzy_qual_names ["std::mutex::unlock"] in
      fun pname ->
        QualifiedCppName.Match.match_qualifiers matcher (Typ.Procname.get_qualifiers pname)
    in
    function
    | Typ.Procname.Java java_pname ->
        if is_thread_utils_method "assertHoldsLock" (Typ.Procname.Java java_pname) then Lock
        else
          begin
            match Typ.Procname.java_get_class_name java_pname,
                  Typ.Procname.java_get_method java_pname with
            | ("java.util.concurrent.locks.Lock"
              | "java.util.concurrent.locks.ReentrantLock"
              | "java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock"
              | "java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock"),
              ("lock" | "lockInterruptibly") ->
                Lock
            | ("java.util.concurrent.locks.Lock"
              |"java.util.concurrent.locks.ReentrantLock"
              | "java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock"
              | "java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock"),
              "unlock" ->
                Unlock
            | ("java.util.concurrent.locks.Lock"
              | "java.util.concurrent.locks.ReentrantLock"
              | "java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock"
              | "java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock"),
              "tryLock" ->
                LockedIfTrue
            | "com.facebook.buck.util.concurrent.AutoCloseableReadWriteUpdateLock",
              ("readLock" | "updateLock" | "writeLock") ->
                Lock
            | _ ->
                NoEffect
          end
    | (Typ.Procname.ObjC_Cpp _ as pname) when is_cpp_lock pname ->
        Lock
    | (Typ.Procname.ObjC_Cpp _ as pname) when is_std_mutex_unlock pname ->
        Unlock
    | pname when Typ.Procname.equal pname BuiltinDecl.__set_locked_attribute ->
        Lock
    | pname when Typ.Procname.equal pname BuiltinDecl.__delete_locked_attribute ->
        Unlock
    | _ ->
        NoEffect

  let get_thread_model = function
    | Typ.Procname.Java java_pname ->
        if is_thread_utils_type java_pname then
          match Typ.Procname.java_get_method java_pname with
          | "assertMainThread"
          | "checkOnMainThread"
          | "assertOnUiThread"   -> Threaded
          | "isMainThread"
          | "isUiThread"         -> ThreadedIfTrue
          | _                    -> Unknown
        else Unknown
    (*Note we are not modelling assertOnNonUiThread or
      assertOnBackgroundThread. These treated as Unknown*)
    | _ -> Unknown

  let add_conditional_ownership_attribute access_path formal_map attribute_map attributes =
    match FormalMap.get_formal_index (fst access_path) formal_map with
    | Some formal_index when not (is_owned access_path attribute_map) ->
        Domain.AttributeSetDomain.add (Domain.Attribute.OwnedIf (Some formal_index)) attributes
    | _ ->
        attributes


  let remove_ownership_attributes attributes =
    Domain.AttributeSetDomain.filter
      (function | Domain.Attribute.OwnedIf _ -> false | _ -> true)
      attributes

  (* propagate attributes from the leaves to the root of an RHS Hil expression.
     Special casing on ownership. *)
  let rec attributes_of_expr formal_map attribute_map e =
    let open HilExp in
    let open Domain in
    match e with
    | Constant _ ->
        AttributeSetDomain.of_list [Attribute.unconditionally_owned; Attribute.Functional]
    | AccessPath ap ->
        begin
          try AttributeMapDomain.find ap attribute_map
          with Not_found -> AttributeSetDomain.empty
        end
        |> add_conditional_ownership_attribute ap formal_map attribute_map
    | Exception expr (* treat exceptions as transparent wrt attributes *)
    | Cast(_, expr) ->
        attributes_of_expr formal_map attribute_map expr
    | UnaryOperator(_, expr, _) ->
        attributes_of_expr formal_map attribute_map expr
        |> remove_ownership_attributes
    | BinaryOperator(_, expr1, expr2) ->
        let attributes1 = attributes_of_expr formal_map attribute_map expr1 in
        let attributes2 = attributes_of_expr formal_map attribute_map expr2 in
        AttributeSetDomain.join attributes1 attributes2
        |> remove_ownership_attributes
    | Closure _ | Sizeof _ ->
        AttributeSetDomain.empty

  (* will return true on x.f.g.h when x.f and x.f.g are owned, but not requiring x.f.g.h *)
  (* must not be called with an empty access list *)
  let all_prefixes_owned (base, accesses) attribute_map =
    let but_last_rev = List.rev accesses |> List.tl_exn in
    let rec aux acc = function
      | [] -> acc
      | (_::tail) as all -> aux ((List.rev all)::acc) tail in
    let prefixes = aux [] but_last_rev in
    List.for_all ~f:(fun ap -> is_owned (base, ap) attribute_map) prefixes

  let propagate_attributes lhs_access_path rhs_exp attribute_map formal_map =
    let rhs_attributes = attributes_of_expr formal_map attribute_map rhs_exp in
    let lhs_root = fst lhs_access_path in
    let filter_on_globals =
      (* do not assign ownership to access paths rooted at globals *)
      if Var.is_global (fst lhs_root)
      then remove_ownership_attributes else Fn.id
    in
    let filter_on_lhs =
      (* do not assign ownership when lhs is not a single var or
         a single-field ap whose root is conditionally owned, or,
         all prefixes are owned *)
      match snd lhs_access_path with
      | [] -> Fn.id
      | [_] when FormalMap.is_formal lhs_root formal_map -> Fn.id
      | _ when all_prefixes_owned lhs_access_path attribute_map -> Fn.id
      | _ -> remove_ownership_attributes
    in
    let final_attributes = filter_on_globals rhs_attributes |> filter_on_lhs in
    Domain.AttributeMapDomain.add lhs_access_path final_attributes attribute_map

  let propagate_return_attributes ret_opt ret_attributes actuals attribute_map formal_map =
    let open Domain in
    match ret_opt with
    | None ->
        attribute_map
    | Some ret ->
        let ownership_attributes, other_attributes =
          AttributeSetDomain.partition (function | OwnedIf _ -> true | _ -> false) ret_attributes in
        let caller_return_attributes =
          match AttributeSetDomain.elements ownership_attributes with
          | [] -> other_attributes
          | [(OwnedIf None) as unconditionally_owned] ->
              AttributeSetDomain.add unconditionally_owned other_attributes
          | [OwnedIf (Some formal_index)] ->
              begin
                match List.nth actuals formal_index with
                | Some (HilExp.AccessPath actual_ap) ->
                    if is_owned actual_ap attribute_map
                    then
                      AttributeSetDomain.add Attribute.unconditionally_owned other_attributes
                    else
                      add_conditional_ownership_attribute
                        actual_ap formal_map attribute_map other_attributes
                | Some (HilExp.Constant _) ->
                    AttributeSetDomain.add Attribute.unconditionally_owned other_attributes
                | _ ->
                    other_attributes
              end
          | _multiple_ownership_attributes ->
              (* TODO: handle multiple ownership attributes *)
              other_attributes in
        AttributeMapDomain.add (ret, []) caller_return_attributes attribute_map

  let is_unprotected is_locked is_threaded pdesc =
    not is_locked && not is_threaded
    && not (Procdesc.is_java_synchronized pdesc)


  let add_access
      exp loc access_kind accesses locks threads
      attribute_map (proc_data : FormalMap.t ProcData.t) =
    let open Domain in
    (* we don't want to warn on accesses to the field if it is (a) thread-confined, or
       (b) volatile *)
    let is_safe_access access prefix_path tenv =
      match access, AccessPath.Raw.get_typ prefix_path tenv with
      | AccessPath.FieldAccess fieldname,
        Some ({Typ.desc=Tstruct typename} | {desc=Tptr ({desc=Tstruct typename}, _)}) ->
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
    let rec add_field_accesses pre prefix_path access_acc = function
      | [] ->
          access_acc
      | access :: access_list' ->
          let kind =
            if List.is_empty access_list'
            then access_kind
            else ThreadSafetyDomain.Access.Read in
          let access_path = fst prefix_path, (snd prefix_path) @ [access] in
          let access_acc' =
            if is_owned prefix_path attribute_map ||
               is_safe_access access prefix_path proc_data.tenv
            then
              access_acc
            else
              (* TODO: I think there's a utility function for this somewhere *)
              let accesses = AccessDomain.get_accesses pre access_acc in
              let accesses' = PathDomain.add_sink (make_access access_path kind loc) accesses in
              AccessDomain.add pre accesses' access_acc in
          add_field_accesses pre access_path access_acc' access_list' in
    let add_access_ acc (base, accesses) =
      if List.is_empty accesses
      then acc
      else
        let pre =
          if is_unprotected locks threads proc_data.pdesc
          then
            match FormalMap.get_formal_index base proc_data.extras with
            | Some formal_index -> AccessPrecondition.Unprotected (Some formal_index)
            | None -> AccessPrecondition.unprotected
          else
            AccessPrecondition.Protected
              (make_excluder (locks || Procdesc.is_java_synchronized proc_data.pdesc)
                 threads) in
        add_field_accesses pre (base, []) acc accesses in

    List.fold ~f:add_access_ ~init:accesses (HilExp.get_access_paths exp)

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
            | ("java.lang.Class" | "java.lang.reflect.Constructor"), "newInstance" ->
                (* reflection can perform allocations *)
                true
            | "java.lang.Object" , "clone" ->
                (* cloning is like allocation *)
                true
            | "java.lang.ThreadLocal", "get" ->
                (* ThreadLocal prevents sharing between threads behind the scenes *)
                true
            | ("android.app.Activity" | "android.view.View"), "findViewById" ->
                (* assume findViewById creates fresh View's (note: not always true) *)
                true
            | "android.support.v4.util.Pools$Pool", "acquire" ->
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

  let is_container_write pn tenv = match pn with
    | Typ.Procname.Java java_pname ->
        let typename = Typ.Name.Java.from_string (Typ.Procname.java_get_class_name java_pname) in
        let is_container_write_ typename _ =
          match Typ.Name.name typename, Typ.Procname.java_get_method java_pname with
          | ("android.util.SparseArray" | "android.support.v4.util.SparseArrayCompat"),
            ("append" | "clear" | "delete" | "put" | "remove" | "removeAt" | "removeAtRange"
            | "setValueAt") -> true
          | "android.support.v4.util.SimpleArrayMap",
            ("clear" | "ensureCapacity" | "put" | "putAll" | "remove" | "removeAt"
            | "setValueAt") -> true
          | "android.support.v4.util.Pools$SimplePool",
            ("acquire" | "release") -> true
          | "java.util.List", ("add" | "addAll" | "clear" | "remove" | "set") -> true
          | "java.util.Map", ("clear" | "put" | "putAll" | "remove") -> true
          | _ -> false in
        PatternMatch.supertype_exists tenv is_container_write_ typename
    | _ ->
        false

  let is_threadsafe_collection pn tenv = match pn with
    | Typ.Procname.Java java_pname ->
        let typename = Typ.Name.Java.from_string (Typ.Procname.java_get_class_name java_pname) in
        let aux tn _ =
          match Typ.Name.name tn with
          | "java.util.concurrent.ConcurrentMap"
          | "java.util.concurrent.CopyOnWriteArrayList"
          | "android.support.v4.util.Pools$SynchronizedPool" -> true
          | _ -> false in
        PatternMatch.supertype_exists tenv aux typename
    | _ -> false

  let is_synchronized_container callee_pname ((_, (base_typ : Typ.t)), accesses) tenv =
    if is_threadsafe_collection callee_pname tenv
    then
      true
    else
      let is_annotated_synchronized base_typename container_field tenv =
        match Tenv.lookup tenv base_typename with
        | Some base_typ ->
            Annotations.field_has_annot
              container_field
              base_typ Annotations.ia_is_synchronized_collection
        | None ->
            false in
      match List.rev accesses with
      | AccessPath.FieldAccess base_field ::
        AccessPath.FieldAccess container_field :: _->
          let base_typename =
            Typ.Name.Java.from_string (Fieldname.java_get_class base_field) in
          is_annotated_synchronized base_typename container_field tenv
      | [AccessPath.FieldAccess container_field] ->
          begin
            match base_typ.desc with
            | Typ.Tstruct base_typename | Tptr ({Typ.desc=Tstruct base_typename}, _) ->
                is_annotated_synchronized base_typename container_field tenv
            | _ ->
                false
          end
      | _ ->
          false

  let make_container_write callee_pname receiver_ap callee_loc tenv =
    (* create a dummy write that represents mutating the contents of the container *)
    let open Domain in
    let callee_accesses =
      if is_synchronized_container callee_pname receiver_ap tenv
      then
        AccessDomain.empty
      else
        let dummy_fieldname =
          Fieldname.Java.from_string
            (container_write_string ^ (Typ.Procname.get_method callee_pname)) in
        let dummy_access_ap =
          fst receiver_ap, (snd receiver_ap) @ [AccessPath.FieldAccess dummy_fieldname] in
        AccessDomain.add_access
          (Unprotected (Some 0))
          (make_access dummy_access_ap Write callee_loc)
          AccessDomain.empty in
    Some (true, false, false, callee_accesses, AttributeSetDomain.empty)

  let get_summary caller_pdesc callee_pname actuals callee_loc tenv =
    if is_container_write callee_pname tenv
    then
      let receiver_ap = match List.hd actuals with
        | Some (HilExp.AccessPath receiver_ap) -> receiver_ap
        | _ ->
            failwithf
              "Call to %a is marked as a container write, but has no receiver"
              Typ.Procname.pp callee_pname in
      make_container_write callee_pname receiver_ap callee_loc tenv
    else
      Summary.read_summary caller_pdesc callee_pname

  (* return true if the given procname boxes a primitive type into a reference type *)
  let is_box = function
    | Typ.Procname.Java java_pname ->
        begin
          match Typ.Procname.java_get_class_name java_pname,
                Typ.Procname.java_get_method java_pname with
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
        false

  let add_reads exps loc accesses locks threads attribute_map proc_data =
    List.fold
      ~f:(fun acc exp -> add_access exp loc Read acc locks threads attribute_map proc_data)
      exps
      ~init:accesses

  let exec_instr
      (astate : Domain.astate)
      ({ ProcData.extras; tenv; pdesc; } as proc_data)
      _
      (instr : HilInstr.t) =
    let open Domain in
    match instr with
    | Call (Some ret_base, Direct procname, actuals, _, loc)
      when acquires_ownership procname tenv ->
        let accesses =
          add_reads actuals loc astate.accesses astate.locks
            astate.threads astate.attribute_map proc_data in
        let attribute_map =
          AttributeMapDomain.add_attribute
            (ret_base, []) Attribute.unconditionally_owned astate.attribute_map in
        { astate with accesses; attribute_map; }

    | Call (ret_opt, Direct callee_pname, actuals, call_flags, loc) ->
        let accesses =
          add_reads actuals loc astate.accesses astate.locks
            astate.threads astate.attribute_map proc_data in
        let astate = { astate with accesses; } in
        let astate =
          match get_thread_model callee_pname with
          | Threaded ->
              { astate with threads = true; }
          | ThreadedIfTrue ->
              begin
                match ret_opt with
                | Some ret_access_path ->
                    let attribute_map =
                      AttributeMapDomain.add_attribute
                        (ret_access_path, [])
                        (Choice Choice.OnMainThread)
                        astate.attribute_map in
                    { astate with attribute_map; }
                | None ->
                    failwithf
                      "Procedure %a specified as returning boolean, but returns nothing"
                      Typ.Procname.pp callee_pname
              end
          | Unknown -> astate in
        let astate_callee =
          (* assuming that modeled procedures do not have useful summaries *)
          if is_thread_utils_method "assertMainThread" callee_pname
          then
            { astate with threads = true; }
          else
            match get_lock_model callee_pname with
            | Lock ->
                { astate with locks = true; }
            | Unlock ->
                { astate with locks = false; }
            | LockedIfTrue ->
                begin
                  match ret_opt with
                  | Some ret_access_path ->
                      let attribute_map =
                        AttributeMapDomain.add_attribute
                          (ret_access_path, [])
                          (Choice Choice.LockHeld)
                          astate.attribute_map in
                      { astate with attribute_map; }
                  | None ->
                      failwithf
                        "Procedure %a specified as returning boolean, but returns nothing"
                        Typ.Procname.pp callee_pname
                end
            | NoEffect ->
                match get_summary pdesc callee_pname actuals loc tenv with
                | Some (callee_thumbs_up, callee_threads, callee_locks,
                        callee_accesses, return_attributes) ->
                    let update_caller_accesses pre callee_accesses caller_accesses =
                      let combined_accesses =
                        PathDomain.with_callsite callee_accesses (CallSite.make callee_pname loc)
                        |> PathDomain.join (AccessDomain.get_accesses pre caller_accesses) in
                      AccessDomain.add pre combined_accesses caller_accesses in
                    let thumbs_up = callee_thumbs_up && astate.thumbs_up in
                    let locks = callee_locks || astate.locks in
                    let threads = callee_threads || astate.threads in
                    let unprotected = is_unprotected locks threads pdesc in
                    (* add [ownership_accesses] to the [accesses_acc] with a protected pre if
                       [exp] is owned, and an appropriate unprotected pre otherwise *)
                    let add_ownership_access ownership_accesses actual_exp accesses_acc =
                      match actual_exp with
                      | HilExp.Constant _ ->
                          (* the actual is a constant, so it's owned in the caller. *)
                          accesses_acc
                      | HilExp.AccessPath actual_access_path ->
                          if is_owned actual_access_path astate.attribute_map
                          then
                            (* the actual passed to the current callee is owned. drop all the
                                 conditional accesses for that actual, since they're all safe *)
                            accesses_acc
                          else
                            let pre =
                              if unprotected
                              then
                                let base = fst actual_access_path in
                                match FormalMap.get_formal_index base extras with
                                | Some formal_index ->
                                    (* the actual passed to the current callee is rooted in a
                                         formal *)
                                    AccessPrecondition.Unprotected (Some formal_index)
                                | None ->
                                    match
                                      AttributeMapDomain.get_conditional_ownership_index
                                        actual_access_path
                                        astate.attribute_map
                                    with
                                    | Some formal_index ->
                                        (* access path conditionally owned if [formal_index] is
                                             owned *)
                                        AccessPrecondition.Unprotected (Some formal_index)
                                    | None ->
                                        (* access path not rooted in a formal and not
                                             conditionally owned *)
                                        AccessPrecondition.unprotected
                              else
                                (* access protected by held lock *)
                                AccessPrecondition.Protected (make_excluder true threads) in
                            update_caller_accesses pre ownership_accesses accesses_acc
                      | _ ->
                          (* couldn't find access path, don't know if it's owned *)
                          update_caller_accesses
                            AccessPrecondition.unprotected ownership_accesses accesses_acc in
                    let accesses =
                      let update_accesses pre callee_accesses accesses_acc = match pre with
                        | AccessPrecondition.Protected _ ->
                            update_caller_accesses pre callee_accesses accesses_acc
                        | AccessPrecondition.Unprotected None ->
                            let pre' =
                              if unprotected
                              then pre
                              else AccessPrecondition.Protected (make_excluder true threads) in
                            update_caller_accesses pre' callee_accesses accesses_acc
                        | AccessPrecondition.Unprotected (Some index) ->
                            add_ownership_access
                              callee_accesses (List.nth_exn actuals index) accesses_acc in
                      AccessDomain.fold update_accesses callee_accesses astate.accesses in
                    let attribute_map =
                      propagate_return_attributes
                        ret_opt
                        return_attributes
                        actuals
                        astate.attribute_map
                        extras in
                    { thumbs_up; locks; threads; accesses; attribute_map; }
                | None ->
                    let should_assume_returns_ownership (call_flags : CallFlags.t) actuals =
                      (* assume non-interface methods with no summary and no parameters return
                         ownership *)
                      not (call_flags.cf_interface) && List.is_empty actuals in
                    if is_box callee_pname
                    then
                      match ret_opt, actuals with
                      | Some ret, HilExp.AccessPath actual_ap :: _
                        when AttributeMapDomain.has_attribute
                            actual_ap Functional astate.attribute_map ->
                          (* TODO: check for constants, which are functional? *)
                          let attribute_map =
                            AttributeMapDomain.add_attribute
                              (ret, [])
                              Functional
                              astate.attribute_map in
                          { astate with attribute_map; }
                      | _ ->
                          astate
                    else if should_assume_returns_ownership call_flags actuals
                    then
                      match ret_opt with
                      | Some ret ->
                          let attribute_map =
                            AttributeMapDomain.add_attribute
                              (ret, [])
                              Attribute.unconditionally_owned
                              astate.attribute_map in
                          { astate with attribute_map; }
                      | None ->
                          astate
                    else
                      astate in
        begin
          match ret_opt with
          | Some (_, { Typ.desc=Typ.Tint ILong | Tfloat FDouble }) ->
              (* writes to longs and doubles are not guaranteed to be atomic in Java, so don't
                 bother tracking whether a returned long or float value is functional *)
              astate_callee
          | Some ret ->
              let add_if_annotated predicate attribute attribute_map =
                if PatternMatch.override_exists predicate tenv callee_pname
                then
                  AttributeMapDomain.add_attribute (ret, []) attribute attribute_map
                else
                  attribute_map in
              let attribute_map =
                add_if_annotated is_functional Functional astate_callee.attribute_map
                |> add_if_annotated
                  (has_return_annot Annotations.ia_is_returns_ownership)
                  Domain.Attribute.unconditionally_owned in
              { astate_callee with attribute_map; }
          | _ ->
              astate_callee
        end

    | Assign (lhs_access_path, rhs_exp, loc) ->
        let rhs_accesses =
          add_access
            rhs_exp loc Read astate.accesses astate.locks
            astate.threads astate.attribute_map proc_data in
        let rhs_access_paths = HilExp.get_access_paths rhs_exp in
        let is_functional =
          not (List.is_empty rhs_access_paths) &&
          List.for_all
            ~f:(fun access_path ->
                AttributeMapDomain.has_attribute access_path Functional astate.attribute_map)
            rhs_access_paths in
        let accesses =
          if is_functional
          then
            (* we want to forget about writes to @Functional fields altogether, otherwise we'll
               report spurious read/write races *)
            rhs_accesses
          else
            add_access
              (AccessPath lhs_access_path)
              loc
              Write
              rhs_accesses
              astate.locks
              astate.threads
              astate.attribute_map
              proc_data in
        let attribute_map =
          propagate_attributes lhs_access_path rhs_exp astate.attribute_map extras in
        { astate with accesses; attribute_map; }

    | Assume (assume_exp, _, _, loc) ->
        let rec eval_binop op var e1 e2 =
          match eval_bexp var e1, eval_bexp var e2 with
          | Some b1, Some b2 -> Some (op b1 b2)
          | _ -> None
        (* return Some bool_value if the given boolean expression evaluates to bool_value when
           [var] is set to true. return None if it has free variables that stop us from
           evaluating it *)
        and eval_bexp var = function
          | HilExp.AccessPath ap when AccessPath.Raw.equal ap var ->
              Some true
          | HilExp.Constant c ->
              Some (not (Const.iszero_int_float c))
          | HilExp.UnaryOperator (Unop.LNot, e, _) ->
              let b_opt = eval_bexp var e in
              Option.map ~f:not b_opt
          | HilExp.BinaryOperator (Binop.LAnd, e1, e2) ->
              eval_binop (&&) var e1 e2
          | HilExp.BinaryOperator (Binop.LOr, e1, e2) ->
              eval_binop (||) var e1 e2
          | HilExp.BinaryOperator (Binop.Eq, e1, e2) ->
              eval_binop Bool.equal var e1 e2
          | HilExp.BinaryOperator (Binop.Ne, e1, e2) ->
              eval_binop (<>) var e1 e2
          | _ ->
              (* non-boolean expression; can't evaluate it *)
              None in
        let add_choice bool_value acc = function
          | Choice.LockHeld ->
              let locks = bool_value in
              { acc with locks; }
          | Choice.OnMainThread ->
              let threads = bool_value in
              { acc with threads; } in

        let accesses =
          add_access
            assume_exp loc Read astate.accesses astate.locks
            astate.threads astate.attribute_map proc_data in
        let astate' =
          match HilExp.get_access_paths assume_exp with
          | [access_path] ->
              let choices = AttributeMapDomain.get_choices access_path astate.attribute_map in
              begin
                match eval_bexp access_path assume_exp with
                | Some bool_value ->
                    (* prune (prune_exp) can only evaluate to true if the choice is [bool_value].
                       add the constraint that the the choice must be [bool_value] to the state *)
                    List.fold ~f:(add_choice bool_value) ~init:astate choices
                | None ->
                    astate
              end
          | _ ->
              astate in
        { astate' with accesses; }
    | Call (_, Indirect _, _, _, _) ->
        failwithf "Unexpected indirect call instruction %a" HilInstr.pp instr
end

module Analyzer = AbstractInterpreter.Make (ProcCfg.Normal) (LowerHil.Make(TransferFunctions))

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
       List.mem ~equal:String.equal immutable_collections (Typ.Name.name typename))
    class_name

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
  not (is_call_to_immutable_collection_method tenv pn) &&
  not (pdesc_is_assumed_thread_safe pdesc tenv)

let is_thread_safe_method pdesc tenv =
  PatternMatch.override_exists
    (fun pn ->
       Annotations.pname_has_return_annot
         pn
         ~attrs_of_pname:Specs.proc_resolve_attributes
         is_thread_safe)
    tenv
    (Procdesc.get_proc_name pdesc)

let empty_post =
  let initial_thumbs_up = true
  and initial_known_on_ui_thread = false
  and has_lock = false
  and return_attrs = ThreadSafetyDomain.AttributeSetDomain.empty in
  (initial_thumbs_up, initial_known_on_ui_thread, has_lock, ThreadSafetyDomain.AccessDomain.empty, return_attrs)

let analyze_procedure { Callbacks.proc_desc; tenv; summary; } =
  let is_initializer tenv proc_name =
    Typ.Procname.is_constructor proc_name || FbThreadSafety.is_custom_init tenv proc_name in
  let open ThreadSafetyDomain in
  (* convert the abstract state to a summary by dropping the id map *)
  if should_analyze_proc proc_desc tenv
  then
    begin
      if not (Procdesc.did_preanalysis proc_desc) then Preanal.do_liveness proc_desc tenv;
      let extras = FormalMap.make proc_desc in
      let proc_data = ProcData.make proc_desc tenv extras in
      let initial =
        let threads = runs_on_ui_thread proc_desc || is_thread_confined_method tenv proc_desc in
        if is_initializer tenv (Procdesc.get_proc_name proc_desc)
        then
          let add_owned_formal acc formal_index =
            match FormalMap.get_formal_base formal_index extras with
            | Some base ->
                AttributeMapDomain.add_attribute (base, []) Attribute.unconditionally_owned acc
            | None ->
                acc in
          let owned_formals =
            (* if a constructer is called via DI, all of its formals will be freshly allocated
               and therefore owned. we assume that constructors annotated with @Inject will only
               be called via DI or using fresh parameters. *)
            if Annotations.pdesc_has_return_annot proc_desc Annotations.ia_is_inject
            then List.mapi ~f:(fun i _ -> i)  (Procdesc.get_formals proc_desc)
            else [0] (* express that the constructor owns [this] *) in
          let attribute_map =
            List.fold
              ~f:add_owned_formal
              owned_formals
              ~init:ThreadSafetyDomain.empty.attribute_map in
          { ThreadSafetyDomain.empty with attribute_map; threads; }, IdAccessPathMapDomain.empty
        else
          { ThreadSafetyDomain.empty with threads; }, IdAccessPathMapDomain.empty in

      match Analyzer.compute_post proc_data ~initial ~debug:false with
      | Some ({ thumbs_up; threads; locks; accesses; attribute_map; }, _) ->
          let return_var_ap =
            AccessPath.of_pvar
              (Pvar.get_ret_pvar (Procdesc.get_proc_name proc_desc))
              (Procdesc.get_ret_type proc_desc) in
          let return_attributes =
            try AttributeMapDomain.find return_var_ap attribute_map
            with Not_found -> AttributeSetDomain.empty in
          (* A hack for modeling lock_guard by releasing a
             lock at the end of the procedure, as destructors are not modeled yet *)
          let update_locks = match Procdesc.get_proc_name proc_desc with
            | ObjC_Cpp _ when locks ->
                let matcher = QualifiedCppName.Match.of_fuzzy_qual_names ["std::lock_guard"] in
                (* Unlock, if the procedure contains a local field of type std::lock_guard *)
                not (List.exists (Procdesc.get_locals proc_desc) ~f:(fun (_, ft) ->
                    Option.exists (Typ.name ft) ~f:(fun name ->
                        QualifiedCppName.Match.match_qualifiers matcher (Typ.Name.qual_name name))
                  ))
            | _ -> locks in
          let post = thumbs_up, threads, update_locks, accesses, return_attributes in
          Summary.update_summary post summary
      | None ->
          summary
    end
  else
    Summary.update_summary empty_post summary

(* we assume two access paths can alias if their access parts are equal (we ignore the base). *)
let can_alias access_path1 access_path2 =
  List.compare AccessPath.compare_access (snd access_path1) (snd access_path2)

module AccessListMap = Caml.Map.Make(struct
    type t = AccessPath.Raw.t [@@deriving compare]
    let compare = can_alias
  end)

let get_current_class_and_threadsafe_superclasses tenv pname =
  match pname with
  | Typ.Procname.Java java_pname ->
      let current_class = Typ.Procname.java_get_class_type_name java_pname in
      let thread_safe_annotated_classes =
        PatternMatch.find_superclasses_with_attributes
          is_thread_safe tenv current_class
      in
      Some (current_class, thread_safe_annotated_classes)
  | _ -> None  (*shouldn't happen*)

(** The addendum message says that a superclass is marked @ThreadSafe,
    when the current class is not so marked*)
let calculate_addendum_message tenv pname =
  match get_current_class_and_threadsafe_superclasses tenv pname with
  | Some (current_class, thread_safe_annotated_classes) ->
      if not (List.mem ~equal:Typ.Name.equal thread_safe_annotated_classes current_class) then
        match thread_safe_annotated_classes with
        | hd::_ ->
            F.asprintf "@\n Note: Superclass %a is marked %a."
              (MF.wrap_monospaced Typ.Name.pp) hd
              MF.pp_monospaced "@ThreadSafe"
        | [] -> ""
      else ""
  | _ -> ""

let filter_by_access access_filter trace =
  let open ThreadSafetyDomain in
  PathDomain.Sinks.filter access_filter (PathDomain.sinks trace)
  |> PathDomain.update_sinks trace

let get_all_accesses_with_pre pre_filter access_filter accesses =
  let open ThreadSafetyDomain in
  AccessDomain.fold
    (fun pre trace acc ->
       if pre_filter pre
       then PathDomain.join (filter_by_access access_filter trace) acc
       else acc)
    accesses
    PathDomain.empty

let get_all_accesses = get_all_accesses_with_pre (fun _ -> true)

let pp_container_access fmt (container_name, function_name) =
  F.fprintf
    fmt
    "container %s via call to %s"
    (MF.monospaced_to_string container_name)
    (MF.monospaced_to_string function_name)

let pp_access fmt sink =
  match get_container_write_desc sink with
  | Some container_write_desc ->
      pp_container_access fmt container_write_desc
  | None ->
      let access_path, _ = ThreadSafetyDomain.PathDomain.Sink.kind sink in
      F.fprintf fmt "%a" (MF.wrap_monospaced AccessPath.pp_access_list) (snd access_path)

let desc_of_sink sink =
  match get_container_write_desc sink with
  | Some container_write_desc ->
      F.asprintf "%a" pp_container_access container_write_desc
  | None ->
      let sink_pname = CallSite.pname (ThreadSafetyDomain.PathDomain.Sink.call_site sink) in
      if Typ.Procname.equal sink_pname Typ.Procname.empty_block
      then F.asprintf "access to %a" pp_access sink
      else F.asprintf "call to %a" Typ.Procname.pp sink_pname

let trace_of_pname orig_sink orig_pdesc callee_pname =
  let open ThreadSafetyDomain in
  let orig_access = PathDomain.Sink.kind orig_sink in
  match Summary.read_summary orig_pdesc callee_pname with
  | Some (_, _, _, access_map, _) ->
      get_all_accesses
        (fun access ->
           Int.equal (Access.compare (PathDomain.Sink.kind access) orig_access) 0)
        access_map
  | _ ->
      PathDomain.empty

let make_trace_with_conflicts conflicts original_path pdesc =
  let open ThreadSafetyDomain in
  let loc_trace_of_path path =
    PathDomain.to_sink_loc_trace ~desc_of_sink path in
  let make_trace_for_sink sink =
    let trace_of_pname = trace_of_pname sink pdesc in
    match PathDomain.get_reportable_sink_path sink ~trace_of_pname with
    | Some path -> loc_trace_of_path path
    | None -> [] in

  let original_trace = loc_trace_of_path original_path in
  match conflicts with
  | conflict_sink :: _ ->
      (* create a trace for one of the conflicts and append it to the trace for the original sink *)
      let conflict_trace = make_trace_for_sink conflict_sink in
      let get_start_loc = function
        | head :: _ -> head.Errlog.lt_loc
        | [] -> Location.dummy in
      let first_trace_spacer =
        Errlog.make_trace_element
          0 (get_start_loc original_trace) "<Beginning of read trace>" [] in
      let second_trace_spacer =
        Errlog.make_trace_element
          0 (get_start_loc conflict_trace) "<Beginning of write trace>" [] in
      (first_trace_spacer :: original_trace) @ (second_trace_spacer :: conflict_trace)
  | [] ->
      original_trace

let report_thread_safety_violation tenv pdesc ~make_description ~conflicts access =
  let open ThreadSafetyDomain in
  let pname = Procdesc.get_proc_name pdesc in
  let report_one_path ((_, sinks) as path) =
    let initial_sink, _ = List.last_exn sinks in
    let final_sink, _ = List.hd_exn sinks in
    let initial_sink_site = PathDomain.Sink.call_site initial_sink in
    let final_sink_site = PathDomain.Sink.call_site final_sink in
    let loc = CallSite.loc initial_sink_site in
    let ltr = make_trace_with_conflicts conflicts path pdesc in
    let msg = Localise.to_issue_id Localise.thread_safety_violation in
    let description = make_description tenv pname final_sink_site initial_sink_site final_sink in
    let exn = Exceptions.Checkers (msg, Localise.verbatim_desc description) in
    Reporting.log_error_deprecated ~store_summary:true pname ~loc ~ltr exn in

  let trace_of_pname = trace_of_pname access pdesc in
  Option.iter ~f:report_one_path (PathDomain.get_reportable_sink_path access ~trace_of_pname)

let pp_procname_short fmt = function
  | Typ.Procname.Java java ->
      F.fprintf fmt "%s.%s"
        (Typ.Procname.java_get_class_name java) (Typ.Procname.java_get_method java)
  | pname ->
      Typ.Procname.pp fmt pname

let make_unprotected_write_description tenv pname final_sink_site initial_sink_site final_sink =
  Format.asprintf
    "Unprotected write. Non-private method %a%s %s %a outside of synchronization.%s"
    (MF.wrap_monospaced pp_procname_short) pname
    (if CallSite.equal final_sink_site initial_sink_site then "" else " indirectly")
    (if is_container_write_sink final_sink then "mutates" else "writes to field")
    pp_access final_sink
    (calculate_addendum_message tenv pname)

let make_read_write_race_description
    conflicts tenv pname final_sink_site initial_sink_site final_sink =
  let race_with_main_thread = List.exists
      ~f:(fun (_, _, threaded, _, _) -> threaded)
      conflicts in
  let conflicting_proc_names =
    List.map
      ~f:(fun (_, _, _, _, pdesc) -> Procdesc.get_proc_name pdesc)
      conflicts
    |> Typ.Procname.Set.of_list in
  let pp_conflicts fmt conflicts =
    if Int.equal (Typ.Procname.Set.cardinal conflicts) 1
    then Typ.Procname.pp fmt (Typ.Procname.Set.choose conflicts)
    else Typ.Procname.Set.pp fmt conflicts in
  let conflicts_description =
    Format.asprintf "Potentially races with writes in method%s %a. %s"
      (if Typ.Procname.Set.cardinal conflicting_proc_names > 1 then "s" else "")
      (MF.wrap_monospaced pp_conflicts) conflicting_proc_names
      (if race_with_main_thread then
         "\n Note: some of these write conflicts are confined to the UI or another thread, \
          but the current method is not specified to be. Consider adding synchronization \
          or a @ThreadConfined annotation to the current method."
       else "") in
  Format.asprintf "Read/Write race. Non-private method %a%s reads from field %a. %s %s"
    (MF.wrap_monospaced pp_procname_short) pname
    (if CallSite.equal final_sink_site initial_sink_site then "" else " indirectly")
    pp_access final_sink
    conflicts_description
    (calculate_addendum_message tenv pname)

(** type for remembering what we have already reported to avoid duplicates. our policy is to report
    each kind of access (read/write) to the same field reachable from the same procedure only once.
    in addition, if a call to a procedure (transitively) accesses multiple fields, we will only
    report one of each kind of access *)
type reported =
  {
    reported_sites : CallSite.Set.t;
    reported_writes : Typ.Procname.Set.t;
    reported_reads : Typ.Procname.Set.t;
  }

let empty_reported =
  let reported_sites = CallSite.Set.empty in
  let reported_writes = Typ.Procname.Set.empty in
  let reported_reads = Typ.Procname.Set.empty in
  { reported_sites; reported_reads; reported_writes; }

(* return true if procedure is at an abstraction boundary or reporting has been explicitly
   requested via @ThreadSafe *)
let should_report_on_proc proc_desc tenv =
  let proc_name = Procdesc.get_proc_name proc_desc in
  is_thread_safe_method proc_desc tenv ||
  (not (Typ.Procname.java_is_autogen_method proc_name) &&
   Procdesc.get_access proc_desc <> PredSymb.Private &&
   not (Annotations.pdesc_return_annot_ends_with proc_desc Annotations.visibleForTesting))

(** Report accesses that may race with each other.

    Principles for race reporting.

    Two accesses are excluded if they are both protected by the same lock or are known to be on the
    same thread. Otherwise they are in conflict. We want to report conflicting accesses one of which
    is a write.

    To cut down on duplication noise we don't always report at both sites (line numbers) involved in
    a race.

    -- If a protected access races with an unprotected one, we don't report the protected but we do
       report the unprotected one (and we point to the protected from the unprotected one).  This
       way the report is at the line number in a race-pair where the programmer should take action.

    -- Similarly, if a threaded and unthreaded (not known to be threaded) access race, we report at
       the unthreaded site.

    Also, we avoid reporting multiple races at the same line (which can happen a lot in an
    interprocedural scenario) or multiple accesses to the same field in a single method, expecting
    that the programmer already gets signal from one report. To report all the races with separate
    warnings leads to a lot of noise.  But note, we never suppress all the potential issues in a
    class: if we don't report any races, it means we didn't find any.

    The above is tempered at the moment by abstractions of "same lock" and "same thread": we are
    currently not distinguishing different locks, and are treating "known to be confined to a
    thread" as if "known to be confined to UI thread".
*)
let report_unsafe_accesses aggregated_access_map =
  let open ThreadSafetyDomain in
  let is_duplicate_report access pname { reported_sites; reported_writes; reported_reads; } =
    CallSite.Set.mem (TraceElem.call_site access) reported_sites ||
    Typ.Procname.Set.mem
      pname
      (match snd (TraceElem.kind access) with
       | Access.Write -> reported_writes
       | Access.Read -> reported_reads) in
  let update_reported access pname reported =
    let reported_sites = CallSite.Set.add (TraceElem.call_site access) reported.reported_sites in
    match snd (TraceElem.kind access) with
    | Access.Write ->
        let reported_writes = Typ.Procname.Set.add pname reported.reported_writes in
        { reported with reported_writes; reported_sites; }
    | Access.Read ->
        let reported_reads = Typ.Procname.Set.add pname reported.reported_reads in
        { reported with reported_reads; reported_sites; } in
  let report_unsafe_access (access, pre, threaded, tenv, pdesc) accesses reported_acc =
    let pname = Procdesc.get_proc_name pdesc in
    if is_duplicate_report access pname reported_acc
    then
      reported_acc
    else
      match snd (TraceElem.kind access), pre with
      | Access.Write, AccessPrecondition.Unprotected _ ->
          begin
            match Procdesc.get_proc_name pdesc with
            | Java _ ->
                if threaded
                then
                  reported_acc
                else
                  begin
                    (* unprotected write. warn. *)
                    report_thread_safety_violation
                      tenv
                      pdesc
                      ~make_description:make_unprotected_write_description
                      ~conflicts:[]
                      access;
                    update_reported access pname reported_acc
                  end
            | _ ->
                (* Do not report unprotected writes for ObjC_Cpp *)
                reported_acc
          end
      | Access.Write, AccessPrecondition.Protected _ ->
          (* protected write, do nothing *)
          reported_acc
      | Access.Read, AccessPrecondition.Unprotected _ ->
          (* unprotected read. report all writes as conflicts for java *)
          (* for c++ filter out unprotected writes *)
          let is_cpp_protected_write pre = match pre with
            | AccessPrecondition.Unprotected _ -> Typ.Procname.is_java pname
            | AccessPrecondition.Protected _ -> true in
          let all_writes =
            List.filter
              ~f:(fun (other_access, pre, other_threaded, _, _) ->
                  TraceElem.is_write other_access && not (threaded && other_threaded)
                  && is_cpp_protected_write pre)
              accesses in
          if List.is_empty all_writes
          then
            reported_acc
          else
            begin
              report_thread_safety_violation
                tenv
                pdesc
                ~make_description:(make_read_write_race_description all_writes)
                ~conflicts:(List.map ~f:(fun (access, _, _, _, _) -> access) all_writes)
                access;
              update_reported access pname reported_acc
            end
      | Access.Read, AccessPrecondition.Protected excl ->
          (* protected read.
             report unprotected writes and opposite protected writes as conflicts
             Thread and Lock are opposites of one another, and
             Both has no opposite*)
          let is_opposite  = function
            | Excluder.Lock , Excluder.Thread
              -> true
            | Excluder.Thread , Excluder.Lock
              -> true
            | _ , _ -> false in
          let conflicting_writes =
            List.filter
              ~f:(fun (access, pre, _, _, _) ->
                  match pre with
                  | AccessPrecondition.Unprotected _ ->
                      TraceElem.is_write access
                  | AccessPrecondition.Protected other_excl
                    when is_opposite (excl , other_excl) ->
                      TraceElem.is_write access
                  | _ ->
                      false)
              accesses in
          if List.is_empty conflicting_writes
          then
            reported_acc
          else
            begin
              (* protected read with conflicting unprotected write(s). warn. *)
              report_thread_safety_violation
                tenv
                pdesc
                ~make_description:(make_read_write_race_description conflicting_writes)
                ~conflicts:(List.map ~f:(fun (access, _, _, _, _) -> access) conflicting_writes)
                access;
              update_reported access pname reported_acc
            end in
  AccessListMap.fold
    (fun _ grouped_accesses reported_acc ->
       (* reset the reported reads and writes for each memory location *)
       let reported =
         { reported_acc with reported_writes = Typ.Procname.Set.empty;
                             reported_reads = Typ.Procname.Set.empty; } in
       let accessed_by_threadsafe_method =
         List.exists
           ~f:(fun (_, _, _, tenv, pdesc) -> is_thread_safe_method pdesc tenv)
           grouped_accesses in
       let class_has_mutex_member objc_cpp tenv =
         let class_name = Typ.Procname.objc_cpp_get_class_type_name objc_cpp in
         let matcher = QualifiedCppName.Match.of_fuzzy_qual_names ["std::mutex"] in
         Option.exists (Tenv.lookup tenv class_name) ~f:(fun class_str ->
             (* check if the class contains a member of type std::mutex *)
             List.exists (class_str.Typ.Struct.fields) ~f:(fun (_, ft, _) ->
                 Option.exists (Typ.name ft) ~f:(fun name ->
                     QualifiedCppName.Match.match_qualifiers matcher (Typ.Name.qual_name name))
               )
           ) in
       let should_report pdesc tenv =
         match Procdesc.get_proc_name pdesc with
         | Java _ as pname ->
             (* report if
                - the method/class of the access is thread-safe
                  (or an override or superclass is), or
                - any access is in a field marked thread-safe (or an override) *)
             (accessed_by_threadsafe_method
              ||
              let is_class_threadsafe =
                not
                  (let current_class_marked_not_threadsafe =
                     PatternMatch.check_current_class_attributes
                       Annotations.ia_is_not_thread_safe tenv pname
                   in
                   current_class_marked_not_threadsafe)
                &&
                (let current_class_or_super_marked_threadsafe =
                   match get_current_class_and_threadsafe_superclasses tenv pname with
                   | Some (_, thread_safe_annotated_classes) ->
                       not (List.is_empty thread_safe_annotated_classes)
                   | _ ->
                       false
                 in
                 current_class_or_super_marked_threadsafe)
              in
              is_class_threadsafe)
             && should_report_on_proc pdesc tenv
         | ObjC_Cpp objc_cpp ->
             (* report if the class has a mutex member  *)
             class_has_mutex_member objc_cpp tenv
         | _ ->
             false
       in
       let reportable_accesses =
         List.filter ~f:(fun (_, _, _, tenv, pdesc) -> should_report pdesc tenv) grouped_accesses in
       List.fold
         ~f:(fun acc access -> report_unsafe_access access reportable_accesses acc)
         reportable_accesses
         ~init:reported)
    aggregated_access_map
    empty_reported
  |> ignore

(* equivalence relation computing whether two access paths may refer to the
   same heap location. *)
let may_alias tenv p1 p2 =
  let open Typ in
  let open AccessPath in
  phys_equal p1 p2 ||
  match List.last_exn (snd p1), List.last_exn (snd p2) with
  | FieldAccess _, ArrayAccess _ | ArrayAccess _, FieldAccess _ -> false
  (* fields in Java contain the class name /declaring/ them
     thus two fields can be aliases *iff* they are equal *)
  | FieldAccess f1, FieldAccess f2 -> Fieldname.equal f1 f2
  (* if arrays of objects that have an inheritance rel then they can alias *)
  | ArrayAccess {desc=Tptr ({desc=Tstruct tn1}, _)},
    ArrayAccess {desc=Tptr ({desc=Tstruct tn2}, _)} ->
      PatternMatch.is_subtype tenv tn1 tn2 ||
      PatternMatch.is_subtype tenv tn2 tn1
  (* primitive type arrays can alias if the prim. type is the same *)
  | ArrayAccess t1, ArrayAccess t2 ->
      equal_desc t1.desc t2.desc

(* take a results table and quotient it by the may_alias relation *)
let quotient_access_map acc_map =
  let rec aux acc m =
    if AccessListMap.is_empty m then
      acc
    else
      let k, vals = AccessListMap.choose m in
      let (_, _, _, tenv, _) =
        List.find_exn vals
          ~f:(fun (elem, _, _, _, _) ->
              AccessPath.Raw.equal k (ThreadSafetyDomain.TraceElem.kind elem |> fst))
      in
      (* assumption: the tenv for k is sufficient for k' too *)
      let (k_part, non_k_part) =
        AccessListMap.partition (fun k' _ -> may_alias tenv k k') m in
      if AccessListMap.is_empty k_part
      then failwith "may_alias is not reflexive!";
      let k_accesses =
        AccessListMap.fold
          (fun _ v acc' -> List.append v acc')
          k_part
          []
      in
      let new_acc = AccessListMap.add k k_accesses acc in
      aux new_acc non_k_part
  in
  aux AccessListMap.empty acc_map

(* decide if we should throw away a path before doing safety analysis
   for now, just check for whether the access is within a switch-map
   that is auto-generated by Java. *)
let should_filter_access (_, path) =
  let check_access_step = function
    | AccessPath.ArrayAccess _ -> false
    | AccessPath.FieldAccess fld ->
        String.is_substring ~substring:"$SwitchMap" (Fieldname.to_string fld) in
  List.exists path ~f:check_access_step

(* create a map from [abstraction of a memory loc] -> accesses that may touch that memory loc. for
   now, our abstraction is an access path like x.f.g whose concretization is the set of memory cells
   that x.f.g may point to during execution *)
let make_results_table file_env =
  let aggregate_post (_ , threaded, _, accesses, _) tenv pdesc acc =
    let open ThreadSafetyDomain in
    AccessDomain.fold
      (fun pre accesses acc ->
         PathDomain.Sinks.fold
           (fun access acc ->
              let access_path, _ = TraceElem.kind access in
              if should_filter_access access_path then acc else
                let grouped_accesses =
                  try AccessListMap.find access_path acc
                  with Not_found -> [] in
                AccessListMap.add
                  access_path
                  ((access, pre, threaded, tenv, pdesc) :: grouped_accesses)
                  acc)
           (PathDomain.sinks accesses)
           acc)
      accesses
      acc in
  let aggregate_posts acc (_, tenv, proc_name, proc_desc) =
    match Summary.read_summary proc_desc proc_name with
    | Some summary -> aggregate_post summary tenv proc_desc acc
    | None -> acc in
  List.fold ~f:aggregate_posts file_env ~init:AccessListMap.empty |> quotient_access_map

(* Gathers results by analyzing all the methods in a file, then post-processes the results to check
   an (approximation of) thread safety *)
let file_analysis _ _ _ file_env =
  report_unsafe_accesses (make_results_table file_env)
