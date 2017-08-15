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

  let update_payload post (summary: Specs.summary) =
    {summary with payload= {summary.payload with threadsafety= Some post}}

  let read_payload (summary: Specs.summary) = summary.payload.threadsafety
end)

(*Bit of redundancy with code in is_unprotected, might alter later *)
let make_excluder locks threads =
  if locks && not threads then ThreadSafetyDomain.Excluder.Lock
  else if not locks && threads then ThreadSafetyDomain.Excluder.Thread
  else if locks && threads then ThreadSafetyDomain.Excluder.Both
  else failwithf "called when neither lock nor thread known"

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = ThreadSafetyDomain

  type extras = ProcData.no_extras

  type lock_model = Lock | Unlock | LockedIfTrue | NoEffect

  type thread_model = Threaded | Unknown | ThreadedIfTrue

  type container_access_model = ContainerRead | ContainerWrite

  let is_thread_utils_type java_pname =
    let pn = Typ.Procname.java_get_class_name java_pname in
    String.is_suffix ~suffix:"ThreadUtils" pn || String.is_suffix ~suffix:"ThreadUtil" pn

  let is_thread_utils_method method_name_str = function
    | Typ.Procname.Java java_pname
     -> is_thread_utils_type java_pname
        && String.equal (Typ.Procname.java_get_method java_pname) method_name_str
    | _
     -> false

  let get_lock_model =
    let is_cpp_lock =
      let matcher_lock =
        QualifiedCppName.Match.of_fuzzy_qual_names ["std::mutex::lock"; "std::unique_lock::lock"]
      in
      let matcher_lock_constructor =
        QualifiedCppName.Match.of_fuzzy_qual_names
          ["std::lock_guard::lock_guard"; "std::unique_lock::unique_lock"]
      in
      fun pname actuals ->
        QualifiedCppName.Match.match_qualifiers matcher_lock (Typ.Procname.get_qualifiers pname)
        || QualifiedCppName.Match.match_qualifiers matcher_lock_constructor
             (Typ.Procname.get_qualifiers pname)
           (* Passing additional parameter allows to defer the lock *)
           && Int.equal 2 (List.length actuals)
    and is_cpp_unlock =
      let matcher =
        QualifiedCppName.Match.of_fuzzy_qual_names
          [ "std::mutex::unlock"
          ; "std::unique_lock::unlock"
          ; "std::lock_guard::~lock_guard"
          ; "std::unique_lock::~unique_lock" ]
      in
      fun pname ->
        QualifiedCppName.Match.match_qualifiers matcher (Typ.Procname.get_qualifiers pname)
    in
    fun pname actuals ->
      match pname with
      | Typ.Procname.Java java_pname
       -> (
          if is_thread_utils_method "assertHoldsLock" (Typ.Procname.Java java_pname) then Lock
          else
            match
              (Typ.Procname.java_get_class_name java_pname, Typ.Procname.java_get_method java_pname)
            with
            | ( ( "java.util.concurrent.locks.Lock" | "java.util.concurrent.locks.ReentrantLock"
                | "java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock"
                | "java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock" )
              , ("lock" | "lockInterruptibly") )
             -> Lock
            | ( ( "java.util.concurrent.locks.Lock" | "java.util.concurrent.locks.ReentrantLock"
                | "java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock"
                | "java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock" )
              , "unlock" )
             -> Unlock
            | ( ( "java.util.concurrent.locks.Lock" | "java.util.concurrent.locks.ReentrantLock"
                | "java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock"
                | "java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock" )
              , "tryLock" )
             -> LockedIfTrue
            | ( "com.facebook.buck.util.concurrent.AutoCloseableReadWriteUpdateLock"
              , ("readLock" | "updateLock" | "writeLock") )
             -> Lock
            | _
             -> NoEffect )
      | Typ.Procname.ObjC_Cpp _ as pname when is_cpp_lock pname actuals
       -> Lock
      | Typ.Procname.ObjC_Cpp _ as pname when is_cpp_unlock pname
       -> Unlock
      | pname when Typ.Procname.equal pname BuiltinDecl.__set_locked_attribute
       -> Lock
      | pname when Typ.Procname.equal pname BuiltinDecl.__delete_locked_attribute
       -> Unlock
      | _
       -> NoEffect

  let get_thread_model = function
    | Typ.Procname.Java java_pname
     -> if is_thread_utils_type java_pname then
          match Typ.Procname.java_get_method java_pname with
          | "assertMainThread" | "checkOnMainThread" | "assertOnUiThread"
           -> Threaded
          | "isMainThread" | "isUiThread"
           -> ThreadedIfTrue
          | _
           -> Unknown
        else Unknown
    (*Note we are not modelling assertOnNonUiThread or
      assertOnBackgroundThread. These treated as Unknown*)
    | _
     -> Unknown

  let get_container_access pn tenv =
    match pn with
    | Typ.Procname.Java java_pname
     -> let typename = Typ.Name.Java.from_string (Typ.Procname.java_get_class_name java_pname) in
        let get_container_access_ typename =
          match (Typ.Name.name typename, Typ.Procname.java_get_method java_pname) with
          | ( ("android.util.SparseArray" | "android.support.v4.util.SparseArrayCompat")
            , ( "append" | "clear" | "delete" | "put" | "remove" | "removeAt" | "removeAtRange"
              | "setValueAt" ) )
           -> Some ContainerWrite
          | ( ("android.util.SparseArray" | "android.support.v4.util.SparseArrayCompat")
            , ("clone" | "get" | "indexOfKey" | "indexOfValue" | "keyAt" | "size" | "valueAt") )
           -> Some ContainerRead
          | ( "android.support.v4.util.SimpleArrayMap"
            , ("clear" | "ensureCapacity" | "put" | "putAll" | "remove" | "removeAt" | "setValueAt")
            )
           -> Some ContainerWrite
          | ( "android.support.v4.util.SimpleArrayMap"
            , ( "containsKey" | "containsValue" | "get" | "hashCode" | "indexOfKey" | "isEmpty"
              | "keyAt" | "size" | "valueAt" ) )
           -> Some ContainerRead
          | "android.support.v4.util.Pools$SimplePool", ("acquire" | "release")
           -> Some ContainerWrite
          | "java.util.List", ("add" | "addAll" | "clear" | "remove" | "set")
           -> Some ContainerWrite
          | ( "java.util.List"
            , ( "contains" | "containsAll" | "equals" | "get" | "hashCode" | "indexOf" | "isEmpty"
              | "iterator" | "lastIndexOf" | "listIterator" | "size" | "toArray" ) )
           -> Some ContainerRead
          | "java.util.Map", ("clear" | "put" | "putAll" | "remove")
           -> Some ContainerWrite
          | ( "java.util.Map"
            , ( "containsKey" | "containsValue" | "entrySet" | "equals" | "get" | "hashCode"
              | "isEmpty" | "keySet" | "size" | "values" ) )
           -> Some ContainerRead
          | _
           -> None
        in
        PatternMatch.supertype_find_map_opt tenv get_container_access_ typename
    | _
     -> None

  (* propagate attributes from the leaves to the root of an RHS Hil expression *)
  let rec attributes_of_expr attribute_map e =
    let open HilExp in
    let open Domain in
    match e with
    | HilExp.AccessPath ap -> (
      try AttributeMapDomain.find ap attribute_map
      with Not_found -> AttributeSetDomain.empty )
    | Constant _
     -> AttributeSetDomain.of_list [Attribute.Functional]
    | Exception expr (* treat exceptions as transparent wrt attributes *) | Cast (_, expr)
     -> attributes_of_expr attribute_map expr
    | UnaryOperator (_, expr, _)
     -> attributes_of_expr attribute_map expr
    | BinaryOperator (_, expr1, expr2)
     -> let attributes1 = attributes_of_expr attribute_map expr1 in
        let attributes2 = attributes_of_expr attribute_map expr2 in
        AttributeSetDomain.join attributes1 attributes2
    | Closure _ | Sizeof _
     -> AttributeSetDomain.empty

  let rec ownership_of_expr expr ownership =
    let open Domain in
    let open HilExp in
    match expr with
    | AccessPath ap
     -> OwnershipDomain.get_owned ap ownership
    | Constant _
     -> OwnershipAbstractValue.owned
    | Exception e (* treat exceptions as transparent wrt ownership *) | Cast (_, e)
     -> ownership_of_expr e ownership
    | _
     -> OwnershipAbstractValue.unowned

  (* will return true on x.f.g.h when x.f and x.f.g are owned, but not requiring x.f.g.h *)
  (* must not be called with an empty access list *)
  let all_prefixes_owned (base, accesses) attribute_map =
    let but_last_rev = List.rev accesses |> List.tl_exn in
    let rec aux acc = function [] -> acc | _ :: tail as all -> aux (List.rev all :: acc) tail in
    let prefixes = aux [] but_last_rev in
    List.for_all
      ~f:(fun ap -> ThreadSafetyDomain.OwnershipDomain.is_owned (base, ap) attribute_map)
      prefixes

  let propagate_attributes lhs_access_path rhs_exp attribute_map =
    let rhs_attributes = attributes_of_expr attribute_map rhs_exp in
    Domain.AttributeMapDomain.add lhs_access_path rhs_attributes attribute_map

  let propagate_ownership (lhs_root, accesses as lhs_access_path) rhs_exp ownership =
    if Var.is_global (fst lhs_root) then
      (* do not assign ownership to access paths rooted at globals *)
      ownership
    else
      let ownership_value =
        match accesses with
        | []
         -> ownership_of_expr rhs_exp ownership
        | [_]
          when match Domain.OwnershipDomain.get_owned (lhs_root, []) ownership with
               | Domain.OwnershipAbstractValue.OwnedIf _
                -> true
               | _
                -> false
         -> ownership_of_expr rhs_exp ownership
        | _ when all_prefixes_owned lhs_access_path ownership
         -> ownership_of_expr rhs_exp ownership
        | _
         -> Domain.OwnershipAbstractValue.unowned
      in
      Domain.OwnershipDomain.add lhs_access_path ownership_value ownership

  let propagate_return ret_opt ret_ownership ret_attributes actuals
      {Domain.ownership; attribute_map} =
    let open Domain in
    match ret_opt with
    | None
     -> (ownership, attribute_map)
    | Some ret
     -> let ret_access_path = (ret, []) in
        let get_ownership formal_index acc =
          match List.nth actuals formal_index with
          | Some HilExp.AccessPath actual_ap
           -> OwnershipDomain.get_owned actual_ap ownership |> OwnershipAbstractValue.join acc
          | Some HilExp.Constant _
           -> acc
          | _
           -> OwnershipAbstractValue.unowned
        in
        let ownership' =
          match ret_ownership with
          | OwnershipAbstractValue.Owned | Unowned
           -> OwnershipDomain.add ret_access_path ret_ownership ownership
          | OwnershipAbstractValue.OwnedIf formal_indexes
           -> let actuals_ownership =
                IntSet.fold get_ownership formal_indexes OwnershipAbstractValue.owned
              in
              OwnershipDomain.add ret_access_path actuals_ownership ownership
        in
        let attribute_map' = AttributeMapDomain.add ret_access_path ret_attributes attribute_map in
        (ownership', attribute_map')

  let is_unprotected is_locked is_threaded pdesc =
    not is_locked && not is_threaded && not (Procdesc.is_java_synchronized pdesc)

  (** return true if this function is library code from the JDK core libraries or Android *)
  let is_java_library = function
    | Typ.Procname.Java java_pname -> (
      match Typ.Procname.java_get_package java_pname with
      | Some package_name
       -> String.is_prefix ~prefix:"java." package_name
          || String.is_prefix ~prefix:"android." package_name
          || String.is_prefix ~prefix:"com.google." package_name
      | None
       -> false )
    | _
     -> false

  let is_builder_function = function
    | Typ.Procname.Java java_pname
     -> String.is_suffix ~suffix:"$Builder" (Typ.Procname.java_get_class_name java_pname)
    | _
     -> false

  let has_return_annot predicate pn =
    Annotations.pname_has_return_annot pn ~attrs_of_pname:Specs.proc_resolve_attributes predicate

  let add_unannotated_call_access pname (call_flags: CallFlags.t) loc tenv ~locks ~threads
      attribute_map (proc_data: ProcData.no_extras ProcData.t) =
    if call_flags.cf_interface && Typ.Procname.is_java pname
       && not (is_java_library pname || is_builder_function pname)
       (* can't ask anyone to annotate interfaces in library code, and Builder's should always be
          thread-safe (would be unreasonable to ask everyone to annotate them) *)
       && not (PatternMatch.check_class_attributes Annotations.ia_is_thread_safe tenv pname)
       && not (has_return_annot Annotations.ia_is_thread_safe pname)
    then
      let open Domain in
      let pre =
        if is_unprotected locks threads proc_data.pdesc then AccessPrecondition.TotallyUnprotected
        else
          AccessPrecondition.Protected
            (make_excluder (locks || Procdesc.is_java_synchronized proc_data.pdesc) threads)
      in
      AccessDomain.add_access pre (make_unannotated_call_access pname loc) attribute_map
    else attribute_map

  let add_access exp loc ~is_write_access accesses locks threads ownership
      (proc_data: ProcData.no_extras ProcData.t) =
    let open Domain in
    (* we don't want to warn on accesses to the field if it is (a) thread-confined, or
       (b) volatile *)
    let is_safe_access access prefix_path tenv =
      match (access, AccessPath.get_typ prefix_path tenv) with
      | ( AccessPath.FieldAccess fieldname
        , Some ({Typ.desc= Tstruct typename} | {desc= Tptr ({desc= Tstruct typename}, _)}) ) -> (
        match Tenv.lookup tenv typename with
        | Some struct_typ
         -> Annotations.struct_typ_has_annot struct_typ Annotations.ia_is_thread_confined
            || Annotations.field_has_annot fieldname struct_typ Annotations.ia_is_thread_confined
            || Annotations.field_has_annot fieldname struct_typ Annotations.ia_is_volatile
        | None
         -> false )
      | _
       -> false
    in
    let rec add_field_accesses pre prefix_path access_acc = function
      | []
       -> access_acc
      | access :: access_list'
       -> let is_write = if List.is_empty access_list' then is_write_access else false in
          let access_path = (fst prefix_path, snd prefix_path @ [access]) in
          let access_acc' =
            if OwnershipDomain.is_owned prefix_path ownership
               || is_safe_access access prefix_path proc_data.tenv
            then access_acc
            else
              AccessDomain.add_access pre (make_field_access access_path ~is_write loc) access_acc
          in
          add_field_accesses pre access_path access_acc' access_list'
    in
    let add_access_ acc (base, accesses) =
      let base_access_path = (base, []) in
      if List.is_empty accesses || OwnershipDomain.is_owned base_access_path ownership then acc
      else
        let pre =
          if is_unprotected locks threads proc_data.pdesc then
            match OwnershipDomain.get_owned base_access_path ownership with
            | OwnershipAbstractValue.OwnedIf formal_indexes
             -> AccessPrecondition.Unprotected formal_indexes
            | OwnershipAbstractValue.Owned
             -> assert false
            | OwnershipAbstractValue.Unowned
             -> AccessPrecondition.TotallyUnprotected
          else
            AccessPrecondition.Protected
              (make_excluder (locks || Procdesc.is_java_synchronized proc_data.pdesc) threads)
        in
        add_field_accesses pre base_access_path acc accesses
    in
    List.fold ~f:add_access_ ~init:accesses (HilExp.get_access_paths exp)

  let is_functional pname =
    let is_annotated_functional = has_return_annot Annotations.ia_is_functional in
    let is_modeled_functional = function
      | Typ.Procname.Java java_pname -> (
        match
          (Typ.Procname.java_get_class_name java_pname, Typ.Procname.java_get_method java_pname)
        with
        | "android.content.res.Resources", method_name
         -> (* all methods of Resources are considered @Functional except for the ones in this
                     blacklist *)
            let non_functional_resource_methods =
              [ "getAssets"
              ; "getConfiguration"
              ; "getSystem"
              ; "newTheme"
              ; "openRawResource"
              ; "openRawResourceFd" ]
            in
            not (List.mem ~equal:String.equal non_functional_resource_methods method_name)
        | _
         -> false )
      | _
       -> false
    in
    is_annotated_functional pname || is_modeled_functional pname

  let acquires_ownership pname tenv =
    let is_allocation pn =
      Typ.Procname.equal pn BuiltinDecl.__new || Typ.Procname.equal pn BuiltinDecl.__new_array
    in
    (* identify library functions that maintain ownership invariants behind the scenes *)
    let is_owned_in_library = function
      | Typ.Procname.Java java_pname -> (
        match
          (Typ.Procname.java_get_class_name java_pname, Typ.Procname.java_get_method java_pname)
        with
        | "javax.inject.Provider", "get"
         -> (* in dependency injection, the library allocates fresh values behind the scenes *)
            true
        | ("java.lang.Class" | "java.lang.reflect.Constructor"), "newInstance"
         -> (* reflection can perform allocations *)
            true
        | "java.lang.Object", "clone"
         -> (* cloning is like allocation *)
            true
        | "java.lang.ThreadLocal", "get"
         -> (* ThreadLocal prevents sharing between threads behind the scenes *)
            true
        | ("android.app.Activity" | "android.view.View"), "findViewById"
         -> (* assume findViewById creates fresh View's (note: not always true) *)
            true
        | ( ( "android.support.v4.util.Pools$Pool" | "android.support.v4.util.Pools$SimplePool"
            | "android.support.v4.util.Pools$SynchronizedPool" )
          , "acquire" )
         -> (* a pool should own all of its objects *)
            true
        | _
         -> false )
      | _
       -> false
    in
    is_allocation pname || is_owned_in_library pname
    || PatternMatch.override_exists is_owned_in_library tenv pname

  let is_threadsafe_collection pn tenv =
    match pn with
    | Typ.Procname.Java java_pname
     -> let typename = Typ.Name.Java.from_string (Typ.Procname.java_get_class_name java_pname) in
        let aux tn _ =
          match Typ.Name.name tn with
          | "java.util.concurrent.ConcurrentMap"
          | "java.util.concurrent.CopyOnWriteArrayList"
          | "android.support.v4.util.Pools$SynchronizedPool"
           -> true
          | _
           -> false
        in
        PatternMatch.supertype_exists tenv aux typename
    | _
     -> false

  let is_synchronized_container callee_pname ((_, (base_typ: Typ.t)), accesses) tenv =
    if is_threadsafe_collection callee_pname tenv then true
    else
      let is_annotated_synchronized base_typename container_field tenv =
        match Tenv.lookup tenv base_typename with
        | Some base_typ
         -> Annotations.field_has_annot container_field base_typ
              Annotations.ia_is_synchronized_collection
        | None
         -> false
      in
      match List.rev accesses with
      | (AccessPath.FieldAccess base_field) :: (AccessPath.FieldAccess container_field) :: _
       -> let base_typename =
            Typ.Name.Java.from_string (Typ.Fieldname.java_get_class base_field)
          in
          is_annotated_synchronized base_typename container_field tenv
      | [(AccessPath.FieldAccess container_field)] -> (
        match base_typ.desc with
        | Typ.Tstruct base_typename | Tptr ({Typ.desc= Tstruct base_typename}, _)
         -> is_annotated_synchronized base_typename container_field tenv
        | _
         -> false )
      | _
       -> false

  let make_container_access callee_pname ~is_write actuals receiver_ap callee_loc tenv =
    (* create a dummy write that represents mutating the contents of the container *)
    let open Domain in
    let callee_accesses =
      if is_synchronized_container callee_pname receiver_ap tenv then AccessDomain.empty
      else
        let container_access =
          make_container_access receiver_ap ~is_write callee_pname callee_loc
        in
        AccessDomain.add_access (Unprotected (IntSet.singleton 0)) container_access
          AccessDomain.empty
    in
    (* TODO: for now all formals escape *)
    (* we need a more intelligent escape analysis, that branches on whether
             we own the container *)
    let escapee_formals = List.length actuals |> List.range 0 |> FormalsDomain.of_list in
    Some
      ( true
      , false
      , false
      , callee_accesses
      , OwnershipAbstractValue.unowned
      , AttributeSetDomain.empty
      , escapee_formals )

  let get_summary caller_pdesc callee_pname actuals callee_loc tenv =
    let get_receiver_ap actuals =
      match List.hd actuals with
      | Some HilExp.AccessPath receiver_ap
       -> receiver_ap
      | _
       -> failwithf "Call to %a is marked as a container write, but has no receiver"
            Typ.Procname.pp callee_pname
    in
    match get_container_access callee_pname tenv with
    | Some ContainerWrite
     -> make_container_access callee_pname ~is_write:true actuals (get_receiver_ap actuals)
          callee_loc tenv
    | Some ContainerRead
     -> make_container_access callee_pname ~is_write:false actuals (get_receiver_ap actuals)
          callee_loc tenv
    | None
     -> Summary.read_summary caller_pdesc callee_pname

  (* return true if the given procname boxes a primitive type into a reference type *)
  let is_box = function
    | Typ.Procname.Java java_pname -> (
      match
        (Typ.Procname.java_get_class_name java_pname, Typ.Procname.java_get_method java_pname)
      with
      | ( ( "java.lang.Boolean" | "java.lang.Byte" | "java.lang.Char" | "java.lang.Double"
          | "java.lang.Float" | "java.lang.Integer" | "java.lang.Long" | "java.lang.Short" )
        , "valueOf" )
       -> true
      | _
       -> false )
    | _
     -> false

  let add_reads exps loc accesses locks threads ownership proc_data =
    List.fold
      ~f:(fun acc exp ->
        add_access exp loc ~is_write_access:false acc locks threads ownership proc_data)
      exps ~init:accesses

  let add_escapees_from_exp rhs_exp ownership escapees =
    HilExp.get_access_paths rhs_exp |> List.rev_map ~f:(Domain.Escapee.of_access_path ownership)
    |> List.concat_no_order |> Domain.EscapeeDomain.add_from_list escapees

  let add_escapees_from_exp_list exp_list extras escapees =
    List.fold ~init:escapees exp_list ~f:(fun acc exp -> add_escapees_from_exp exp extras acc)

  let add_escapees_from_call actuals escapee_formals extras escapees =
    let escapee_actuals =
      Domain.FormalsDomain.elements escapee_formals |> List.rev_map ~f:(List.nth_exn actuals)
    in
    add_escapees_from_exp_list escapee_actuals extras escapees

  let exec_instr (astate: Domain.astate) ({ProcData.tenv; pdesc} as proc_data) _
      (instr: HilInstr.t) =
    let open Domain in
    match instr with
    | Call (Some ret_base, Direct procname, actuals, _, loc) when acquires_ownership procname tenv
     -> let accesses =
          add_reads actuals loc astate.accesses astate.locks astate.threads astate.ownership
            proc_data
        in
        let ownership =
          OwnershipDomain.add (ret_base, []) OwnershipAbstractValue.owned astate.ownership
        in
        (* TODO: we need to model escaping formals, now just assume all escape *)
        let escapees = add_escapees_from_exp_list actuals astate.ownership astate.escapees in
        {astate with accesses; ownership; escapees}
    | Call (ret_opt, Direct callee_pname, actuals, call_flags, loc)
     -> (
        let accesses_with_unannotated_calls =
          add_unannotated_call_access callee_pname call_flags loc tenv ~locks:astate.locks
            ~threads:astate.threads astate.accesses proc_data
        in
        let accesses =
          add_reads actuals loc accesses_with_unannotated_calls astate.locks astate.threads
            astate.ownership proc_data
        in
        let astate = {astate with accesses} in
        let astate =
          match get_thread_model callee_pname with
          | Threaded
           -> {astate with threads= true}
          | ThreadedIfTrue -> (
            match ret_opt with
            | Some ret_access_path
             -> let attribute_map =
                  AttributeMapDomain.add_attribute (ret_access_path, [])
                    (Choice Choice.OnMainThread) astate.attribute_map
                in
                {astate with attribute_map}
            | None
             -> failwithf "Procedure %a specified as returning boolean, but returns nothing"
                  Typ.Procname.pp callee_pname )
          | Unknown
           -> astate
        in
        let astate_callee =
          (* assuming that modeled procedures do not have useful summaries *)
          if is_thread_utils_method "assertMainThread" callee_pname then {astate with threads= true}
          else
            match get_lock_model callee_pname actuals with
            | Lock
             -> {astate with locks= true}
            | Unlock
             -> {astate with locks= false}
            | LockedIfTrue -> (
              match ret_opt with
              | Some ret_access_path
               -> let attribute_map =
                    AttributeMapDomain.add_attribute (ret_access_path, []) (Choice Choice.LockHeld)
                      astate.attribute_map
                  in
                  {astate with attribute_map}
              | None
               -> failwithf "Procedure %a specified as returning boolean, but returns nothing"
                    Typ.Procname.pp callee_pname )
            | NoEffect ->
              match get_summary pdesc callee_pname actuals loc tenv with
              | Some
                  ( callee_thumbs_up
                  , callee_threads
                  , callee_locks
                  , callee_accesses
                  , return_ownership
                  , return_attributes
                  , escapee_formals )
               -> let update_caller_accesses pre callee_accesses caller_accesses =
                    let combined_accesses =
                      PathDomain.with_callsite callee_accesses (CallSite.make callee_pname loc)
                      |> PathDomain.join (AccessDomain.get_accesses pre caller_accesses)
                    in
                    AccessDomain.add pre combined_accesses caller_accesses
                  in
                  let thumbs_up = callee_thumbs_up && astate.thumbs_up in
                  let locks = callee_locks || astate.locks in
                  let threads = callee_threads || astate.threads in
                  let unprotected = is_unprotected locks threads pdesc in
                  (* add [ownership_accesses] to the [accesses_acc] with a protected pre if
                       [exp] is owned, and an appropriate unprotected pre otherwise *)
                  let add_ownership_access ownership_accesses actual_exp accesses_acc =
                    match actual_exp with
                    | HilExp.Constant _
                     -> (* the actual is a constant, so it's owned in the caller. *)
                        accesses_acc
                    | HilExp.AccessPath actual_access_path
                     -> if OwnershipDomain.is_owned actual_access_path astate.ownership then
                          (* the actual passed to the current callee is owned. drop all the
                             conditional accesses for that actual, since they're all safe *)
                          accesses_acc
                        else
                          let pre =
                            if unprotected then
                              let base = fst actual_access_path in
                              match OwnershipDomain.get_owned (base, []) astate.ownership with
                              | OwnershipAbstractValue.OwnedIf formal_indexes
                               -> (* the actual passed to the current callee is rooted in a
                                     formal *)
                                  AccessPrecondition.Unprotected formal_indexes
                              | OwnershipAbstractValue.Unowned | OwnershipAbstractValue.Owned ->
                                match
                                  OwnershipDomain.get_owned actual_access_path astate.ownership
                                with
                                | OwnershipAbstractValue.OwnedIf formal_indexes
                                 -> (* access path conditionally owned if [formal_indexex] are
                                           owned *)
                                    AccessPrecondition.Unprotected formal_indexes
                                | OwnershipAbstractValue.Owned
                                 -> assert false
                                | OwnershipAbstractValue.Unowned
                                 -> (* access path not rooted in a formal and not conditionally
                                       owned *)
                                    AccessPrecondition.TotallyUnprotected
                            else
                              (* access protected by held lock *)
                              AccessPrecondition.Protected (make_excluder true threads)
                          in
                          update_caller_accesses pre ownership_accesses accesses_acc
                    | _
                     -> (* couldn't find access path, don't know if it's owned *)
                        update_caller_accesses AccessPrecondition.TotallyUnprotected
                          ownership_accesses accesses_acc
                  in
                  let accesses =
                    let update_accesses pre callee_accesses accesses_acc =
                      match pre with
                      | AccessPrecondition.Protected _
                       -> update_caller_accesses pre callee_accesses accesses_acc
                      | AccessPrecondition.TotallyUnprotected
                       -> let pre' =
                            if unprotected then pre
                            else AccessPrecondition.Protected (make_excluder true threads)
                          in
                          update_caller_accesses pre' callee_accesses accesses_acc
                      | AccessPrecondition.Unprotected formal_indexes
                       -> IntSet.fold
                            (fun index acc ->
                              add_ownership_access callee_accesses (List.nth_exn actuals index) acc)
                            formal_indexes accesses_acc
                    in
                    AccessDomain.fold update_accesses callee_accesses astate.accesses
                  in
                  let ownership, attribute_map =
                    propagate_return ret_opt return_ownership return_attributes actuals astate
                  in
                  let escapees =
                    add_escapees_from_call actuals escapee_formals astate.ownership astate.escapees
                  in
                  {thumbs_up; locks; threads; accesses; ownership; attribute_map; escapees}
              | None
               -> (* TODO: assume actuals escape, should we? *)
                  let new_escapees =
                    add_escapees_from_exp_list actuals astate.ownership astate.escapees
                  in
                  let astate = {astate with escapees= new_escapees} in
                  let should_assume_returns_ownership (call_flags: CallFlags.t) actuals =
                    (* assume non-interface methods with no summary and no parameters return
                         ownership *)
                    not call_flags.cf_interface && List.is_empty actuals
                  in
                  if is_box callee_pname then
                    match (ret_opt, actuals) with
                    | Some ret, (HilExp.AccessPath actual_ap) :: _
                      when AttributeMapDomain.has_attribute actual_ap Functional
                             astate.attribute_map
                     -> (* TODO: check for constants, which are functional? *)
                        let attribute_map =
                          AttributeMapDomain.add_attribute (ret, []) Functional
                            astate.attribute_map
                        in
                        {astate with attribute_map}
                    | _
                     -> astate
                  else if should_assume_returns_ownership call_flags actuals then
                    match ret_opt with
                    | Some ret
                     -> let ownership =
                          OwnershipDomain.add (ret, []) OwnershipAbstractValue.owned
                            astate.ownership
                        in
                        {astate with ownership}
                    | None
                     -> astate
                  else astate
        in
        match ret_opt with
        | Some ret
         -> let add_if_annotated predicate attribute attribute_map =
              if PatternMatch.override_exists predicate tenv callee_pname then
                AttributeMapDomain.add_attribute (ret, []) attribute attribute_map
              else attribute_map
            in
            let attribute_map =
              add_if_annotated is_functional Functional astate_callee.attribute_map
            in
            let ownership =
              if PatternMatch.override_exists
                   (has_return_annot Annotations.ia_is_returns_ownership) tenv callee_pname
              then
                OwnershipDomain.add (ret, []) OwnershipAbstractValue.owned astate_callee.ownership
              else astate_callee.ownership
            in
            {astate_callee with ownership; attribute_map}
        | _
         -> astate_callee )
    | Assign (lhs_access_path, rhs_exp, loc)
     -> let rhs_accesses =
          add_access rhs_exp loc ~is_write_access:false astate.accesses astate.locks astate.threads
            astate.ownership proc_data
        in
        let rhs_access_paths = HilExp.get_access_paths rhs_exp in
        let is_functional =
          not (List.is_empty rhs_access_paths)
          && List.for_all
               ~f:(fun access_path ->
                 AttributeMapDomain.has_attribute access_path Functional astate.attribute_map)
               rhs_access_paths
          &&
          match AccessPath.get_typ lhs_access_path tenv with
          | Some {Typ.desc= Typ.Tint ILong | Tfloat FDouble}
           -> (* writes to longs and doubles are not guaranteed to be atomic in Java
                 (http://docs.oracle.com/javase/specs/jls/se7/html/jls-17.html#jls-17.7), so there
                 can be a race even if the RHS is functional *)
              false
          | _
           -> true
        in
        let accesses =
          if is_functional then
            (* we want to forget about writes to @Functional fields altogether, otherwise we'll
               report spurious read/write races *)
            rhs_accesses
          else
            add_access (AccessPath lhs_access_path) loc ~is_write_access:true rhs_accesses
              astate.locks astate.threads astate.ownership proc_data
        in
        let ownership = propagate_ownership lhs_access_path rhs_exp astate.ownership in
        let attribute_map = propagate_attributes lhs_access_path rhs_exp astate.attribute_map in
        (* assigning to the return variable leads to no new escapees *)
        let escapees =
          if fst lhs_access_path |> fst |> Var.is_return then astate.escapees
          else add_escapees_from_exp rhs_exp astate.ownership astate.escapees
        in
        {astate with accesses; ownership; attribute_map; escapees}
    | Assume (assume_exp, _, _, loc)
     -> let rec eval_binop op var e1 e2 =
          match (eval_bexp var e1, eval_bexp var e2) with
          | Some b1, Some b2
           -> Some (op b1 b2)
          | _
           -> None
        (* return Some bool_value if the given boolean expression evaluates to bool_value when
           [var] is set to true. return None if it has free variables that stop us from
           evaluating it *)
        and eval_bexp var = function
          | HilExp.AccessPath ap when AccessPath.equal ap var
           -> Some true
          | HilExp.Constant c
           -> Some (not (Const.iszero_int_float c))
          | HilExp.UnaryOperator (Unop.LNot, e, _)
           -> let b_opt = eval_bexp var e in
              Option.map ~f:not b_opt
          | HilExp.BinaryOperator (Binop.LAnd, e1, e2)
           -> eval_binop ( && ) var e1 e2
          | HilExp.BinaryOperator (Binop.LOr, e1, e2)
           -> eval_binop ( || ) var e1 e2
          | HilExp.BinaryOperator (Binop.Eq, e1, e2)
           -> eval_binop Bool.equal var e1 e2
          | HilExp.BinaryOperator (Binop.Ne, e1, e2)
           -> eval_binop ( <> ) var e1 e2
          | _
           -> (* non-boolean expression; can't evaluate it *)
              None
        in
        let add_choice bool_value acc = function
          | Choice.LockHeld
           -> let locks = bool_value in
              {acc with locks}
          | Choice.OnMainThread
           -> let threads = bool_value in
              {acc with threads}
        in
        let accesses =
          add_access assume_exp loc ~is_write_access:false astate.accesses astate.locks
            astate.threads astate.ownership proc_data
        in
        let astate' =
          match HilExp.get_access_paths assume_exp with
          | [access_path]
           -> (
              let choices = AttributeMapDomain.get_choices access_path astate.attribute_map in
              match eval_bexp access_path assume_exp with
              | Some bool_value
               -> (* prune (prune_exp) can only evaluate to true if the choice is [bool_value].
                       add the constraint that the the choice must be [bool_value] to the state *)
                  List.fold ~f:(add_choice bool_value) ~init:astate choices
              | None
               -> astate )
          | _
           -> astate
        in
        {astate' with accesses}
    | Call (_, Indirect _, _, _, _) ->
      match Procdesc.get_proc_name pdesc with
      | Typ.Procname.Java _
       -> failwithf "Unexpected indirect call instruction %a" HilInstr.pp instr
      | _
       -> astate
end

module Analyzer =
  AbstractInterpreter.Make (ProcCfg.Normal) (LowerHil.MakeDefault (TransferFunctions))

(* similarly, we assume that immutable classes safely encapsulate their state *)
let is_immutable_collection_class class_name tenv =
  let immutable_collections =
    [ "com.google.common.collect.ImmutableCollection"
    ; "com.google.common.collect.ImmutableMap"
    ; "com.google.common.collect.ImmutableTable" ]
  in
  PatternMatch.supertype_exists tenv
    (fun typename _ -> List.mem ~equal:String.equal immutable_collections (Typ.Name.name typename))
    class_name

let is_call_to_immutable_collection_method tenv = function
  | Typ.Procname.Java java_pname
   -> is_immutable_collection_class (Typ.Procname.java_get_class_type_name java_pname) tenv
  | _
   -> false

(* Methods in @ThreadConfined classes and methods annotated with @ThreadConfied are assumed to all
   run on the same thread. For the moment we won't warn on accesses resulting from use of such
   methods at all. In future we should account for races between these methods and methods from
   completely different classes that don't necessarily run on the same thread as the confined
   object. *)
let is_thread_confined_method tenv pdesc =
  Annotations.pdesc_return_annot_ends_with pdesc Annotations.thread_confined
  || PatternMatch.check_current_class_attributes Annotations.ia_is_thread_confined tenv
       (Procdesc.get_proc_name pdesc)

(* we don't want to warn on methods that run on the UI thread because they should always be
   single-threaded *)
let runs_on_ui_thread proc_desc =
  (* assume that methods annotated with @UiThread, @OnEvent, @OnBind, @OnMount, @OnUnbind,
     @OnUnmount always run on the UI thread *)
  Annotations.pdesc_has_return_annot proc_desc (fun annot ->
      Annotations.ia_is_ui_thread annot || Annotations.ia_is_on_bind annot
      || Annotations.ia_is_on_event annot || Annotations.ia_is_on_mount annot
      || Annotations.ia_is_on_unbind annot || Annotations.ia_is_on_unmount annot )

let threadsafe_annotations =
  Annotations.thread_safe :: ThreadSafetyConfig.AnnotationAliases.of_json Config.threadsafe_aliases

(* returns true if the annotation is @ThreadSafe, @ThreadSafe(enableChecks = true), or is defined
   as an alias of @ThreadSafe in a .inferconfig file. *)
let is_thread_safe item_annot =
  let f ((annot: Annot.t), _) =
    List.exists
      ~f:(fun annot_string ->
        Annotations.annot_ends_with annot annot_string
        || String.equal annot.class_name annot_string)
      threadsafe_annotations
    && match annot.Annot.parameters with ["false"] -> false | _ -> true
  in
  List.exists ~f item_annot

(* returns true if the annotation is @ThreadSafe(enableChecks = false) *)
let is_assumed_thread_safe item_annot =
  let f (annot, _) =
    Annotations.annot_ends_with annot Annotations.thread_safe
    && match annot.Annot.parameters with ["false"] -> true | _ -> false
  in
  List.exists ~f item_annot

let pdesc_is_assumed_thread_safe pdesc tenv =
  is_assumed_thread_safe (Annotations.pdesc_get_return_annot pdesc)
  || PatternMatch.check_current_class_attributes is_assumed_thread_safe tenv
       (Procdesc.get_proc_name pdesc)

(* return true if we should compute a summary for the procedure. if this returns false, we won't
   analyze the procedure or report any warnings on it *)
(* note: in the future, we will want to analyze the procedures in all of these cases in order to
   find more bugs. this is just a temporary measure to avoid obvious false positives *)
let should_analyze_proc pdesc tenv =
  let pn = Procdesc.get_proc_name pdesc in
  not (Typ.Procname.is_class_initializer pn) && not (FbThreadSafety.is_logging_method pn)
  && not (is_call_to_immutable_collection_method tenv pn)
  && not (pdesc_is_assumed_thread_safe pdesc tenv)

let is_thread_safe_method pdesc tenv =
  PatternMatch.override_exists
    (fun pn ->
      Annotations.pname_has_return_annot pn ~attrs_of_pname:Specs.proc_resolve_attributes
        is_thread_safe)
    tenv (Procdesc.get_proc_name pdesc)

let empty_post =
  let initial_thumbs_up = true
  and initial_known_on_ui_thread = false
  and has_lock = false
  and return_ownership = ThreadSafetyDomain.OwnershipAbstractValue.unowned
  and return_attrs = ThreadSafetyDomain.AttributeSetDomain.empty
  and escapee_formals = ThreadSafetyDomain.FormalsDomain.empty in
  ( initial_thumbs_up
  , initial_known_on_ui_thread
  , has_lock
  , ThreadSafetyDomain.AccessDomain.empty
  , return_ownership
  , return_attrs
  , escapee_formals )

let analyze_procedure {Callbacks.proc_desc; tenv; summary} =
  let is_initializer tenv proc_name =
    Typ.Procname.is_constructor proc_name || FbThreadSafety.is_custom_init tenv proc_name
  in
  let open ThreadSafetyDomain in
  (* convert the abstract state to a summary by dropping the id map *)
  if should_analyze_proc proc_desc tenv then (
    if not (Procdesc.did_preanalysis proc_desc) then Preanal.do_liveness proc_desc tenv ;
    let formal_map = FormalMap.make proc_desc in
    let proc_data = ProcData.make_default proc_desc tenv in
    let initial =
      let threads = runs_on_ui_thread proc_desc || is_thread_confined_method tenv proc_desc in
      if is_initializer tenv (Procdesc.get_proc_name proc_desc) then
        let add_owned_formal acc formal_index =
          match FormalMap.get_formal_base formal_index formal_map with
          | Some base
           -> OwnershipDomain.add (base, []) OwnershipAbstractValue.owned acc
          | None
           -> acc
        in
        let owned_formals =
          (* if a constructer is called via DI, all of its formals will be freshly allocated
               and therefore owned. we assume that constructors annotated with @Inject will only
               be called via DI or using fresh parameters. *)
          if Annotations.pdesc_has_return_annot proc_desc Annotations.ia_is_inject then
            List.mapi ~f:(fun i _ -> i) (Procdesc.get_formals proc_desc)
          else [0]
          (* express that the constructor owns [this] *)
        in
        let ownership = List.fold ~f:add_owned_formal owned_formals ~init:OwnershipDomain.empty in
        ({ThreadSafetyDomain.empty with ownership; threads}, IdAccessPathMapDomain.empty)
      else
        (* add Owned(formal_index) predicates for each formal to indicate that each one is owned if
           it is owned in the caller *)
        let add_owned_formal acc (formal, formal_index) =
          OwnershipDomain.add (formal, []) (OwnershipAbstractValue.make_owned_if formal_index) acc
        in
        let ownership =
          List.fold ~f:add_owned_formal (FormalMap.get_formals_indexes formal_map)
            ~init:OwnershipDomain.empty
        in
        ({ThreadSafetyDomain.empty with ownership; threads}, IdAccessPathMapDomain.empty)
    in
    match Analyzer.compute_post proc_data ~initial ~debug:false with
    | Some ({thumbs_up; threads; locks; accesses; ownership; attribute_map; escapees}, _)
     -> let return_var_ap =
          AccessPath.of_pvar
            (Pvar.get_ret_pvar (Procdesc.get_proc_name proc_desc))
            (Procdesc.get_ret_type proc_desc)
        in
        let return_ownership = OwnershipDomain.get_owned return_var_ap ownership in
        let return_attributes =
          try AttributeMapDomain.find return_var_ap attribute_map
          with Not_found -> AttributeSetDomain.empty
        in
        let escapee_formals = FormalsDomain.of_escapees escapees in
        let post =
          ( thumbs_up
          , threads
          , locks
          , accesses
          , return_ownership
          , return_attributes
          , escapee_formals )
        in
        Summary.update_summary post summary
    | None
     -> summary )
  else Summary.update_summary empty_post summary

module AccessListMap = Caml.Map.Make (struct
  type t = ThreadSafetyDomain.Access.t

  (* TODO -- keep this compare to satisfy the order of tests, consider using Raw.compare *)
  let compare access1 access2 =
    let open ThreadSafetyDomain in
    match (access1, access2) with
    | ( (Access.Read access_path1 | Write access_path1)
      , (Access.Read access_path2 | Write access_path2) )
     -> List.compare AccessPath.compare_access (snd access_path1) (snd access_path2)
    | _
     -> Access.compare access1 access2
end)

let get_current_class_and_threadsafe_superclasses tenv pname =
  match pname with
  | Typ.Procname.Java java_pname
   -> let current_class = Typ.Procname.java_get_class_type_name java_pname in
      let thread_safe_annotated_classes =
        PatternMatch.find_superclasses_with_attributes is_thread_safe tenv current_class
      in
      Some (current_class, thread_safe_annotated_classes)
  | _
   -> None

(*shouldn't happen*)

(** The addendum message says that a superclass is marked @ThreadSafe,
    when the current class is not so marked*)
let calculate_addendum_message tenv pname =
  match get_current_class_and_threadsafe_superclasses tenv pname with
  | Some (current_class, thread_safe_annotated_classes)
   -> if not (List.mem ~equal:Typ.Name.equal thread_safe_annotated_classes current_class) then
        match thread_safe_annotated_classes with
        | hd :: _
         -> F.asprintf "@\n Note: Superclass %a is marked %a." (MF.wrap_monospaced Typ.Name.pp) hd
              MF.pp_monospaced "@ThreadSafe"
        | []
         -> ""
      else ""
  | _
   -> ""

let filter_by_access access_filter trace =
  let open ThreadSafetyDomain in
  PathDomain.Sinks.filter access_filter (PathDomain.sinks trace) |> PathDomain.update_sinks trace

let get_all_accesses_with_pre pre_filter access_filter accesses =
  let open ThreadSafetyDomain in
  AccessDomain.fold
    (fun pre trace acc ->
      if pre_filter pre then PathDomain.join (filter_by_access access_filter trace) acc else acc)
    accesses PathDomain.empty

let get_all_accesses = get_all_accesses_with_pre (fun _ -> true)

let pp_container_access fmt (access_path, access_pname) =
  F.fprintf fmt "container %a via call to %s" (MF.wrap_monospaced AccessPath.pp) access_path
    (MF.monospaced_to_string (Typ.Procname.get_method access_pname))

let pp_access fmt sink =
  match ThreadSafetyDomain.PathDomain.Sink.kind sink with
  | Read access_path | Write access_path
   -> F.fprintf fmt "%a" (MF.wrap_monospaced AccessPath.pp_access_list) (snd access_path)
  | ContainerRead (access_path, access_pname) | ContainerWrite (access_path, access_pname)
   -> pp_container_access fmt (access_path, access_pname)
  | InterfaceCall _ as access
   -> F.fprintf fmt "%a" ThreadSafetyDomain.Access.pp access

let desc_of_sink sink =
  let sink_pname = CallSite.pname (ThreadSafetyDomain.PathDomain.Sink.call_site sink) in
  match ThreadSafetyDomain.PathDomain.Sink.kind sink with
  | Read _ | Write _
   -> if Typ.Procname.equal sink_pname Typ.Procname.empty_block then
        F.asprintf "access to %a" pp_access sink
      else F.asprintf "call to %a" Typ.Procname.pp sink_pname
  | ContainerRead (access_path, access_pname)
   -> if Typ.Procname.equal sink_pname access_pname then
        F.asprintf "Read of %a" pp_container_access (access_path, access_pname)
      else F.asprintf "call to %a" Typ.Procname.pp sink_pname
  | ContainerWrite (access_path, access_pname)
   -> if Typ.Procname.equal sink_pname access_pname then
        F.asprintf "Write to %a" pp_container_access (access_path, access_pname)
      else F.asprintf "call to %a" Typ.Procname.pp sink_pname
  | InterfaceCall _ as access
   -> if Typ.Procname.equal sink_pname Typ.Procname.empty_block then
        F.asprintf "%a1" ThreadSafetyDomain.Access.pp access
      else F.asprintf "call to %a" Typ.Procname.pp sink_pname

let trace_of_pname orig_sink orig_pdesc callee_pname =
  let open ThreadSafetyDomain in
  let orig_access = PathDomain.Sink.kind orig_sink in
  match Summary.read_summary orig_pdesc callee_pname with
  | Some (_, _, _, access_map, _, _, _)
   -> get_all_accesses
        (fun access -> Int.equal (Access.compare (PathDomain.Sink.kind access) orig_access) 0)
        access_map
  | _
   -> PathDomain.empty

let make_trace_with_conflicts conflicts original_path pdesc =
  let open ThreadSafetyDomain in
  let loc_trace_of_path path = PathDomain.to_sink_loc_trace ~desc_of_sink path in
  let make_trace_for_sink sink =
    let trace_of_pname = trace_of_pname sink pdesc in
    match PathDomain.get_reportable_sink_path sink ~trace_of_pname with
    | Some path
     -> loc_trace_of_path path
    | None
     -> []
  in
  let original_trace = loc_trace_of_path original_path in
  match conflicts with
  | conflict_sink :: _
   -> (* create a trace for one of the conflicts and append it to the trace for the original sink *)
      let conflict_trace = make_trace_for_sink conflict_sink in
      let get_start_loc = function head :: _ -> head.Errlog.lt_loc | [] -> Location.dummy in
      let first_trace_spacer =
        Errlog.make_trace_element 0 (get_start_loc original_trace) "<Beginning of read trace>" []
      in
      let second_trace_spacer =
        Errlog.make_trace_element 0 (get_start_loc conflict_trace) "<Beginning of write trace>" []
      in
      first_trace_spacer :: original_trace @ second_trace_spacer :: conflict_trace
  | []
   -> original_trace

let report_thread_safety_violation tenv pdesc ~make_description ~conflicts access =
  let open ThreadSafetyDomain in
  let pname = Procdesc.get_proc_name pdesc in
  let report_one_path (_, sinks as path) =
    let initial_sink, _ = List.last_exn sinks in
    let final_sink, _ = List.hd_exn sinks in
    let initial_sink_site = PathDomain.Sink.call_site initial_sink in
    let final_sink_site = PathDomain.Sink.call_site final_sink in
    let loc = CallSite.loc initial_sink_site in
    let ltr = make_trace_with_conflicts conflicts path pdesc in
    let msg = IssueType.thread_safety_violation.unique_id in
    let description = make_description tenv pname final_sink_site initial_sink_site final_sink in
    let exn = Exceptions.Checkers (msg, Localise.verbatim_desc description) in
    Reporting.log_error_deprecated ~store_summary:true pname ~loc ~ltr exn
  in
  let trace_of_pname = trace_of_pname access pdesc in
  Option.iter ~f:report_one_path (PathDomain.get_reportable_sink_path access ~trace_of_pname)

let _report_unannotated_interface_violation tenv pdesc access reported_pname =
  match reported_pname with
  | Typ.Procname.Java java_pname
   -> let class_name = Typ.Procname.java_get_class_name java_pname in
      let make_description _ _ _ _ _ =
        F.asprintf
          "Unprotected call to method of un-annotated interface %s. Consider annotating the class with %a or adding a lock"
          class_name MF.pp_monospaced "@ThreadSafe"
      in
      report_thread_safety_violation tenv pdesc ~make_description ~conflicts:[] access
  | _
   -> (* skip reporting on C++ *)
      ()

let pp_procname_short fmt = function
  | Typ.Procname.Java java
   -> F.fprintf fmt "%s.%s" (Typ.Procname.java_get_class_name java)
        (Typ.Procname.java_get_method java)
  | pname
   -> Typ.Procname.pp fmt pname

let make_unprotected_write_description tenv pname final_sink_site initial_sink_site final_sink =
  Format.asprintf "Unprotected write. Non-private method %a%s %s %a outside of synchronization.%s"
    (MF.wrap_monospaced pp_procname_short) pname
    (if CallSite.equal final_sink_site initial_sink_site then "" else " indirectly")
    ( if ThreadSafetyDomain.TraceElem.is_container_write final_sink then "mutates"
    else "writes to field" )
    pp_access final_sink (calculate_addendum_message tenv pname)

let make_read_write_race_description conflicts tenv pname final_sink_site initial_sink_site
    final_sink =
  let race_with_main_thread = List.exists ~f:(fun (_, _, threaded, _, _) -> threaded) conflicts in
  let conflicting_proc_names =
    List.map ~f:(fun (_, _, _, _, pdesc) -> Procdesc.get_proc_name pdesc) conflicts
    |> Typ.Procname.Set.of_list
  in
  let pp_conflicts fmt conflicts =
    if Int.equal (Typ.Procname.Set.cardinal conflicts) 1 then
      Typ.Procname.pp fmt (Typ.Procname.Set.choose conflicts)
    else Typ.Procname.Set.pp fmt conflicts
  in
  let conflicts_description =
    Format.asprintf "Potentially races with writes in method%s %a. %s"
      (if Typ.Procname.Set.cardinal conflicting_proc_names > 1 then "s" else "")
      (MF.wrap_monospaced pp_conflicts) conflicting_proc_names
      ( if race_with_main_thread then
          "\n Note: some of these write conflicts are confined to the UI or another thread, but the current method is not specified to be. Consider adding synchronization or a @ThreadConfined annotation to the current method."
      else "" )
  in
  Format.asprintf "Read/Write race. Non-private method %a%s reads from %a. %s %s"
    (MF.wrap_monospaced pp_procname_short) pname
    (if CallSite.equal final_sink_site initial_sink_site then "" else " indirectly")
    pp_access final_sink conflicts_description (calculate_addendum_message tenv pname)

(** type for remembering what we have already reported to avoid duplicates. our policy is to report
    each kind of access (read/write) to the same field reachable from the same procedure only once.
    in addition, if a call to a procedure (transitively) accesses multiple fields, we will only
    report one of each kind of access *)
type reported =
  { reported_sites: CallSite.Set.t
  ; reported_writes: Typ.Procname.Set.t
  ; reported_reads: Typ.Procname.Set.t }

let empty_reported =
  let reported_sites = CallSite.Set.empty in
  let reported_writes = Typ.Procname.Set.empty in
  let reported_reads = Typ.Procname.Set.empty in
  {reported_sites; reported_reads; reported_writes}

(* return true if procedure is at an abstraction boundary or reporting has been explicitly
   requested via @ThreadSafe *)
let should_report_on_proc proc_desc tenv =
  let proc_name = Procdesc.get_proc_name proc_desc in
  is_thread_safe_method proc_desc tenv
  || not (Typ.Procname.java_is_autogen_method proc_name)
     && Procdesc.get_access proc_desc <> PredSymb.Private
     && not (Annotations.pdesc_return_annot_ends_with proc_desc Annotations.visibleForTesting)

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
  let is_duplicate_report access pname {reported_sites; reported_writes; reported_reads} =
    CallSite.Set.mem (TraceElem.call_site access) reported_sites
    ||
    match TraceElem.kind access with
    | Access.Write _ | Access.ContainerWrite _
     -> Typ.Procname.Set.mem pname reported_writes
    | Access.Read _ | Access.ContainerRead _
     -> Typ.Procname.Set.mem pname reported_reads
    | Access.InterfaceCall _
     -> false
  in
  let update_reported access pname reported =
    let reported_sites = CallSite.Set.add (TraceElem.call_site access) reported.reported_sites in
    match TraceElem.kind access with
    | Access.Write _ | Access.ContainerWrite _
     -> let reported_writes = Typ.Procname.Set.add pname reported.reported_writes in
        {reported with reported_writes; reported_sites}
    | Access.Read _ | Access.ContainerRead _
     -> let reported_reads = Typ.Procname.Set.add pname reported.reported_reads in
        {reported with reported_reads; reported_sites}
    | Access.InterfaceCall _
     -> reported
  in
  let report_unsafe_access (access, pre, threaded, tenv, pdesc) accesses reported_acc =
    let pname = Procdesc.get_proc_name pdesc in
    if is_duplicate_report access pname reported_acc then reported_acc
    else
      match (TraceElem.kind access, pre) with
      | ( Access.InterfaceCall _
        , (AccessPrecondition.Unprotected _ | AccessPrecondition.TotallyUnprotected) )
       -> (* un-annotated interface call + no lock. warn *)
          update_reported access pname reported_acc
      | Access.InterfaceCall _, AccessPrecondition.Protected _
       -> (* un-annotated interface call, but it's protected by a lock/thread. don't report *)
          reported_acc
      | ( (Access.Write _ | ContainerWrite _)
        , (AccessPrecondition.Unprotected _ | AccessPrecondition.TotallyUnprotected) ) -> (
        match Procdesc.get_proc_name pdesc with
        | Java _
         -> if threaded then reported_acc
            else (
              (* unprotected write. warn. *)
              report_thread_safety_violation tenv pdesc
                ~make_description:make_unprotected_write_description ~conflicts:[] access ;
              update_reported access pname reported_acc )
        | _
         -> (* Do not report unprotected writes for ObjC_Cpp *)
            reported_acc )
      | (Access.Write _ | ContainerWrite _), AccessPrecondition.Protected _
       -> (* protected write, do nothing *)
          reported_acc
      | ( (Access.Read _ | ContainerRead _)
        , (AccessPrecondition.Unprotected _ | AccessPrecondition.TotallyUnprotected) )
       -> (* unprotected read. report all writes as conflicts for java *)
          (* for c++ filter out unprotected writes *)
          let is_cpp_protected_write pre =
            match pre with
            | AccessPrecondition.Unprotected _ | TotallyUnprotected
             -> Typ.Procname.is_java pname
            | AccessPrecondition.Protected _
             -> true
          in
          let all_writes =
            List.filter
              ~f:(fun (other_access, pre, other_threaded, _, _) ->
                TraceElem.is_write other_access && not (threaded && other_threaded)
                && is_cpp_protected_write pre)
              accesses
          in
          if List.is_empty all_writes then reported_acc
          else (
            report_thread_safety_violation tenv pdesc
              ~make_description:(make_read_write_race_description all_writes)
              ~conflicts:(List.map ~f:(fun (access, _, _, _, _) -> access) all_writes)
              access ;
            update_reported access pname reported_acc )
      | (Access.Read _ | ContainerRead _), AccessPrecondition.Protected excl
       -> (* protected read.
             report unprotected writes and opposite protected writes as conflicts
             Thread and Lock are opposites of one another, and
             Both has no opposite*)
          let is_opposite = function
            | Excluder.Lock, Excluder.Thread
             -> true
            | Excluder.Thread, Excluder.Lock
             -> true
            | _, _
             -> false
          in
          let conflicting_writes =
            List.filter
              ~f:(fun (access, pre, _, _, _) ->
                match pre with
                | AccessPrecondition.Unprotected _
                 -> TraceElem.is_write access
                | AccessPrecondition.Protected other_excl when is_opposite (excl, other_excl)
                 -> TraceElem.is_write access
                | _
                 -> false)
              accesses
          in
          if List.is_empty conflicting_writes then reported_acc
          else (
            (* protected read with conflicting unprotected write(s). warn. *)
            report_thread_safety_violation tenv pdesc
              ~make_description:(make_read_write_race_description conflicting_writes)
              ~conflicts:(List.map ~f:(fun (access, _, _, _, _) -> access) conflicting_writes)
              access ;
            update_reported access pname reported_acc )
  in
  AccessListMap.fold
    (fun _ grouped_accesses reported_acc ->
      (* reset the reported reads and writes for each memory location *)
      let reported =
        { reported_acc with
          reported_writes= Typ.Procname.Set.empty; reported_reads= Typ.Procname.Set.empty }
      in
      let accessed_by_threadsafe_method =
        List.exists
          ~f:(fun (_, _, _, tenv, pdesc) -> is_thread_safe_method pdesc tenv)
          grouped_accesses
      in
      let class_has_mutex_member objc_cpp tenv =
        let class_name = Typ.Procname.objc_cpp_get_class_type_name objc_cpp in
        let matcher = QualifiedCppName.Match.of_fuzzy_qual_names ["std::mutex"] in
        Option.exists (Tenv.lookup tenv class_name) ~f:(fun class_str ->
            (* check if the class contains a member of type std::mutex *)
            List.exists class_str.Typ.Struct.fields ~f:(fun (_, ft, _) ->
                Option.exists (Typ.name ft) ~f:(fun name ->
                    QualifiedCppName.Match.match_qualifiers matcher (Typ.Name.qual_name name) ) ) )
      in
      let should_report pdesc tenv =
        match Procdesc.get_proc_name pdesc with
        | Java _ as pname
         -> (* report if
                - the method/class of the access is thread-safe
                  (or an override or superclass is), or
                - any access is in a field marked thread-safe (or an override) *)
            ( accessed_by_threadsafe_method
            ||
            let is_class_threadsafe =
              not
                (let current_class_marked_not_threadsafe =
                   PatternMatch.check_current_class_attributes Annotations.ia_is_not_thread_safe
                     tenv pname
                 in
                 current_class_marked_not_threadsafe)
              &&
              let current_class_or_super_marked_threadsafe =
                match get_current_class_and_threadsafe_superclasses tenv pname with
                | Some (_, thread_safe_annotated_classes)
                 -> not (List.is_empty thread_safe_annotated_classes)
                | _
                 -> false
              in
              current_class_or_super_marked_threadsafe
            in
            is_class_threadsafe )
            && should_report_on_proc pdesc tenv
        | ObjC_Cpp objc_cpp
         -> (* do not report if a procedure is private  *)
            Procdesc.get_access pdesc <> PredSymb.Private
            && (* report if the class has a mutex member  *)
               class_has_mutex_member objc_cpp tenv
        | _
         -> false
      in
      let reportable_accesses =
        List.filter ~f:(fun (_, _, _, tenv, pdesc) -> should_report pdesc tenv) grouped_accesses
      in
      List.fold
        ~f:(fun acc access -> report_unsafe_access access reportable_accesses acc)
        reportable_accesses ~init:reported)
    aggregated_access_map empty_reported
  |> ignore

let sound = false

let may_alias_container tenv p1 p2 =
  if sound then
    (* this is much too noisy: we'll warn that accesses to *any* Map can race with accesses to any
       other Map, etc. Instead, do something simple and unsound: just assume that two accesses can
       be to the same container if they are to the same access path *)
    match (AccessPath.get_typ p1 tenv, AccessPath.get_typ p2 tenv) with
    | Some {desc= Tptr ({desc= Tstruct tn1}, _)}, Some {desc= Tptr ({desc= Tstruct tn2}, _)}
     -> PatternMatch.is_subtype tenv tn1 tn2 || PatternMatch.is_subtype tenv tn2 tn1
    | _
     -> true
  else
    (* unsound, but effective: report that the containers alias if their access paths are
       syntactically identical *)
    match (fst p1, fst p2) with
    | (Var.ProgramVar pvar1, typ1), (Var.ProgramVar pvar2, typ2)
      when Pvar.is_this pvar1 && Pvar.is_this pvar2
           && ( Prover.Subtyping_check.check_subtype tenv typ1 typ2
              || Prover.Subtyping_check.check_subtype tenv typ2 typ1 )
     -> (* the `this` used in C.foo and C.bar will compare unequal if we're not careful `this` is
           represented as a local pvar, and a local pvar contains its parent procedure name. Count
           the `this`'s as equal if their types are compatible *)
        AccessPath.equal_access_list (snd p1) (snd p2)
    | _
     -> AccessPath.equal p1 p2

(* equivalence relation computing whether two access paths may refer to the
   same heap location. *)
let may_alias tenv p1 p2 =
  let open Typ in
  let open AccessPath in
  phys_equal p1 p2
  ||
  match (List.last_exn (snd p1), List.last_exn (snd p2)) with
  | FieldAccess _, ArrayAccess _ | ArrayAccess _, FieldAccess _
   -> false
  (* fields in Java contain the class name /declaring/ them
     thus two fields can be aliases *iff* they are equal *)
  | FieldAccess f1, FieldAccess f2
   -> Typ.Fieldname.equal f1 f2
  (* if arrays of objects that have an inheritance rel then they can alias *)
  | ( ArrayAccess ({desc= Tptr ({desc= Tstruct tn1}, _)}, _)
    , ArrayAccess ({desc= Tptr ({desc= Tstruct tn2}, _)}, _) )
   -> if sound then PatternMatch.is_subtype tenv tn1 tn2 || PatternMatch.is_subtype tenv tn2 tn1
      else may_alias_container tenv p1 p2
  (* primitive type arrays can alias if the prim. type is the same *)
  | ArrayAccess (t1, _), ArrayAccess (t2, _)
   -> if sound then equal_desc t1.desc t2.desc else may_alias_container tenv p1 p2

(* take a results table and quotient it by the may_alias relation *)
let quotient_access_map acc_map =
  let rec aux acc m =
    if AccessListMap.is_empty m then acc
    else
      let k, vals = AccessListMap.choose m in
      let _, _, _, tenv, _ =
        List.find_exn vals ~f:(fun (elem, _, _, _, _) ->
            ThreadSafetyDomain.Access.equal (ThreadSafetyDomain.TraceElem.kind elem) k )
      in
      (* assumption: the tenv for k is sufficient for k' too *)
      let k_part, non_k_part =
        AccessListMap.partition
          (fun k' _ ->
            match (k, k') with
            | (Read ap1 | Write ap1), (Read ap2 | Write ap2)
             -> may_alias tenv ap1 ap2
            | ( (ContainerRead (ap1, _) | ContainerWrite (ap1, _))
              , (ContainerRead (ap2, _) | ContainerWrite (ap2, _)) )
             -> may_alias_container tenv ap1 ap2
            | _
             -> ThreadSafetyDomain.Access.equal k k')
          m
      in
      if AccessListMap.is_empty k_part then failwith "may_alias is not reflexive!" ;
      let k_accesses = AccessListMap.fold (fun _ v acc' -> List.append v acc') k_part [] in
      let new_acc = AccessListMap.add k k_accesses acc in
      aux new_acc non_k_part
  in
  aux AccessListMap.empty acc_map

(* decide if we should throw away a path before doing safety analysis
   for now, just check for whether the access is within a switch-map
   that is auto-generated by Java. *)
let should_filter_access access =
  match ThreadSafetyDomain.Access.get_access_path access with
  | Some (_, path)
   -> let check_access_step = function
        | AccessPath.ArrayAccess _
         -> false
        | AccessPath.FieldAccess fld
         -> String.is_substring ~substring:"$SwitchMap" (Typ.Fieldname.to_string fld)
      in
      List.exists path ~f:check_access_step
  | None
   -> false

(* create a map from [abstraction of a memory loc] -> accesses that may touch that memory loc. for
   now, our abstraction is an access path like x.f.g whose concretization is the set of memory cells
   that x.f.g may point to during execution *)
let make_results_table file_env =
  let aggregate_post (_, threaded, _, accesses, _, _, _) tenv pdesc acc =
    let open ThreadSafetyDomain in
    AccessDomain.fold
      (fun pre accesses acc ->
        PathDomain.Sinks.fold
          (fun access acc ->
            let access_kind = TraceElem.kind access in
            if should_filter_access access_kind then acc
            else
              let grouped_accesses =
                try AccessListMap.find access_kind acc
                with Not_found -> []
              in
              AccessListMap.add access_kind
                ((access, pre, threaded, tenv, pdesc) :: grouped_accesses) acc)
          (PathDomain.sinks accesses) acc)
      accesses acc
  in
  let aggregate_posts acc (_, tenv, proc_name, proc_desc) =
    match Summary.read_summary proc_desc proc_name with
    | Some summary
     -> aggregate_post summary tenv proc_desc acc
    | None
     -> acc
  in
  List.fold ~f:aggregate_posts file_env ~init:AccessListMap.empty |> quotient_access_map

(* Gathers results by analyzing all the methods in a file, then post-processes the results to check
   an (approximation of) thread safety *)
let file_analysis _ _ _ file_env = report_unsafe_accesses (make_results_table file_env)
