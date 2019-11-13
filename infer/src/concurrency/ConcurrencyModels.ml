(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

type lock_effect =
  | Lock of HilExp.t list
  | Unlock of HilExp.t list
  | LockedIfTrue of HilExp.t list
  | GuardConstruct of {guard: HilExp.t; lock: HilExp.t; acquire_now: bool}
  | GuardLock of HilExp.t
  | GuardLockedIfTrue of HilExp.t
  | GuardUnlock of HilExp.t
  | GuardDestroy of HilExp.t
  | NoEffect

type thread = BackgroundThread | MainThread | MainThreadIfTrue | UnknownThread

let is_thread_utils_type java_pname =
  let pn = Typ.Procname.Java.get_class_name java_pname in
  String.is_suffix ~suffix:"ThreadUtils" pn || String.is_suffix ~suffix:"ThreadUtil" pn


let is_thread_utils_method method_name_str = function
  | Typ.Procname.Java java_pname ->
      is_thread_utils_type java_pname
      && String.equal (Typ.Procname.Java.get_method java_pname) method_name_str
  | _ ->
      false


let get_thread_assert_effect = function
  | Typ.Procname.Java java_pname when is_thread_utils_type java_pname -> (
    match Typ.Procname.Java.get_method java_pname with
    | "assertMainThread" | "assertOnUiThread" | "checkOnMainThread" | "checkIsOnMainThread" ->
        MainThread
    | "isMainThread" | "isOnMainThread" | "isUiThread" ->
        MainThreadIfTrue
    | "assertOnBackgroundThread"
    | "assertOnNonUiThread"
    | "checkOnNonUiThread"
    | "checkOnWorkerThread" ->
        BackgroundThread
    | _ ->
        UnknownThread )
  | _ ->
      UnknownThread


module Clang : sig
  val get_lock_effect : Typ.Procname.t -> HilExp.t list -> lock_effect

  val lock_types_matcher : QualifiedCppName.Match.quals_matcher

  val is_recursive_lock_type : QualifiedCppName.t -> bool
end = struct
  type lock_model =
    { classname: string
    ; lock: string list
    ; trylock: string list
    ; unlock: string list
    ; recursive: bool }

  let lock_models =
    let def =
      {classname= ""; lock= ["lock"]; trylock= ["try_lock"]; unlock= ["unlock"]; recursive= false}
    in
    let shd =
      { def with
        lock= "lock_shared" :: def.lock
      ; trylock= "try_lock_shared" :: def.trylock
      ; unlock= "unlock_shared" :: def.unlock }
    in
    let rwm =
      { def with
        lock= ["acquireRead"; "acquireWrite"]
      ; trylock= ["attemptRead"; "attemptWrite"]
      ; unlock= ["release"] }
    in
    [ { def with
        classname= "apache::thrift::concurrency::Monitor"
      ; trylock= "timedlock" :: def.trylock }
    ; {def with classname= "apache::thrift::concurrency::Mutex"; trylock= "timedlock" :: def.trylock}
    ; {rwm with classname= "apache::thrift::concurrency::NoStarveReadWriteMutex"}
    ; {rwm with classname= "apache::thrift::concurrency::ReadWriteMutex"}
    ; {shd with classname= "boost::shared_mutex"}
    ; {def with classname= "boost::mutex"}
    ; {def with classname= "folly::MicroSpinLock"}
    ; {shd with classname= "folly::RWSpinLock"}
    ; {shd with classname= "folly::SharedMutex"}
    ; {shd with classname= "folly::SharedMutexImpl"}
    ; {def with classname= "folly::SpinLock"}
    ; {def with classname= "std::mutex"}
    ; {def with classname= "std::recursive_mutex"; recursive= true}
    ; {def with classname= "std::recursive_timed_mutex"; recursive= true}
    ; {shd with classname= "std::shared_mutex"}
    ; {def with classname= "std::timed_mutex"} ]


  let is_recursive_lock_type qname =
    let qname_str = QualifiedCppName.to_qual_string qname in
    match List.find lock_models ~f:(fun mdl -> String.equal qname_str mdl.classname) with
    | None ->
        L.debug Analysis Verbose "is_recursive_lock_type: Could not find lock type %a@."
          QualifiedCppName.pp qname ;
        true
    | Some mdl ->
        mdl.recursive


  let mk_matcher methods =
    let matcher = QualifiedCppName.Match.of_fuzzy_qual_names methods in
    fun pname -> QualifiedCppName.Match.match_qualifiers matcher (Typ.Procname.get_qualifiers pname)


  let is_lock, is_unlock, is_trylock, is_std_lock =
    (* TODO std::try_lock *)
    let mk_model_matcher ~f =
      let lock_methods =
        List.concat_map lock_models ~f:(fun mdl ->
            List.map (f mdl) ~f:(fun mtd -> mdl.classname ^ "::" ^ mtd) )
      in
      mk_matcher lock_methods
    in
    ( mk_model_matcher ~f:(fun mdl -> mdl.lock)
    , mk_model_matcher ~f:(fun mdl -> mdl.unlock)
    , mk_model_matcher ~f:(fun mdl -> mdl.trylock)
    , mk_matcher ["std::lock"] )


  let lock_types_matcher =
    let class_names = List.map lock_models ~f:(fun mdl -> mdl.classname) in
    QualifiedCppName.Match.of_fuzzy_qual_names class_names


  (** C++ guard classes used for scope-based lock management.
    NB we pretend all classes below implement the mutex interface even though only
    [shared_lock] and [unique_lock] do, for simplicity.  The comments summarise which
    methods are implemented. *)
  let guards =
    (* TODO std::scoped_lock *)
    [ (* no lock/unlock *)
      "apache::thrift::concurrency::Guard"
    ; (* no lock/unlock *)
      "apache::thrift::concurrency::RWGuard"
    ; (* only unlock *)
      "folly::SharedMutex::ReadHolder"
    ; (* only unlock *)
      "folly::SharedMutex::WriteHolder"
    ; (* read/write locks under operator() etc *)
      "folly::LockedPtr"
    ; (* no lock/unlock *)
      "folly::SpinLockGuard"
    ; (* no lock/unlock *)
      "std::lock_guard"
    ; (* everything *)
      "std::shared_lock"
    ; (* everything *)
      "std::unique_lock" ]


  let ( get_guard_constructor
      , get_guard_destructor
      , get_guard_lock
      , get_guard_unlock
      , get_guard_trylock ) =
    let get_class_and_qual_name guard =
      let qual_name = QualifiedCppName.of_qual_string guard in
      let class_name, _ = Option.value_exn (QualifiedCppName.extract_last qual_name) in
      (class_name, qual_name)
    in
    let make_with_classname ~f guard =
      let class_name, qual_name = get_class_and_qual_name guard in
      let qual = f class_name in
      let qual_constructor = QualifiedCppName.append_qualifier qual_name ~qual in
      QualifiedCppName.to_qual_string qual_constructor
    in
    let make_lock_unlock ~mthd guard =
      let qual_name = QualifiedCppName.of_qual_string guard in
      let qual_mthd = QualifiedCppName.append_qualifier qual_name ~qual:mthd in
      QualifiedCppName.to_qual_string qual_mthd
    in
    let make_trylock ~mthds guard =
      let qual_name = QualifiedCppName.of_qual_string guard in
      List.map mthds ~f:(fun qual ->
          QualifiedCppName.append_qualifier qual_name ~qual |> QualifiedCppName.to_qual_string )
    in
    ( make_with_classname ~f:(fun class_name -> class_name)
    , make_with_classname ~f:(fun class_name -> "~" ^ class_name)
    , make_lock_unlock ~mthd:"lock"
    , make_lock_unlock ~mthd:"unlock"
    , make_trylock ~mthds:["try_lock"; "owns_lock"] )


  let is_guard_constructor, is_guard_destructor, is_guard_unlock, is_guard_lock, is_guard_trylock =
    let make ~f =
      let constructors = List.map guards ~f in
      mk_matcher constructors
    in
    let make_trylock ~f =
      let methods = List.concat_map guards ~f in
      mk_matcher methods
    in
    ( make ~f:get_guard_constructor
    , make ~f:get_guard_destructor
    , make ~f:get_guard_unlock
    , make ~f:get_guard_lock
    , make_trylock ~f:get_guard_trylock )


  let get_lock_effect pname actuals =
    let log_parse_error error =
      L.debug Analysis Verbose "%s pname:%a actuals:%a@." error Typ.Procname.pp pname
        (PrettyPrintable.pp_collection ~pp_item:HilExp.pp)
        actuals
    in
    let guard_action ~f ~error =
      match actuals with [guard] -> f guard | _ -> log_parse_error error ; NoEffect
    in
    let fst_arg = match actuals with x :: _ -> [x] | _ -> [] in
    if is_std_lock pname then Lock actuals
    else if is_lock pname then Lock fst_arg
    else if is_unlock pname then Unlock fst_arg
    else if is_trylock pname then LockedIfTrue fst_arg
    else if is_guard_constructor pname then (
      match actuals with
      | [guard; lock] ->
          GuardConstruct {guard; lock; acquire_now= true}
      | [guard; lock; _defer_lock] ->
          GuardConstruct {guard; lock; acquire_now= false}
      | _ ->
          log_parse_error "Cannot parse guard constructor call" ;
          NoEffect )
    else if is_guard_lock pname then
      guard_action ~f:(fun guard -> GuardLock guard) ~error:"Can't parse guard lock"
    else if is_guard_unlock pname then
      guard_action ~f:(fun guard -> GuardUnlock guard) ~error:"Can't parse guard unlock"
    else if is_guard_destructor pname then
      guard_action ~f:(fun guard -> GuardDestroy guard) ~error:"Can't parse guard destructor"
    else if is_guard_trylock pname then
      guard_action ~f:(fun guard -> GuardLockedIfTrue guard) ~error:"Can't parse guard trylock"
    else NoEffect
end

module Java : sig
  val get_lock_effect : Typ.Procname.t -> Typ.Procname.Java.t -> HilExp.t list -> lock_effect
end = struct
  let std_locks =
    [ "java.util.concurrent.locks.Lock"
    ; "java.util.concurrent.locks.ReentrantLock"
    ; "java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock"
    ; "java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock" ]


  let is_lock classname methodname =
    List.mem std_locks classname ~equal:String.equal
    && List.mem ["lock"; "lockInterruptibly"] methodname ~equal:String.equal
    || String.equal classname "com.facebook.buck.util.concurrent.AutoCloseableReadWriteUpdateLock"
       && List.mem ["readLock"; "updateLock"; "writeLock"] methodname ~equal:String.equal


  let is_unlock classname methodname =
    String.equal methodname "unlock" && List.mem std_locks classname ~equal:String.equal


  let is_trylock classname methodname =
    String.equal methodname "tryLock" && List.mem std_locks classname ~equal:String.equal


  let get_lock_effect pname java_pname actuals =
    let fst_arg = match actuals with x :: _ -> [x] | _ -> [] in
    if is_thread_utils_method "assertHoldsLock" pname then Lock fst_arg
    else
      let classname = Typ.Procname.Java.get_class_name java_pname in
      let methodname = Typ.Procname.Java.get_method java_pname in
      if is_lock classname methodname then Lock fst_arg
      else if is_unlock classname methodname then Unlock fst_arg
      else if is_trylock classname methodname then LockedIfTrue fst_arg
      else NoEffect
end

let get_lock_effect pname actuals =
  let fst_arg = match actuals with x :: _ -> [x] | _ -> [] in
  if Typ.Procname.equal pname BuiltinDecl.__set_locked_attribute then Lock fst_arg
  else if Typ.Procname.equal pname BuiltinDecl.__delete_locked_attribute then Unlock fst_arg
  else
    match pname with
    | Typ.Procname.Java java_pname ->
        Java.get_lock_effect pname java_pname actuals
    | Typ.Procname.(ObjC_Cpp _ | C _) ->
        Clang.get_lock_effect pname actuals
    | _ ->
        NoEffect


let get_current_class_and_annotated_superclasses is_annot tenv pname =
  match pname with
  | Typ.Procname.Java java_pname ->
      let current_class = Typ.Procname.Java.get_class_type_name java_pname in
      let annotated_classes =
        PatternMatch.find_superclasses_with_attributes is_annot tenv current_class
      in
      Some (current_class, annotated_classes)
  | _ ->
      None


let ui_matcher_records =
  let open MethodMatcher in
  let fragment_methods =
    (* sort police: this is in lifecycle order *)
    [ "onAttach"
    ; "onCreate"
    ; "onCreateView"
    ; "onActivityCreated"
    ; "onStart"
    ; "onResume"
    ; "onPause"
    ; "onStop"
    ; "onDestroyView"
    ; "onDestroy"
    ; "onDetach" ]
  in
  (* search_superclasses is true by default in how [default] is treated *)
  [ {default with classname= "android.support.v4.app.Fragment"; methods= fragment_methods}
  ; {default with classname= "android.app.Fragment"; methods= fragment_methods}
  ; {default with classname= "androidx.fragment.app.Fragment"; methods= fragment_methods}
  ; {default with classname= "android.content.ContentProvider"; methods= ["onCreate"]}
  ; {default with classname= "android.content.BroadcastReceiver"; methods= ["onReceive"]}
  ; { default with
      classname= "android.app.Service"
    ; methods= ["onBind"; "onCreate"; "onDestroy"; "onStartCommand"] }
  ; {default with classname= "android.app.Application"; methods= ["onCreate"]}
  ; { default with
      classname= "android.app.Activity"
    ; methods= ["onCreate"; "onStart"; "onRestart"; "onResume"; "onPause"; "onStop"; "onDestroy"] }
  ; { default with
      (* according to Android documentation, *all* methods of the View class run on UI thread, but
         let's be a bit conservative and catch all methods that start with "on".
         https://developer.android.com/reference/android/view/View.html *)
      method_prefix= true
    ; classname= "android.view.View"
    ; methods= ["on"] } ]


let is_modeled_ui_method =
  let matchers = List.map ui_matcher_records ~f:MethodMatcher.of_record in
  (* we pass an empty actuals list because all matching is done on class and method name here *)
  fun tenv pname -> MethodMatcher.of_list matchers tenv pname []


type annotation_trail = DirectlyAnnotated | Override of Typ.Procname.t | SuperClass of Typ.name
[@@deriving compare]

let find_override_or_superclass_annotated ~attrs_of_pname is_annot tenv proc_name =
  let is_annotated pn = Annotations.pname_has_return_annot pn ~attrs_of_pname is_annot in
  let is_override = Staged.unstage (PatternMatch.is_override_of proc_name) in
  let rec find_override_or_superclass_aux class_name =
    match Tenv.lookup tenv class_name with
    | None ->
        None
    | Some tstruct when Annotations.struct_typ_has_annot tstruct is_annot ->
        Some (SuperClass class_name)
    | Some (tstruct : Typ.Struct.t) -> (
      match
        List.find_map tstruct.methods ~f:(fun pn ->
            if is_override pn && is_annotated pn then Some (Override pn) else None )
      with
      | Some _ as result ->
          result
      | None ->
          List.find_map tstruct.supers ~f:find_override_or_superclass_aux )
  in
  if is_annotated proc_name then Some DirectlyAnnotated
  else Typ.Procname.get_class_type_name proc_name |> Option.bind ~f:find_override_or_superclass_aux


let annotated_as ~attrs_of_pname predicate tenv pname =
  find_override_or_superclass_annotated ~attrs_of_pname predicate tenv pname |> Option.is_some


let annotated_as_worker_thread ~attrs_of_pname tenv pname =
  annotated_as ~attrs_of_pname Annotations.ia_is_worker_thread tenv pname


let annotated_as_uithread_equivalent ~attrs_of_pname tenv pname =
  annotated_as ~attrs_of_pname Annotations.ia_is_uithread_equivalent tenv pname


let runs_on_ui_thread ~attrs_of_pname tenv pname =
  is_modeled_ui_method tenv pname || annotated_as_uithread_equivalent ~attrs_of_pname tenv pname


let cpp_lock_types_matcher = Clang.lock_types_matcher

let is_recursive_lock_type = function
  | Typ.CppClass (qname, _) ->
      Clang.is_recursive_lock_type qname
  | _ ->
      (* non-C++ lock types are always considered recursive *)
      true
