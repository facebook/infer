(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module MF = MarkupFormatter

type lock = Lock | Unlock | LockedIfTrue | NoEffect

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


let get_thread = function
  | Typ.Procname.Java java_pname when is_thread_utils_type java_pname -> (
    match Typ.Procname.Java.get_method java_pname with
    | "assertMainThread" | "assertOnUiThread" | "checkOnMainThread" ->
        MainThread
    | "isMainThread" | "isUiThread" ->
        MainThreadIfTrue
    | "assertOnBackgroundThread" | "assertOnNonUiThread" | "checkOnNonUiThread" ->
        BackgroundThread
    | _ ->
        UnknownThread )
  | _ ->
      UnknownThread


let get_lock =
  let is_cpp_lock =
    let matcher_lock =
      QualifiedCppName.Match.of_fuzzy_qual_names
        [ "apache::thrift::concurrency::ReadWriteMutex::acquireRead"
        ; "apache::thrift::concurrency::ReadWriteMutex::acquireWrite"
        ; "folly::MicroSpinLock::lock"
        ; "folly::RWSpinLock::lock"
        ; "folly::RWSpinLock::lock_shared"
        ; "folly::SharedMutexImpl::lockExclusiveImpl"
        ; "folly::SharedMutexImpl::lockSharedImpl"
        ; "std::mutex::lock"
        ; "std::unique_lock::lock"
        ; "std::lock" ]
    in
    let matcher_lock_constructor =
      QualifiedCppName.Match.of_fuzzy_qual_names
        [ "std::lock_guard::lock_guard"
        ; "std::unique_lock::unique_lock"
        ; "folly::LockedPtr::LockedPtr" ]
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
        [ "apache::thrift::concurrency::ReadWriteMutex::release"
        ; "folly::MicroSpinLock::unlock"
        ; "folly::RWSpinLock::unlock"
        ; "folly::RWSpinLock::unlock_shared"
        ; "folly::SharedMutexImpl::unlock"
        ; "folly::SharedMutexImpl::unlock_shared"
        ; "std::lock_guard::~lock_guard"
        ; "std::mutex::unlock"
        ; "std::unique_lock::unlock"
        ; "std::unique_lock::~unique_lock"
        ; "folly::LockedPtr::~LockedPtr" ]
    in
    fun pname ->
      QualifiedCppName.Match.match_qualifiers matcher (Typ.Procname.get_qualifiers pname)
  and is_cpp_trylock =
    let matcher =
      QualifiedCppName.Match.of_fuzzy_qual_names
        ["std::unique_lock::owns_lock"; "std::unique_lock::try_lock"]
    in
    fun pname ->
      QualifiedCppName.Match.match_qualifiers matcher (Typ.Procname.get_qualifiers pname)
  in
  fun pname actuals ->
    match pname with
    | Typ.Procname.Java java_pname -> (
        if is_thread_utils_method "assertHoldsLock" (Typ.Procname.Java java_pname) then Lock
        else
          match
            (Typ.Procname.Java.get_class_name java_pname, Typ.Procname.Java.get_method java_pname)
          with
          | ( ( "java.util.concurrent.locks.Lock"
              | "java.util.concurrent.locks.ReentrantLock"
              | "java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock"
              | "java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock" )
            , ("lock" | "lockInterruptibly") ) ->
              Lock
          | ( ( "java.util.concurrent.locks.Lock"
              | "java.util.concurrent.locks.ReentrantLock"
              | "java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock"
              | "java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock" )
            , "unlock" ) ->
              Unlock
          | ( ( "java.util.concurrent.locks.Lock"
              | "java.util.concurrent.locks.ReentrantLock"
              | "java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock"
              | "java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock" )
            , "tryLock" ) ->
              LockedIfTrue
          | ( "com.facebook.buck.util.concurrent.AutoCloseableReadWriteUpdateLock"
            , ("readLock" | "updateLock" | "writeLock") ) ->
              Lock
          | _ ->
              NoEffect )
    | (Typ.Procname.ObjC_Cpp _ | C _) as pname when is_cpp_lock pname actuals ->
        Lock
    | (Typ.Procname.ObjC_Cpp _ | C _) as pname when is_cpp_unlock pname ->
        Unlock
    | (Typ.Procname.ObjC_Cpp _ | C _) as pname when is_cpp_trylock pname ->
        LockedIfTrue
    | pname when Typ.Procname.equal pname BuiltinDecl.__set_locked_attribute ->
        Lock
    | pname when Typ.Procname.equal pname BuiltinDecl.__delete_locked_attribute ->
        Unlock
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


let find_annotated_or_overriden_annotated_method is_annot pname tenv =
  PatternMatch.override_find
    (fun pn ->
      Annotations.pname_has_return_annot pn ~attrs_of_pname:Summary.proc_resolve_attributes
        is_annot )
    tenv pname


(* we don't want to warn on methods that run on the UI thread because they should always be
      single-threaded *)
let runs_on_ui_thread tenv proc_desc =
  (* assume that methods annotated with @UiThread, @OnEvent, @OnBind, @OnMount, @OnUnbind,
        @OnUnmount always run on the UI thread *)
  let is_annot annot =
    Annotations.ia_is_ui_thread annot || Annotations.ia_is_on_bind annot
    || Annotations.ia_is_on_event annot || Annotations.ia_is_on_mount annot
    || Annotations.ia_is_on_unbind annot
    || Annotations.ia_is_on_unmount annot
  in
  let pname = Procdesc.get_proc_name proc_desc in
  if
    Annotations.pdesc_has_return_annot proc_desc Annotations.ia_is_worker_thread
    || find_annotated_or_overriden_annotated_method Annotations.ia_is_worker_thread pname tenv
       |> Option.is_some
    || get_current_class_and_annotated_superclasses Annotations.ia_is_worker_thread tenv pname
       |> Option.value_map ~default:false ~f:(function _, [] -> false | _ -> true)
  then None
  else if Annotations.pdesc_has_return_annot proc_desc is_annot then
    Some
      (F.asprintf "%a is annotated %s"
         (MF.wrap_monospaced Typ.Procname.pp)
         pname
         (MF.monospaced_to_string Annotations.ui_thread))
  else
    match find_annotated_or_overriden_annotated_method is_annot pname tenv with
    | Some override_pname ->
        Some
          (F.asprintf "class %a overrides %a, which is annotated %s"
             (MF.wrap_monospaced Typ.Procname.pp)
             pname
             (MF.wrap_monospaced Typ.Procname.pp)
             override_pname
             (MF.monospaced_to_string Annotations.ui_thread))
    | None -> (
      match
        get_current_class_and_annotated_superclasses Annotations.ia_is_ui_thread tenv pname
      with
      | Some (current_class, _)
        when let open PatternMatch in
             is_subtype_of_str tenv current_class "android.app.Service"
             && not (is_subtype_of_str tenv current_class "android.app.IntentService") ->
          Some
            (F.asprintf "class %s extends %s"
               (MF.monospaced_to_string (Typ.Name.name current_class))
               (MF.monospaced_to_string "android.app.Service"))
      | Some (current_class, (super_class :: _ as super_classes)) ->
          let middle =
            if List.exists super_classes ~f:(Typ.Name.equal current_class) then ""
            else F.asprintf " extends %a, which" (MF.wrap_monospaced Typ.Name.pp) super_class
          in
          Some
            (F.asprintf "class %s%s is annotated %s"
               (MF.monospaced_to_string (Typ.Name.name current_class))
               middle
               (MF.monospaced_to_string Annotations.ui_thread))
      | _ ->
          None )


let is_call_of_class ?(search_superclasses = true) ?(method_prefix = false)
    ?(actuals_pred = fun _ -> true) clazz methods =
  let method_matcher =
    if method_prefix then fun current_method target_method ->
      String.is_prefix current_method ~prefix:target_method
    else fun current_method target_method -> String.equal current_method target_method
  in
  let class_matcher =
    let is_target_class =
      let target = Typ.Name.Java.from_string clazz in
      fun tname -> Typ.Name.equal tname target
    in
    if search_superclasses then fun tenv classname ->
      let is_target_struct tname _ = is_target_class tname in
      PatternMatch.supertype_exists tenv is_target_struct classname
    else fun _ classname -> is_target_class classname
  in
  (fun tenv pn actuals ->
    actuals_pred actuals
    &&
    match pn with
    | Typ.Procname.Java java_pname ->
        let mthd = Typ.Procname.Java.get_method java_pname in
        List.exists methods ~f:(method_matcher mthd)
        &&
        let classname = Typ.Procname.Java.get_class_type_name java_pname in
        class_matcher tenv classname
    | _ ->
        false )
  |> Staged.stage
