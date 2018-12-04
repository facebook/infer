(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module MF = MarkupFormatter

type lock_effect =
  | Lock of HilExp.t list
  | Unlock of HilExp.t list
  | LockedIfTrue of HilExp.t list
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


module Clang : sig
  val get_lock_effect : Typ.Procname.t -> HilExp.t list -> lock_effect

  val lock_types_matcher : QualifiedCppName.Match.quals_matcher
end = struct
  type lock_model = {cls: string; lck: string list; tlk: string list; unl: string list}

  let lock_models =
    let def = {cls= ""; lck= ["lock"]; tlk= ["try_lock"]; unl= ["unlock"]} in
    let shd =
      { cls= "std::shared_mutex"
      ; lck= "lock_shared" :: def.lck
      ; tlk= "try_lock_shared" :: def.tlk
      ; unl= "unlock_shared" :: def.unl }
    in
    let rwm =
      { cls= "apache::thrift::concurrency::ReadWriteMutex"
      ; lck= ["acquireRead"; "acquireWrite"]
      ; tlk= ["attemptRead"; "attemptWrite"]
      ; unl= ["release"] }
    in
    [ {def with cls= "apache::thrift::concurrency::Monitor"; tlk= "timedlock" :: def.tlk}
    ; {def with cls= "apache::thrift::concurrency::Mutex"; tlk= "timedlock" :: def.tlk}
    ; {rwm with cls= "apache::thrift::concurrency::NoStarveReadWriteMutex"}
    ; rwm
    ; {shd with cls= "boost::shared_mutex"}
    ; {def with cls= "boost::mutex"}
    ; {def with cls= "folly::MicroSpinLock"}
    ; {shd with cls= "folly::RWSpinLock"}
    ; {shd with cls= "folly::SharedMutex"}
    ; {shd with cls= "folly::SharedMutexImpl"}
    ; {def with cls= "folly::SpinLock"}
    ; {def with cls= "std::shared_lock"}
    ; {def with cls= "std::mutex"}
    ; shd
    ; {def with cls= "std::unique_lock"; tlk= "owns_lock" :: def.tlk} ]


  let mk_model_matcher ~f =
    let lock_methods =
      List.concat_map lock_models ~f:(fun mdl ->
          List.map (f mdl) ~f:(fun mtd -> mdl.cls ^ "::" ^ mtd) )
    in
    let matcher = QualifiedCppName.Match.of_fuzzy_qual_names lock_methods in
    fun pname ->
      QualifiedCppName.Match.match_qualifiers matcher (Typ.Procname.get_qualifiers pname)


  let is_lock = mk_model_matcher ~f:(fun mdl -> mdl.lck)

  let is_unlock = mk_model_matcher ~f:(fun mdl -> mdl.unl)

  let is_trylock = mk_model_matcher ~f:(fun mdl -> mdl.tlk)

  let lock_types_matcher =
    let class_names = List.map lock_models ~f:(fun mdl -> mdl.cls) in
    QualifiedCppName.Match.of_fuzzy_qual_names class_names


  (* TODO std::scoped_lock *)

  let guards =
    [ "apache::thrift::concurrency::Guard"
    ; "apache::thrift::concurrency::RWGuard"
    ; "folly::SharedMutex::ReadHolder"
    ; "folly::SharedMutex::WriteHolder"
    ; "folly::LockedPtr"
    ; "folly::SpinLockGuard"
    ; "std::lock_guard"
    ; "std::shared_lock"
    ; "std::unique_lock" ]


  let get_guard_constructor, get_guard_destructor =
    let get_class_and_qual_name guard =
      let qual_name = QualifiedCppName.of_qual_string guard in
      let class_name, _ = Option.value_exn (QualifiedCppName.extract_last qual_name) in
      (class_name, qual_name)
    in
    ( (fun guard ->
        let class_name, qual_name = get_class_and_qual_name guard in
        let qual_constructor = QualifiedCppName.append_qualifier qual_name ~qual:class_name in
        QualifiedCppName.to_qual_string qual_constructor )
    , fun guard ->
        let class_name, qual_name = get_class_and_qual_name guard in
        let qual_destructor =
          QualifiedCppName.append_qualifier qual_name ~qual:("~" ^ class_name)
        in
        QualifiedCppName.to_qual_string qual_destructor )


  let is_guard_lock =
    let constructors = List.map guards ~f:get_guard_constructor in
    let matcher = QualifiedCppName.Match.of_fuzzy_qual_names constructors in
    fun pname actuals ->
      QualifiedCppName.Match.match_qualifiers matcher (Typ.Procname.get_qualifiers pname)
      (* Passing additional parameter allows to defer the lock *)
      && Int.equal 2 (List.length actuals)


  let is_guard_unlock =
    let destructors = List.map guards ~f:get_guard_destructor in
    let matcher = QualifiedCppName.Match.of_fuzzy_qual_names destructors in
    fun pname ->
      QualifiedCppName.Match.match_qualifiers matcher (Typ.Procname.get_qualifiers pname)


  let is_std_lock =
    let matcher = QualifiedCppName.Match.of_fuzzy_qual_names ["std::lock"] in
    fun pname ->
      QualifiedCppName.Match.match_qualifiers matcher (Typ.Procname.get_qualifiers pname)


  let get_lock_effect pname actuals =
    let fst_arg = match actuals with x :: _ -> [x] | _ -> [] in
    let snd_arg = match actuals with _ :: x :: _ -> [x] | _ -> [] in
    if is_std_lock pname then Lock actuals
    else if is_lock pname then Lock fst_arg
    else if is_guard_lock pname actuals then Lock snd_arg
    else if is_unlock pname then Unlock fst_arg
    else if is_guard_unlock pname then Unlock snd_arg
    else if is_trylock pname then LockedIfTrue fst_arg
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


let find_annotated_or_overriden_annotated_method is_annot pname tenv =
  PatternMatch.override_find
    (fun pn ->
      Annotations.pname_has_return_annot pn ~attrs_of_pname:Summary.proc_resolve_attributes
        is_annot )
    tenv pname


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
  ; {default with classname= "android.content.ContentProvider"; methods= ["onCreate"]}
  ; {default with classname= "android.content.BroadcastReceiver"; methods= ["onReceive"]}
  ; { default with
      classname= "android.app.Service"
    ; methods= ["onBind"; "onCreate"; "onDestroy"; "onStartCommand"] }
  ; {default with classname= "android.app.Application"; methods= ["onCreate"]}
  ; { default with
      classname= "android.app.Activity"
    ; methods= ["onCreate"; "onStart"; "onRestart"; "onResume"; "onPause"; "onStop"; "onDestroy"]
    }
  ; { default with
      (* according to Android documentation, *all* methods of the View class run on UI thread, but
       let's be a bit conservative and catch all methods that start with "on".
       https://developer.android.com/reference/android/view/View.html *)
      method_prefix= true
    ; classname= "android.view.View"
    ; methods= ["on"] } ]


let is_ui_method =
  let matchers = List.map ui_matcher_records ~f:MethodMatcher.of_record in
  (* we pass an empty actuals list because all matching is done on class and method name here *)
  fun tenv pname -> MethodMatcher.of_list matchers tenv pname []


let runs_on_ui_thread =
  (* assume that methods annotated with @UiThread, @OnEvent, @OnBind, @OnMount, @OnUnbind,
     @OnUnmount always run on the UI thread *)
  let annotation_matchers =
    [ Annotations.ia_is_mainthread
    ; Annotations.ia_is_ui_thread
    ; Annotations.ia_is_on_bind
    ; Annotations.ia_is_on_event
    ; Annotations.ia_is_on_mount
    ; Annotations.ia_is_on_unbind
    ; Annotations.ia_is_on_unmount ]
  in
  let is_annot annot = List.exists annotation_matchers ~f:(fun m -> m annot) in
  let mono_pname = MF.wrap_monospaced Typ.Procname.pp in
  fun tenv proc_desc ->
    let pname = Procdesc.get_proc_name proc_desc in
    if
      Annotations.pdesc_has_return_annot proc_desc Annotations.ia_is_worker_thread
      || find_annotated_or_overriden_annotated_method Annotations.ia_is_worker_thread pname tenv
         |> Option.is_some
      || get_current_class_and_annotated_superclasses Annotations.ia_is_worker_thread tenv pname
         |> Option.value_map ~default:false ~f:(function _, [] -> false | _ -> true)
    then None
    else if is_ui_method tenv pname then
      Some (F.asprintf "%a is a standard UI-thread method" mono_pname pname)
    else if Annotations.pdesc_has_return_annot proc_desc is_annot then
      Some
        (F.asprintf "%a is annotated %s" mono_pname pname
           (MF.monospaced_to_string Annotations.ui_thread))
    else
      match find_annotated_or_overriden_annotated_method is_annot pname tenv with
      | Some override_pname ->
          Some
            (F.asprintf "class %a overrides %a, which is annotated %s" mono_pname pname mono_pname
               override_pname
               (MF.monospaced_to_string Annotations.ui_thread))
      | None -> (
        match get_current_class_and_annotated_superclasses is_annot tenv pname with
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


let cpp_lock_types_matcher = Clang.lock_types_matcher
