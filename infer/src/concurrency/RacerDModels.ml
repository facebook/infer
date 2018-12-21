(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
open ConcurrencyModels

module AnnotationAliases = struct
  let of_json = function
    | `List aliases ->
        List.map ~f:Yojson.Basic.Util.to_string aliases
    | _ ->
        L.(die UserError)
          "Couldn't parse thread-safety annotation aliases; expected list of strings"
end

type container_access = ContainerRead | ContainerWrite

let is_java_container_write =
  let open MethodMatcher in
  let array_methods =
    ["append"; "clear"; "delete"; "put"; "remove"; "removeAt"; "removeAtRange"; "setValueAt"]
  in
  [ { default with
      classname= "android.support.v4.util.Pools$SimplePool"; methods= ["acquire"; "release"] }
  ; { default with
      classname= "android.support.v4.util.SimpleArrayMap"
    ; methods= ["clear"; "ensureCapacity"; "put"; "putAll"; "remove"; "removeAt"; "setValueAt"] }
  ; {default with classname= "android.support.v4.util.SparseArrayCompat"; methods= array_methods}
  ; {default with classname= "android.util.SparseArray"; methods= array_methods}
  ; {default with classname= "java.util.List"; methods= ["add"; "addAll"; "clear"; "remove"; "set"]}
  ; {default with classname= "java.util.Map"; methods= ["clear"; "put"; "putAll"; "remove"]} ]
  |> of_records


let is_java_container_read =
  let open MethodMatcher in
  let array_methods = ["clone"; "get"; "indexOfKey"; "indexOfValue"; "keyAt"; "size"; "valueAt"] in
  [ { default with
      classname= "android.support.v4.util.SimpleArrayMap"
    ; methods=
        [ "containsKey"
        ; "containsValue"
        ; "get"
        ; "hashCode"
        ; "indexOfKey"
        ; "isEmpty"
        ; "keyAt"
        ; "size"
        ; "valueAt" ] }
  ; {default with classname= "android.support.v4.util.SparseArrayCompat"; methods= array_methods}
  ; {default with classname= "android.util.SparseArray"; methods= array_methods}
  ; { default with
      classname= "java.util.List"
    ; methods=
        [ "contains"
        ; "containsAll"
        ; "equals"
        ; "get"
        ; "hashCode"
        ; "indexOf"
        ; "isEmpty"
        ; "iterator"
        ; "lastIndexOf"
        ; "listIterator"
        ; "size"
        ; "toArray" ] }
  ; { default with
      classname= "java.util.Map"
    ; methods=
        [ "containsKey"
        ; "containsValue"
        ; "entrySet"
        ; "equals"
        ; "get"
        ; "hashCode"
        ; "isEmpty"
        ; "keySet"
        ; "size"
        ; "values" ] } ]
  |> of_records


let is_cpp_container_read =
  let is_container_operator pname_qualifiers =
    QualifiedCppName.extract_last pname_qualifiers
    |> Option.exists ~f:(fun (last, _) -> String.equal last "operator[]")
  in
  let matcher = QualifiedCppName.Match.of_fuzzy_qual_names ["std::map::find"] in
  fun pname ->
    let pname_qualifiers = Typ.Procname.get_qualifiers pname in
    QualifiedCppName.Match.match_qualifiers matcher pname_qualifiers
    || is_container_operator pname_qualifiers


let is_cpp_container_write =
  let matcher =
    QualifiedCppName.Match.of_fuzzy_qual_names ["std::map::operator[]"; "std::map::erase"]
  in
  fun pname -> QualifiedCppName.Match.match_qualifiers matcher (Typ.Procname.get_qualifiers pname)


let get_container_access pn tenv =
  match pn with
  | Typ.Procname.Java _ when is_java_container_write tenv pn [] ->
      Some ContainerWrite
  | Typ.Procname.Java _ when is_java_container_read tenv pn [] ->
      Some ContainerRead
  | Typ.Procname.Java _ ->
      None
  (* The following order matters: we want to check if pname is a container write
       before we check if pname is a container read. This is due to a different
       treatment between std::map::operator[] and all other operator[]. *)
  | (Typ.Procname.ObjC_Cpp _ | C _) when is_cpp_container_write pn ->
      Some ContainerWrite
  | (Typ.Procname.ObjC_Cpp _ | C _) when is_cpp_container_read pn ->
      Some ContainerRead
  | _ ->
      None


(** holds of procedure names which should not be analyzed in order to avoid known sources of
            inaccuracy *)
let should_skip =
  let matcher =
    lazy
      (QualifiedCppName.Match.of_fuzzy_qual_names ~prefix:true
         [ "folly::AtomicStruct"
         ; "folly::fbstring_core"
         ; "folly::Future"
         ; "folly::futures"
         ; "folly::LockedPtr"
         ; "folly::Optional"
         ; "folly::Promise"
         ; "folly::ThreadLocal"
         ; "folly::detail::SingletonHolder"
         ; "std::atomic"
         ; "std::vector" ])
  in
  function
  | Typ.Procname.ObjC_Cpp cpp_pname as pname ->
      Typ.Procname.ObjC_Cpp.is_destructor cpp_pname
      || QualifiedCppName.Match.match_qualifiers (Lazy.force matcher)
           (Typ.Procname.get_qualifiers pname)
  | _ ->
      false


let has_return_annot predicate pn =
  Annotations.pname_has_return_annot pn ~attrs_of_pname:Summary.proc_resolve_attributes predicate


let is_functional pname =
  let is_annotated_functional = has_return_annot Annotations.ia_is_functional in
  let is_modeled_functional = function
    | Typ.Procname.Java java_pname -> (
      match
        (Typ.Procname.Java.get_class_name java_pname, Typ.Procname.Java.get_method java_pname)
      with
      | "android.content.res.Resources", method_name ->
          (* all methods of Resources are considered @Functional except for the ones in this
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
      | _ ->
          false )
    | _ ->
        false
  in
  is_annotated_functional pname || is_modeled_functional pname


let nsobject = Typ.Name.Objc.from_qual_name (QualifiedCppName.of_qual_string "NSObject")

let acquires_ownership pname tenv =
  let is_nsobject_init = function
    | Typ.Procname.ObjC_Cpp
        {kind= Typ.Procname.ObjC_Cpp.ObjCInstanceMethod; method_name= "init"; class_name} ->
        Typ.Name.equal class_name nsobject
    | _ ->
        false
  in
  let is_allocation pn =
    Typ.Procname.equal pn BuiltinDecl.__new
    || Typ.Procname.equal pn BuiltinDecl.__new_array
    || is_nsobject_init pn
  in
  (* identify library functions that maintain ownership invariants behind the scenes *)
  let is_owned_in_library = function
    | Typ.Procname.Java java_pname -> (
      match
        (Typ.Procname.Java.get_class_name java_pname, Typ.Procname.Java.get_method java_pname)
      with
      | "javax.inject.Provider", "get" ->
          (* in dependency injection, the library allocates fresh values behind the scenes *)
          true
      | ("java.lang.Class" | "java.lang.reflect.Constructor"), "newInstance" ->
          (* reflection can perform allocations *)
          true
      | "java.lang.Object", "clone" ->
          (* cloning is like allocation *)
          true
      | "java.lang.ThreadLocal", "get" ->
          (* ThreadLocal prevents sharing between threads behind the scenes *)
          true
      | ("android.app.Activity" | "android.view.View"), "findViewById" ->
          (* assume findViewById creates fresh View's (note: not always true) *)
          true
      | ( ( "android.support.v4.util.Pools$Pool"
          | "android.support.v4.util.Pools$SimplePool"
          | "android.support.v4.util.Pools$SynchronizedPool" )
        , "acquire" ) ->
          (* a pool should own all of its objects *)
          true
      | _ ->
          false )
    | _ ->
        false
  in
  is_allocation pname || is_owned_in_library pname
  || PatternMatch.override_exists is_owned_in_library tenv pname


(* return true if the given procname boxes a primitive type into a reference type *)
let is_box = function
  | Typ.Procname.Java java_pname -> (
    match
      (Typ.Procname.Java.get_class_name java_pname, Typ.Procname.Java.get_method java_pname)
    with
    | ( ( "java.lang.Boolean"
        | "java.lang.Byte"
        | "java.lang.Char"
        | "java.lang.Double"
        | "java.lang.Float"
        | "java.lang.Integer"
        | "java.lang.Long"
        | "java.lang.Short" )
      , "valueOf" ) ->
        true
    | _ ->
        false )
  | _ ->
      false


(* Methods in @ThreadConfined classes and methods annotated with @ThreadConfined are assumed to all
                 run on the same thread. For the moment we won't warn on accesses resulting from use of such
                 methods at all. In future we should account for races between these methods and methods from
                 completely different classes that don't necessarily run on the same thread as the confined
                 object. *)
let is_thread_confined_method tenv pdesc =
  Annotations.pdesc_return_annot_ends_with pdesc Annotations.thread_confined
  || PatternMatch.check_current_class_attributes Annotations.ia_is_thread_confined tenv
       (Procdesc.get_proc_name pdesc)


let threadsafe_annotations =
  Annotations.thread_safe :: AnnotationAliases.of_json Config.threadsafe_aliases


(* returns true if the annotation is @ThreadSafe, @ThreadSafe(enableChecks = true), or is defined
   as an alias of @ThreadSafe in a .inferconfig file. *)
let is_thread_safe item_annot =
  let f ((annot : Annot.t), _) =
    List.exists
      ~f:(fun annot_string ->
        Annotations.annot_ends_with annot annot_string
        || String.equal annot.class_name annot_string )
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
  (not
     ( match pn with
     | Typ.Procname.Java java_pname ->
         Typ.Procname.Java.is_class_initializer java_pname
         || Typ.Name.Java.is_external (Typ.Procname.Java.get_class_type_name java_pname)
     (* third party code may be hard to change, not useful to report races there *)
     | _ ->
         false ))
  && (not (FbThreadSafety.is_logging_method pn))
  && (not (pdesc_is_assumed_thread_safe pdesc tenv))
  && not (should_skip pn)


let get_current_class_and_threadsafe_superclasses tenv pname =
  get_current_class_and_annotated_superclasses is_thread_safe tenv pname


let is_thread_safe_class pname tenv =
  (not
     ((* current class not marked thread-safe *)
      PatternMatch.check_current_class_attributes Annotations.ia_is_not_thread_safe tenv pname))
  &&
  (* current class or superclass is marked thread-safe *)
  match get_current_class_and_threadsafe_superclasses tenv pname with
  | Some (_, thread_safe_annotated_classes) ->
      not (List.is_empty thread_safe_annotated_classes)
  | _ ->
      false


let is_thread_safe_method pname tenv =
  find_annotated_or_overriden_annotated_method ~attrs_of_pname:Summary.proc_resolve_attributes
    is_thread_safe pname tenv
  |> Option.is_some


let is_marked_thread_safe pdesc tenv =
  let pname = Procdesc.get_proc_name pdesc in
  is_thread_safe_class pname tenv || is_thread_safe_method pname tenv


let is_safe_access access prefix_path tenv =
  match (access, AccessPath.get_typ prefix_path tenv) with
  | ( AccessPath.FieldAccess fieldname
    , Some ({Typ.desc= Tstruct typename} | {desc= Tptr ({desc= Tstruct typename}, _)}) ) -> (
    match Tenv.lookup tenv typename with
    | Some struct_typ ->
        Annotations.struct_typ_has_annot struct_typ Annotations.ia_is_thread_confined
        || Annotations.field_has_annot fieldname struct_typ Annotations.ia_is_thread_confined
        || Annotations.field_has_annot fieldname struct_typ Annotations.ia_is_volatile
    | None ->
        false )
  | _ ->
      false


let should_flag_interface_call tenv exp call_flags pname =
  (* return true if this function is library code from the JDK core libraries or Android *)
  let is_java_library = function
    | Typ.Procname.Java java_pname -> (
      match Typ.Procname.Java.get_package java_pname with
      | Some package_name ->
          String.is_prefix ~prefix:"java." package_name
          || String.is_prefix ~prefix:"android." package_name
          || String.is_prefix ~prefix:"com.google." package_name
      | None ->
          false )
    | _ ->
        false
  in
  let is_builder_function = function
    | Typ.Procname.Java java_pname ->
        String.is_suffix ~suffix:"$Builder" (Typ.Procname.Java.get_class_name java_pname)
    | _ ->
        false
  in
  let thread_safe_or_thread_confined annot =
    Annotations.ia_is_thread_safe annot || Annotations.ia_is_thread_confined annot
  in
  call_flags.CallFlags.cf_interface && Typ.Procname.is_java pname
  && (not (is_java_library pname || is_builder_function pname))
  (* can't ask anyone to annotate interfaces in library code, and Builder's should always be
     thread-safe (would be unreasonable to ask everyone to annotate them) *)
  && (not (PatternMatch.check_class_attributes thread_safe_or_thread_confined tenv pname))
  && (not (has_return_annot thread_safe_or_thread_confined pname))
  &&
  match exp with
  | HilExp.AccessExpression receiver_access_exp :: _ -> (
      HilExp.AccessExpression.to_access_path receiver_access_exp
      |> AccessPath.truncate
      |> function
      | receiver_prefix, Some receiver_field ->
          is_safe_access receiver_field receiver_prefix tenv |> not
      | _ ->
          true )
  | _ ->
      false


let is_synchronized_container callee_pname ((_, (base_typ : Typ.t)), accesses) tenv =
  let is_threadsafe_collection pn tenv =
    match pn with
    | Typ.Procname.Java java_pname ->
        let typename = Typ.Name.Java.from_string (Typ.Procname.Java.get_class_name java_pname) in
        let aux tn _ =
          match Typ.Name.name tn with
          | "java.util.concurrent.ConcurrentMap"
          | "java.util.concurrent.CopyOnWriteArrayList"
          | "android.support.v4.util.Pools$SynchronizedPool" ->
              true
          | _ ->
              false
        in
        PatternMatch.supertype_exists tenv aux typename
    | _ ->
        false
  in
  if is_threadsafe_collection callee_pname tenv then true
  else
    let is_annotated_synchronized base_typename container_field tenv =
      match Tenv.lookup tenv base_typename with
      | Some base_typ ->
          Annotations.field_has_annot container_field base_typ
            Annotations.ia_is_synchronized_collection
      | None ->
          false
    in
    match List.rev accesses with
    | AccessPath.FieldAccess base_field :: AccessPath.FieldAccess container_field :: _
      when Typ.Procname.is_java callee_pname ->
        let base_typename = Typ.Name.Java.from_string (Typ.Fieldname.Java.get_class base_field) in
        is_annotated_synchronized base_typename container_field tenv
    | [AccessPath.FieldAccess container_field] -> (
      match base_typ.desc with
      | Typ.Tstruct base_typename | Tptr ({Typ.desc= Tstruct base_typename}, _) ->
          is_annotated_synchronized base_typename container_field tenv
      | _ ->
          false )
    | _ ->
        false
