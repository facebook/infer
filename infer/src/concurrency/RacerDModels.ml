(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
        List.map ~f:Yojson.Safe.Util.to_string aliases
    | _ ->
        L.(die UserError)
          "Couldn't parse thread-safety annotation aliases; expected list of strings"
end

let make_android_support_template suffix methods =
  let open MethodMatcher in
  [ {default with classname= "android.support.v4.util." ^ suffix; methods}
  ; {default with classname= "androidx.core.util." ^ suffix; methods} ]


let is_csharp_container_write =
  let open MethodMatcher in
  [ { default with
      classname= "System.Collections.Generic.List`1"
    ; methods=
        [ "Add"
        ; "AddRange"
        ; "Clear"
        ; "Insert"
        ; "InsertRange"
        ; "Remove"
        ; "RemoveAll"
        ; "RemoveAt"
        ; "RemoveRange"
        ; "set_Item" ] }
  ; { default with
      classname= "System.Collections.Generic.Dictionary`2"
    ; methods= ["Add"; "Clear"; "Remove"; "TryAdd"; "set_Item"] } ]
  |> of_records


let is_csharp_container_read =
  let open MethodMatcher in
  [ { default with
      classname= "System.Collections.Generic.List`1"
    ; methods=
        [ "BinarySearch"
        ; "Contains"
        ; "CopyTo"
        ; "Equals"
        ; "Exists"
        ; "Find"
        ; "FindAll"
        ; "FindIndex"
        ; "FindLast"
        ; "FindLastIndex"
        ; "GetEnumerator"
        ; "GetHashCode"
        ; "GetRange"
        ; "IndexOf"
        ; "LastIndexOf"
        ; "MemberwiseClone"
        ; "ToArray"
        ; "TrueForAll"
        ; "get_Item"
        ; "get_Count" ] }
  ; { default with
      classname= "System.Collections.Generic.Dictionary`2"
    ; methods=
        [ "ContainsKey"
        ; "ContainsValue"
        ; "Equals"
        ; "GetHashCode"
        ; "TryGetValue"
        ; "get_Item"
        ; "get_Count" ] } ]
  |> of_records


let is_java_container_write =
  let matcher =
    let open MethodMatcher in
    let array_methods =
      ["append"; "clear"; "delete"; "put"; "remove"; "removeAt"; "removeAtRange"; "setValueAt"]
    in
    (* https://developer.android.com/reference/androidx/core/util/Pools.SimplePool *)
    make_android_support_template "Pools$SimplePool" ["acquire"; "release"]
    (* https://developer.android.com/reference/android/support/v4/util/SimpleArrayMap *)
    @ make_android_support_template "SimpleArrayMap"
        ["clear"; "ensureCapacity"; "put"; "putAll"; "remove"; "removeAt"; "setValueAt"]
    (* https://developer.android.com/reference/android/support/v4/util/SparseArrayCompat *)
    @ make_android_support_template "SparseArrayCompat" array_methods
    @ (* https://developer.android.com/reference/android/util/SparseArray *)
    [ {default with classname= "android.util.SparseArray"; methods= array_methods}
    ; (* https://docs.oracle.com/javase/8/docs/api/java/util/List.html
         Only methods not in parent interface [Collection] are listed *)
      {default with classname= "java.util.List"; methods= ["replaceAll"; "retainAll"; "set"; "sort"]}
    ; (* https://docs.oracle.com/javase/8/docs/api/java/util/Map.html *)
      { default with
        classname= "java.util.Map"
      ; methods=
          ["clear"; "merge"; "put"; "putAll"; "putIfAbsent"; "remove"; "replace"; "replaceAll"] }
    ; (* https://docs.oracle.com/javase/8/docs/api/java/util/Collection.html *)
      { default with
        classname= "java.util.Collection"
      ; methods= ["add"; "addAll"; "clear"; "remove"; "removeAll"; "removeIf"] }
    ; (* https://docs.oracle.com/javase/8/docs/api/javax/crypto/Mac.html *)
      {default with classname= "javax.crypto.Mac"; methods= ["update"; "init"; "doFinal"]} ]
    |> of_records
  in
  fun tenv (pn : Procname.t) ->
    match pn with
    | Java java_pn ->
        (not (Procname.Java.is_static java_pn)) && matcher tenv pn []
    | _ ->
        L.die InternalError "is_java_container_write called with a non-Java procname.@\n"


let is_java_container_read =
  let matcher =
    let open MethodMatcher in
    let array_methods =
      ["clone"; "get"; "indexOfKey"; "indexOfValue"; "keyAt"; "size"; "valueAt"]
    in
    (* https://developer.android.com/reference/android/support/v4/util/SimpleArrayMap *)
    make_android_support_template "SimpleArrayMap"
      [ "containsKey"
      ; "containsValue"
      ; "get"
      ; "hashCode"
      ; "indexOfKey"
      ; "isEmpty"
      ; "keyAt"
      ; "size"
      ; "valueAt" ]
    (* https://developer.android.com/reference/android/support/v4/util/SparseArrayCompat *)
    @ make_android_support_template "SparseArrayCompat" array_methods
    @ (* https://developer.android.com/reference/android/util/SparseArray *)
    [ {default with classname= "android.util.SparseArray"; methods= array_methods}
    ; (* https://docs.oracle.com/javase/8/docs/api/java/util/List.html
         Only methods not in parent interface [Collection] are listed *)
      { default with
        classname= "java.util.List"
      ; methods= ["get"; "indexOf"; "isEmpty"; "lastIndexOf"; "listIterator"] }
    ; (* https://docs.oracle.com/javase/8/docs/api/java/util/Map.html *)
      { default with
        classname= "java.util.Map"
      ; methods=
          [ "compute"
          ; "computeIfAbsent"
          ; "computeIfPresent"
          ; "containsKey"
          ; "containsValue"
          ; "entrySet"
          ; "equals"
          ; "forEach"
          ; "get"
          ; "getOrDefault"
          ; "hashCode"
          ; "isEmpty"
          ; "keySet"
          ; "size"
          ; "values" ] }
    ; (* https://docs.oracle.com/javase/8/docs/api/java/util/Collection.html *)
      { default with
        classname= "java.util.Collection"
      ; methods=
          [ "contains"
          ; "containsAll"
          ; "equals"
          ; "get"
          ; "hashCode"
          ; "isEmpty"
          ; "iterator"
          ; "parallelStream"
          ; "size"
          ; "spliterator"
          ; "stream"
          ; "toArray" ] }
    ; (* https://docs.oracle.com/javase/8/docs/api/javax/crypto/Mac.html *)
      {default with classname= "javax.crypto.Mac"; methods= ["doFinal"]} ]
    |> of_records
  in
  fun tenv (pn : Procname.t) ->
    match pn with
    | Java java_pn ->
        (not (Procname.Java.is_static java_pn)) && matcher tenv pn []
    | _ ->
        L.die InternalError "is_java_container_read called with a non-Java procname.@\n"


let is_cpp_container_read =
  let is_container_operator pname_qualifiers =
    QualifiedCppName.extract_last pname_qualifiers
    |> Option.exists ~f:(fun (last, _) -> String.equal last "operator[]")
  in
  let matcher = QualifiedCppName.Match.of_fuzzy_qual_names ["std::map::find"] in
  fun pname ->
    let pname_qualifiers = Procname.get_qualifiers pname in
    QualifiedCppName.Match.match_qualifiers matcher pname_qualifiers
    || is_container_operator pname_qualifiers


let is_cpp_container_write =
  let matcher =
    QualifiedCppName.Match.of_fuzzy_qual_names ["std::map::operator[]"; "std::map::erase"]
  in
  fun pname -> QualifiedCppName.Match.match_qualifiers matcher (Procname.get_qualifiers pname)


let is_container_write tenv pn =
  match (pn : Procname.t) with
  | CSharp _ ->
      is_csharp_container_write tenv pn []
  | Java _ ->
      is_java_container_write tenv pn
  | ObjC_Cpp _ | C _ ->
      is_cpp_container_write pn
  | _ ->
      false


let is_container_read tenv pn =
  match (pn : Procname.t) with
  | CSharp _ ->
      is_csharp_container_read tenv pn []
  | Java _ ->
      is_java_container_read tenv pn
  (* The following order matters: we want to check if pname is a container write
     before we check if pname is a container read. This is due to a different
     treatment between std::map::operator[] and all other operator[]. *)
  | ObjC_Cpp _ | C _ ->
      (not (is_cpp_container_write pn)) && is_cpp_container_read pn
  | Erlang _ | Hack _ | Block _ | Python _ ->
      false


let has_return_annot predicate pn = Annotations.pname_has_return_annot pn predicate

let is_functional pname =
  let is_annotated_functional = has_return_annot Annotations.ia_is_functional in
  let is_modeled_functional = function
    | Procname.Java java_pname -> (
      match (Procname.Java.get_class_name java_pname, Procname.Java.get_method java_pname) with
      | "android.content.res.Resources", method_name ->
          (* all methods of Resources are considered @Functional except for the ones in this
             block list *)
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
    | Procname.ObjC_Cpp {kind= Procname.ObjC_Cpp.ObjCInstanceMethod; method_name= "init"; class_name}
      ->
        Typ.Name.equal class_name nsobject
    | _ ->
        false
  in
  let is_allocation pn =
    Procname.equal pn BuiltinDecl.__new
    || Procname.equal pn BuiltinDecl.__new_array
    || is_nsobject_init pn
  in
  (* identify library functions that maintain ownership invariants behind the scenes *)
  let is_owned_in_library = function
    | Procname.Java java_pname -> (
      match (Procname.Java.get_class_name java_pname, Procname.Java.get_method java_pname) with
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
          | "android.support.v4.util.Pools$SynchronizedPool"
          | "androidx.core.util.Pools$Pool"
          | "androidx.core.util.Pools$SimplePool"
          | "androidx.core.util.Pools$SynchronizedPool" )
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
  | Procname.Java java_pname -> (
    match (Procname.Java.get_class_name java_pname, Procname.Java.get_method java_pname) with
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
let is_thread_confined_method tenv pname =
  ConcurrencyModels.find_override_or_superclass_annotated Annotations.ia_is_thread_confined tenv
    pname
  |> Option.is_some


let threadsafe_annotations =
  Annotations.thread_safe :: AnnotationAliases.of_json Config.threadsafe_aliases


(* returns true if the annotation is @ThreadSafe, @ThreadSafe(enableChecks = true), or is defined
   as an alias of @ThreadSafe in a .inferconfig file. *)
let is_thread_safe item_annot =
  let f (annot : Annot.t) =
    List.exists ~f:(Annotations.annot_ends_with annot) threadsafe_annotations
    &&
    match annot.Annot.parameters with
    | [Annot.{name= Some "enableChecks"; value= Bool false}] ->
        false
    | _ ->
        true
  in
  List.exists ~f item_annot


(* returns true if the annotation is @ThreadSafe(enableChecks = false) *)
let is_assumed_thread_safe item_annot =
  let f annot =
    Annotations.annot_ends_with annot Annotations.thread_safe
    &&
    match annot.Annot.parameters with
    | [Annot.{name= Some "enableChecks"; value= Bool false}] ->
        true
    | _ ->
        false
  in
  List.exists ~f item_annot


let is_assumed_thread_safe tenv pname =
  ConcurrencyModels.find_override_or_superclass_annotated is_assumed_thread_safe tenv pname
  |> Option.is_some


(* return true if we should compute a summary for the procedure. if this returns false, we won't
         analyze the procedure or report any warnings on it *)
let should_analyze_proc =
  (* holds of procedure names which should not be analyzed in order to avoid known sources of
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
           ; "std::vector" ] )
    in
    function
    | Procname.ObjC_Cpp cpp_pname as pname ->
        Procname.ObjC_Cpp.is_destructor cpp_pname
        || QualifiedCppName.Match.match_qualifiers (Lazy.force matcher)
             (Procname.get_qualifiers pname)
    | Procname.Java java_pname ->
        Procname.Java.is_autogen_method java_pname
        || Typ.Name.Java.is_external (Procname.Java.get_class_type_name java_pname)
    | _ ->
        false
  in
  fun tenv pn ->
    (not (should_skip pn))
    && (not (FbThreadSafety.is_logging_method pn))
    && not (is_assumed_thread_safe tenv pn)


let get_current_class_and_threadsafe_superclasses tenv pname =
  get_current_class_and_annotated_superclasses is_thread_safe tenv pname


let is_thread_safe_method pname tenv =
  Config.racerd_always_report_java
  ||
  match find_override_or_superclass_annotated is_thread_safe tenv pname with
  | Some (DirectlyAnnotated | Override _) ->
      true
  | _ ->
      false


let is_marked_thread_safe pname tenv =
  ((* current class not marked [@NotThreadSafe] *)
   not
     (PatternMatch.Java.check_current_class_attributes Annotations.ia_is_not_thread_safe tenv pname) )
  && ConcurrencyModels.find_override_or_superclass_annotated is_thread_safe tenv pname
     |> Option.is_some


let is_safe_access (access : 'a MemoryAccess.t) prefix_exp tenv =
  match (access, HilExp.AccessExpression.get_typ prefix_exp tenv) with
  | ( MemoryAccess.FieldAccess fieldname
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


let is_builder_class tname = String.is_suffix ~suffix:"$Builder" (Typ.Name.to_string tname)

let is_builder_method java_pname = is_builder_class (Procname.Java.get_class_type_name java_pname)

let should_flag_interface_call tenv exps call_flags pname =
  let thread_safe_or_thread_confined annot =
    Annotations.ia_is_thread_safe annot || Annotations.ia_is_thread_confined annot
  in
  (* is this function in library code from the JDK core libraries or Android? *)
  let is_java_library java_pname =
    Procname.Java.get_package java_pname
    |> Option.exists ~f:(fun package_name ->
           String.is_prefix ~prefix:"java." package_name
           || String.is_prefix ~prefix:"android." package_name
           || String.is_prefix ~prefix:"com.google." package_name )
  in
  let receiver_is_not_safe exps tenv =
    List.hd exps
    |> Option.bind ~f:(fun exp -> HilExp.get_access_exprs exp |> List.hd)
    |> Option.map ~f:HilExp.AccessExpression.truncate
    |> Option.exists ~f:(function
         | Some (receiver_prefix, receiver_access) ->
             not (is_safe_access receiver_access receiver_prefix tenv)
         | _ ->
             true )
  in
  let implements_threadsafe_interface java_pname tenv =
    (* generated classes implementing this interface are always threadsafe *)
    Procname.Java.get_class_type_name java_pname
    |> fun tname -> PatternMatch.is_subtype_of_str tenv tname "android.os.IInterface"
  in
  match pname with
  | Procname.Java java_pname ->
      call_flags.CallFlags.cf_interface
      && (not (is_java_library java_pname))
      && (not (is_builder_method java_pname))
      (* can't ask anyone to annotate interfaces in library code, and Builders should always be
         thread-safe (would be unreasonable to ask everyone to annotate them) *)
      && ConcurrencyModels.find_override_or_superclass_annotated thread_safe_or_thread_confined tenv
           pname
         |> Option.is_none
      && receiver_is_not_safe exps tenv
      && not (implements_threadsafe_interface java_pname tenv)
  | _ ->
      false


(** Set of standard classes/interfaces that guarantee thread-safe access. This list is heavily
    deduplicated using the inheritance relation as represented in Infer, that is, any class that
    implements/inherits the interfaces/classes below is considered to be thread-safe. For instance,
    all thread-safe maps implement [ConcurrentMap] and thus need not be explicitly represented. On
    the other hand, there is no equivalent interface for sets [ConcurrentSet], so all set-like
    classes have to be listed. Information is mostly drawn from
    https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/package-summary.html *)
let synchronized_container_classes =
  [ "android.support.v4.util.Pools$SynchronizedPool"
  ; "androidx.core.util.Pools$SynchronizedPool"
  ; "java.util.concurrent.BlockingDeque"
  ; "java.util.concurrent.BlockingQueue"
  ; "java.util.concurrent.ConcurrentLinkedDeque"
  ; "java.util.concurrent.ConcurrentMap"
  ; "java.util.concurrent.ConcurrentSkipListSet"
  ; "java.util.concurrent.CopyOnWriteArrayList"
  ; "java.util.concurrent.CopyOnWriteArraySet"
  ; "java.util.Hashtable" ]


let is_converter_to_synchronized_container =
  let open MethodMatcher in
  [ (* any method in [java.util.Collections] that starts with [synchronized] is a wrapper that produces a thread-safe container *)
    {default with classname= "java.util.Collections"; method_prefix= true; methods= ["synchronized"]}
  ; {default with classname= "com.google.common.collect.Maps"; methods= ["newConcurrentMap"]}
  ; {default with classname= "com.google.common.collect.Sets"; methods= ["newConcurrentHashSet"]}
  ; { default with
      classname= "com.google.common.collect.Queues"
    ; methods= ["newConcurrentLinkedQueue"] } ]
  |> of_records


let is_synchronized_container_constructor =
  let open MethodMatcher in
  let default = {default with methods= [Procname.Java.constructor_method_name]} in
  List.map synchronized_container_classes ~f:(fun classname -> {default with classname})
  |> of_records


let is_synchronized_container callee_pname (access_exp : HilExp.AccessExpression.t) tenv =
  let is_threadsafe_collection pn tenv =
    match pn with
    | Procname.Java java_pname ->
        let typename = Procname.Java.get_class_type_name java_pname in
        let aux tn _ =
          List.mem synchronized_container_classes ~equal:String.equal (Typ.Name.name tn)
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
    let module AccessExpression = HilExp.AccessExpression in
    match
      AccessExpression.to_accesses access_exp
      |> snd
      |> List.rev_filter ~f:MemoryAccess.is_field_or_array_access
    with
    | FieldAccess base_field :: FieldAccess container_field :: _ when Procname.is_java callee_pname
      ->
        let base_typename = Fieldname.get_class_name base_field in
        is_annotated_synchronized base_typename container_field tenv
    | [FieldAccess container_field] -> (
      match (AccessExpression.get_base access_exp |> snd).desc with
      | Typ.Tstruct base_typename | Tptr ({Typ.desc= Tstruct base_typename}, _) ->
          is_annotated_synchronized base_typename container_field tenv
      | _ ->
          false )
    | _ ->
        false


let is_initializer tenv proc_name =
  Procname.is_constructor proc_name
  || FbThreadSafety.is_custom_init tenv proc_name
  || PatternMatch.override_exists
       (fun pname -> Annotations.pname_has_return_annot pname Annotations.ia_is_initializer)
       tenv proc_name


let get_current_class_and_superclasses_satisfying_attr_check check tenv pname =
  match pname with
  | Procname.Java java_pname ->
      let current_class = Procname.Java.get_class_type_name java_pname in
      let satisfying_classes =
        PatternMatch.Java.find_superclasses_with_attributes check tenv current_class
      in
      Some (current_class, satisfying_classes)
  | _ ->
      None


module Litho = struct
  let component_annots = ["MountSpec"; "LayoutSpec"]

  let section_annots = ["DiffSectionSpec"; "GroupSectionSpec"]

  let spec_annots = List.append component_annots section_annots

  let get_class_annot pname tenv =
    let helper annot =
      let is_annotated ia = Annotations.ia_ends_with ia annot in
      match get_current_class_and_superclasses_satisfying_attr_check is_annotated tenv pname with
      | Some (current_class, (_ :: _ as classes)) ->
          if List.mem ~equal:Typ.Name.equal classes current_class then Some ("current ", annot)
          else Some ("a super", annot)
      | _ ->
          None
    in
    List.find_map ~f:helper spec_annots


  (* "ann" is typically a suffix of an annotation, so if ann is "LayoutSpec" the developer would have
     written "@LayoutSpec".  qualifier is " this " or "a super", the latter corresponding to
     superclass. *)
  let message (qualifier, ann) =
    let mes1 =
      if List.mem ~equal:String.equal component_annots ann then
        "Litho components are required to be thread safe because of multi-threaded layout."
      else if List.mem ~equal:String.equal section_annots ann then
        "Sections are required to be thread safe because changesets are calculated in the \
         background."
      else (*should not get here*)
        ""
    in
    (* This "round the houses" way of doing things, where developer writes annotation with @, we pass
       around annotation without @, then add it back here, is just because we use ia_ends_with and it
       is not worth adding stuff to annotations.ml to make the code here simpler *)
    Format.asprintf "@\n %s Reporting because %sclass is annotated %a" mes1 qualifier
      MarkupFormatter.pp_monospaced ("@" ^ ann)
end

let get_litho_explanation tenv pname =
  Litho.get_class_annot pname tenv |> Option.map ~f:Litho.message


let class_is_ignored_by_racerd class_name =
  Typ.Name.name class_name |> String.Set.mem Config.racerd_ignore_classes


let proc_is_ignored_by_racerd callee =
  Procname.get_class_type_name callee |> Option.exists ~f:class_is_ignored_by_racerd


let is_kotlin_coroutine_generated classname =
  Tenv.load_global ()
  |> Option.bind ~f:(fun tenv -> Tenv.lookup tenv classname)
  |> Option.exists ~f:(fun (tstruct : Struct.t) ->
         List.mem tstruct.supers ~equal:Typ.Name.equal
           StdTyp.Name.Java.kotlin_coroutines_jvm_internal_restrictedsuspendlambda )
