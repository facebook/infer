(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
module F = Format
module L = Logging

module AnnotationAliases = struct
  let of_json = function
    | `List aliases ->
        List.map ~f:Yojson.Basic.Util.to_string aliases
    | _ ->
        L.(die UserError)
          "Couldn't parse thread-safety annotation aliases; expected list of strings"

end

module Models = struct
  type lock = Lock | Unlock | LockedIfTrue | NoEffect

  type thread = BackgroundThread | MainThread | MainThreadIfTrue | UnknownThread

  type container_access = ContainerRead | ContainerWrite

  let is_thread_utils_type java_pname =
    let pn = Typ.Procname.java_get_class_name java_pname in
    String.is_suffix ~suffix:"ThreadUtils" pn || String.is_suffix ~suffix:"ThreadUtil" pn


  let is_thread_utils_method method_name_str = function
    | Typ.Procname.Java java_pname ->
        is_thread_utils_type java_pname
        && String.equal (Typ.Procname.java_get_method java_pname) method_name_str
    | _ ->
        false


  let get_thread = function
    | Typ.Procname.Java java_pname when is_thread_utils_type java_pname -> (
      match Typ.Procname.java_get_method java_pname with
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
          ; "std::unique_lock::lock" ]
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
          [ "apache::thrift::concurrency::ReadWriteMutex::release"
          ; "folly::MicroSpinLock::unlock"
          ; "folly::RWSpinLock::unlock"
          ; "folly::RWSpinLock::unlock_shared"
          ; "folly::SharedMutexImpl::unlock"
          ; "folly::SharedMutexImpl::unlock_shared"
          ; "std::lock_guard::~lock_guard"
          ; "std::mutex::unlock"
          ; "std::unique_lock::unlock"
          ; "std::unique_lock::~unique_lock" ]
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
              , ("lock" | "lockInterruptibly") ) ->
                Lock
            | ( ( "java.util.concurrent.locks.Lock" | "java.util.concurrent.locks.ReentrantLock"
                | "java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock"
                | "java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock" )
              , "unlock" ) ->
                Unlock
            | ( ( "java.util.concurrent.locks.Lock" | "java.util.concurrent.locks.ReentrantLock"
                | "java.util.concurrent.locks.ReentrantReadWriteLock$ReadLock"
                | "java.util.concurrent.locks.ReentrantReadWriteLock$WriteLock" )
              , "tryLock" ) ->
                LockedIfTrue
            | ( "com.facebook.buck.util.concurrent.AutoCloseableReadWriteUpdateLock"
              , ("readLock" | "updateLock" | "writeLock") ) ->
                Lock
            | _ ->
                NoEffect )
      | Typ.Procname.ObjC_Cpp _ as pname when is_cpp_lock pname actuals ->
          Lock
      | Typ.Procname.ObjC_Cpp _ as pname when is_cpp_unlock pname ->
          Unlock
      | Typ.Procname.ObjC_Cpp _ as pname when is_cpp_trylock pname ->
          LockedIfTrue
      | pname when Typ.Procname.equal pname BuiltinDecl.__set_locked_attribute ->
          Lock
      | pname when Typ.Procname.equal pname BuiltinDecl.__delete_locked_attribute ->
          Unlock
      | _ ->
          NoEffect


  let get_container_access =
    let is_cpp_container_read =
      let matcher = QualifiedCppName.Match.of_fuzzy_qual_names ["std::map::find"] in
      fun pname ->
        QualifiedCppName.Match.match_qualifiers matcher (Typ.Procname.get_qualifiers pname)
    and is_cpp_container_write =
      let matcher =
        QualifiedCppName.Match.of_fuzzy_qual_names ["std::map::operator[]"; "std::map::erase"]
      in
      fun pname ->
        QualifiedCppName.Match.match_qualifiers matcher (Typ.Procname.get_qualifiers pname)
    in
    fun pn tenv ->
      match pn with
      | Typ.Procname.Java java_pname ->
          let typename = Typ.Name.Java.from_string (Typ.Procname.java_get_class_name java_pname) in
          let get_container_access_ typename =
            match (Typ.Name.name typename, Typ.Procname.java_get_method java_pname) with
            | ( ("android.util.SparseArray" | "android.support.v4.util.SparseArrayCompat")
              , ( "append" | "clear" | "delete" | "put" | "remove" | "removeAt" | "removeAtRange"
                | "setValueAt" ) ) ->
                Some ContainerWrite
            | ( ("android.util.SparseArray" | "android.support.v4.util.SparseArrayCompat")
              , ("clone" | "get" | "indexOfKey" | "indexOfValue" | "keyAt" | "size" | "valueAt") ) ->
                Some ContainerRead
            | ( "android.support.v4.util.SimpleArrayMap"
              , ( "clear" | "ensureCapacity" | "put" | "putAll" | "remove" | "removeAt"
                | "setValueAt" ) ) ->
                Some ContainerWrite
            | ( "android.support.v4.util.SimpleArrayMap"
              , ( "containsKey" | "containsValue" | "get" | "hashCode" | "indexOfKey" | "isEmpty"
                | "keyAt" | "size" | "valueAt" ) ) ->
                Some ContainerRead
            | "android.support.v4.util.Pools$SimplePool", ("acquire" | "release") ->
                Some ContainerWrite
            | "java.util.List", ("add" | "addAll" | "clear" | "remove" | "set") ->
                Some ContainerWrite
            | ( "java.util.List"
              , ( "contains" | "containsAll" | "equals" | "get" | "hashCode" | "indexOf"
                | "isEmpty" | "iterator" | "lastIndexOf" | "listIterator" | "size" | "toArray" ) ) ->
                Some ContainerRead
            | "java.util.Map", ("clear" | "put" | "putAll" | "remove") ->
                Some ContainerWrite
            | ( "java.util.Map"
              , ( "containsKey" | "containsValue" | "entrySet" | "equals" | "get" | "hashCode"
                | "isEmpty" | "keySet" | "size" | "values" ) ) ->
                Some ContainerRead
            | _ ->
                None
          in
          PatternMatch.supertype_find_map_opt tenv get_container_access_ typename
      | Typ.Procname.ObjC_Cpp _ as pname when is_cpp_container_read pname ->
          Some ContainerRead
      | Typ.Procname.ObjC_Cpp _ as pname when is_cpp_container_write pname ->
          Some ContainerWrite
      | _ ->
          None


  let should_skip =
    let matcher =
      lazy
        (QualifiedCppName.Match.of_fuzzy_qual_names
           [ "folly::AtomicStruct::AtomicStruct"
           ; "folly::Future::Future"
           ; "folly::LockedPtr::LockedPtr"
           ; "folly::Optional::Optional"
           ; "folly::Optional::hasValue"
           ; "folly::Promise::Promise"
           ; "folly::ThreadLocal::ThreadLocal"
           ; "folly::detail::SingletonHolder::createInstance"
           ; "std::atomic"
           ; "std::vector::vector" ])
    in
    function
      | Typ.Procname.ObjC_Cpp _ | C _ as pname ->
          Typ.Procname.is_destructor pname
          || QualifiedCppName.Match.match_qualifiers (Lazy.force matcher)
               (Typ.Procname.get_qualifiers pname)
      | _ ->
          false

end
