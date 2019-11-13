(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module BuiltinPureMethods = Caml.Set.Make (String)

let pure_builtins =
  BuiltinPureMethods.of_list
    [ "__instanceof"
    ; "__cast"
    ; "__get_array_length"
    ; "__set_array_length"
    ; "__get_type_of"
    ; "__infer_assume"
    ; "__infer_skip"
    ; "__infer_fail" ]


let modifies_first = PurityDomain.impure_params (PurityDomain.ModifiedParamIndices.of_list [0])

let modifies_third = PurityDomain.impure_params (PurityDomain.ModifiedParamIndices.of_list [2])

let pure_builtins _ s = BuiltinPureMethods.mem s pure_builtins

let endsWith suffix _ s = String.is_suffix ~suffix s

let startsWith prefix _ s = String.is_prefix ~prefix s

(* matches get*Value *)
let getStarValue tenv s = startsWith "get" tenv s && endsWith "Value" tenv s

module ProcName = struct
  let dispatch : (Tenv.t, PurityDomain.t) ProcnameDispatcher.ProcName.dispatcher =
    let open ProcnameDispatcher.ProcName in
    make_dispatcher
      [ +pure_builtins <>--> PurityDomain.pure
      ; -"__variable_initialization" <>--> PurityDomain.pure
      ; -"__new" <>--> PurityDomain.pure
      ; -"__new_array" <>--> PurityDomain.pure
      ; -"__cast" <>--> PurityDomain.pure
      ; -"__variable_initialization" <>--> PurityDomain.pure
      ; +(fun _ name -> BuiltinDecl.is_declared (Typ.Procname.from_string_c_fun name))
        <>--> PurityDomain.impure_global
      ; +PatternMatch.implements_android "text.TextUtils" &:: "isEmpty" <>--> PurityDomain.pure
      ; +PatternMatch.implements_android "view.ViewGroup" &:: "getChildAt" <>--> PurityDomain.pure
      ; +PatternMatch.implements_android "view.View" &::+ startsWith "get" <>--> PurityDomain.pure
      ; +PatternMatch.implements_android "view.View"
        &::+ startsWith "findViewById" <>--> PurityDomain.pure
      ; +PatternMatch.implements_android "view.ViewGroup"
        &:: "getChildCount" <>--> PurityDomain.pure
      ; +PatternMatch.implements_android "content.Context"
        &::+ startsWith "get" <>--> PurityDomain.pure
      ; +PatternMatch.implements_android "content.res.Resources"
        &::+ startsWith "get" <>--> PurityDomain.pure
      ; +PatternMatch.implements_arrays &:: "asList" <>--> PurityDomain.pure
      ; +PatternMatch.implements_lang "Iterable" &:: "iterator" <>--> PurityDomain.pure
      ; +PatternMatch.implements_list &:: "listIterator" <>--> PurityDomain.pure
      ; +PatternMatch.implements_collection &:: "iterator" <>--> PurityDomain.pure
      ; +PatternMatch.implements_iterator &:: "hasNext" <>--> PurityDomain.pure
      ; +PatternMatch.implements_iterator &:: "next" <>--> modifies_first
      ; +PatternMatch.implements_iterator &:: "remove" <>--> modifies_first
      ; +PatternMatch.implements_collection &:: "size" <>--> PurityDomain.pure
      ; +PatternMatch.implements_map &:: "size" <>--> PurityDomain.pure
      ; +PatternMatch.implements_collection &:: "add" <>--> modifies_first
      ; +PatternMatch.implements_collection &:: "addAll" <>--> modifies_first
      ; +PatternMatch.implements_collection &:: "remove" <>--> modifies_first
      ; +PatternMatch.implements_collection &:: "isEmpty" <>--> PurityDomain.pure
      ; +PatternMatch.implements_collection &:: "get" <>--> PurityDomain.pure
      ; +PatternMatch.implements_collection &:: "set" <>--> modifies_first
        (* Unlike Set.contains, List.contains is linear *)
      ; +PatternMatch.implements_list &:: "contains" <>--> PurityDomain.pure
      ; +PatternMatch.implements_collection &:: "contains" <>--> PurityDomain.pure
      ; +PatternMatch.implements_enumeration &:: "hasMoreElements" <>--> PurityDomain.pure
      ; +PatternMatch.implements_enumeration &:: "nextElement" <>--> modifies_first
      ; +PatternMatch.implements_google "common.base.Preconditions"
        &::+ startsWith "check" <>--> PurityDomain.pure
      ; +PatternMatch.implements_inject "Provider" &:: "get" <>--> PurityDomain.pure
      ; +PatternMatch.implements_io "OutputStream" &:: "write" <>--> PurityDomain.impure_global
      ; +PatternMatch.implements_io "InputStream" &:: "read" <>--> PurityDomain.impure_global
      ; +PatternMatch.implements_io "PrintStream" &:: "print" <>--> PurityDomain.impure_global
      ; +PatternMatch.implements_io "PrintStream" &:: "println" <>--> PurityDomain.impure_global
      ; +PatternMatch.implements_io "Reader" &:: "read" <>--> PurityDomain.impure_global
      ; +PatternMatch.implements_io "BufferedReader" &:: "readLine" <>--> PurityDomain.impure_global
        (* deserialization is often expensive *)
      ; +PatternMatch.implements_jackson "databind.JsonDeserializer"
        &:: "deserialize" <>--> PurityDomain.pure
      ; +PatternMatch.implements_jackson "core.JsonParser" &:: "nextToken" <>--> modifies_first
      ; +PatternMatch.implements_jackson "core.JsonParser"
        &:: "getCurrentName" <>--> PurityDomain.pure
      ; +PatternMatch.implements_jackson "core.JsonParser" &::+ getStarValue <>--> PurityDomain.pure
      ; +PatternMatch.implements_jackson "core.JsonParser"
        &::+ startsWith "get" <>--> PurityDomain.pure
      ; +PatternMatch.implements_pseudo_collection &:: "size" <>--> PurityDomain.pure
      ; +PatternMatch.implements_pseudo_collection &:: "get" <>--> PurityDomain.pure
      ; +PatternMatch.implements_lang "Math" &:: "random" <>--> PurityDomain.impure_global
      ; +PatternMatch.implements_lang "Math" &::.*--> PurityDomain.pure
        (* for (int|short|byte...)Value*)
      ; +PatternMatch.implements_lang "Number" &::+ endsWith "Value" <>--> PurityDomain.pure
      ; +PatternMatch.implements_lang "Boolean" &:: "valueOf" <>--> PurityDomain.pure
      ; +PatternMatch.implements_lang "Boolean" &:: "parseBoolean" <>--> PurityDomain.pure
      ; +PatternMatch.implements_lang "Boolean" &::+ endsWith "Value" <>--> PurityDomain.pure
      ; +PatternMatch.implements_lang "Number" &:: "valueOf" <>--> PurityDomain.pure
      ; +PatternMatch.implements_lang "String" &:: "length" <>--> PurityDomain.pure
      ; +PatternMatch.implements_lang "String" &:: "charAt" <>--> PurityDomain.pure
        (* substring in Java >= 1.7  has linear complexity *)
      ; +PatternMatch.implements_lang "String" &:: "substring" <>--> PurityDomain.pure
      ; +PatternMatch.implements_lang "CharSequence" &:: "charAt" <>--> PurityDomain.pure
      ; +PatternMatch.implements_lang "String" &:: "equals" <>--> PurityDomain.pure
      ; +PatternMatch.implements_lang "String" &:: "startsWith" <>--> PurityDomain.pure
      ; +PatternMatch.implements_lang "String" &:: "valueOf" <>--> PurityDomain.pure
      ; +PatternMatch.implements_lang "String" &:: "replace" <>--> modifies_first
      ; +PatternMatch.implements_lang "String" &:: "format" <>--> PurityDomain.pure
        (* String.hashCode is deterministic whereas Object's might not be *)
      ; +PatternMatch.implements_lang "String" &:: "hashCode" <>--> PurityDomain.pure
      ; +PatternMatch.implements_lang "StringBuilder" &:: "<init>" <>--> PurityDomain.pure
      ; +PatternMatch.implements_lang "StringBuilder" &:: "append" <>--> modifies_first
      ; +PatternMatch.implements_lang "StringBuilder" &:: "length" <>--> PurityDomain.pure
      ; +PatternMatch.implements_lang "Object" &:: "equals" <>--> PurityDomain.pure
      ; +PatternMatch.implements_lang "Object" &:: "toString" <>--> PurityDomain.pure
      ; +PatternMatch.implements_lang "Object" &:: "getClass" <>--> PurityDomain.pure
      ; +PatternMatch.implements_lang "Class" &:: "getSimpleName" <>--> PurityDomain.pure
      ; +PatternMatch.implements_lang "Class" &::+ startsWith "get" <>--> PurityDomain.pure
      ; +PatternMatch.implements_lang "System" &:: "arraycopy" <>--> modifies_third
      ; +PatternMatch.implements_lang "Enum" &:: "valueOf" <>--> PurityDomain.pure
      ; +PatternMatch.implements_lang "Enum" &:: "ordinal" <>--> PurityDomain.pure
      ; +PatternMatch.implements_map &:: "isEmpty" <>--> PurityDomain.pure
      ; +PatternMatch.implements_map &:: "get" <>--> PurityDomain.pure
      ; +PatternMatch.implements_map &:: "put" <>--> modifies_first
      ; +PatternMatch.implements_map &:: "putAll" <>--> modifies_first
      ; +PatternMatch.implements_map &:: "containsKey" <>--> PurityDomain.pure
      ; +PatternMatch.implements_map &:: "keySet" <>--> PurityDomain.pure
      ; +PatternMatch.implements_map &:: "values" <>--> PurityDomain.pure
      ; +PatternMatch.implements_map &:: "entrySet" <>--> PurityDomain.pure
      ; +PatternMatch.implements_map &:: "size" <>--> PurityDomain.pure
      ; +PatternMatch.implements_map_entry &:: "getKey" <>--> PurityDomain.pure
      ; +PatternMatch.implements_map_entry &:: "getValue" <>--> PurityDomain.pure
      ; +PatternMatch.implements_queue &:: "poll" <>--> modifies_first
      ; +PatternMatch.implements_queue &:: "add" <>--> modifies_first
      ; +PatternMatch.implements_queue &:: "remove" <>--> modifies_first
      ; +PatternMatch.implements_queue &:: "peek" <>--> PurityDomain.pure
      ; +PatternMatch.implements_list &:: "subList" <>--> PurityDomain.pure
      ; +PatternMatch.implements_arrays &:: "binarySearch" <>--> PurityDomain.pure
      ; +PatternMatch.implements_org_json "JSONArray" &::+ startsWith "get" <>--> PurityDomain.pure
      ; +PatternMatch.implements_org_json "JSONObject" &::+ startsWith "get" <>--> PurityDomain.pure
      ; +PatternMatch.implements_org_json "JSONArray" &:: "length" <>--> PurityDomain.pure ]
end
