(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module BuiltinPureMethods = Caml.Set.Make (String)

let pure_builtins =
  List.map ~f:Procname.get_method
    [ BuiltinDecl.__instanceof
    ; BuiltinDecl.__cast
    ; BuiltinDecl.__get_array_length
    ; BuiltinDecl.__set_array_length
    ; BuiltinDecl.__get_type_of
    ; BuiltinDecl.__infer_assume
    ; BuiltinDecl.__infer_initializer_list
    ; BuiltinDecl.__infer_skip
    ; BuiltinDecl.__infer_skip_gcc_asm_stmt
    ; BuiltinDecl.__infer_generic_selection_expr
    ; BuiltinDecl.__infer_skip_function
    ; BuiltinDecl.__infer_fail ]
  |> BuiltinPureMethods.of_list


let modifies_first = PurityDomain.impure_params (PurityDomain.ModifiedParamIndices.of_list [0])

let modifies_third = PurityDomain.impure_params (PurityDomain.ModifiedParamIndices.of_list [2])

let pure_builtins _ s = BuiltinPureMethods.mem s pure_builtins

(* matches get*Value *)
let getStarValue tenv s =
  let open ProcnameDispatcher.ProcName in
  startsWith "get" tenv s && endsWith "Value" tenv s


module ProcName = struct
  let dispatch : (Tenv.t, PurityDomain.t, unit) ProcnameDispatcher.ProcName.dispatcher =
    let open ProcnameDispatcher.ProcName in
    make_dispatcher
      [ +pure_builtins <>--> PurityDomain.pure
      ; -"__variable_initialization" <>--> PurityDomain.pure
      ; +BuiltinDecl.(match_builtin __new) <>--> PurityDomain.pure
      ; +BuiltinDecl.(match_builtin __new_array) <>--> PurityDomain.pure
      ; +BuiltinDecl.(match_builtin __cast) <>--> PurityDomain.pure
      ; -"__variable_initialization" <>--> PurityDomain.pure
      ; +(fun _ name -> BuiltinDecl.is_declared (Procname.from_string_c_fun name))
        <>--> PurityDomain.impure_global
      ; +PatternMatch.ObjectiveC.implements "NSEnumerator" &:: "nextObject" <>--> modifies_first
      ; +PatternMatch.Java.implements_android "text.TextUtils" &:: "isEmpty" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_android "view.ViewGroup"
        &:: "getChildAt" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_android "view.View"
        &::+ startsWith "get" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_android "view.View"
        &::+ startsWith "findViewById" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_android "view.ViewGroup"
        &:: "getChildCount" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_android "content.Context"
        &::+ startsWith "get" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_android "content.res.Resources"
        &::+ startsWith "get" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_arrays &:: "asList" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "Iterable" &:: "iterator" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_list &:: "listIterator" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_collection &:: "iterator" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_iterator &:: "hasNext" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_iterator &:: "next" <>--> modifies_first
      ; +PatternMatch.Java.implements_iterator &:: "remove" <>--> modifies_first
      ; +PatternMatch.Java.implements_collection &:: "size" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_collection &:: "add" <>--> modifies_first
      ; +PatternMatch.Java.implements_collection &:: "addAll" <>--> modifies_first
      ; +PatternMatch.Java.implements_collection &:: "remove" <>--> modifies_first
      ; +PatternMatch.Java.implements_collection &:: "isEmpty" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_collection &:: "get" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_collection &:: "set" <>--> modifies_first
      ; +PatternMatch.Java.implements_nio "Buffer" &::+ startsWith "get" <>--> modifies_first
      ; +PatternMatch.Java.implements_nio "Buffer" &::+ startsWith "put" <>--> modifies_first
      ; +PatternMatch.Java.implements_list &:: "contains" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_collection &:: "contains" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_enumeration &:: "hasMoreElements" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_enumeration &:: "nextElement" <>--> modifies_first
      ; +PatternMatch.Java.implements_google "common.base.Preconditions"
        &::+ startsWith "check" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_inject "Provider" &:: "get" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_io "File" &::+ startsWith "get" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_io "OutputStream" &:: "write" <>--> PurityDomain.impure_global
      ; +PatternMatch.Java.implements_io "InputStream" &:: "read" <>--> PurityDomain.impure_global
      ; +PatternMatch.Java.implements_io "PrintStream" &:: "print" <>--> PurityDomain.impure_global
      ; +PatternMatch.Java.implements_io "PrintStream"
        &:: "println" <>--> PurityDomain.impure_global
      ; +PatternMatch.Java.implements_io "Reader" &:: "read" <>--> PurityDomain.impure_global
      ; +PatternMatch.Java.implements_io "BufferedReader"
        &:: "readLine" <>--> PurityDomain.impure_global
      ; +PatternMatch.Java.implements_jackson "databind.JsonDeserializer"
        &:: "deserialize" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_jackson "core.JsonParser" &:: "nextToken" <>--> modifies_first
      ; +PatternMatch.Java.implements_jackson "core.JsonParser"
        &:: "getCurrentName" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_jackson "core.JsonParser"
        &::+ getStarValue <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_jackson "core.JsonParser"
        &::+ startsWith "get" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_pseudo_collection &:: "size" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_pseudo_collection &:: "get" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_pseudo_collection &:: "valueAt" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "Math" &:: "random" <>--> PurityDomain.impure_global
      ; +PatternMatch.Java.implements_lang "Math" &::.*--> PurityDomain.pure
        (* for (int|short|byte...)Value*)
      ; +PatternMatch.Java.implements_lang "Number" &::+ endsWith "Value" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "Boolean" &:: "valueOf" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "Boolean" &:: "parseBoolean" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "Boolean" &::+ endsWith "Value" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "Number" &:: "valueOf" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "String" &:: "length" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "String" &:: "charAt" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "String" &:: "substring" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "CharSequence" &:: "charAt" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "String" &:: "equals" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "String" &:: "startsWith" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "String" &:: "valueOf" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "String" &:: "replace" <>--> modifies_first
      ; +PatternMatch.Java.implements_lang "String" &:: "format" <>--> PurityDomain.pure
        (* String.hashCode is deterministic whereas Object's might not be *)
      ; +PatternMatch.Java.implements_lang "String" &:: "hashCode" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "StringBuilder" &:: "<init>" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "StringBuilder" &:: "append" <>--> modifies_first
      ; +PatternMatch.Java.implements_lang "StringBuilder" &:: "length" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "Object" &:: "clone" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "Object" &:: "equals" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "Object" &:: "toString" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "Object" &:: "getClass" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "Class" &:: "getSimpleName" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "Class" &::+ startsWith "get" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "System" &:: "arraycopy" <>--> modifies_third
      ; +PatternMatch.Java.implements_lang "Enum" &:: "valueOf" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_lang "Enum" &:: "ordinal" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_map &:: "isEmpty" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_map &:: "get" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_map &:: "put" <>--> modifies_first
      ; +PatternMatch.Java.implements_map &:: "putAll" <>--> modifies_first
      ; +PatternMatch.Java.implements_map &:: "containsKey" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_map &:: "keySet" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_map &:: "values" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_map &:: "entrySet" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_map &:: "size" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_map_entry &:: "getKey" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_map_entry &:: "getValue" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_queue &:: "poll" <>--> modifies_first
      ; +PatternMatch.Java.implements_queue &:: "add" <>--> modifies_first
      ; +PatternMatch.Java.implements_queue &:: "remove" <>--> modifies_first
      ; +PatternMatch.Java.implements_queue &:: "peek" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_list &:: "subList" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_arrays &:: "binarySearch" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_org_json "JSONArray"
        &::+ startsWith "get" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_org_json "JSONObject"
        &::+ startsWith "get" <>--> PurityDomain.pure
      ; +PatternMatch.Java.implements_org_json "JSONArray" &:: "length" <>--> PurityDomain.pure ]
end
