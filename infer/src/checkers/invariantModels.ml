(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module BuiltinInvariantSet = Caml.Set.Make (String)

type model = Invariant | Variant

let is_invariant = function Invariant -> true | Variant -> false

let invariants =
  BuiltinInvariantSet.of_list
    [ "__instanceof"
    ; "__cast"
    ; "__get_array_length"
    ; "__get_type_of"
    ; "__infer_assume"
    ; "__infer_skip"
    ; "__infer_fail" ]


let invariant_builtins _ s = BuiltinInvariantSet.mem s invariants

let endsWith suffix _ s = String.is_suffix ~suffix s

let startsWith prefix _ s = String.is_prefix ~prefix s

(* matches get*Value *)
let getStarValue tenv s = startsWith "get" tenv s && endsWith "Value" tenv s

module ProcName = struct
  let dispatch : (Tenv.t, model) ProcnameDispatcher.ProcName.dispatcher =
    let open ProcnameDispatcher.ProcName in
    make_dispatcher
      [ +invariant_builtins <>--> Invariant
      ; -"__variable_initialization" <>--> Invariant
      ; +(fun _ name -> BuiltinDecl.is_declared (Typ.Procname.from_string_c_fun name))
        <>--> Variant
      ; +PatternMatch.implements_android "text.TextUtils" &:: "isEmpty" <>--> Invariant
      ; +PatternMatch.implements_android "view.ViewGroup" &:: "getChildAt" <>--> Invariant
      ; +PatternMatch.implements_android "view.View" &::+ startsWith "get" <>--> Invariant
      ; +PatternMatch.implements_android "view.View" &::+ startsWith "findViewById" <>--> Invariant
      ; +PatternMatch.implements_android "view.ViewGroup" &:: "getChildCount" <>--> Invariant
      ; +PatternMatch.implements_android "content.Context" &::+ startsWith "get" <>--> Invariant
      ; +PatternMatch.implements_android "content.res.Resources"
        &::+ startsWith "get" <>--> Invariant
      ; +PatternMatch.implements_lang "Iterable" &:: "iterator" <>--> Invariant
      ; +PatternMatch.implements_collection &:: "iterator" <>--> Invariant
      ; +PatternMatch.implements_iterator &:: "hasNext" <>--> Invariant
      ; +PatternMatch.implements_iterator &:: "next" <>--> Variant
      ; +PatternMatch.implements_iterator &:: "remove" <>--> Variant
      ; +PatternMatch.implements_collection &:: "size" <>--> Invariant
      ; +PatternMatch.implements_collection &:: "add" <>--> Variant
      ; +PatternMatch.implements_collection &:: "addAll" <>--> Variant
      ; +PatternMatch.implements_collection &:: "remove" <>--> Variant
      ; +PatternMatch.implements_collection &:: "isEmpty" <>--> Invariant
      ; +PatternMatch.implements_collection &:: "get" <>--> Invariant
      ; +PatternMatch.implements_collection &:: "set" <>--> Variant
        (* Unlike Set.contains, List.contains is linear *)
      ; +PatternMatch.implements_list &:: "contains" <>--> Invariant
      ; +PatternMatch.implements_collection &:: "contains" <>--> Invariant
      ; +PatternMatch.implements_enumeration &:: "hasMoreElements" <>--> Invariant
      ; +PatternMatch.implements_enumeration &:: "nextElement" <>--> Variant
      ; +PatternMatch.implements_google "common.base.Preconditions"
        &::+ startsWith "check" <>--> Invariant
      ; +PatternMatch.implements_inject "Provider" &:: "get" <>--> Invariant
      ; +PatternMatch.implements_io "OutputStream" &:: "write" <>--> Variant
      ; +PatternMatch.implements_io "InputStream" &:: "read" <>--> Variant
      ; +PatternMatch.implements_io "PrintStream" &:: "print" <>--> Variant
      ; +PatternMatch.implements_io "PrintStream" &:: "println" <>--> Variant
      ; +PatternMatch.implements_io "Reader" &:: "read" <>--> Variant
      ; +PatternMatch.implements_io "BufferedReader" &:: "readLine" <>--> Variant
        (* deserialization is often expensive *)
      ; +PatternMatch.implements_jackson "databind.JsonDeserializer"
        &:: "deserialize" <>--> Invariant
      ; +PatternMatch.implements_jackson "core.JsonParser" &:: "nextToken" <>--> Variant
      ; +PatternMatch.implements_jackson "core.JsonParser" &:: "getCurrentName" <>--> Invariant
      ; +PatternMatch.implements_jackson "core.JsonParser" &::+ getStarValue <>--> Invariant
      ; +PatternMatch.implements_jackson "core.JsonParser" &::+ startsWith "get" <>--> Invariant
      ; +PatternMatch.implements_pseudo_collection &:: "size" <>--> Invariant
      ; +PatternMatch.implements_pseudo_collection &:: "get" <>--> Invariant
      ; +PatternMatch.implements_lang "Math" &::.*--> Invariant (* for (int|short|byte...)Value*)
      ; +PatternMatch.implements_lang "Number" &::+ endsWith "Value" <>--> Invariant
      ; +PatternMatch.implements_lang "Boolean" &:: "valueOf" <>--> Invariant
      ; +PatternMatch.implements_lang "Boolean" &:: "parseBoolean" <>--> Invariant
      ; +PatternMatch.implements_lang "Boolean" &::+ endsWith "Value" <>--> Invariant
      ; +PatternMatch.implements_lang "Number" &:: "valueOf" <>--> Invariant
      ; +PatternMatch.implements_lang "String" &:: "length" <>--> Invariant
      ; +PatternMatch.implements_lang "String" &:: "charAt" <>--> Invariant
        (* substring in Java >= 1.7  has linear complexity *)
      ; +PatternMatch.implements_lang "String" &:: "substring" <>--> Invariant
      ; +PatternMatch.implements_lang "CharSequence" &:: "charAt" <>--> Invariant
      ; +PatternMatch.implements_lang "String" &:: "equals" <>--> Invariant
      ; +PatternMatch.implements_lang "String" &:: "startsWith" <>--> Invariant
      ; +PatternMatch.implements_lang "String" &:: "valueOf" <>--> Invariant
      ; +PatternMatch.implements_lang "String" &:: "replace" <>--> Variant
      ; +PatternMatch.implements_lang "String" &:: "format" <>--> Invariant
        (* String.hashCode is deterministic whereas Object's might not be *)
      ; +PatternMatch.implements_lang "String" &:: "hashCode" <>--> Invariant
      ; +PatternMatch.implements_lang "StringBuilder" &:: "<init>" <>--> Variant
      ; +PatternMatch.implements_lang "StringBuilder" &:: "append" <>--> Variant
      ; +PatternMatch.implements_lang "StringBuilder" &:: "length" <>--> Invariant
      ; +PatternMatch.implements_lang "Object" &:: "equals" <>--> Invariant
      ; +PatternMatch.implements_lang "Object" &:: "toString" <>--> Invariant
      ; +PatternMatch.implements_lang "Object" &:: "getClass" <>--> Invariant
      ; +PatternMatch.implements_lang "Class" &:: "getSimpleName" <>--> Invariant
      ; +PatternMatch.implements_lang "Class" &::+ startsWith "get" <>--> Invariant
      ; +PatternMatch.implements_lang "System" &:: "arraycopy" <>--> Variant
      ; +PatternMatch.implements_lang "Enum" &:: "valueOf" <>--> Invariant
      ; +PatternMatch.implements_lang "Enum" &:: "ordinal" <>--> Invariant
      ; +PatternMatch.implements_map &:: "isEmpty" <>--> Invariant
      ; +PatternMatch.implements_map &:: "get" <>--> Invariant
      ; +PatternMatch.implements_map &:: "put" <>--> Variant
      ; +PatternMatch.implements_map &:: "containsKey" <>--> Invariant
      ; +PatternMatch.implements_map_entry &:: "getKey" <>--> Invariant
      ; +PatternMatch.implements_map_entry &:: "getValue" <>--> Invariant
      ; +PatternMatch.implements_queue &:: "poll" <>--> Variant
      ; +PatternMatch.implements_queue &:: "add" <>--> Variant
      ; +PatternMatch.implements_queue &:: "remove" <>--> Variant
      ; +PatternMatch.implements_queue &:: "peek" <>--> Invariant ]
end
