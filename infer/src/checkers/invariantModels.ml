(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module BuiltinInvariantSet = Caml.Set.Make (String)

type model = Invariant | Variant | VariantForHoisting

let is_invariant = function Invariant -> true | VariantForHoisting -> true | Variant -> false

let is_variant_for_hoisting = function VariantForHoisting -> true | _ -> false

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

module Call = struct
  let dispatch : (Tenv.t, model) ProcnameDispatcher.Call.dispatcher =
    let open ProcnameDispatcher.Call in
    make_dispatcher
      [ +invariant_builtins <>--> VariantForHoisting
      ; +(fun _ name -> BuiltinDecl.is_declared (Typ.Procname.from_string_c_fun name))
        <>--> Variant
      ; +PatternMatch.implements_android "text.TextUtils" &:: "isEmpty" <>--> VariantForHoisting
      ; +PatternMatch.implements_android "view.ViewGroup" &:: "getChildAt" <>--> VariantForHoisting
      ; +PatternMatch.implements_android "view.View" &::+ startsWith "get" <>--> VariantForHoisting
      ; +PatternMatch.implements_android "view.View"
        &::+ startsWith "findViewById" <>--> VariantForHoisting
      ; +PatternMatch.implements_android "view.ViewGroup"
        &:: "getChildCount" <>--> VariantForHoisting
      ; +PatternMatch.implements_android "content.Context"
        &::+ startsWith "get" <>--> VariantForHoisting
      ; +PatternMatch.implements_android "content.res.Resources"
        &::+ startsWith "get" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "Iterable" &:: "iterator" <>--> VariantForHoisting
      ; +PatternMatch.implements_collection &:: "iterator" <>--> VariantForHoisting
      ; +PatternMatch.implements_iterator &:: "hasNext" <>--> VariantForHoisting
      ; +PatternMatch.implements_iterator &:: "next" <>--> Variant
      ; +PatternMatch.implements_iterator &:: "remove" <>--> Variant
      ; +PatternMatch.implements_collection &:: "size" <>--> VariantForHoisting
      ; +PatternMatch.implements_collection &:: "add" <>--> Variant
      ; +PatternMatch.implements_collection &:: "addAll" <>--> Variant
      ; +PatternMatch.implements_collection &:: "remove" <>--> Variant
      ; +PatternMatch.implements_collection &:: "isEmpty" <>--> VariantForHoisting
      ; +PatternMatch.implements_collection &:: "get" <>--> VariantForHoisting
      ; +PatternMatch.implements_collection &:: "set" <>--> Variant
        (* Unlike Set.contains, List.contains is linear *)
      ; +PatternMatch.implements_list &:: "contains" <>--> Invariant
      ; +PatternMatch.implements_collection &:: "contains" <>--> VariantForHoisting
      ; +PatternMatch.implements_enumeration &:: "hasMoreElements" <>--> VariantForHoisting
      ; +PatternMatch.implements_enumeration &:: "nextElement" <>--> Variant
      ; +PatternMatch.implements_google "common.base.Preconditions"
        &::+ startsWith "check" <>--> VariantForHoisting
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
      ; +PatternMatch.implements_jackson "core.JsonParser"
        &:: "getCurrentName" <>--> VariantForHoisting
      ; +PatternMatch.implements_jackson "core.JsonParser"
        &::+ getStarValue <>--> VariantForHoisting
      ; +PatternMatch.implements_jackson "core.JsonParser"
        &::+ startsWith "get" <>--> VariantForHoisting
      ; +PatternMatch.implements_pseudo_collection &:: "size" <>--> VariantForHoisting
      ; +PatternMatch.implements_pseudo_collection &:: "get" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "Math" &::.*--> VariantForHoisting
        (* for (int|short|byte...)Value*)
      ; +PatternMatch.implements_lang "Number" &::+ endsWith "Value" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "Boolean" &:: "valueOf" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "Boolean" &:: "parseBoolean" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "Boolean" &::+ endsWith "Value" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "Number" &:: "valueOf" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "String" &:: "length" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "String" &:: "charAt" <>--> VariantForHoisting
        (* substring in Java >= 1.7  has linear complexity *)
      ; +PatternMatch.implements_lang "String" &:: "substring" <>--> Invariant
      ; +PatternMatch.implements_lang "CharSequence" &:: "charAt" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "String" &:: "equals" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "String" &:: "startsWith" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "String" &:: "valueOf" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "String" &:: "replace" <>--> Variant
      ; +PatternMatch.implements_lang "String" &:: "format" <>--> VariantForHoisting
        (* String.hashCode is deterministic whereas Object's might not be *)
      ; +PatternMatch.implements_lang "String" &:: "hashCode" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "StringBuilder" &:: "<init>" <>--> Variant
      ; +PatternMatch.implements_lang "StringBuilder" &:: "append" <>--> Variant
      ; +PatternMatch.implements_lang "StringBuilder" &:: "length" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "Object" &:: "equals" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "Object" &:: "toString" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "Object" &:: "getClass" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "Class" &:: "getSimpleName" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "Class" &::+ startsWith "get" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "System" &:: "arraycopy" <>--> Variant
      ; +PatternMatch.implements_lang "Enum" &:: "valueOf" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "Enum" &:: "ordinal" <>--> VariantForHoisting
      ; +PatternMatch.implements_map &:: "isEmpty" <>--> VariantForHoisting
      ; +PatternMatch.implements_map &:: "get" <>--> VariantForHoisting
      ; +PatternMatch.implements_map &:: "put" <>--> Variant
      ; +PatternMatch.implements_map &:: "containsKey" <>--> VariantForHoisting
      ; +PatternMatch.implements_map_entry &:: "getKey" <>--> VariantForHoisting
      ; +PatternMatch.implements_map_entry &:: "getValue" <>--> VariantForHoisting
      ; +PatternMatch.implements_queue &:: "poll" <>--> Variant
      ; +PatternMatch.implements_queue &:: "add" <>--> Variant
      ; +PatternMatch.implements_queue &:: "remove" <>--> Variant
      ; +PatternMatch.implements_queue &:: "peek" <>--> VariantForHoisting ]
end
