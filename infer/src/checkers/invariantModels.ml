(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type model = Variant | VariantForHoisting

(* Even though functions marked with VariantForHoisting are pure, we
   don't want to report them in hoisting. Hence, we model them as
   Variant. *)
let is_invariant = function VariantForHoisting -> not Config.loop_hoisting | Variant -> false

module Call = struct
  let dispatch : (Tenv.t, model) ProcnameDispatcher.Call.dispatcher =
    let open ProcnameDispatcher.Call in
    make_dispatcher
      [ -"__cast" <>--> VariantForHoisting
      ; +PatternMatch.implements_collection &:: "iterator" <>--> Variant
      ; +PatternMatch.implements_iterator &:: "hasNext" <>--> Variant
      ; +PatternMatch.implements_enumeration &:: "hasMoreElements" <>--> Variant
      ; +PatternMatch.implements_enumeration &:: "nextElement" <>--> Variant
      ; +PatternMatch.implements_iterator &:: "next" <>--> Variant
      ; +PatternMatch.implements_collection &:: "size" <>--> VariantForHoisting
      ; +PatternMatch.implements_collection &:: "add" <>--> Variant
      ; +PatternMatch.implements_collection &:: "remove" <>--> Variant
      ; +PatternMatch.implements_collection &:: "isEmpty" <>--> VariantForHoisting
      ; +PatternMatch.implements_collection &:: "get" <>--> VariantForHoisting
      ; +PatternMatch.implements_collection &:: "set" <>--> Variant
      ; +PatternMatch.implements_pseudo_collection &:: "size" <>--> VariantForHoisting
      ; +PatternMatch.implements_pseudo_collection &:: "get" <>--> VariantForHoisting
      ; +PatternMatch.implements_map &:: "isEmpty" <>--> VariantForHoisting
      ; +PatternMatch.implements_map &:: "put" <>--> Variant
      ; +PatternMatch.implements_queue &:: "poll" <>--> Variant
      ; +PatternMatch.implements_queue &:: "add" <>--> Variant
      ; +PatternMatch.implements_queue &:: "remove" <>--> Variant
      ; +PatternMatch.implements_queue &:: "peek" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "Boolean" &:: "valueOf" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "Number" &:: "valueOf" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "Boolean" &:: "toString" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "Number" &:: "toString" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "String" &:: "length" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "String" &:: "equals" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "String" &:: "startsWith" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "String" &:: "valueOf" <>--> VariantForHoisting
      ; +PatternMatch.implements_lang "String" &:: "replace" <>--> Variant
      ; +PatternMatch.implements_lang "StringBuilder" &:: "append" <>--> Variant
      ; +PatternMatch.implements_lang "StringBuilder" &:: "length" <>--> VariantForHoisting
      ; +PatternMatch.implements_io "OutputStream" &:: "write" <>--> Variant
      ; +PatternMatch.implements_io "InputStream" &:: "read" <>--> Variant
      ; +PatternMatch.implements_io "PrintStream" &:: "print" <>--> Variant
      ; +PatternMatch.implements_io "PrintStream" &:: "println" <>--> Variant
      ; +PatternMatch.implements_io "Reader" &:: "read" <>--> Variant
      ; +PatternMatch.implements_io "BufferedReader" &:: "readLine" <>--> Variant
      ; -"__new" <>--> Variant
      ; +(fun _ name -> BuiltinDecl.is_declared (Typ.Procname.from_string_c_fun name))
        <>--> VariantForHoisting ]
end
