(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let is_object_nullability_self_explanatory ~object_expression object_origin =
  (* Fundamentally, object can be of two kinds:
     1. Indirect: local variable that was instantiated before.
     In this case, normally origin is NOT trivial
     (because for complex flows it might be tricky to figure out where the offending
     initialization came from, and it is a good idea to at least point to the line).
     2. Direct: some sort of expression. In this case, this expression itself is often
     self-explanatory and it is OK to be offended.Infer
     NOTE: code below is heuristic, it indends to cover most practical cases and does not try
     to be 100% precise.
  *)
  match object_origin with
  | TypeOrigin.NullConst _ ->
      (* Expect either a local variable or null literal (latter case is trivial) *)
      String.equal object_expression "null"
  | TypeOrigin.MethodParameter {mangled} ->
      (* Either local variable or literally parameter. In latter case, its nullability is
         self-explanatory because the user can quickly see the current method signature.
      *)
      let param_name = Mangled.to_string mangled in
      String.equal object_expression param_name
  | TypeOrigin.Field {field_name} ->
      (* Either local variable or expression like `<smth>.field_name`. Latter case is trivial:
         the user can quickly go to field_name definition and see if its annotation. *)
      let field_name_str = Typ.Fieldname.to_flat_string field_name in
      String.is_suffix object_expression ~suffix:field_name_str
  | TypeOrigin.MethodCall {pname} ->
      (* TODO: take into account external models as well *)
      let is_modelled = Models.is_modelled_for_nullability_as_internal pname in
      if is_modelled then (* This is non-trivial and should always be explained to the user *)
        false
      else
        (* Either local variable or expression like <smth>.method_name(...).
           Latter case is self-explanatory: it is easy to the user to jump to definition
           and check out the method annotation.
        *)
        let method_name = Typ.Procname.to_simplified_string pname in
        String.is_suffix object_expression ~suffix:method_name
  (* These cases are not yet supported because they normally mean non-nullable case, for which
     we don't render important messages yet.
  *)
  | TypeOrigin.NonnullConst _
  | TypeOrigin.This
  | TypeOrigin.New
  | TypeOrigin.ArrayLengthResult
  | TypeOrigin.ArrayAccess
  | TypeOrigin.InferredNonnull _
  | TypeOrigin.OptimisticFallback
  | TypeOrigin.Undef ->
      false
