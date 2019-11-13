(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = {nullability: Nullability.t; origin: TypeOrigin.t} [@@deriving compare]

let get_nullability {nullability} = nullability

let is_nonnull_or_declared_nonnull {nullability} =
  match nullability with Nullable -> false | DeclaredNonnull -> true | Nonnull -> true


let is_nonnull {nullability} =
  match nullability with Nullable -> false | DeclaredNonnull -> false | Nonnull -> true


let set_nullability nullability t = {t with nullability}

let set_nonnull = set_nullability Nullability.Nonnull

let descr_origin t =
  let descr_opt = TypeOrigin.get_description t.origin in
  match descr_opt with
  | None ->
      ("", None, None)
  | Some (str, loc_opt, sig_opt) ->
      ("(Origin: " ^ str ^ ")", loc_opt, sig_opt)


let to_string {nullability} = Printf.sprintf "[%s]" (Nullability.to_string nullability)

let join t1 t2 =
  let joined_nullability = Nullability.join t1.nullability t2.nullability in
  let is_equal_to_t1 = Nullability.equal t1.nullability joined_nullability in
  let is_equal_to_t2 = Nullability.equal t2.nullability joined_nullability in
  (* Origin complements nullability information. It is the best effort to explain how was the nullability inferred.
     If nullability is fully determined by one of the arguments, origin should be get from this argument.
     Otherwise we apply heuristics to choose origin either from t1 or t2.
  *)
  let joined_origin =
    match (is_equal_to_t1, is_equal_to_t2) with
    | true, false ->
        (* Nullability was fully determined by t1. *)
        t1.origin
    | false, true ->
        (* Nullability was fully determined by t2 *)
        t2.origin
    | false, false | true, true ->
        (* Nullability is not fully determined by neither t1 nor t2
           Let TypeOrigin logic to decide what to prefer in this case.
        *)
        TypeOrigin.join t1.origin t2.origin
  in
  {nullability= joined_nullability; origin= joined_origin}


let get_origin t = t.origin

let origin_is_fun_library t =
  match get_origin t with
  | TypeOrigin.Proc proc_origin ->
      proc_origin.TypeOrigin.is_library
  | _ ->
      false


let create_nullable origin = {origin; nullability= Nullability.Nullable}

let create_nonnull origin = {origin; nullability= Nullability.Nonnull}

let of_annotated_nullability annotated_nullability origin =
  match annotated_nullability with
  | AnnotatedNullability.Nullable _ ->
      {origin; nullability= Nullability.Nullable}
  | AnnotatedNullability.DeclaredNonnull _ ->
      {origin; nullability= Nullability.DeclaredNonnull}
  | AnnotatedNullability.Nonnull _ ->
      {origin; nullability= Nullability.Nonnull}
