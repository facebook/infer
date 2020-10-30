(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t =
  { nullability: Nullability.t
  ; origins: TypeOrigin.t list  (** Origins responsible for this nullability type *) }
[@@deriving compare]

let rec sanitize_origin = function
  (* Collapse consecutive chains of InferredNonnull to get rid of infinite chains in loops and
     hence allowing to reach the fixpoint *)
  | TypeOrigin.InferredNonnull
      {previous_origin= TypeOrigin.InferredNonnull {previous_origin= underlying}} ->
      TypeOrigin.InferredNonnull {previous_origin= underlying} |> sanitize_origin
  | other ->
      other


let create origin =
  {nullability= TypeOrigin.get_nullability origin; origins= [sanitize_origin origin]}


let get_nullability {nullability} = nullability

let is_nonnullish {nullability} = Nullability.is_nonnullish nullability

let pp fmt {nullability} = Nullability.pp fmt nullability

(* Join two lists with removing duplicates and preserving the order of join *)
let join_origins origins1 origins2 =
  (IList.append_no_duplicates ~cmp:TypeOrigin.compare |> Staged.unstage) origins1 origins2


let join t1 t2 =
  let joined_nullability = Nullability.join t1.nullability t2.nullability in
  let is_equal_to_t1 = Nullability.equal t1.nullability joined_nullability in
  let is_equal_to_t2 = Nullability.equal t2.nullability joined_nullability in
  (* Origin complements nullability information. It is the best effort to explain how was the nullability inferred.
     If nullability is fully determined by one of the arguments, origin should be get from this argument.
     Otherwise we apply heuristics to choose origin either from t1 or t2.
  *)
  let joined_origins =
    match (is_equal_to_t1, is_equal_to_t2) with
    | _ when Nullability.equal t1.nullability Nullability.Null ->
        t1.origins
    | _ when Nullability.equal t2.nullability Nullability.Null ->
        t2.origins
    | true, false ->
        (* Nullability was fully determined by t1. *)
        t1.origins
    | false, true ->
        (* Nullability was fully determined by t2 *)
        t2.origins
    | false, false | true, true ->
        (* Nullability is not fully determined by neither t1 nor t2 - join both lists
        *)
        join_origins t1.origins t2.origins
  in
  {nullability= joined_nullability; origins= joined_origins}


let get_simple_origin t = List.nth_exn t.origins 0

let get_provisional_annotations t =
  List.filter_map t.origins ~f:TypeOrigin.get_provisional_annotation
  |> List.dedup_and_sort ~compare:ProvisionalAnnotation.compare


let origin_is_fun_defined t =
  match get_simple_origin t with TypeOrigin.MethodCall {is_defined; _} -> is_defined | _ -> false
