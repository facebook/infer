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

type t = {
  ret : Annot.Item.t * Typ.t;
  params: (Mangled.t * Annot.Item.t * Typ.t) list;
} [@@deriving compare]

type annotation =
  | Nullable
  | Present
[@@deriving compare]

let ia_is ann ia = match ann with
  | Nullable -> Annotations.ia_is_nullable ia
  | Present -> Annotations.ia_is_present ia

let get proc_attributes : t =
  let method_annotation = proc_attributes.ProcAttributes.method_annotation in
  let formals = proc_attributes.ProcAttributes.formals in
  let ret_type = proc_attributes.ProcAttributes.ret_type in
  let (ia, ial0) = method_annotation in
  let natl =
    let rec extract ial parl = match ial, parl with
      | ia :: ial', (name, typ) :: parl' ->
          (name, ia, typ) :: extract ial' parl'
      | [], (name, typ) :: parl' ->
          (name, Annot.Item.empty, typ) :: extract [] parl'
      | [], [] ->
          []
      | _ :: _, [] ->
          assert false in
    List.rev (extract (List.rev ial0) (List.rev formals)) in
  let annotated_signature = { ret = (ia, ret_type); params = natl } in
  annotated_signature

let param_is_nullable pvar ann_sig =
  List.exists
    ~f:(fun (param, annot, _) ->
        Mangled.equal param (Pvar.get_name pvar) && Annotations.ia_is_nullable annot)
    ann_sig.params

let pp proc_name fmt annotated_signature =
  let pp_ia fmt ia = if ia <> [] then F.fprintf fmt "%a " Annot.Item.pp ia in
  let pp_annotated_param fmt (p, ia, t) =
    F.fprintf fmt " %a%a %a" pp_ia ia (Typ.pp_full Pp.text) t Mangled.pp p in
  let ia, ret_type = annotated_signature.ret in
  F.fprintf fmt "%a%a %s (%a )"
    pp_ia ia
    (Typ.pp_full Pp.text) ret_type
    (Procname.to_simplified_string proc_name)
    (Pp.comma_seq pp_annotated_param) annotated_signature.params

let is_anonymous_inner_class_wrapper ann_sig proc_name =
  let check_ret (ia, t) =
    Annot.Item.is_empty ia && PatternMatch.type_is_object t in
  let x_param_found = ref false in
  let name_is_x_number name =
    let name_str = Mangled.to_string name in
    let len = String.length name_str in
    len >= 2 &&
    String.equal (String.sub name_str ~pos:0 ~len:1) "x" &&
    let s = String.sub name_str ~pos:1 ~len:(len - 1) in
    let is_int =
      try
        ignore (int_of_string s);
        x_param_found := true;
        true
      with Failure _ -> false in
    is_int in
  let check_param (name, ia, t) =
    if String.equal (Mangled.to_string name) "this" then true
    else
      name_is_x_number name &&
      Annot.Item.is_empty ia &&
      PatternMatch.type_is_object t in
  Procname.java_is_anonymous_inner_class proc_name
  && check_ret ann_sig.ret
  && List.for_all ~f:check_param ann_sig.params
  && !x_param_found

let mk_ann_str s = { Annot.class_name = s; parameters = [] }
let mk_ann = function
  | Nullable -> mk_ann_str Annotations.nullable
  | Present -> mk_ann_str Annotations.present
let mk_ia ann ia =
  if ia_is ann ia then ia
  else (mk_ann ann, true) :: ia
let mark_ia ann ia x =
  if x then mk_ia ann ia else ia

let mk_ia_strict ia =
  if Annotations.ia_get_strict ia <> None then ia
  else (mk_ann_str Annotations.strict, true) :: ia
let mark_ia_strict ia x =
  if x then mk_ia_strict ia else ia

let method_annotation_mark_return ann method_annotation =
  let ia_ret, params = method_annotation in
  let ia_ret' = mark_ia ann ia_ret true in
  ia_ret', params

let mark proc_name ann asig (b, bs) =
  let ia, t = asig.ret in
  let ret' = mark_ia ann ia b, t in
  let mark_param (s, ia, t) x =
    let ia' = if x then mk_ia ann ia else ia in
    (s, ia', t) in
  let params' =
    let fail () =
      L.stdout
        "INTERNAL ERROR: annotation for procedure %s has wrong number of arguments@."
        (Procname.to_unique_id proc_name);
      L.stdout "  ANNOTATED SIGNATURE: %a@." (pp proc_name) asig;
      assert false in
    let rec combine l1 l2 = match l1, l2 with
      | (p, ia, t):: l1', l2' when String.equal (Mangled.to_string p) "this" ->
          (p, ia, t) :: combine l1' l2'
      | (s, ia, t):: l1', x:: l2' ->
          mark_param (s, ia, t) x :: combine l1' l2'
      | [], _:: _ -> fail ()
      | _:: _, [] -> fail ()
      | [], [] -> [] in
    combine asig.params bs in
  { ret = ret'; params = params'}

let mark_return ann asig =
  let ia, t = asig.ret in
  let ret' = mark_ia ann ia true, t in
  { asig with ret = ret'}
