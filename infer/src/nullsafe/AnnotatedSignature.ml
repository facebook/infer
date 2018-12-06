(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

type t = {ret: Annot.Item.t * Typ.t; params: (Mangled.t * Annot.Item.t * Typ.t) list}
[@@deriving compare]

type annotation = Nullable | Present [@@deriving compare]

let ia_is ann ia =
  match ann with
  | Nullable ->
      Annotations.ia_is_nullable ia
  | Present ->
      Annotations.ia_is_present ia


let get proc_attributes : t =
  let method_annotation = proc_attributes.ProcAttributes.method_annotation in
  let formals = proc_attributes.ProcAttributes.formals in
  let ret_type = proc_attributes.ProcAttributes.ret_type in
  let Annot.Method.({return; params}) = method_annotation in
  let natl =
    let rec extract ial parl =
      match (ial, parl) with
      | ia :: ial', (name, typ) :: parl' ->
          (name, ia, typ) :: extract ial' parl'
      | [], (name, typ) :: parl' ->
          (name, Annot.Item.empty, typ) :: extract [] parl'
      | [], [] ->
          []
      | _ :: _, [] ->
          assert false
    in
    List.rev (extract (List.rev params) (List.rev formals))
  in
  let annotated_signature = {ret= (return, ret_type); params= natl} in
  annotated_signature


let param_has_annot predicate pvar ann_sig =
  List.exists
    ~f:(fun (param, param_annot, _) ->
      Mangled.equal param (Pvar.get_name pvar) && predicate param_annot )
    ann_sig.params


let pp proc_name fmt annotated_signature =
  let pp_ia fmt ia = if ia <> [] then F.fprintf fmt "%a " Annot.Item.pp ia in
  let pp_annotated_param fmt (p, ia, t) =
    F.fprintf fmt " %a%a %a" pp_ia ia (Typ.pp_full Pp.text) t Mangled.pp p
  in
  let ia, ret_type = annotated_signature.ret in
  F.fprintf fmt "%a%a %s (%a )" pp_ia ia (Typ.pp_full Pp.text) ret_type
    (Typ.Procname.to_simplified_string proc_name)
    (Pp.comma_seq pp_annotated_param) annotated_signature.params


let mk_ann_str s = {Annot.class_name= s; parameters= []}

let mk_ann = function
  | Nullable ->
      mk_ann_str Annotations.nullable
  | Present ->
      mk_ann_str Annotations.present


let mk_ia ann ia = if ia_is ann ia then ia else (mk_ann ann, true) :: ia

let mark_ia ann ia x = if x then mk_ia ann ia else ia

let mark proc_name ann asig (b, bs) =
  let ia, t = asig.ret in
  let ret' = (mark_ia ann ia b, t) in
  let mark_param (s, ia, t) x =
    let ia' = if x then mk_ia ann ia else ia in
    (s, ia', t)
  in
  let params' =
    let fail () =
      L.die InternalError
        "Annotation for procedure %s has wrong number of arguments.@\n  Annotated signature: %a"
        (Typ.Procname.to_unique_id proc_name)
        (pp proc_name) asig
    in
    let rec combine l1 l2 =
      match (l1, l2) with
      | (p, ia, t) :: l1', l2' when Mangled.is_this p ->
          (p, ia, t) :: combine l1' l2'
      | (s, ia, t) :: l1', x :: l2' ->
          mark_param (s, ia, t) x :: combine l1' l2'
      | [], _ :: _ ->
          fail ()
      | _ :: _, [] ->
          fail ()
      | [], [] ->
          []
    in
    combine asig.params bs
  in
  {ret= ret'; params= params'}
