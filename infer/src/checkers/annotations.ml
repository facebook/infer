(*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module F = Format
module L = Logging
open Utils

(** Annotations. *)

(** Method signature with annotations. *)
type annotated_signature =
  { ret : Sil.item_annotation * Sil.typ; (** Annotated return type. *)
    params: (string * Sil.item_annotation * Sil.typ) list } (** Annotated parameters. *)

let param_equal (s1, ia1, t1) (s2, ia2, t2) =
  string_equal s1 s2 &&
  Sil.item_annotation_compare ia1 ia2 = 0 &&
  Sil.typ_equal t1 t2

let equal as1 as2 =
  let ia1, t1 = as1.ret
  and ia2, t2 = as2.ret in
  Sil.item_annotation_compare ia1 ia2 = 0 &&
  Sil.typ_equal t1 t2 &&
  list_for_all2 param_equal as1.params as2.params

let visibleForTesting = "com.google.common.annotations.VisibleForTesting"
let javaxNullable = "javax.annotation.Nullable"
let javaxNonnull = "javax.annotation.Nonnull"
let suppressLint = "android.annotation.SuppressLint"


let get_field_type_and_annotation fn = function
  | Sil.Tptr (Sil.Tstruct (ftal, sftal, _, _, _, _, _), _)
  | Sil.Tstruct (ftal, sftal, _, _, _, _, _) ->
      (try
         let (_, t, a) = list_find (fun (f, t, a) -> Sil.fld_equal f fn) (ftal @ sftal) in
         Some (t, a)
       with Not_found -> None)
  | _ -> None

let ia_iter f =
  let ann_iter (a, b) = f a in
  list_iter ann_iter

let ma_iter f ((ia, ial) : Sil.method_annotation) =
  list_iter (ia_iter f) (ia:: ial)

let ma_has_annotation_with
    (ma: Sil.method_annotation)
    (predicate: Sil.annotation -> bool): bool =
  let found = ref false in
  ma_iter
    (fun a -> if predicate a then found := true)
    ma;
  !found

let ia_has_annotation_with
    (ia: Sil.item_annotation)
    (predicate: Sil.annotation -> bool): bool =
  let found = ref false in
  ia_iter
    (fun a -> if predicate a then found := true)
    ia;
  !found

(** Check if there is an annotation which ends with the given name *)
let ia_ends_with ia ann_name =
  let found = ref false in
  let filter s =
    let sl = String.length s in
    let al = String.length ann_name in
    sl >= al && String.sub s (sl - al) al = ann_name in
  ia_iter (fun a -> if filter a.Sil.class_name then found := true) ia;
  !found

let ia_contains ia ann_name =
  let found = ref false in
  ia_iter (fun a -> if ann_name = a.Sil.class_name then found := true) ia;
  !found

let ia_get ia ann_name =
  let found = ref None in
  ia_iter (fun a -> if ann_name = a.Sil.class_name then found := Some a) ia;
  !found

let ma_contains ma ann_names =
  let found = ref false in
  ma_iter (fun a -> if list_exists (string_equal a.Sil.class_name) ann_names then found := true) ma;
  !found

let initializer_ = "Initializer"
let inject = "Inject"
let inject_view = "InjectView"
let bind = "Bind"
let false_on_null = "FalseOnNull"
let mutable_ = "Mutable"
let nullable = "Nullable"
let nonnull = "Nonnull"
let camel_nonnull = "NonNull"
let notnull = "NotNull"
let present = "Present"
let strict = "com.facebook.infer.annotation.Strict"
let true_on_null = "TrueOnNull"
let verify_annotation = "com.facebook.infer.annotation.Verify"

let ia_is_nullable ia =
  ia_ends_with ia nullable

let ia_is_present ia =
  ia_ends_with ia present

let ia_is_nonnull ia =
  list_exists
    (ia_ends_with ia)
    [nonnull; notnull; camel_nonnull]

let ia_is_false_on_null ia =
  ia_ends_with ia false_on_null

let ia_is_true_on_null ia =
  ia_ends_with ia true_on_null

let ia_is_initializer ia =
  ia_ends_with ia initializer_

let ia_is_inject ia =
  list_exists
    (ia_ends_with ia)
    [inject; inject_view; bind]

let ia_is_inject_view ia =
  ia_ends_with ia inject_view

let ia_is_mutable ia =
  ia_ends_with ia mutable_

let ia_get_strict ia =
  ia_get ia strict

let ia_is_verify ia =
  ia_contains ia verify_annotation

type annotation =
  | Nullable
  | Present

let ia_is ann ia = match ann with
  | Nullable -> ia_is_nullable ia
  | Present -> ia_is_present ia


(** Get a method signature with annotations from a proc_attributes. *)
let get_annotated_signature proc_attributes : annotated_signature =
  let method_annotation = proc_attributes.ProcAttributes.method_annotation in
  let formals = proc_attributes.ProcAttributes.formals in
  let ret_type = proc_attributes.ProcAttributes.ret_type in
  let (ia, ial0) = method_annotation in
  let natl =
    let rec extract ial parl = match ial, parl with
      | ia :: ial', (name, typ) :: parl' ->
          (name, ia, typ) :: extract ial' parl'
      | [], (name, typ) :: parl' ->
          (name, Sil.item_annotation_empty, typ) :: extract [] parl'
      | [], [] ->
          []
      | _ :: _, [] ->
          assert false in
    list_rev (extract (list_rev ial0) (list_rev formals)) in
  let annotated_signature = { ret = (ia, ret_type); params = natl } in
  annotated_signature


(** Check if the annotated signature is for a wrapper of an anonymous inner class method.
    These wrappers have the same name as the original method, every type is Object, and the parameters
    are called x0, x1, x2. *)
let annotated_signature_is_anonymous_inner_class_wrapper ann_sig proc_name =
  let check_ret (ia, t) =
    Sil.item_annotation_is_empty ia && PatternMatch.type_is_object t in
  let x_param_found = ref false in
  let name_is_x_number name =
    let len = String.length name in
    len >= 2 &&
    String.sub name 0 1 = "x" &&
    let s = String.sub name 1 (len - 1) in
    let is_int =
      try
        ignore (int_of_string s);
        x_param_found := true;
        true
      with Failure _ -> false in
    is_int in
  let check_param (name, ia, t) =
    if name = "this" then true
    else
      name_is_x_number name &&
      Sil.item_annotation_is_empty ia &&
      PatternMatch.type_is_object t in
  Procname.java_is_anonymous_inner_class proc_name
  && check_ret ann_sig.ret
  && list_for_all check_param ann_sig.params
  && !x_param_found

(** Check if the given parameter has a Nullable annotation in the given signature *)
let param_is_nullable pvar ann_sig =
  let pvar_str = Mangled.to_string (Sil.pvar_get_name pvar) in
  list_exists
    (fun (param_str, annot, _) -> param_str = pvar_str && ia_is_nullable annot)
    ann_sig.params

(** Pretty print a method signature with annotations. *)
let pp_annotated_signature proc_name fmt annotated_signature =
  let pp_ia fmt ia = if ia <> [] then F.fprintf fmt "%a " Sil.pp_item_annotation ia in
  let pp_annotated_param fmt (s, ia, t) =
    F.fprintf fmt " %a%a %s" pp_ia ia (Sil.pp_typ_full pe_text) t s in
  let ia, ret_type = annotated_signature.ret in
  F.fprintf fmt "%a%a %s (%a )"
    pp_ia ia
    (Sil.pp_typ_full pe_text) ret_type
    (Procname.to_simplified_string proc_name)
    (pp_comma_seq pp_annotated_param) annotated_signature.params

let mk_ann_str s = { Sil.class_name = s; Sil.parameters = [] }
let mk_ann = function
  | Nullable -> mk_ann_str nullable
  | Present -> mk_ann_str present
let mk_ia ann ia =
  if ia_is ann ia then ia
  else (mk_ann ann, true) :: ia
let mark_ia ann ia x =
  if x then mk_ia ann ia else ia

let mk_ia_strict ia =
  if ia_get_strict ia <> None then ia
  else (mk_ann_str strict, true) :: ia
let mark_ia_strict ia x =
  if x then mk_ia_strict ia else ia

(** Mark the annotated signature with the given annotation map. *)
let annotated_signature_mark proc_name ann asig (b, bs) =
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
      L.stdout "  ANNOTATED SIGNATURE: %a@." (pp_annotated_signature proc_name) asig;
      assert false in
    let rec combine l1 l2 = match l1, l2 with
      | ("this", ia, t):: l1', l2' ->
          ("this", ia, t) :: combine l1' l2'
      | (s, ia, t):: l1', x:: l2' ->
          mark_param (s, ia, t) x :: combine l1' l2'
      | [], _:: _ -> fail ()
      | _:: _, [] -> fail ()
      | [], [] -> [] in
    combine asig.params bs in
  { ret = ret'; params = params'}

(** Mark the return of the annotated signature with the given annotation. *)
let annotated_signature_mark_return ann asig =
  let ia, t = asig.ret in
  let ret' = mark_ia ann ia true, t in
  { asig with ret = ret'}

(** Mark the return of the annotated signature @Strict. *)
let annotated_signature_mark_return_strict asig =
  let ia, t = asig.ret in
  let ret' = mark_ia_strict ia true, t in
  { asig with ret = ret'}

(** Mark the return of the method_annotation with the given annotation. *)
let method_annotation_mark_return ann method_annotation =
  let ia_ret, params = method_annotation in
  let ia_ret' = mark_ia ann ia_ret true in
  ia_ret', params
