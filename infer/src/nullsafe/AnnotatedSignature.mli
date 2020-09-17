(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Method signature with annotations. *)

open! IStd

type t =
  {nullsafe_mode: NullsafeMode.t; kind: kind; ret: ret_signature; params: param_signature list}
[@@deriving compare]

and ret_signature = {ret_annotation_deprecated: Annot.Item.t; ret_annotated_type: AnnotatedType.t}
[@@deriving compare]

and param_signature =
  { param_annotation_deprecated: Annot.Item.t
  ; mangled: Mangled.t
  ; param_annotated_type: AnnotatedType.t }
[@@deriving compare]

and kind =
  | FirstParty  (** Code under control. Its nullability should be expressed via annotations. *)
  | ThirdParty of third_party_model_source [@deriving compare]

and third_party_model_source =
  | Unregistered
      (** This is an unregistered third party method. It's nullability is best effort based on its
          annotations. Lack of annotation is treated depending on the mode. *)
  | ModelledInternally
  | InThirdPartyRepo of {filename: string; line_number: int}
[@@deriving compare]

val get_non_virtual_params : t -> param_signature list
(** List of params, not counting the optional first ("this") param used to represent non-static
    methods. *)

val set_modelled_nullability : Procname.t -> t -> third_party_model_source -> bool * bool list -> t
(** Override nullability for a function signature given its modelled nullability (for ret value and
    params) *)

val get : is_callee_in_trust_list:bool -> nullsafe_mode:NullsafeMode.t -> ProcAttributes.t -> t
(** Get a method signature with annotations from a proc_attributes. *)

val get_for_class_under_analysis : Tenv.t -> ProcAttributes.t -> t
(** Signature of the method belonging to the currently analyzed class. *)

val pp : Procname.t -> Format.formatter -> t -> unit
(** Pretty print a method signature with annotations. *)
