(*
* Copyright (c) 2014 - Facebook. All rights reserved.
*)


type proc_origin = (** Case Proc *)
  Procname.t * Sil.location * Annotations.annotated_signature * bool (* is_library *)

type t =
  | Const of Sil.location (** A constant in the source *)
  | Field of Ident.fieldname * Sil.location (** A field access *)
  | Formal of string (** A formal parameter *)
  | Proc of proc_origin (** A procedure call *)
  | New (** A new object creation *)
  | ONone (** No origin is known *)
  | Undef (** Undefined value before initialization *)

val equal : t -> t -> bool

(** Get a description to be used for error messages. *)
val get_description : t -> TypeErr.origin_descr option

(** Join with left priority *)
val join : t -> t -> t

(** Raw string representation. *)
val to_string : t -> string
