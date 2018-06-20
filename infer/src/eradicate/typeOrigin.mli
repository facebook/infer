(*
 * Copyright (c) 2014-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Case Proc *)
type proc_origin =
  { pname: Typ.Procname.t
  ; loc: Location.t
  ; annotated_signature: AnnotatedSignature.t
  ; is_library: bool }
[@@deriving compare]

type t =
  | Const of Location.t  (** A constant in the source *)
  | Field of t * Typ.Fieldname.t * Location.t  (** A field access *)
  | Formal of Mangled.t  (** A formal parameter *)
  | Proc of proc_origin  (** A procedure call *)
  | New  (** A new object creation *)
  | ONone  (** No origin is known *)
  | Undef  (** Undefined value before initialization *)
[@@deriving compare]

val equal : t -> t -> bool

val get_description : t -> TypeErr.origin_descr option
(** Get a description to be used for error messages. *)

val join : t -> t -> t
(** Join with left priority *)

val to_string : t -> string
(** Raw string representation. *)
