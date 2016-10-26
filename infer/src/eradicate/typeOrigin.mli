(*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Case Proc *)
type proc_origin =
  {
    pname : Procname.t;
    loc: Location.t;
    annotated_signature : Annotations.annotated_signature;
    is_library : bool;
  }

type t =
  | Const of Location.t (** A constant in the source *)
  | Field of Ident.fieldname * Location.t (** A field access *)
  | Formal of Mangled.t (** A formal parameter *)
  | Proc of proc_origin (** A procedure call *)
  | New (** A new object creation *)
  | ONone (** No origin is known *)
  | Undef (** Undefined value before initialization *)

val equal : t -> t -> bool

(** Get a description to be used for error messages. *)
val get_description : Tenv.t -> t -> TypeErr.origin_descr option

(** Join with left priority *)
val join : t -> t -> t

(** Raw string representation. *)
val to_string : t -> string
