(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

(** Interprocedural footprint analysis *)

(** Frame and anti-frame *)
type splitting

(** Remove constant string or class from a prop *)
val remove_constant_string_class : 'a Prop.t -> Prop.normal Prop.t

(** Check if the attribute change is a mismatch between a kind of allocation
    and a different kind of deallocation *)
val check_attr_dealloc_mismatch : Sil.attribute -> Sil.attribute -> unit

(** Check whether a sexp contains a dereference without null check,
    and return the line number and path position *)
val find_dereference_without_null_check_in_sexp : Sil.strexp -> (int * Sil.path_pos) option

(** raise a cast exception *)
val raise_cast_exception :
  Logging.ml_loc -> Procname.t option -> Sil.exp -> Sil.exp -> Sil.exp -> 'a

(** check if a prop is an exception *)
val prop_is_exn : Procname.t -> 'a Prop.t -> bool

(** when prop is an exception, return the exception name *)
val prop_get_exn_name : Procname.t -> 'a Prop.t -> Typename.t

(** search in prop contains an error state *)
val lookup_custom_errors : 'a Prop.t -> string option

(** Dump a splitting *)
val d_splitting : splitting -> unit

(** Execute the function call and return the list of results with return value *)
val exe_function_call:
  ProcAttributes.t -> Tenv.t -> Ident.t list -> Cfg.Procdesc.t -> Procname.t -> Location.t ->
  (Sil.exp * Sil.typ) list -> Prop.normal Prop.t -> Paths.Path.t ->
  (Prop.normal Prop.t * Paths.Path.t) list

(* Set Ataint attribute to list of parameteres in a prop *)
val add_param_taint :
  Procname.t -> (Mangled.t * Sil.typ) list -> Prop.normal Prop.t ->
  int list -> Prop.normal Prop.t
