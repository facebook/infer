(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(* This file collects several utilities that are used both for instrumentation (Topl) and for
code generation (ToplMonitor). *)

val any_type : Typ.t
(** For now, Topl is untyped. The "any" type is simulated with "java.lang.Object". *)

val topl_class_name : Typ.Name.t

val topl_class_typ : Typ.t

val static_var : string -> Exp.t

val local_var : Typ.Procname.t -> string -> Exp.t

val constant_int : int -> Exp.t

val topl_call : Ident.t -> Typ.desc -> Location.t -> string -> (Exp.t * Typ.t) list -> Sil.instr
(** Call a TOPL function; that is, a static member of "topl.Property" with java.lang.Object arguments
and return [ret_id] of type [ret_typ].*)

val is_synthesized : Typ.Procname.t -> bool

val debug : ('a, Format.formatter, unit) IStd.format -> 'a
