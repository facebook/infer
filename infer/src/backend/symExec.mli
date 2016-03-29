(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Symbolic Execution *)

exception Cannot_convert_string_to_typ of string

val lookup_java_typ_from_string : Tenv.t -> string -> Sil.typ
(** Lookup Java types by name. May raise [Cannot_convert_string_to_typ]. *)

val resolve_method : Tenv.t -> Typename.t -> Procname.t -> Procname.t
(** OO method resolution: given a class name and a method name, climb the class hierarchy to find
    the procname that the method name will actually resolve to at runtime. For example, if we have a
    procname like Foo.toString() and Foo does not override toString(), we must resolve the call to
    toString(). We will end up with Super.toString() where Super is some superclass of Foo. *)

val prune_polarity : bool -> Sil.exp -> Prop.normal Prop.t -> Propset.t

val exp_norm_check_arith :
  Procname.t -> Prop.normal Prop.t -> Sil.exp -> Sil.exp * Prop.normal Prop.t
(** Normalize an expression and check for arithmetic problems *)

val execute_diverge :
  Prop.normal Prop.t -> Paths.Path.t -> (Prop.normal Prop.t * Paths.Path.t) list

val sym_exec_generated :
  bool -> Tenv.t -> Cfg.Procdesc.t -> Sil.instr list ->
  (Prop.normal Prop.t * Paths.Path.t) list -> (Prop.normal Prop.t * Paths.Path.t) list
(** Execute generated instructions from a symbolic heap.
    If errors occur and [mask_errors] is false, just treat as skip.*)

val sym_exe_check_variadic_sentinel :
  ?fails_on_nil:bool -> int -> int * int -> Builtin.args ->
  (Prop.normal Prop.t * Paths.Path.t) list

val check_untainted :
  Sil.exp -> Procname.t -> Procname.t -> Prop.normal Prop.t -> Prop.normal Prop.t

val call_unknown_or_scan :
  Tenv.t -> bool -> Cfg.Procdesc.t -> Prop.normal Prop.t -> Paths.Path.t -> Ident.t list ->
  Sil.typ option -> (Sil.exp * Sil.typ) list -> Procname.t -> Location.t ->
  (Prop.normal Prop.t * Paths.Path.t) list

val sym_exec_call :
  Cfg.Procdesc.t -> Tenv.t -> Prop.normal Prop.t -> Paths.Path.t -> Ident.t list ->
  (Sil.exp * Sil.typ) list -> Specs.summary -> Location.t ->
  (Prop.normal Prop.t * Paths.Path.t) list

val lifted_sym_exec : (exn -> unit) -> Tenv.t -> Cfg.Procdesc.t ->
  Paths.PathSet.t -> Cfg.Node.t -> Sil.instr list -> Paths.PathSet.t
(** symbolic execution on the level of sets of propositions *)
