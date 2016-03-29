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

(** Symbolic execution of the instructions of a node, lifted to sets of propositions. *)
val node : (exn -> unit) -> Tenv.t -> Cfg.Node.t -> Paths.PathSet.t -> Paths.PathSet.t

(** Symbolic execution of a sequence of instructions.
    If errors occur and [mask_errors] is true, just treat as skip. *)
val instrs :
  ?mask_errors:bool -> Tenv.t -> Cfg.Procdesc.t -> Sil.instr list ->
  (Prop.normal Prop.t * Paths.Path.t) list -> (Prop.normal Prop.t * Paths.Path.t) list

(** Symbolic execution of the divergent pure computation. *)
val diverge : Prop.normal Prop.t -> Paths.Path.t -> (Prop.normal Prop.t * Paths.Path.t) list

val proc_call : Specs.summary -> Builtin.t

val unknown_or_scan_call : is_scan:bool -> Sil.typ option -> Builtin.t

val check_variadic_sentinel : ?fails_on_nil:bool -> int -> int * int -> Builtin.t

val check_untainted :
  Sil.exp -> Procname.t -> Procname.t -> Prop.normal Prop.t -> Prop.normal Prop.t

(** Check for arithmetic problems and normalize an expression. *)
val check_arith_norm_exp :
  Procname.t -> Sil.exp -> Prop.normal Prop.t -> Sil.exp * Prop.normal Prop.t

val prune : positive:bool -> Sil.exp -> Prop.normal Prop.t -> Propset.t

(** OO method resolution: given a class name and a method name, climb the class hierarchy to find
    the procname that the method name will actually resolve to at runtime. For example, if we have a
    procname like Foo.toString() and Foo does not override toString(), we must resolve the call to
    toString(). We will end up with Super.toString() where Super is some superclass of Foo. *)
val resolve_method : Tenv.t -> Typename.t -> Procname.t -> Procname.t
