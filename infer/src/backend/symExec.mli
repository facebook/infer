(*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

(** Symbolic Execution *)

val node :
  (exn -> unit) -> Tenv.t -> ProcCfg.Exceptional.t -> ProcCfg.Exceptional.node -> Paths.PathSet.t
  -> Paths.PathSet.t
(** Symbolic execution of the instructions of a node, lifted to sets of propositions. *)

val instrs :
  ?mask_errors:bool -> Tenv.t -> Procdesc.t -> Sil.instr list
  -> (Prop.normal Prop.t * Paths.Path.t) list -> (Prop.normal Prop.t * Paths.Path.t) list
(** Symbolic execution of a sequence of instructions.
    If errors occur and [mask_errors] is true, just treat as skip. *)

val diverge : Prop.normal Prop.t -> Paths.Path.t -> (Prop.normal Prop.t * Paths.Path.t) list
(** Symbolic execution of the divergent pure computation. *)

val proc_call : Specs.summary -> Builtin.t

val unknown_or_scan_call : is_scan:bool -> Typ.t option -> Annot.Item.t -> Builtin.t

val check_variadic_sentinel : ?fails_on_nil:bool -> int -> int * int -> Builtin.t

val check_arith_norm_exp :
  Tenv.t -> Typ.Procname.t -> Exp.t -> Prop.normal Prop.t -> Exp.t * Prop.normal Prop.t
(** Check for arithmetic problems and normalize an expression. *)

val prune : Tenv.t -> positive:bool -> Exp.t -> Prop.normal Prop.t -> Propset.t

val resolve_method : Tenv.t -> Typ.Name.t -> Typ.Procname.t -> Typ.Procname.t
(** OO method resolution: given a class name and a method name, climb the class hierarchy to find
    the procname that the method name will actually resolve to at runtime. For example, if we have a
    procname like Foo.toString() and Foo does not override toString(), we must resolve the call to
    toString(). We will end up with Super.toString() where Super is some superclass of Foo. *)
