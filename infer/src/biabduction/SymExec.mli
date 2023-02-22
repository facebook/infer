(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Symbolic Execution *)

val declare_locals_and_ret : Tenv.t -> Procdesc.t -> Prop.normal Prop.t -> Prop.normal Prop.t
(** Symbolic execution of the declaration of locals and return value. *)

val node :
     (exn -> unit)
  -> BiabductionSummary.t InterproceduralAnalysis.t
  -> ProcCfg.Exceptional.t
  -> ProcCfg.Exceptional.Node.t
  -> Paths.PathSet.t
  -> Paths.PathSet.t
(** Symbolic execution of the instructions of a node, lifted to sets of propositions. *)

val instrs :
     ?mask_errors:bool
  -> BiabductionSummary.t InterproceduralAnalysis.t
  -> Instrs.not_reversed_t
  -> (Prop.normal Prop.t * Paths.Path.t) list
  -> (Prop.normal Prop.t * Paths.Path.t) list
(** Symbolic execution of a sequence of instructions. If errors occur and [mask_errors] is true,
    just treat as skip. *)

val diverge : Prop.normal Prop.t -> Paths.Path.t -> (Prop.normal Prop.t * Paths.Path.t) list
(** Symbolic execution of the divergent pure computation. *)

val proc_call : Procname.t -> BiabductionSummary.t -> Builtin.t

val unknown_or_scan_call : is_scan:bool -> reason:string -> Typ.t -> Annot.Item.t -> Builtin.t

val check_variadic_sentinel : ?fails_on_nil:bool -> int -> int * int -> Builtin.t

val check_arith_norm_exp :
     BiabductionSummary.t InterproceduralAnalysis.t
  -> Exp.t
  -> Prop.normal Prop.t
  -> Exp.t * Prop.normal Prop.t
(** Check for arithmetic problems and normalize an expression. *)

val prune : Tenv.t -> positive:bool -> Exp.t -> Prop.normal Prop.t -> Propset.t
