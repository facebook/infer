(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val is_stack_exp : Exp.t -> BufferOverrunDomain.Mem.t -> bool
(** Check if an expression is a stack variable such as [n$0] or local variable for C array *)

val eval : IntegerWidths.t -> Exp.t -> BufferOverrunDomain.Mem.t -> BufferOverrunDomain.Val.t
(** Evalute an expression *)

val eval_locs : Exp.t -> BufferOverrunDomain.Mem.t -> AbsLoc.PowLoc.t
(** [eval_locs exp mem] is like [eval exp mem |> Val.get_all_locs] but takes some shortcuts to avoid
    computing useless and/or problematic intermediate values *)

val eval_arr : IntegerWidths.t -> Exp.t -> BufferOverrunDomain.Mem.t -> BufferOverrunDomain.Val.t
(** Return the array value of the input expression. For example, when [x] is a program variable,
    [eval_arr x] returns array blocks the [x] is pointing to, on the other hand, [eval x] returns
    the abstract location of [x]. *)

val eval_lindex :
  IntegerWidths.t -> Exp.t -> Exp.t -> BufferOverrunDomain.Mem.t -> BufferOverrunDomain.Val.t
(** Evaluate array location with index, i.e.,
    [eval_lindex integer_type_widths array_exp index_exp mem] *)

val eval_array_locs_length :
  AbsLoc.PowLoc.t -> _ BufferOverrunDomain.Mem.t0 -> BufferOverrunDomain.Val.t
(** Evaluate length of array locations *)

val eval_string_len : Exp.t -> BufferOverrunDomain.Mem.t -> BufferOverrunDomain.Val.t
(** Evaluate length of C string *)

val conservative_array_length :
     ?traces:BufferOverrunTrace.Set.t
  -> AbsLoc.PowLoc.t
  -> _ BufferOverrunDomain.Mem.t0
  -> BufferOverrunDomain.Val.t
(** Evaluate the array length conservatively, which is useful when there are multiple array
    locations and their lengths are joined to top. For example, if the [arr_locs] points to two
    arrays [a] and [b] and if their lengths are [a.length] and [b.length], this function evaluates
    its length as [[0, a.length.ub + b.length.ub]]. *)

(** Several modes of ondemand evaluations *)
type eval_mode =
  | EvalNormal
      (** Given a symbolic value of an unknown function [Symb.SymbolPath.Callsite], it returns a
          symbolic interval value. *)
  | EvalPOCond
      (** Given a symbolic value of an unknown function, it returns the top interval value. This is
          used when substituting condition expressions of proof obligations. *)
  | EvalPOReachability
      (** This is similar to [EvalPOCond], but it returns the bottom location, instead of the
          unknown location, when a location to substitute is not found. This is used when
          substituting reachabilities of proof obligations. *)
  | EvalCost
      (** This is similar to [EvalNormal], but it is designed to be used in substitutions of the
          cost results, avoiding precision loss by joining of symbolic values. Normal join of two
          different symbolic values, [s1] and [s2], becomes top due to the limitation of our domain.
          On the other hand, in this mode, it returns an upperbound [s1+s2] for the case, because
          the cost values only care about the upperbounds. *)

val mk_eval_sym_trace :
     ?is_args_ref:bool
  -> IntegerWidths.t
  -> (Pvar.t * Typ.t) list
  -> (Exp.t * Typ.t) list
  -> (Exp.t * Pvar.t * Typ.t * CapturedVar.capture_mode) list
  -> BufferOverrunDomain.Mem.t
  -> mode:eval_mode
  -> BufferOverrunDomain.eval_sym_trace
(** Make [eval_sym] function for on-demand symbol evaluation *)

val mk_eval_sym_cost :
     IntegerWidths.t
  -> (Pvar.t * Typ.t) list
  -> (Exp.t * Typ.t) list
  -> (Exp.t * Pvar.t * Typ.t * CapturedVar.capture_mode) list
  -> BufferOverrunDomain.Mem.t
  -> BufferOverrunDomain.eval_sym_trace
(** Make [eval_sym] function of [EvalCost] mode for on-demand symbol evaluation *)

module Prune : sig
  val prune :
    Location.t -> IntegerWidths.t -> Exp.t -> BufferOverrunDomain.Mem.t -> BufferOverrunDomain.Mem.t
  (** Prune memory with the given condition expression *)
end
