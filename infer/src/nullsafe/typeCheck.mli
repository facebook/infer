(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Module type for the type checking functions. *)

type check_return_type = Procdesc.t -> Typ.t -> Typ.t option -> Location.t -> unit

type find_canonical_duplicate = Procdesc.Node.t -> Procdesc.Node.t

type checks = {eradicate: bool; check_ret_type: check_return_type list}

type typecheck_result =
  { normal_flow_typestate: TypeState.t option
        (** Typestate at the exit of the node. [None] if node is determined dead end (e.g. noreturn
            function). Will be passed to all output nodes of the current node. *)
  ; exception_flow_typestates: TypeState.t list
        (** If an exception might be thrown after this node, this list should contain all possible
            states at which the exception can be thrown. (Can be several states because different
            instructions in the single node can potentially throw). These typestates (joined
            together) will be passed to all "exception output" nodes of the current node. *) }

val typecheck_node :
     IntraproceduralAnalysis.t
  -> bool ref
  -> checks
  -> Idenv.t
  -> find_canonical_duplicate
  -> AnnotatedSignature.t
  -> TypeState.t
  -> Procdesc.Node.t
  -> LineReader.t
  -> typecheck_result
(** Main entry point. Typecheck the CFG node given input typestate, and report issues, if found *)
