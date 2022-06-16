(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  type t [@@deriving compare, equal, sexp_of]
  type status [@@deriving compare]

  val status : t -> Llair.block option -> status
  (** [status goal block] is a measure of the progress made toward [goal] by
      an execution that has reached [block]. [block] is [None] for
      executions that end in a terminated thread. *)

  val pp : t pp

  val reached : t -> bool
  (** True iff the goal has been reached and there is no more work to do. *)

  val update_after_call : Llair.Function.t -> t -> t
  (** Update the goal, having called the given Llair function. *)

  val initialize : pgm:Llair.program -> entry:Llair.block -> t -> unit
  (** Perform any upfront metadata computation and decorate [pgm] with it. *)
end

module Undirected : S with type t = unit

module Sparse_trace : sig
  include S

  (** Raised when [parse] encounters a function name not corresponding to
      any function in the given [Llair.program] *)
  exception Failed_lookup of string

  val of_fns_exn : string list -> Llair.program -> t
  (** Convert a list of function names to a sparse trace over the given
      [Llair.program] IR. Raises [Failed_lookup] if a function name is
      encountered that is not in the IR. *)
end
