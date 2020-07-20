(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module SkippedTrace = struct
  type t = PulseISLTrace.t [@@deriving compare]

  let pp fmt =
    PulseISLTrace.pp fmt ~pp_immediate:(fun fmt ->
        F.pp_print_string fmt "call to skipped function occurs here" )


  let leq ~lhs ~rhs = phys_equal lhs rhs

  let join s1 _ = s1

  let widen ~prev ~next ~num_iters:_ = join prev next
end

include AbstractDomain.Map (Procname) (SkippedTrace)
