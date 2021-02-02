(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module SkippedTrace = struct
  type t = PulseTrace.t [@@deriving compare, equal]

  let pp fmt =
    PulseTrace.pp fmt ~pp_immediate:(fun fmt ->
        F.pp_print_string fmt "call to skipped function occurs here" )


  let leq ~lhs ~rhs = phys_equal lhs rhs

  let join s1 _ = s1

  let widen ~prev ~next ~num_iters:_ = join prev next
end

module M = AbstractDomain.Map (Procname) (SkippedTrace)
include M

let compare = M.compare SkippedTrace.compare

let equal = M.equal SkippedTrace.equal

let yojson_of_t = [%yojson_of: _]

(* ignore traces, just compare if the set of skipped procedures is the same *)
let leq ~lhs ~rhs = M.equal (fun _ _ -> true) lhs rhs
