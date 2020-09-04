(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Process SMT-LIB benchmarks using SLEdge's first-order theory solver. *)

val process : string -> Report.status
