(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val run : ?cwd:string -> prog:string -> argv:string list -> unit -> (unit, string) Result.t
(** Spawn an infer subprocess with a sanitized environment.

    The [INFER_ARGS] env var is stripped of internal child-process flags (e.g. [--run-as-child]) so
    workers spawned from a [ProcessPool] do not inherit them. [stdin] and [stdout] are redirected to
    [/dev/null]; [stderr] is inherited. Optionally [cwd] sets the working directory of the child.
    Returns [Ok ()] on success, or [Error msg] describing the failure. *)
