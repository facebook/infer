(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val infer_deps_of_build_report : string -> unit
(** parse a buck build report and construct resulting [infer-deps.txt] *)

val capture : BuckMode.t -> string list -> unit
(** do genrule capture with the given buck command line *)
