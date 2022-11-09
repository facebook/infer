(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type flavored_arguments = {command: string; rev_not_targets: string list; targets: string list}

val add_flavors_to_buck_arguments :
  BuckMode.t -> extra_flavors:string list -> string list -> flavored_arguments
(** Add infer flavors to the targets in the given buck arguments, depending on the infer analyzer.
    For instance, in clang capture mode, the buck command: [build //foo/bar:baz#some,flavor]
    becomes: build [//foo/bar:baz#infer-capture-all,some,flavor] NB this is for buck1 only. *)

val capture : string list -> unit
(** do a buck/clang flavor capture given the prog and build command (buck args) *)
