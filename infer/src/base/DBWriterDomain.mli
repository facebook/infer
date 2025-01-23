(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

val perform : DBWriterCommand.t -> unit
(** A domain-based implementation of a serializing database writer. *)
