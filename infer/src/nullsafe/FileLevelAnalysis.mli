(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val analyze_file : Callbacks.file_callback_t
(** File-level callback for nullsafe. Is called after all proc-level callbacks are called and
    calculated their summaries. At this stage, additional issues can be emitted. *)
