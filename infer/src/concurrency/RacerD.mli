(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val file_analysis : Callbacks.cluster_callback_t

val analyze_procedure : Callbacks.proc_callback_t
