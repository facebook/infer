(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val emit_specs_to_file : DB.filename -> Prop.normal BiabductionSummary.spec list -> unit
(** emit specs in the "dot" format to the specified file *)
