(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val get_autofix : Procdesc.t -> PulseDiagnostic.t -> Jsonbug_t.autofix list
