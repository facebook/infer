(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Currently generates the following:

    {[
      let initial = _initial_stats_

      let global_stats = _initial_stats_
    ]} *)
