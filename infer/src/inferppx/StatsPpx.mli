(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Currently generates the following (see implementation for examples and details):

    {[
      let initial = _initial_stats_

      let global_stats = _initial_stats_

      let copy from ~into =
        (* set all the fields of [into] with values from the fields of [from] *)
        ...

      let merge stats1 stats2 =
        (* creates a new stats object merging stats1 and stats2 field by field *)
        ...

      to_log_entries stats =
        (* concatenate the result of calling [to_log_entries] on each field *)
        ...
    ]} *)
