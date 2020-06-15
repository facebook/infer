(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val is_config_class : 'pvar -> bool

val is_config_check : 'tenv -> 'pname -> bool

val is_marker_start : 'tenv -> 'pname -> bool

val is_marker_end : 'tenv -> 'pname -> bool
