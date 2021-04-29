(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let file_exists ?follow_symlinks path =
  match Sys.file_exists ?follow_symlinks path with `Yes -> true | `No | `Unknown -> false
