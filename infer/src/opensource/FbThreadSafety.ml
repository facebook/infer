(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

let is_custom_init _ _ = false

let is_logging_method _ = false

let get_fbthreadsafe_class_annot _ _ = None

let message_fbthreadsafe_class _ _ = ""
