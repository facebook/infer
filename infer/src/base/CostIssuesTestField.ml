(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = IsOnUIThread | Procedure | File | Cost | AutoreleasepoolSize | Trace [@@deriving equal]

let all_fields =
  [ File
  ; Procedure
  ; (if Config.cost_tests_only_autoreleasepool then AutoreleasepoolSize else Cost)
  ; IsOnUIThread
  ; Trace ]
