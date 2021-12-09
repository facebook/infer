(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t =
  { err_data: Errlog.err_data
  ; err_key: Errlog.err_key
  ; proc_location_opt: Location.t option
  ; proc_name: Procname.t }

val sort_filter_issues : t list -> t list
