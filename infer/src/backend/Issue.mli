(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t =
  { proc_name: Typ.Procname.t
  ; proc_location: Location.t
  ; err_key: Errlog.err_key
  ; err_data: Errlog.err_data }

val sort_filter_issues : t list -> t list
