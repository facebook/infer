(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type err_data_ = Errlog.err_data

type proc_name_ = Typ.Procname.t

type t =
  {proc_name: proc_name_; proc_location: Location.t; err_key: Errlog.err_key; err_data: err_data_}

val sort_filter_issues : t list -> t list
