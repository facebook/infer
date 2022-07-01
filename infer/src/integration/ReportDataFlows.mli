(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type flow_type = FromSource | ToSink

val report_data_flows_of_procname : string -> flow_type:flow_type -> unit
