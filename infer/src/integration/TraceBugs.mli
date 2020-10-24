(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

val explore :
     selector_limit:int option
  -> report_txt:string
  -> report_json:string
  -> show_source_context:bool
  -> selected:[`All | `Select of int] option
  -> max_nested_level:int option
  -> unit

val gen_html_report :
     show_source_context:bool
  -> max_nested_level:int option
  -> report_json:string
  -> report_html_dir:string
  -> unit
