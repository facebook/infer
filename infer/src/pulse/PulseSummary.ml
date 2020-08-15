(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseDomainInterface

type t = ExecutionDomain.t list

let pp fmt summary =
  F.open_vbox 0 ;
  F.fprintf fmt "%d pre/post(s)@;" (List.length summary) ;
  List.iteri summary ~f:(fun i pre_post ->
      F.fprintf fmt "#%d: @[%a@]@;" i ExecutionDomain.pp pre_post ) ;
  F.close_box ()


let of_posts pdesc posts =
  AnalysisCallbacks.html_debug_new_node_session (Procdesc.get_exit_node pdesc)
    ~pp_name:(fun fmt -> F.pp_print_string fmt "pulse summary creation")
    ~f:(fun () -> ExecutionDomain.of_posts pdesc posts)
