(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

type t = PulseAbductiveDomain.PrePost.t list

let of_posts pdesc posts = List.map posts ~f:(PulseAbductiveDomain.PrePost.of_post pdesc)

let pp fmt summary =
  F.open_vbox 0 ;
  F.fprintf fmt "%d pre/post(s)@;" (List.length summary) ;
  List.iteri summary ~f:(fun i pre_post ->
      F.fprintf fmt "#%d: @[%a@]@;" i PulseAbductiveDomain.PrePost.pp pre_post ) ;
  F.close_box ()
