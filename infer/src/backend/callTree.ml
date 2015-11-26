(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)


module F = Format


type t =
  | Direct of Procname.t
  | Indirect of Procname.t * t list


let pp fmt tree =
  let rec loop stack = function
    | Direct pname ->
        F.fprintf fmt "%s -> %s" stack (Procname.to_string pname)
    | Indirect (pname, l) ->
        let stack' = stack ^ " -> " ^ (Procname.to_string pname) in
        IList.iter (loop stack') l in
  loop "@" tree
