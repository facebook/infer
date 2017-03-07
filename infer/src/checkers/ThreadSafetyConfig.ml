(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module F = Format

module AnnotationAliases = struct

  let of_json = function
    | `List aliases -> List.map ~f:Yojson.Basic.Util.to_string aliases
    | _ -> failwith "Couldn't parse thread-safety annotation aliases; expected list of strings"
end
