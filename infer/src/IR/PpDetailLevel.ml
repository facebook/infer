(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = FullNameOnly | NameOnly | Non_verbose | Simple | Verbose

let is_verbose v = match v with Verbose -> true | _ -> false
