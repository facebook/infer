(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = Location.t list

let encode decoded = B64.encode (Marshal.to_string decoded [])

let decode encoded = Marshal.from_string (B64.decode encoded) 0
