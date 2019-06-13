(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = Location.t list

let encode decoded = Base64.encode_exn (Marshal.to_string decoded [])

let decode encoded = Marshal.from_string (Base64.decode_exn encoded) 0
