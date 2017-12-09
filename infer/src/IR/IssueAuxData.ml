(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

type t = Typ.Procname.t * RacerDDomain.TraceElem.t * Location.t list

let encode decoded = B64.encode (Marshal.to_string decoded [])

let decode encoded = Marshal.from_string (B64.decode encoded) 0
