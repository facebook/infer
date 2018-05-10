(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module type Payload = sig
  type t

  val update_summary : t -> Summary.t -> Summary.t

  val of_summary : Summary.t -> t option
end

module type S = sig
  type t

  val update_summary : t -> Summary.t -> Summary.t

  val read_summary : Procdesc.t -> Typ.Procname.t -> t option
end

module Make (P : Payload) : S with type t = P.t = struct
  type t = P.t

  let update_summary = P.update_summary

  let read_summary caller_pdesc callee_pname =
    Ondemand.analyze_proc_name ~caller_pdesc callee_pname |> Option.bind ~f:P.of_summary
end
