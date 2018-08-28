(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module type Payload = sig
  type t

  val update_payloads : t -> Payloads.t -> Payloads.t

  val of_payloads : Payloads.t -> t option
end

module type S = sig
  type t

  val update_summary : t -> Summary.t -> Summary.t

  val of_summary : Summary.t -> t option

  val read : Procdesc.t -> Typ.Procname.t -> t option
end

module Make (P : Payload) : S with type t = P.t = struct
  type t = P.t

  let update_summary p (summary : Summary.t) =
    {summary with payloads= P.update_payloads p summary.payloads}


  let of_summary (summary : Summary.t) = P.of_payloads summary.payloads

  let read caller_pdesc callee_pname =
    Ondemand.analyze_proc_name ~caller_pdesc callee_pname |> Option.bind ~f:of_summary
end
