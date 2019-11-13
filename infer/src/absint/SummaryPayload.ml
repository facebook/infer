(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module type Payload = sig
  type t

  val field : (Payloads.t, t option) Field.t
end

module type S = sig
  type t

  val update_summary : t -> Summary.t -> Summary.t

  val of_summary : Summary.t -> t option

  val read_full : caller_summary:Summary.t -> callee_pname:Typ.Procname.t -> (Procdesc.t * t) option

  val read : caller_summary:Summary.t -> callee_pname:Typ.Procname.t -> t option

  val read_toplevel_procedure : Typ.Procname.t -> t option
end

module Make (P : Payload) : S with type t = P.t = struct
  type t = P.t

  let update_payloads = Field.fset P.field

  let of_payloads = Field.get P.field

  let update_summary p (summary : Summary.t) =
    {summary with payloads= update_payloads summary.payloads (Some p)}


  let of_summary (summary : Summary.t) = of_payloads summary.payloads

  let get_payload analysis_result =
    let open Option.Monad_infix in
    analysis_result
    >>= fun summary -> of_summary summary >>| fun payload -> (Summary.get_proc_desc summary, payload)


  let read_full ~caller_summary ~callee_pname =
    Ondemand.analyze_proc_name ~caller_summary callee_pname |> get_payload


  let read ~caller_summary ~callee_pname =
    Ondemand.analyze_proc_name ~caller_summary callee_pname |> get_payload |> Option.map ~f:snd


  let read_toplevel_procedure callee_pname =
    Ondemand.analyze_proc_name_no_caller callee_pname |> get_payload |> Option.map ~f:snd
end
