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

  val read_full : Procdesc.t -> Typ.Procname.t -> (Procdesc.t * t) option

  val read : Procdesc.t -> Typ.Procname.t -> t option
end

module Make (P : Payload) : S with type t = P.t = struct
  type t = P.t

  let update_payloads = Field.fset P.field

  let of_payloads = Field.get P.field

  let update_summary p (summary : Summary.t) =
    {summary with payloads= update_payloads summary.payloads (Some p)}


  let of_summary (summary : Summary.t) = of_payloads summary.payloads

  let read_full caller_pdesc callee_pname =
    let open Option.Monad_infix in
    Ondemand.analyze_proc_name ~caller_pdesc callee_pname
    >>= fun summary ->
    of_summary summary
    >>| fun payload ->
    (* we could return the proc_desc if some client needed this but this would complicate the return
       type so for now let's not do that *)
    (Summary.get_proc_desc summary, payload)


  let read caller_pdesc callee_pname = read_full caller_pdesc callee_pname |> Option.map ~f:snd
end
