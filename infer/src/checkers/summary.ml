(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module type Helper = sig
  type summary

  (* update the specs payload with [summary] *)
  val update_payload : summary -> Specs.payload -> Specs.payload
  (* extract [summmary] from the specs payload *)
  val read_from_payload : Specs.payload -> summary
end

module type S = sig
  type summary

  (* write the summary for [name] to persistent storage *)
  val write_summary : Procname.t -> summary -> unit
  (* read and return the summary for [pname]. does the analysis to create the summary if needed *)
  val read_summary : Procname.t -> summary option
end

module Make (H : Helper) = struct
  type summary = H.summary

  let write_summary pname summary =
    match Specs.get_summary pname with
    | Some global_summary ->
        let payload = H.update_payload summary global_summary.Specs.payload in
        Specs.add_summary pname { global_summary with payload }
    | None ->
        Printf.sprintf "Summary for %s should exist, but does not!@." (Procname.to_string pname)
        |> failwith

  let read_summary pname =
    (* this is reprehensible. but the caller_pdesc is only used for debug printing *)
    (* TODO: fix the ondemand API so we can choose to pass a pdesc option *)
    let dummy_caller_pdesc =
      Cfg.Procdesc.create (Cfg.Node.create_cfg ()) (ProcAttributes.default pname Config.Java) in
    Ondemand.analyze_proc_name ~propagate_exceptions:false dummy_caller_pdesc pname;
    match Specs.get_summary pname with
    | None -> None
    | Some summary -> Some (H.read_from_payload summary.Specs.payload)
end
