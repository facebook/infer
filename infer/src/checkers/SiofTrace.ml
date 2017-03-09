(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open! PVariant

module F = Format
module L = Logging

module GlobalsAccesses = PrettyPrintable.MakePPSet (struct
    type t = (Pvar.t * Location.t)
    let compare (v1, l1) (v2, l2) =
      (* compare by loc first to present reports in the right order *)
      [%compare : (Location.t * Pvar.t)] (l1, v1) (l2, v2)
    let pp fmt (v, _) =
      F.fprintf fmt "%a" Mangled.pp (Pvar.get_name v);
      match Pvar.get_source_file v with
      | Some fname -> F.fprintf fmt "%a" SourceFile.pp fname
      | None -> ()
  end)

module TraceElem = struct
  module Kind = GlobalsAccesses

  type t = {
    site : CallSite.t;
    kind: [`Call | `Access] * Kind.t;
  } [@@deriving compare]

  let call_site { site; } = site

  let kind { kind; } = snd kind

  let make kind site = { kind = (`Call, kind); site; }

  let with_callsite { kind=(_, kind); } site = { kind=(`Call, kind); site; }

  let pp fmt { site; kind; } =
    F.fprintf fmt "%saccess to %a"
      (match fst kind with | `Call -> "indirect " | `Access -> "")
      Kind.pp (snd kind);
    match fst kind with
    | `Call -> F.fprintf fmt " at %a" CallSite.pp site
    | `Access -> ()

  module Set = PrettyPrintable.MakePPSet (struct
      (* Don't use nonrec due to https://github.com/janestreet/ppx_compare/issues/2 *)
      (* type nonrec t = t [@@deriving compare]; *)
      type nonrec t = t
      let compare = compare
      let pp = pp
    end)

end

include SinkTrace.Make(TraceElem)

let make_access kind loc =
  let site = CallSite.make Typ.Procname.empty_block loc in
  { TraceElem.kind = (`Access, kind); site; }

let is_intraprocedural_access { TraceElem.kind=(kind, _); } = kind = `Access

let trace_of_error loc gname path =
  let desc_of_sink sink =
    let callsite = Sink.call_site sink in
    if is_intraprocedural_access sink then
      Format.asprintf "%a" Sink.pp sink
    else
      Format.asprintf "call to %a" Typ.Procname.pp (CallSite.pname callsite) in
  let sink_should_nest sink = not (is_intraprocedural_access sink) in
  let trace_elem_of_global =
    Errlog.make_trace_element 0 loc
      (Format.asprintf "initialization of %s" gname)
      [] in
  let trace =
    let trace_with_set_of_globals = to_sink_loc_trace ~desc_of_sink ~sink_should_nest path in
    (* the last element of the trace gotten by [to_sink_loc_trace] contains a set of procedure-local
       accesses to globals. We want to remove it in exchange for as many trace elems as there are
       accesses. *)
    match (List.rev trace_with_set_of_globals, snd path) with
    | telem::rest, ({TraceElem.kind = (`Access, globals)}, _)::_ ->
        let nesting = telem.Errlog.lt_level in
        let add_trace_elem_of_access err_trace (global, loc) =
          Errlog.make_trace_element nesting loc
            (Format.asprintf "access to %a" Mangled.pp (Pvar.get_name global))
            []
          ::err_trace in
        GlobalsAccesses.elements globals
        |> List.fold ~f:add_trace_elem_of_access ~init:rest
        |> List.rev
    | _ -> trace_with_set_of_globals
  in
  trace_elem_of_global::trace
