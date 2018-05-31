(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PolyVariantEqual
module F = Format

module GlobalVar = struct
  include Pvar

  let matches ~caller ~callee = Pvar.equal caller callee

  let pp fmt v =
    F.fprintf fmt "%a|%a" Mangled.pp (Pvar.get_name v) Pvar.pp_translation_unit
      (Pvar.get_translation_unit v)
end

module GlobalVarSet = PrettyPrintable.MakePPSet (GlobalVar)

module TraceElem = struct
  module Kind = GlobalVar

  type t = {site: CallSite.t; kind: [`Call | `Access] * Kind.t} [@@deriving compare]

  let call_site {site} = site

  let kind {kind} = snd kind

  let make ?indexes:_ kind site = {kind= (`Call, kind); site}

  let with_callsite {kind= _, kind} site = {kind= (`Call, kind); site}

  let pp fmt {site; kind} =
    F.fprintf fmt "%saccess to %a"
      (match fst kind with `Call -> "indirect " | `Access -> "")
      Kind.pp (snd kind) ;
    match fst kind with `Call -> F.fprintf fmt " via call to %a" CallSite.pp site | `Access -> ()


  module Set = PrettyPrintable.MakePPSet (struct
    (* Don't use nonrec due to https://github.com/janestreet/ppx_compare/issues/2 *)
    (* type nonrec t = t [@@deriving compare]; *)
    type nonrec t = t

    let compare = compare

    let pp = pp
  end)
end

include SinkTrace.Make (TraceElem)

let make_access kind loc =
  let site = CallSite.make Typ.Procname.empty_block loc in
  {TraceElem.kind= (`Access, kind); site}


let is_intraprocedural_access {TraceElem.kind= kind, _} = kind = `Access

let trace_of_error loc gname path =
  let desc_of_sink sink =
    if is_intraprocedural_access sink then Format.asprintf "%a" Sink.pp sink
    else
      let callsite = Sink.call_site sink in
      Format.asprintf "call to %a" Typ.Procname.pp (CallSite.pname callsite)
  in
  let sink_should_nest sink = not (is_intraprocedural_access sink) in
  let trace_elem_of_global =
    Errlog.make_trace_element 0 loc (Format.asprintf "initialization of %s" gname) []
  in
  trace_elem_of_global :: to_sink_loc_trace ~desc_of_sink ~sink_should_nest path
