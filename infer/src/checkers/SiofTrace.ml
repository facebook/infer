(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module F = Format
module L = Logging

module Global = struct
  type t = Pvar.t
  let compare = Pvar.compare
  let pp fmt pvar = (Pvar.pp Pp.text) fmt pvar
end

module TraceElem = struct
  module Kind = Global

  type t = {
    site : CallSite.t;
    kind: [`Call | `Access] * Kind.t;
  } [@@deriving compare]

  let call_site { site; } = site

  let kind { kind; } = snd kind

  let make kind site = { kind = (`Call, kind); site; }

  let with_callsite { kind=(_, kind); } site = { kind=(`Call, kind); site; }

  let pp fmt { site; kind; } =
    F.fprintf fmt "Access to %a at %a" Kind.pp (snd kind) CallSite.pp site

  module Set = PrettyPrintable.MakePPSet (struct
      type nonrec t = t
      let compare = compare
      let pp_element = pp
    end)

end

include SinkTrace.Make(TraceElem)

let make_access kind loc =
  let site = CallSite.make Procname.empty_block loc in
  { TraceElem.kind = (`Access, kind); site; }

let is_intraprocedural_access { TraceElem.kind=(kind, _); } = kind = `Access
