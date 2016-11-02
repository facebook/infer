(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! Utils

module F = Format
module L = Logging

module Globals = struct
  type t = Pvar.t
  let compare = Pvar.compare
  let pp fmt pvar = (Pvar.pp pe_text) fmt pvar
end

include SinkTrace.Make(struct
    module Kind = Globals

    type t = {
      site : CallSite.t;
      kind: Kind.t;
    }

    let call_site { site; } = site

    let kind { kind; } = kind

    let make kind site = { kind; site; }

    let to_callee t site = { t with site; }

    let compare t1 t2 =
      CallSite.compare t1.site t2.site
      |> next Kind.compare t1.kind t2.kind

    let pp fmt { site; kind; } =
      F.fprintf fmt "Access to %a at %a" Kind.pp kind CallSite.pp site

    module Set = PrettyPrintable.MakePPSet (struct
        type nonrec t = t
        let compare = compare
        let pp_element = pp
      end)

  end)
