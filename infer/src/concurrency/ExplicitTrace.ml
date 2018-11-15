(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module MF = MarkupFormatter

module type FiniteSet = sig
  include PrettyPrintable.PPSet

  include AbstractDomain.WithBottom with type astate = t

  val with_callsite : astate -> CallSite.t -> astate
end

module type TraceElem = sig
  type elem_t

  type t = private {elem: elem_t; loc: Location.t; trace: CallSite.t list}

  include PrettyPrintable.PrintableOrderedType with type t := t

  val make : elem_t -> Location.t -> t

  val get_loc : t -> Location.t

  val make_loc_trace : ?nesting:int -> t -> Errlog.loc_trace

  val with_callsite : t -> CallSite.t -> t

  module FiniteSet : FiniteSet with type elt = t
end

module MakeTraceElem (Elem : PrettyPrintable.PrintableOrderedType) :
  TraceElem with type elem_t = Elem.t = struct
  type elem_t = Elem.t

  module T = struct
    type t = {elem: Elem.t; loc: Location.t; trace: CallSite.t list [@compare.ignore]}
    [@@deriving compare]

    let pp fmt {elem} = Elem.pp fmt elem
  end

  include T

  let make elem loc = {elem; loc; trace= []}

  let get_loc {loc; trace} = match trace with [] -> loc | hd :: _ -> CallSite.loc hd

  let make_loc_trace ?(nesting = 0) e =
    let call_trace, nesting =
      List.fold e.trace ~init:([], nesting) ~f:(fun (tr, ns) callsite ->
          let elem_descr =
            F.asprintf "Method call: %a"
              (MF.wrap_monospaced Typ.Procname.pp)
              (CallSite.pname callsite)
          in
          let elem = Errlog.make_trace_element ns (CallSite.loc callsite) elem_descr [] in
          (elem :: tr, ns + 1) )
    in
    let endpoint_descr = F.asprintf "%a" Elem.pp e.elem in
    let endpoint = Errlog.make_trace_element nesting e.loc endpoint_descr [] in
    List.rev (endpoint :: call_trace)


  let with_callsite elem callsite = {elem with trace= callsite :: elem.trace}

  module FiniteSet = struct
    include AbstractDomain.FiniteSet (T)

    let with_callsite astate callsite = map (fun e -> with_callsite e callsite) astate
  end
end
