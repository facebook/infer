(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module MF = MarkupFormatter

module type FiniteSet = sig
  include AbstractDomain.FiniteSetS

  val with_callsite : t -> CallSite.t -> t
end

module type Element = sig
  include PrettyPrintable.PrintableOrderedType

  val describe : Format.formatter -> t -> unit
end

module type CallPrinter = PrettyPrintable.PrintableType with type t = CallSite.t

module DefaultCallPrinter : CallPrinter = struct
  type t = CallSite.t

  let pp fmt callsite =
    F.fprintf fmt "Method call: %a" (MF.wrap_monospaced Procname.pp) (CallSite.pname callsite)
end

type 'a comparator = 'a -> Location.t -> 'a -> Location.t -> int

module type Comparator = sig
  type elem_t

  val comparator : elem_t comparator
end

module type TraceElem = sig
  type elem_t

  type t = private {elem: elem_t; loc: Location.t; trace: CallSite.t list}

  include Element with type t := t

  val make : elem_t -> Location.t -> t

  val map : f:(elem_t -> elem_t) -> t -> t

  val get_loc : t -> Location.t

  val make_loc_trace : ?nesting:int -> t -> Errlog.loc_trace

  val with_callsite : t -> CallSite.t -> t

  module FiniteSet : FiniteSet with type elt = t
end

module MakeTraceElemWithComparator
    (Elem : Element)
    (CallPrinter : CallPrinter)
    (Comp : Comparator with type elem_t = Elem.t) : TraceElem with type elem_t = Elem.t = struct
  type elem_t = Elem.t

  module T = struct
    type t = {elem: Elem.t; loc: Location.t; trace: CallSite.t list}

    let compare {elem; loc} {elem= elem'; loc= loc'} = Comp.comparator elem loc elem' loc'

    let pp fmt {elem; loc; trace} =
      let pp_trace fmt trace = PrettyPrintable.pp_collection ~pp_item:CallSite.pp fmt trace in
      F.fprintf fmt "{@[elem= %a;@;loc= %a;@;trace= %a@]}" Elem.pp elem Location.pp loc pp_trace
        trace


    let describe fmt {elem} = Elem.describe fmt elem
  end

  include T

  let make elem loc = {elem; loc; trace= []}

  let map ~f (trace_elem : t) =
    let elem' = f trace_elem.elem in
    if phys_equal trace_elem.elem elem' then trace_elem else {trace_elem with elem= elem'}


  let get_loc {loc; trace} = match trace with [] -> loc | hd :: _ -> CallSite.loc hd

  let make_loc_trace ?(nesting = 0) e =
    let call_trace, nesting =
      List.fold e.trace ~init:([], nesting) ~f:(fun (tr, ns) callsite ->
          let descr = F.asprintf "%a" CallPrinter.pp callsite in
          let call = Errlog.make_trace_element ns (CallSite.loc callsite) descr [] in
          (call :: tr, ns + 1) )
    in
    let endpoint_descr = F.asprintf "%a" Elem.describe e.elem in
    let endpoint = Errlog.make_trace_element nesting e.loc endpoint_descr [] in
    List.rev (endpoint :: call_trace)


  let with_callsite elem callsite = {elem with trace= callsite :: elem.trace}

  module FiniteSet = struct
    include AbstractDomain.FiniteSet (T)

    let with_callsite astate callsite = map (fun e -> with_callsite e callsite) astate
  end
end

module MakeTraceElem (Elem : Element) (CallPrinter : CallPrinter) :
  TraceElem with type elem_t = Elem.t = struct
  module Comp = struct
    type elem_t = Elem.t

    let comparator elem loc elem' loc' = [%compare: Elem.t * Location.t] (elem, loc) (elem', loc')
  end

  include MakeTraceElemWithComparator (Elem) (CallPrinter) (Comp)
end

module MakeTraceElemModuloLocation (Elem : Element) (CallPrinter : CallPrinter) :
  TraceElem with type elem_t = Elem.t = struct
  module Comp = struct
    type elem_t = Elem.t

    let comparator elem _loc elem' _loc' = Elem.compare elem elem'
  end

  include MakeTraceElemWithComparator (Elem) (CallPrinter) (Comp)
end
