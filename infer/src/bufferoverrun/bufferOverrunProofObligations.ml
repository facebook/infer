(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open! AbstractDomain.Types
module F = Format
module L = Logging
module ItvPure = Itv.ItvPure
module MF = MarkupFormatter
module ValTraceSet = BufferOverrunTrace.Set

module Condition = struct
  type t = {idx: ItvPure.astate; size: ItvPure.astate} [@@deriving compare]

  let get_symbols c = ItvPure.get_symbols c.idx @ ItvPure.get_symbols c.size

  let set_size_pos : t -> t =
    fun c ->
      let size' = ItvPure.make_positive c.size in
      if phys_equal size' c.size then c else {c with size= size'}

  let pp : F.formatter -> t -> unit =
    fun fmt c ->
      let c = set_size_pos c in
      F.fprintf fmt "%a < %a" ItvPure.pp c.idx ItvPure.pp c.size

  let pp_description : F.formatter -> t -> unit =
    fun fmt c ->
      let c = set_size_pos c in
      F.fprintf fmt "Offset: %a Size: %a" ItvPure.pp c.idx ItvPure.pp c.size

  let make : idx:ItvPure.t -> size:ItvPure.t -> t option =
    fun ~idx ~size ->
      if ItvPure.is_invalid idx || ItvPure.is_invalid size then None else Some {idx; size}

  let have_similar_bounds {idx= lidx; size= lsiz} {idx= ridx; size= rsiz} =
    ItvPure.have_similar_bounds lidx ridx && ItvPure.have_similar_bounds lsiz rsiz

  let xcompare ~lhs:{idx= lidx; size= lsiz} ~rhs:{idx= ridx; size= rsiz} =
    let idxcmp = ItvPure.xcompare ~lhs:lidx ~rhs:ridx in
    let sizcmp = ItvPure.xcompare ~lhs:lsiz ~rhs:rsiz in
    match (idxcmp, sizcmp) with
    | `Equal, `Equal
     -> `Equal
    | `NotComparable, _
     -> `NotComparable
    | `Equal, (`LeftSmallerThanRight | `LeftSubsumesRight)
     -> `LeftSubsumesRight
    | `Equal, (`RightSmallerThanLeft | `RightSubsumesLeft)
     -> `RightSubsumesLeft
    | `LeftSubsumesRight, (`Equal | `LeftSubsumesRight)
     -> `LeftSubsumesRight
    | `RightSubsumesLeft, (`Equal | `RightSubsumesLeft)
     -> `RightSubsumesLeft
    | (`LeftSmallerThanRight | `RightSmallerThanLeft), _
     -> let lidxpos = ItvPure.le_sem ItvPure.zero lidx in
        let ridxpos = ItvPure.le_sem ItvPure.zero ridx in
        if not (ItvPure.equal lidxpos ridxpos) then `NotComparable
        else if ItvPure.is_true lidxpos then
          (* both idx >= 0 *)
          match (idxcmp, sizcmp) with
          | `LeftSmallerThanRight, (`Equal | `RightSmallerThanLeft | `RightSubsumesLeft)
           -> `RightSubsumesLeft
          | `RightSmallerThanLeft, (`Equal | `LeftSmallerThanRight | `LeftSubsumesRight)
           -> `LeftSubsumesRight
          | _
           -> `NotComparable
        else if ItvPure.is_false lidxpos then
          (* both idx < 0, size doesn't matter *)
          match idxcmp with
          | `LeftSmallerThanRight
           -> `LeftSubsumesRight
          | `RightSmallerThanLeft
           -> `RightSubsumesLeft
          | `Equal
           -> `Equal
          | _
           -> `NotComparable
        else `NotComparable
    | _
     -> `NotComparable

  let filter1 : t -> bool =
    fun c ->
      ItvPure.is_top c.idx || ItvPure.is_top c.size
      || Itv.Bound.eq (ItvPure.lb c.idx) Itv.Bound.MInf
      || Itv.Bound.eq (ItvPure.lb c.size) Itv.Bound.MInf
      || ItvPure.is_nat c.idx && ItvPure.is_nat c.size

  let filter2 : t -> bool =
    fun c ->
      (* basically, alarms involving infinity are filtered *)
      (not (ItvPure.is_finite c.idx) || not (ItvPure.is_finite c.size))
      && (* except the following cases *)
         not
           ( Itv.Bound.is_not_infty (ItvPure.lb c.idx)
             && (* idx non-infty lb < 0 *)
                Itv.Bound.lt (ItvPure.lb c.idx) Itv.Bound.zero
           || Itv.Bound.is_not_infty (ItvPure.lb c.idx)
              && (* idx non-infty lb > size lb *)
                 Itv.Bound.gt (ItvPure.lb c.idx) (ItvPure.lb c.size)
           || Itv.Bound.is_not_infty (ItvPure.lb c.idx)
              && (* idx non-infty lb > size ub *)
                 Itv.Bound.gt (ItvPure.lb c.idx) (ItvPure.ub c.size)
           || Itv.Bound.is_not_infty (ItvPure.ub c.idx)
              && (* idx non-infty ub > size lb *)
                 Itv.Bound.gt (ItvPure.ub c.idx) (ItvPure.lb c.size)
           || Itv.Bound.is_not_infty (ItvPure.ub c.idx)
              && (* idx non-infty ub > size ub *)
                 Itv.Bound.gt (ItvPure.ub c.idx) (ItvPure.ub c.size) )

  (* check buffer overrun and return its confidence *)
  let check : t -> IssueType.t option =
    fun c ->
      (* idx = [il, iu], size = [sl, su], we want to check that 0 <= idx < size *)
      let c' = set_size_pos c in
      (* if sl < 0, use sl' = 0 *)
      let not_overrun = ItvPure.lt_sem c'.idx c'.size in
      let not_underrun = ItvPure.le_sem ItvPure.zero c'.idx in
      (* il >= 0 and iu < sl, definitely not an error *)
      if ItvPure.is_one not_overrun && ItvPure.is_one not_underrun then None
        (* iu < 0 or il >= su, definitely an error *)
      else if ItvPure.is_zero not_overrun || ItvPure.is_zero not_underrun then
        Some IssueType.buffer_overrun_l1 (* su <= iu < +oo, most probably an error *)
      else if Itv.Bound.is_not_infty (ItvPure.ub c.idx)
              && Itv.Bound.le (ItvPure.ub c.size) (ItvPure.ub c.idx)
      then Some IssueType.buffer_overrun_l2 (* symbolic il >= sl, probably an error *)
      else if Itv.Bound.is_symbolic (ItvPure.lb c.idx)
              && Itv.Bound.le (ItvPure.lb c'.size) (ItvPure.lb c.idx)
      then Some IssueType.buffer_overrun_s2 (* other symbolic bounds are probably too noisy *)
      else if Config.bo_debug <= 3 && (ItvPure.is_symbolic c.idx || ItvPure.is_symbolic c.size)
      then None
      else if filter1 c then Some IssueType.buffer_overrun_l5
      else if filter2 c then Some IssueType.buffer_overrun_l4
      else Some IssueType.buffer_overrun_l3

  let subst : t -> Itv.Bound.t bottom_lifted Itv.SubstMap.t -> t option =
    fun c bound_map ->
      match (ItvPure.subst c.idx bound_map, ItvPure.subst c.size bound_map) with
      | NonBottom idx, NonBottom size
       -> Some {idx; size}
      | _
       -> None
end

module ConditionTrace = struct
  type cond_trace =
    | Intra of Typ.Procname.t
    | Inter of Typ.Procname.t * Typ.Procname.t * Location.t
    [@@deriving compare]

  type t =
    { proc_name: Typ.Procname.t
    ; loc: Location.t
    ; id: string
    ; cond_trace: cond_trace
    ; val_traces: ValTraceSet.t }
    [@@deriving compare]

  let pp_location : F.formatter -> t -> unit = fun fmt ct -> Location.pp_file_pos fmt ct.loc

  let pp : F.formatter -> t -> unit =
    fun fmt ct ->
      if Config.bo_debug <= 1 then F.fprintf fmt "at %a" pp_location ct
      else
        match ct.cond_trace with
        | Inter (_, pname, loc)
         -> let pname = Typ.Procname.to_string pname in
            F.fprintf fmt "at %a by call %s() at %a (%a)" pp_location ct pname Location.pp_file_pos
              loc ValTraceSet.pp ct.val_traces
        | Intra _
         -> F.fprintf fmt "%a (%a)" pp_location ct ValTraceSet.pp ct.val_traces

  let pp_description : F.formatter -> t -> unit =
    fun fmt ct ->
      match ct.cond_trace with
      | Inter (_, pname, _)
        when Config.bo_debug >= 1 || not (SourceFile.is_cpp_model ct.loc.Location.file)
       -> F.fprintf fmt " %@ %a by call %a " pp_location ct MF.pp_monospaced
            (Typ.Procname.to_string pname ^ "()")
      | _
       -> ()

  let get_location : t -> Location.t = fun ct -> ct.loc

  let get_cond_trace : t -> cond_trace = fun ct -> ct.cond_trace

  let get_proc_name : t -> Typ.Procname.t = fun ct -> ct.proc_name

  let get_caller_proc_name ct =
    match ct.cond_trace with Intra pname -> pname | Inter (caller_pname, _, _) -> caller_pname

  let make : Typ.Procname.t -> Location.t -> string -> ValTraceSet.t -> t =
    fun proc_name loc id val_traces -> {proc_name; loc; id; cond_trace= Intra proc_name; val_traces}

  let make_call_and_subst ~traces_caller ~caller_pname ~callee_pname loc ct =
    let val_traces = ValTraceSet.instantiate ~traces_caller ~traces_callee:ct.val_traces loc in
    {ct with cond_trace= Inter (caller_pname, callee_pname, loc); val_traces}
end

module ConditionSet = struct
  type condition_with_trace = {cond: Condition.t; trace: ConditionTrace.t}

  type t = condition_with_trace list

  (* invariant: add_one of one of the elements should return the original list *)

  let empty = []

  let compare_by_location cwt1 cwt2 =
    Location.compare (ConditionTrace.get_location cwt1.trace)
      (ConditionTrace.get_location cwt2.trace)

  let try_merge ~existing ~new_ =
    if Condition.have_similar_bounds existing.cond new_.cond then
      match Condition.xcompare ~lhs:existing.cond ~rhs:new_.cond with
      | `Equal
       -> (* keep the first one in the code *)
          if compare_by_location existing new_ <= 0 then `DoNotAddAndStop
          else `RemoveExistingAndContinue
      (* we don't want to remove issues that would end up in a higher bucket,
         e.g. [a, b] < [c, d] is subsumed by [a, +oo] < [c, d] but the latter is less precise *)
      | `LeftSubsumesRight
       -> `DoNotAddAndStop
      | `RightSubsumesLeft
       -> `RemoveExistingAndContinue
      | `NotComparable
       -> `KeepExistingAndContinue
    else `KeepExistingAndContinue

  let add_one condset new_ =
    let rec aux ~new_ acc ~same = function
      | []
       -> if same then new_ :: condset else new_ :: acc
      | existing :: rest as existings ->
        match try_merge ~existing ~new_ with
        | `DoNotAddAndStop
         -> if same then condset else List.rev_append acc existings
        | `RemoveExistingAndContinue
         -> aux ~new_ acc ~same:false rest
        | `KeepExistingAndContinue
         -> aux ~new_ (existing :: acc) ~same rest
    in
    aux ~new_ [] ~same:true condset

  let join condset1 condset2 = List.fold_left ~f:add_one condset1 ~init:condset2

  let add_bo_safety pname loc id ~idx ~size val_traces condset =
    match Condition.make ~idx ~size with
    | None
     -> condset
    | Some cond
     -> let trace = ConditionTrace.make pname loc id val_traces in
        let cwt = {cond; trace} in
        join [cwt] condset

  let subst condset (bound_map, trace_map) caller_pname callee_pname loc =
    let subst_add_cwt condset cwt =
      match Condition.get_symbols cwt.cond with
      | []
       -> add_one condset cwt
      | symbols ->
        match Condition.subst cwt.cond bound_map with
        | None
         -> condset
        | Some cond
         -> let traces_caller =
              List.fold symbols ~init:ValTraceSet.empty ~f:(fun val_traces symbol ->
                  match Itv.SubstMap.find symbol trace_map with
                  | symbol_trace
                   -> ValTraceSet.join symbol_trace val_traces
                  | exception Not_found
                   -> val_traces )
            in
            let make_call_and_subst trace =
              ConditionTrace.make_call_and_subst ~traces_caller ~caller_pname ~callee_pname loc
                trace
            in
            let trace = make_call_and_subst cwt.trace in
            add_one condset {cond; trace}
    in
    List.fold condset ~f:subst_add_cwt ~init:[]

  let iter ~f condset = List.iter condset ~f:(fun cwt -> f cwt.cond cwt.trace)

  let pp_cwt fmt cwt = F.fprintf fmt "%a %a" Condition.pp cwt.cond ConditionTrace.pp cwt.trace

  let pp_summary : F.formatter -> t -> unit =
    fun fmt condset ->
      let pp_sep fmt () = F.fprintf fmt ", @," in
      F.fprintf fmt "@[<v 0>Safety conditions:@," ;
      F.fprintf fmt "@[<hov 2>{ " ;
      F.pp_print_list ~pp_sep pp_cwt fmt condset ;
      F.fprintf fmt " }@]" ;
      F.fprintf fmt "@]"

  let pp : Format.formatter -> t -> unit =
    fun fmt condset ->
      let pp_sep fmt () = F.fprintf fmt ", @," in
      F.fprintf fmt "@[<v 2>Safety conditions :@," ;
      F.fprintf fmt "@[<hov 1>{" ;
      F.pp_print_list ~pp_sep pp_cwt fmt condset ;
      F.fprintf fmt " }@]" ;
      F.fprintf fmt "@]"
end

let description cond trace =
  F.asprintf "%a%a" Condition.pp_description cond ConditionTrace.pp_description trace
