(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! AbstractDomain.Types
module F = Format
module L = Logging
module Bound = Bounds.Bound
module Dom = BufferOverrunDomain
module ItvPure = Itv.ItvPure
module MF = MarkupFormatter
module Sem = BufferOverrunSemantics
module ValTrace = BufferOverrunTrace

module ConditionTrace = struct
  type intra_cond_trace = Intra | Inter of {call_site: Location.t; callee_pname: Procname.t}
  [@@deriving compare]

  type 'cond_trace t0 =
    {cond_trace: 'cond_trace; issue_location: Location.t; val_traces: ValTrace.Issue.t}
  [@@deriving compare]

  type t = intra_cond_trace t0 [@@deriving compare]

  type summary_t = unit t0

  let pp_summary : F.formatter -> _ t0 -> unit =
   fun fmt ct -> F.fprintf fmt "at %a" Location.pp_file_pos ct.issue_location


  let pp : F.formatter -> t -> unit =
   fun fmt ct ->
    pp_summary fmt ct ;
    if Config.bo_debug > 1 then
      match ct.cond_trace with
      | Inter {callee_pname; call_site} ->
          let pname = Procname.to_string callee_pname in
          F.fprintf fmt " by call to %s at %a (%a)" pname Location.pp_file_pos call_site
            ValTrace.Issue.pp ct.val_traces
      | Intra ->
          F.fprintf fmt " (%a)" ValTrace.Issue.pp ct.val_traces


  let pp_description : F.formatter -> t -> unit =
   fun fmt ct ->
    match ct.cond_trace with
    | Inter {callee_pname} ->
        F.fprintf fmt " by call to %a " MF.pp_monospaced (Procname.to_string callee_pname)
    | _ ->
        ()


  let get_val_traces {val_traces} = val_traces

  let get_report_location : t -> Location.t =
   fun ct -> match ct.cond_trace with Intra -> ct.issue_location | Inter {call_site} -> call_site


  let make : Location.t -> ValTrace.Issue.t -> t =
   fun issue_location val_traces -> {issue_location; cond_trace= Intra; val_traces}


  let make_call_and_subst ~traces_caller ~callee_pname call_site ct =
    let val_traces = ValTrace.Issue.call call_site traces_caller ct.val_traces in
    {ct with cond_trace= Inter {callee_pname; call_site}; val_traces}


  let has_unknown ct = ValTrace.Issue.has_unknown ct.val_traces

  let exists_str ~f ct = ValTrace.Issue.exists_str ~f ct.val_traces

  let check ~issue_type_u5 : _ t0 -> IssueType.t option =
   fun ct -> if has_unknown ct then Some issue_type_u5 else None


  let check_buffer_overrun ct = check ~issue_type_u5:IssueType.buffer_overrun_u5 ct

  let check_integer_overflow ct = check ~issue_type_u5:IssueType.integer_overflow_u5 ct

  let for_summary : _ t0 -> summary_t = fun ct -> {ct with cond_trace= ()}
end

type report_issue_type = NotIssue | Issue of IssueType.t | SymbolicIssue

type checked_condition = {report_issue_type: report_issue_type; propagate: bool}

module AllocSizeCondition = struct
  type t = {length: ItvPure.t; can_be_zero: bool} [@@deriving compare, equal]

  let get_symbols {length} = ItvPure.get_symbols length

  let pp fmt {length} = F.fprintf fmt "alloc(%a)" ItvPure.pp length

  let pp_description ~markup fmt {length} =
    F.fprintf fmt "Length: %a" (ItvPure.pp_mark ~markup) length


  let make ~can_be_zero ~length =
    if ItvPure.is_invalid length then None else Some {length; can_be_zero}


  let have_similar_bounds x y =
    ItvPure.have_similar_bounds x.length y.length && Bool.equal x.can_be_zero y.can_be_zero


  let xcompare ~lhs ~rhs =
    if Bool.equal lhs.can_be_zero rhs.can_be_zero then
      match ItvPure.xcompare ~lhs:lhs.length ~rhs:rhs.length with
      | `Equal ->
          `Equal
      | `NotComparable ->
          `NotComparable
      | `LeftSubsumesRight ->
          `LeftSubsumesRight
      | `RightSubsumesLeft ->
          `RightSubsumesLeft
      | (`LeftSmallerThanRight | `RightSmallerThanLeft) as cmp ->
          let lpos = ItvPure.le_sem ItvPure.zero lhs.length in
          let rpos = ItvPure.le_sem ItvPure.zero rhs.length in
          if not (Boolean.equal lpos rpos) then `NotComparable
          else if Boolean.is_true lpos then
            match cmp with
            | `LeftSmallerThanRight ->
                `RightSubsumesLeft
            | `RightSmallerThanLeft ->
                `LeftSubsumesRight
          else if Boolean.is_false lpos then
            match cmp with
            | `LeftSmallerThanRight ->
                `LeftSubsumesRight
            | `RightSmallerThanLeft ->
                `RightSubsumesLeft
          else `NotComparable
    else `NotComparable


  let itv_big = ItvPure.of_int 1_000_000

  let check {length; can_be_zero} =
    match ItvPure.xcompare ~lhs:length ~rhs:ItvPure.zero with
    | `Equal | `RightSubsumesLeft ->
        if can_be_zero then {report_issue_type= NotIssue; propagate= false}
        else {report_issue_type= Issue IssueType.inferbo_alloc_is_zero; propagate= false}
    | `LeftSmallerThanRight ->
        {report_issue_type= Issue IssueType.inferbo_alloc_is_negative; propagate= false}
    | _ -> (
        let is_symbolic = ItvPure.is_symbolic length in
        match ItvPure.xcompare ~lhs:length ~rhs:ItvPure.mone with
        | `Equal | `LeftSmallerThanRight | `RightSubsumesLeft ->
            {report_issue_type= Issue IssueType.inferbo_alloc_is_negative; propagate= false}
        | `LeftSubsumesRight when Bound.is_not_infty (ItvPure.lb length) ->
            { report_issue_type= Issue IssueType.inferbo_alloc_may_be_negative
            ; propagate= is_symbolic }
        | cmp_mone -> (
          match ItvPure.xcompare ~lhs:length ~rhs:itv_big with
          | `Equal | `RightSmallerThanLeft | `RightSubsumesLeft ->
              {report_issue_type= Issue IssueType.inferbo_alloc_is_big; propagate= false}
          | `LeftSubsumesRight when Bound.is_not_infty (ItvPure.ub length) ->
              {report_issue_type= Issue IssueType.inferbo_alloc_may_be_big; propagate= is_symbolic}
          | cmp_big ->
              let propagate =
                match (cmp_mone, cmp_big) with
                | (`NotComparable | `LeftSubsumesRight), _ | _, (`NotComparable | `LeftSubsumesRight)
                  ->
                    is_symbolic
                | _ ->
                    false
              in
              if propagate then {report_issue_type= SymbolicIssue; propagate}
              else {report_issue_type= NotIssue; propagate} ) )


  let subst eval_sym {length; can_be_zero} =
    match ItvPure.subst length eval_sym with
    | NonBottom length ->
        Some {length; can_be_zero}
    | Bottom ->
        None
end

module ArrayAccessCondition = struct
  type t = {offset: ItvPure.t; idx: ItvPure.t; size: ItvPure.t; last_included: bool; void_ptr: bool}
  [@@deriving compare, equal]

  let get_symbols c =
    Symb.SymbolSet.union3 (ItvPure.get_symbols c.offset) (ItvPure.get_symbols c.idx)
      (ItvPure.get_symbols c.size)


  let pp : F.formatter -> t -> unit =
   fun fmt c ->
    let pp_offset fmt =
      if not (ItvPure.is_zero c.offset) then F.fprintf fmt "%a + " ItvPure.pp c.offset
    in
    let cmp = if c.last_included then "<=" else "<" in
    F.fprintf fmt "%t%a %s %a" pp_offset ItvPure.pp c.idx cmp ItvPure.pp
      (ItvPure.make_non_negative c.size)


  let pp_description : markup:bool -> F.formatter -> t -> unit =
   fun ~markup fmt c ->
    let pp_offset fmt =
      if ItvPure.is_zero c.offset then ItvPure.pp_mark ~markup fmt c.idx
      else if ItvPure.is_zero c.idx then ItvPure.pp_mark ~markup fmt c.offset
      else
        F.fprintf fmt "%a (%s %a + %a)" (ItvPure.pp_mark ~markup) (ItvPure.plus c.offset c.idx)
          SpecialChars.leftwards_double_arrow (ItvPure.pp_mark ~markup) c.offset
          (ItvPure.pp_mark ~markup) c.idx
    in
    F.fprintf fmt "Offset%s: %t Size: %a"
      (if c.last_included then " added" else "")
      pp_offset (ItvPure.pp_mark ~markup)
      (ItvPure.make_non_negative c.size)


  let make : offset:ItvPure.t -> idx:ItvPure.t -> size:ItvPure.t -> last_included:bool -> t option =
   fun ~offset ~idx ~size ~last_included ->
    if ItvPure.is_invalid offset || ItvPure.is_invalid idx || ItvPure.is_invalid size then None
    else
      let void_ptr = ItvPure.has_void_ptr_symb offset || ItvPure.has_void_ptr_symb size in
      Some {offset; idx; size; last_included; void_ptr}


  let have_similar_bounds {offset= loff; idx= lidx; size= lsiz; last_included= lcol}
      {offset= roff; idx= ridx; size= rsiz; last_included= rcol} =
    Bool.equal lcol rcol
    && ItvPure.have_similar_bounds loff roff
    && ItvPure.have_similar_bounds lidx ridx
    && ItvPure.have_similar_bounds lsiz rsiz


  let has_infty {offset; idx; size} =
    ItvPure.has_infty offset || ItvPure.has_infty idx || ItvPure.has_infty size


  let xcompare ~lhs:{offset= loff; idx= lidx; size= lsiz; last_included= lcol}
      ~rhs:{offset= roff; idx= ridx; size= rsiz; last_included= rcol} =
    if not (Bool.equal lcol rcol) then `NotComparable
    else
      let offcmp = ItvPure.xcompare ~lhs:loff ~rhs:roff in
      let idxcmp = ItvPure.xcompare ~lhs:lidx ~rhs:ridx in
      let sizcmp = ItvPure.xcompare ~lhs:lsiz ~rhs:rsiz in
      match (offcmp, idxcmp, sizcmp) with
      | `Equal, `Equal, `Equal ->
          `Equal
      | `NotComparable, _, _ | _, `NotComparable, _ ->
          `NotComparable
      | `Equal, `Equal, (`LeftSmallerThanRight | `LeftSubsumesRight) ->
          `LeftSubsumesRight
      | `Equal, `Equal, (`RightSmallerThanLeft | `RightSubsumesLeft) ->
          `RightSubsumesLeft
      | (`Equal | `LeftSubsumesRight), (`Equal | `LeftSubsumesRight), (`Equal | `LeftSubsumesRight)
        ->
          `LeftSubsumesRight
      | (`Equal | `RightSubsumesLeft), (`Equal | `RightSubsumesLeft), (`Equal | `RightSubsumesLeft)
        ->
          `RightSubsumesLeft
      | (`Equal | `LeftSmallerThanRight), (`Equal | `LeftSmallerThanRight), _
      | (`Equal | `RightSmallerThanLeft), (`Equal | `RightSmallerThanLeft), _ ->
          let lidxpos = ItvPure.le_sem (ItvPure.neg lidx) loff in
          let ridxpos = ItvPure.le_sem (ItvPure.neg ridx) roff in
          if not (Boolean.equal lidxpos ridxpos) then `NotComparable
          else if Boolean.is_true lidxpos then
            (* both idx >= 0 *)
            match (offcmp, idxcmp, sizcmp) with
            | ( (`Equal | `LeftSmallerThanRight)
              , (`Equal | `LeftSmallerThanRight)
              , (`Equal | `RightSmallerThanLeft | `RightSubsumesLeft) ) ->
                `RightSubsumesLeft
            | ( (`Equal | `RightSmallerThanLeft)
              , (`Equal | `RightSmallerThanLeft)
              , (`Equal | `LeftSmallerThanRight | `LeftSubsumesRight) ) ->
                `LeftSubsumesRight
            | _ ->
                `NotComparable
          else if Boolean.is_false lidxpos then
            (* both idx < 0, size doesn't matter *)
            match (offcmp, idxcmp) with
            | `Equal, `LeftSmallerThanRight | `LeftSmallerThanRight, `Equal ->
                `LeftSubsumesRight
            | `Equal, `RightSmallerThanLeft | `RightSmallerThanLeft, `Equal ->
                `RightSubsumesLeft
            | `Equal, `Equal ->
                `Equal
            | _ ->
                `NotComparable
          else `NotComparable
      | _ ->
          `NotComparable


  let filter1 : real_idx:ItvPure.t -> t -> bool =
   fun ~real_idx c ->
    ItvPure.is_top real_idx || ItvPure.is_top c.size || ItvPure.is_lb_infty real_idx
    || ItvPure.is_lb_infty c.size
    || (ItvPure.is_nat real_idx && ItvPure.is_nat c.size)


  let filter2 : real_idx:ItvPure.t -> t -> bool =
   fun ~real_idx c ->
    (* basically, alarms involving infinity are filtered *)
    ((not (ItvPure.is_finite real_idx)) || not (ItvPure.is_finite c.size))
    && (* except the following cases *)
    not
      ( Bound.is_not_infty (ItvPure.lb real_idx)
        && (* idx non-infty lb < 0 *)
        Bound.lt (ItvPure.lb real_idx) Bound.zero
      || Bound.is_not_infty (ItvPure.lb real_idx)
         && (* idx non-infty lb > size lb *)
         Bound.gt (ItvPure.lb real_idx) (ItvPure.lb c.size)
      || Bound.is_not_infty (ItvPure.lb real_idx)
         && (* idx non-infty lb > size ub *)
         Bound.gt (ItvPure.lb real_idx) (ItvPure.ub c.size)
      || Bound.is_not_infty (ItvPure.ub real_idx)
         && (* idx non-infty ub > size lb *)
         Bound.gt (ItvPure.ub real_idx) (ItvPure.lb c.size)
      || Bound.is_not_infty (ItvPure.ub real_idx)
         && (* idx non-infty ub > size ub *)
         Bound.gt (ItvPure.ub real_idx) (ItvPure.ub c.size) )


  (* check buffer overrun and return its confidence *)
  let check : t -> checked_condition =
   fun c ->
    (* idx = [il, iu], size = [sl, su],
       For arrays : we want to check that 0 <= idx < size
       For adding into collections : we want to check that 0 <= idx <= size *)
    let real_idx = ItvPure.plus c.offset c.idx in
    let size =
      let size_nonneg = ItvPure.make_non_negative c.size in
      if c.last_included then ItvPure.succ size_nonneg else size_nonneg
    in
    (* if sl < 0, use sl' = 0 *)
    let not_overrun = ItvPure.lt_sem real_idx size in
    let not_underrun = ItvPure.le_sem ItvPure.zero real_idx in
    (* il >= 0 and iu < sl, definitely not an error *)
    if Boolean.is_true not_overrun && Boolean.is_true not_underrun then
      {report_issue_type= NotIssue; propagate= false} (* iu < 0 or il >= su, definitely an error *)
    else if Boolean.is_false not_overrun || Boolean.is_false not_underrun then
      {report_issue_type= Issue IssueType.buffer_overrun_l1; propagate= false}
      (* su <= iu < +oo, most probably an error *)
    else if
      Bound.is_not_infty (ItvPure.ub real_idx) && Bound.le (ItvPure.ub size) (ItvPure.ub real_idx)
    then {report_issue_type= Issue IssueType.buffer_overrun_l2; propagate= false}
      (* symbolic il >= sl, probably an error *)
    else if
      Bound.is_symbolic (ItvPure.lb real_idx) && Bound.le (ItvPure.lb size) (ItvPure.lb real_idx)
    then {report_issue_type= Issue IssueType.buffer_overrun_s2; propagate= true}
    else
      (* other symbolic bounds are probably too noisy *)
      let is_symbolic =
        ItvPure.is_symbolic c.offset || ItvPure.is_symbolic c.idx || ItvPure.is_symbolic size
      in
      let report_issue_type =
        if Config.bo_debug <= 3 && is_symbolic then SymbolicIssue
        else if filter1 ~real_idx c then Issue IssueType.buffer_overrun_l5
        else if filter2 ~real_idx c then Issue IssueType.buffer_overrun_l4
        else Issue IssueType.buffer_overrun_l3
      in
      {report_issue_type; propagate= is_symbolic}


  let subst : Bound.eval_sym -> t -> t option =
   fun eval_sym c ->
    match
      (ItvPure.subst c.offset eval_sym, ItvPure.subst c.idx eval_sym, ItvPure.subst c.size eval_sym)
    with
    | NonBottom offset, NonBottom idx, NonBottom size ->
        let void_ptr =
          c.void_ptr || ItvPure.has_void_ptr_symb offset || ItvPure.has_void_ptr_symb idx
          || ItvPure.has_void_ptr_symb size
        in
        Some {c with offset; idx; size; void_ptr}
    | _ ->
        None
end

module BinaryOperationCondition = struct
  type binop_t = Plus | Minus | Mult [@@deriving compare, equal]

  let binop_to_string = function
    | Plus ->
        "+"
    | Minus ->
        "-"
    | Mult ->
        SpecialChars.multiplication_sign


  type t =
    { binop: binop_t
    ; typ: Typ.ikind
    ; integer_widths: IntegerWidths.t
    ; lhs: ItvPure.t
    ; rhs: ItvPure.t
    ; pname: Procname.t }
  [@@deriving compare, equal]

  let get_symbols c = Symb.SymbolSet.union (ItvPure.get_symbols c.lhs) (ItvPure.get_symbols c.rhs)

  let subst eval_sym c =
    match (ItvPure.subst c.lhs eval_sym, ItvPure.subst c.rhs eval_sym) with
    | NonBottom lhs, NonBottom rhs ->
        Some {c with lhs; rhs}
    | _, _ ->
        None


  let have_similar_bounds {binop= binop1; typ= typ1; lhs= lhs1; rhs= rhs1}
      {binop= binop2; typ= typ2; lhs= lhs2; rhs= rhs2} =
    equal_binop_t binop1 binop2 && Typ.equal_ikind typ1 typ2
    && ItvPure.have_similar_bounds lhs1 lhs2
    && ItvPure.have_similar_bounds rhs1 rhs2


  let has_infty {lhs; rhs} = ItvPure.has_infty lhs || ItvPure.has_infty rhs

  let xcompare ~lhs:{binop= binop1; typ= typ1; lhs= lhs1; rhs= rhs1}
      ~rhs:{binop= binop2; typ= typ2; lhs= lhs2; rhs= rhs2} =
    if not (equal_binop_t binop1 binop2 && Typ.equal_ikind typ1 typ2) then `NotComparable
    else
      let lhscmp = ItvPure.xcompare ~lhs:lhs1 ~rhs:lhs2 in
      let rhscmp = ItvPure.xcompare ~lhs:rhs1 ~rhs:rhs2 in
      match (lhscmp, rhscmp) with
      | `Equal, `Equal ->
          `Equal
      | `Equal, `LeftSubsumesRight
      | `LeftSubsumesRight, `Equal
      | `LeftSubsumesRight, `LeftSubsumesRight ->
          `LeftSubsumesRight
      | `Equal, `RightSubsumesLeft
      | `RightSubsumesLeft, `Equal
      | `RightSubsumesLeft, `RightSubsumesLeft ->
          `RightSubsumesLeft
      | `NotComparable, _
      | _, `NotComparable
      | `LeftSmallerThanRight, _
      | _, `LeftSmallerThanRight
      | `RightSmallerThanLeft, _
      | _, `RightSmallerThanLeft
      | `LeftSubsumesRight, `RightSubsumesLeft
      | `RightSubsumesLeft, `LeftSubsumesRight ->
          `NotComparable


  let pp_description ~markup fmt {binop; typ; integer_widths; lhs; rhs} =
    F.fprintf fmt "(%a %s %a):" (ItvPure.pp_mark ~markup) lhs (binop_to_string binop)
      (ItvPure.pp_mark ~markup) rhs ;
    match typ with
    | Typ.IBool ->
        F.fprintf fmt "bool"
    | _ ->
        F.fprintf fmt "%s%d"
          (if Typ.ikind_is_unsigned typ then "unsigned" else "signed")
          (IntegerWidths.width_of_ikind integer_widths typ)


  let pp = pp_description ~markup:false

  let is_mult_one binop lhs rhs =
    equal_binop_t binop Mult && (ItvPure.is_one lhs || ItvPure.is_one rhs)


  let should_check {binop; typ; lhs; rhs} =
    let cannot_underflow, cannot_overflow =
      match (binop, Typ.ikind_is_unsigned typ) with
      | Plus, true ->
          (true, false)
      | Minus, true ->
          (false, true)
      | Mult, true ->
          (true, false)
      | Plus, false ->
          ( ItvPure.is_ge_zero lhs || ItvPure.is_ge_zero rhs
          , ItvPure.is_le_zero lhs || ItvPure.is_le_zero rhs )
      | Minus, false ->
          ( ItvPure.is_ge_zero lhs || ItvPure.is_le_zero rhs
          , ItvPure.is_le_mone lhs || ItvPure.is_ge_zero rhs )
      | Mult, false ->
          ( (ItvPure.is_ge_zero lhs && ItvPure.is_ge_zero rhs)
            || (ItvPure.is_le_zero lhs && ItvPure.is_le_zero rhs)
          , (ItvPure.is_ge_zero lhs && ItvPure.is_le_zero rhs)
            || (ItvPure.is_le_zero lhs && ItvPure.is_ge_zero rhs) )
    in
    (not cannot_underflow, not cannot_overflow)


  let is_deliberate_integer_overflow =
    let allow_list = ["hash"; "lfsr"; "prng"; "rand"; "seed"] in
    let f x =
      let x = String.lowercase x in
      List.exists allow_list ~f:(fun allow_list -> String.is_substring x ~substring:allow_list)
    in
    fun {typ; lhs; rhs; pname} ct ->
      Typ.ikind_is_unsigned typ
      && ( ConditionTrace.exists_str ~f ct || ItvPure.exists_str ~f lhs || ItvPure.exists_str ~f rhs
         || f (Procname.to_simplified_string pname) )


  let check ({binop; typ; integer_widths; lhs; rhs} as c) (trace : ConditionTrace.t) =
    if is_mult_one binop lhs rhs || is_deliberate_integer_overflow c trace then
      {report_issue_type= NotIssue; propagate= false}
    else
      let v =
        match binop with
        | Plus ->
            ItvPure.plus lhs rhs
        | Minus ->
            ItvPure.minus lhs rhs
        | Mult ->
            ItvPure.mult lhs rhs
      in
      let v_lb, v_ub = (ItvPure.lb v, ItvPure.ub v) in
      let typ_lb, typ_ub =
        let lb, ub = IntegerWidths.range_of_ikind integer_widths typ in
        (Bound.of_big_int lb, Bound.of_big_int ub)
      in
      let check_underflow, check_overflow = should_check c in
      if
        (* typ_lb <= v_lb and v_ub <= typ_ub, not an error *)
        ((not check_underflow) || Bound.le typ_lb v_lb)
        && ((not check_overflow) || Bound.le v_ub typ_ub)
      then {report_issue_type= NotIssue; propagate= false}
      else if
        (* v_ub < typ_lb or typ_ub < v_lb, definitely an error *)
        (check_underflow && Bound.lt v_ub typ_lb) || (check_overflow && Bound.lt typ_ub v_lb)
      then {report_issue_type= Issue IssueType.integer_overflow_l1; propagate= false}
      else if
        (* -oo != v_lb < typ_lb or typ_ub < v_ub != +oo, probably an error *)
        (check_underflow && Bound.lt v_lb typ_lb && Bound.is_not_infty v_lb)
        || (check_overflow && Bound.lt typ_ub v_ub && Bound.is_not_infty v_ub)
      then {report_issue_type= Issue IssueType.integer_overflow_l2; propagate= false}
      else
        let is_symbolic = ItvPure.is_symbolic v in
        let report_issue_type =
          if Config.bo_debug <= 3 && is_symbolic then SymbolicIssue
          else Issue IssueType.integer_overflow_l5
        in
        {report_issue_type; propagate= is_symbolic}


  let make integer_widths pname bop ~lhs ~rhs =
    if ItvPure.is_invalid lhs || ItvPure.is_invalid rhs then None
    else
      let binop, typ =
        match bop with
        | Binop.PlusA (Some typ) ->
            (Plus, typ)
        | Binop.MinusA (Some typ) ->
            (Minus, typ)
        | Binop.Mult (Some typ) ->
            (Mult, typ)
        | _ ->
            L.(die InternalError)
              "Unexpected type %s is given to BinaryOperationCondition." (Binop.str Pp.text bop)
      in
      Some {binop; typ; integer_widths; lhs; rhs; pname}
end

module Condition = struct
  type t =
    | AllocSize of AllocSizeCondition.t
    | ArrayAccess of ArrayAccessCondition.t
    | BinaryOperation of BinaryOperationCondition.t
  [@@deriving compare, equal]

  let make_alloc_size = Option.map ~f:(fun c -> AllocSize c)

  let make_array_access = Option.map ~f:(fun c -> ArrayAccess c)

  let make_binary_operation = Option.map ~f:(fun c -> BinaryOperation c)

  let get_symbols = function
    | AllocSize c ->
        AllocSizeCondition.get_symbols c
    | ArrayAccess c ->
        ArrayAccessCondition.get_symbols c
    | BinaryOperation c ->
        BinaryOperationCondition.get_symbols c


  let subst eval_sym = function
    | AllocSize c ->
        AllocSizeCondition.subst eval_sym c |> make_alloc_size
    | ArrayAccess c ->
        ArrayAccessCondition.subst eval_sym c |> make_array_access
    | BinaryOperation c ->
        BinaryOperationCondition.subst eval_sym c |> make_binary_operation


  let have_similar_bounds c1 c2 =
    match (c1, c2) with
    | AllocSize c1, AllocSize c2 ->
        AllocSizeCondition.have_similar_bounds c1 c2
    | ArrayAccess c1, ArrayAccess c2 ->
        ArrayAccessCondition.have_similar_bounds c1 c2
    | BinaryOperation c1, BinaryOperation c2 ->
        BinaryOperationCondition.have_similar_bounds c1 c2
    | _ ->
        false


  let has_infty = function
    | ArrayAccess c ->
        ArrayAccessCondition.has_infty c
    | BinaryOperation c ->
        BinaryOperationCondition.has_infty c
    | _ ->
        false


  let xcompare ~lhs ~rhs =
    match (lhs, rhs) with
    | AllocSize lhs, AllocSize rhs ->
        AllocSizeCondition.xcompare ~lhs ~rhs
    | ArrayAccess lhs, ArrayAccess rhs ->
        ArrayAccessCondition.xcompare ~lhs ~rhs
    | BinaryOperation lhs, BinaryOperation rhs ->
        BinaryOperationCondition.xcompare ~lhs ~rhs
    | _ ->
        `NotComparable


  let pp fmt = function
    | AllocSize c ->
        AllocSizeCondition.pp fmt c
    | ArrayAccess c ->
        ArrayAccessCondition.pp fmt c
    | BinaryOperation c ->
        BinaryOperationCondition.pp fmt c


  let pp_description ~markup fmt = function
    | AllocSize c ->
        AllocSizeCondition.pp_description ~markup fmt c
    | ArrayAccess c ->
        ArrayAccessCondition.pp_description ~markup fmt c
    | BinaryOperation c ->
        BinaryOperationCondition.pp_description ~markup fmt c


  let check cond trace =
    match cond with
    | AllocSize c ->
        AllocSizeCondition.check c
    | ArrayAccess c ->
        ArrayAccessCondition.check c
    | BinaryOperation c ->
        BinaryOperationCondition.check c trace


  let is_array_access_of_void_ptr = function ArrayAccess {void_ptr} -> void_ptr | _ -> false
end

module Reported = struct
  type t = IssueType.t [@@deriving compare, equal]

  let make issue_type = issue_type
end

module ConditionWithTrace = struct
  type 'cond_trace t0 =
    { cond: Condition.t
    ; trace: 'cond_trace ConditionTrace.t0
    ; reported: Reported.t option
    ; reachability: Dom.Reachability.t }

  let make cond trace latest_prune =
    {cond; trace; reported= None; reachability= Dom.Reachability.make latest_prune}


  let pp fmt {cond; trace; reachability} =
    F.fprintf fmt "%a %a reachable when %a" Condition.pp cond ConditionTrace.pp trace
      Dom.Reachability.pp reachability


  let pp_summary fmt {cond; trace; reachability} =
    F.fprintf fmt "%a %a reachable when %a" Condition.pp cond ConditionTrace.pp_summary trace
      Dom.Reachability.pp reachability


  let have_same_bounds {cond= cond1} {cond= cond2} = Condition.equal cond1 cond2

  let have_similar_bounds {cond= cond1} {cond= cond2} = Condition.have_similar_bounds cond1 cond2

  let xcompare ~lhs ~rhs =
    if Dom.Reachability.equal lhs.reachability rhs.reachability then
      match Condition.xcompare ~lhs:lhs.cond ~rhs:rhs.cond with
      | `Equal ->
          if ConditionTrace.compare lhs.trace rhs.trace <= 0 then `LeftSubsumesRight
          else `RightSubsumesLeft
      | (`LeftSubsumesRight | `RightSubsumesLeft | `NotComparable) as cmp ->
          cmp
    else `NotComparable


  let subst eval_sym_trace callee_pname call_site cwt =
    let symbols = Condition.get_symbols cwt.cond in
    if Symb.SymbolSet.is_empty symbols then
      L.(die InternalError)
        "Trying to substitute a non-symbolic condition %a from %a at %a. Why was it propagated in \
         the first place?"
        pp_summary cwt Procname.pp callee_pname Location.pp call_site ;
    match
      Dom.Reachability.subst cwt.reachability
        (eval_sym_trace ~mode:Sem.EvalPOReachability)
        call_site
    with
    | `Reachable reachability -> (
        let {Dom.eval_sym; trace_of_sym} = eval_sym_trace ~mode:Sem.EvalPOCond in
        match Condition.subst eval_sym cwt.cond with
        | None ->
            None
        | Some cond ->
            let traces_caller =
              Symb.SymbolSet.fold
                (fun symbol val_traces -> ValTrace.Set.join (trace_of_sym symbol) val_traces)
                symbols ValTrace.Set.bottom
            in
            let trace =
              ConditionTrace.make_call_and_subst ~traces_caller ~callee_pname call_site cwt.trace
            in
            Some {cond; trace; reported= cwt.reported; reachability} )
    | `Unreachable ->
        None


  let update_reachability latest_prune ({reachability} as cwt) =
    {cwt with reachability= Dom.Reachability.add_latest_prune latest_prune reachability}


  let set_u5 {cond; trace} issue_type =
    (* It suppresses issues of array accesses by void pointers.  This is not ideal but Inferbo
       cannot analyze them precisely at the moment. *)
    if Condition.is_array_access_of_void_ptr cond then IssueType.buffer_overrun_l5
    else if
      ( IssueType.equal issue_type IssueType.buffer_overrun_l3
      || IssueType.equal issue_type IssueType.buffer_overrun_l4
      || IssueType.equal issue_type IssueType.buffer_overrun_l5 )
      && Condition.has_infty cond
    then Option.value (ConditionTrace.check_buffer_overrun trace) ~default:issue_type
    else if IssueType.equal issue_type IssueType.integer_overflow_l5 && Condition.has_infty cond
    then Option.value (ConditionTrace.check_integer_overflow trace) ~default:issue_type
    else issue_type


  let check cwt =
    let ({report_issue_type; propagate} as checked) = Condition.check cwt.cond cwt.trace in
    match report_issue_type with
    | NotIssue | SymbolicIssue ->
        checked
    | Issue issue_type ->
        let issue_type = set_u5 cwt issue_type in
        (* Only report if the precision has improved.
           This is approximated by: only report if the issue_type has changed. *)
        let report_issue_type =
          match cwt.reported with
          | Some reported when Reported.equal reported issue_type ->
              (* already reported and the precision hasn't improved *) SymbolicIssue
          | _ ->
              (* either never reported or already reported but the precision has improved *)
              Issue issue_type
        in
        {report_issue_type; propagate}


  let report_errors ~report (cwt, checked) =
    match checked.report_issue_type with
    | NotIssue | SymbolicIssue ->
        ()
    | Issue issue_type ->
        report cwt.cond cwt.trace issue_type


  let for_summary = function
    | _, {propagate= false} | _, {report_issue_type= NotIssue} ->
        None
    | {cond; trace; reported; reachability}, {report_issue_type} ->
        let reported =
          match report_issue_type with
          | NotIssue ->
              assert false
          | SymbolicIssue ->
              reported
          | Issue issue_type ->
              Some (Reported.make issue_type)
        in
        let trace = ConditionTrace.for_summary trace in
        Some {cond; trace; reported; reachability}
end

module ConditionSet = struct
  type 'cond_trace t0 = 'cond_trace ConditionWithTrace.t0 list

  type checked_t = (ConditionTrace.intra_cond_trace ConditionWithTrace.t0 * checked_condition) list

  type summary_t = unit t0

  (* invariant: join_one of one of the elements should return the original list *)

  let empty = []

  let try_merge ~existing:(existing_cwt, existing_checked) ~new_:(new_cwt, new_checked) =
    (* we don't want to remove issues that would end up in a higher bucket,
       e.g. [a, b] < [c, d] is subsumed by [a, +oo] < [c, d] but the latter is less precise *)
    let try_deduplicate () =
      match ConditionWithTrace.xcompare ~lhs:existing_cwt ~rhs:new_cwt with
      | `LeftSubsumesRight ->
          `DoNotAddAndStop
      | `RightSubsumesLeft ->
          `RemoveExistingAndContinue
      | `NotComparable ->
          `KeepExistingAndContinue
    in
    match (existing_checked.report_issue_type, new_checked.report_issue_type) with
    | _, NotIssue ->
        `DoNotAddAndStop
    | SymbolicIssue, SymbolicIssue when ConditionWithTrace.have_same_bounds existing_cwt new_cwt ->
        try_deduplicate ()
    | Issue existing_issue_type, Issue new_issue_type
      when IssueType.equal existing_issue_type new_issue_type
           && ConditionWithTrace.have_similar_bounds existing_cwt new_cwt ->
        try_deduplicate ()
    | _, _ ->
        `KeepExistingAndContinue


  let join_one condset new_ =
    let pp_cond fmt (cond, _) = ConditionWithTrace.pp fmt cond in
    let rec aux acc ~same = function
      | [] ->
          if Config.bo_debug >= 3 then
            L.d_printfln_escaped "[InferboPO] Adding new condition %a@." pp_cond new_ ;
          if same then new_ :: condset else new_ :: acc
      | existing :: rest as existings -> (
        match try_merge ~existing ~new_ with
        | `DoNotAddAndStop ->
            if Config.bo_debug >= 3 then
              L.d_printfln_escaped "[InferboPO] Not adding condition %a (because of existing %a)@."
                pp_cond new_ pp_cond existing ;
            if same then condset else List.rev_append acc existings
        | `RemoveExistingAndContinue ->
            if Config.bo_debug >= 3 then
              L.d_printfln_escaped "[InferboPO] Removing condition %a (because of new %a)@." pp_cond
                existing pp_cond new_ ;
            aux acc ~same:false rest
        | `KeepExistingAndContinue ->
            aux (existing :: acc) ~same rest )
    in
    aux [] ~same:true condset


  let join condset1 condset2 = List.fold condset2 ~init:condset1 ~f:join_one

  let check_one cwt = (cwt, ConditionWithTrace.check cwt)

  let add_opt location val_traces latest_prune condset = function
    | None ->
        condset
    | Some cond ->
        let trace = ConditionTrace.make location val_traces in
        let cwt = ConditionWithTrace.make cond trace latest_prune in
        join_one condset (check_one cwt)


  let add_array_access location ~offset ~idx ~size ~last_included ~idx_traces ~arr_traces
      ~latest_prune condset =
    ArrayAccessCondition.make ~offset ~idx ~size ~last_included
    |> Condition.make_array_access
    |> add_opt location
         (ValTrace.Issue.(binary location ArrayAccess) idx_traces arr_traces)
         latest_prune condset


  let add_alloc_size location ~can_be_zero ~length val_traces latest_prune condset =
    AllocSizeCondition.make ~can_be_zero ~length
    |> Condition.make_alloc_size
    |> add_opt location (ValTrace.Issue.alloc location val_traces) latest_prune condset


  let add_binary_operation integer_type_widths location pname bop ~lhs ~rhs ~lhs_traces ~rhs_traces
      ~latest_prune condset =
    BinaryOperationCondition.make integer_type_widths pname bop ~lhs ~rhs
    |> Condition.make_binary_operation
    |> add_opt location
         (ValTrace.Issue.(binary location Binop) lhs_traces rhs_traces)
         latest_prune condset


  let subst condset eval_sym_trace callee_pname call_site latest_prune =
    let subst_add_cwt condset cwt =
      match ConditionWithTrace.subst eval_sym_trace callee_pname call_site cwt with
      | None ->
          condset
      | Some cwt ->
          let cwt = ConditionWithTrace.update_reachability latest_prune cwt in
          join_one condset (check_one cwt)
    in
    List.fold condset ~f:subst_add_cwt ~init:[]


  let report_errors ~report condset =
    List.iter condset ~f:(ConditionWithTrace.report_errors ~report)


  let for_summary condset = List.filter_map condset ~f:ConditionWithTrace.for_summary

  let pp_summary : F.formatter -> _ t0 -> unit =
   fun fmt condset ->
    let pp_sep fmt () = F.fprintf fmt ", @," in
    F.fprintf fmt "@[<v 0>Safety conditions:@," ;
    F.fprintf fmt "@[<hov 2>{ " ;
    F.pp_print_list ~pp_sep ConditionWithTrace.pp_summary fmt condset ;
    F.fprintf fmt " }@]" ;
    F.fprintf fmt "@]"


  let pp : Format.formatter -> checked_t -> unit =
   fun fmt condset ->
    let pp_sep fmt () = F.fprintf fmt ",@;" in
    let pp_elem fmt (x, _) = ConditionWithTrace.pp fmt x in
    F.fprintf fmt "Safety conditions:@;@[<hov 2>{ %a }@]"
      (IList.pp_print_list ~max:30 ~pp_sep pp_elem)
      condset
end

let description ~markup cond trace =
  F.asprintf "%a%a" (Condition.pp_description ~markup) cond ConditionTrace.pp_description trace
