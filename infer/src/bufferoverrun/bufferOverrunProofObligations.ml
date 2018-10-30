(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! AbstractDomain.Types
module F = Format
module L = Logging
module ItvPure = Itv.ItvPure
module Relation = BufferOverrunDomainRelation
module MF = MarkupFormatter
module ValTraceSet = BufferOverrunTrace.Set
module Bound = Bounds.Bound

type checked_condition = {report_issue_type: IssueType.t option; propagate: bool}

module AllocSizeCondition = struct
  type t = ItvPure.astate

  let get_symbols = ItvPure.get_symbols

  let pp fmt length = F.fprintf fmt "alloc(%a)" ItvPure.pp length

  let pp_description fmt length = F.fprintf fmt "Length: %a" ItvPure.pp length

  let make ~length = if ItvPure.is_invalid length then None else Some length

  let have_similar_bounds = ItvPure.have_similar_bounds

  let xcompare ~lhs ~rhs =
    match ItvPure.xcompare ~lhs ~rhs with
    | `Equal ->
        `Equal
    | `NotComparable ->
        `NotComparable
    | `LeftSubsumesRight ->
        `LeftSubsumesRight
    | `RightSubsumesLeft ->
        `RightSubsumesLeft
    | (`LeftSmallerThanRight | `RightSmallerThanLeft) as cmp ->
        let lpos = ItvPure.le_sem ItvPure.zero lhs in
        let rpos = ItvPure.le_sem ItvPure.zero rhs in
        if not (Itv.Boolean.equal lpos rpos) then `NotComparable
        else if Itv.Boolean.is_true lpos then
          match cmp with
          | `LeftSmallerThanRight ->
              `RightSubsumesLeft
          | `RightSmallerThanLeft ->
              `LeftSubsumesRight
        else if Itv.Boolean.is_false lpos then
          match cmp with
          | `LeftSmallerThanRight ->
              `LeftSubsumesRight
          | `RightSmallerThanLeft ->
              `RightSubsumesLeft
        else `NotComparable


  let itv_big = ItvPure.of_int 1_000_000

  let check length =
    match ItvPure.xcompare ~lhs:length ~rhs:ItvPure.zero with
    | `Equal | `RightSubsumesLeft ->
        {report_issue_type= Some IssueType.inferbo_alloc_is_zero; propagate= false}
    | `LeftSmallerThanRight ->
        {report_issue_type= Some IssueType.inferbo_alloc_is_negative; propagate= false}
    | _ -> (
        let is_symbolic = ItvPure.is_symbolic length in
        match ItvPure.xcompare ~lhs:length ~rhs:ItvPure.mone with
        | `Equal | `LeftSmallerThanRight | `RightSubsumesLeft ->
            {report_issue_type= Some IssueType.inferbo_alloc_is_negative; propagate= false}
        | `LeftSubsumesRight when Bound.is_not_infty (ItvPure.lb length) ->
            { report_issue_type= Some IssueType.inferbo_alloc_may_be_negative
            ; propagate= is_symbolic }
        | cmp_mone -> (
          match ItvPure.xcompare ~lhs:length ~rhs:itv_big with
          | `Equal | `RightSmallerThanLeft | `RightSubsumesLeft ->
              {report_issue_type= Some IssueType.inferbo_alloc_is_big; propagate= false}
          | `LeftSubsumesRight when Bound.is_not_infty (ItvPure.ub length) ->
              {report_issue_type= Some IssueType.inferbo_alloc_may_be_big; propagate= is_symbolic}
          | cmp_big ->
              let propagate =
                match (cmp_mone, cmp_big) with
                | (`NotComparable | `LeftSubsumesRight), _
                | _, (`NotComparable | `LeftSubsumesRight) ->
                    is_symbolic
                | _ ->
                    false
              in
              {report_issue_type= None; propagate} ) )


  let subst eval_sym length =
    match ItvPure.subst length eval_sym with NonBottom length -> Some length | Bottom -> None
end

module ArrayAccessCondition = struct
  type t =
    { offset: ItvPure.astate
    ; idx: ItvPure.astate
    ; size: ItvPure.astate
    ; idx_sym_exp: Relation.SymExp.t option
    ; size_sym_exp: Relation.SymExp.t option
    ; relation: Relation.astate }
  [@@deriving compare]

  let get_symbols c =
    ItvPure.get_symbols c.offset @ ItvPure.get_symbols c.idx @ ItvPure.get_symbols c.size


  let pp : F.formatter -> t -> unit =
   fun fmt c ->
    let pp_offset fmt =
      if not (ItvPure.is_zero c.offset) then F.fprintf fmt "%a + " ItvPure.pp c.offset
    in
    F.fprintf fmt "%t%a < %a" pp_offset ItvPure.pp c.idx ItvPure.pp (ItvPure.make_positive c.size) ;
    if Option.is_some Config.bo_relational_domain then
      F.fprintf fmt "@,%a < %a when %a" Relation.SymExp.pp_opt c.idx_sym_exp Relation.SymExp.pp_opt
        c.size_sym_exp Relation.pp c.relation


  let pp_description : F.formatter -> t -> unit =
   fun fmt c ->
    let pp_offset fmt =
      if ItvPure.is_zero c.offset then ItvPure.pp fmt c.idx
      else if ItvPure.is_zero c.idx then ItvPure.pp fmt c.offset
      else
        F.fprintf fmt "%a (= %a + %a)" ItvPure.pp (ItvPure.plus c.offset c.idx) ItvPure.pp c.offset
          ItvPure.pp c.idx
    in
    F.fprintf fmt "Offset: %t Size: %a" pp_offset ItvPure.pp (ItvPure.make_positive c.size)


  let make :
         offset:ItvPure.t
      -> idx:ItvPure.t
      -> size:ItvPure.t
      -> idx_sym_exp:Relation.SymExp.t option
      -> size_sym_exp:Relation.SymExp.t option
      -> relation:Relation.astate
      -> t option =
   fun ~offset ~idx ~size ~idx_sym_exp ~size_sym_exp ~relation ->
    if ItvPure.is_invalid offset || ItvPure.is_invalid idx || ItvPure.is_invalid size then None
    else Some {offset; idx; size; idx_sym_exp; size_sym_exp; relation}


  let have_similar_bounds {offset= loff; idx= lidx; size= lsiz}
      {offset= roff; idx= ridx; size= rsiz} =
    ItvPure.have_similar_bounds loff roff
    && ItvPure.have_similar_bounds lidx ridx
    && ItvPure.have_similar_bounds lsiz rsiz


  let has_infty {offset; idx; size} =
    ItvPure.has_infty offset || ItvPure.has_infty idx || ItvPure.has_infty size


  let xcompare ~lhs:{offset= loff; idx= lidx; size= lsiz} ~rhs:{offset= roff; idx= ridx; size= rsiz}
      =
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
        if not (Itv.Boolean.equal lidxpos ridxpos) then `NotComparable
        else if Itv.Boolean.is_true lidxpos then
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
        else if Itv.Boolean.is_false lidxpos then
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
  let check : is_collection_add:bool -> t -> checked_condition =
   fun ~is_collection_add c ->
    (* idx = [il, iu], size = [sl, su],
       For arrays : we want to check that 0 <= idx < size
       For adding into arraylists: we want to check that 0 <= idx <= size *)
    let real_idx = ItvPure.plus c.offset c.idx in
    let size = ItvPure.make_positive c.size in
    (* if sl < 0, use sl' = 0 *)
    let not_overrun =
      if is_collection_add then ItvPure.le_sem real_idx size
      else if Relation.lt_sat_opt c.idx_sym_exp c.size_sym_exp c.relation then Itv.Boolean.true_
      else ItvPure.lt_sem real_idx size
    in
    let not_underrun =
      if Relation.le_sat_opt (Some Relation.SymExp.zero) c.idx_sym_exp c.relation then
        Itv.Boolean.true_
      else ItvPure.le_sem ItvPure.zero real_idx
    in
    (* il >= 0 and iu < sl, definitely not an error *)
    if Itv.Boolean.is_true not_overrun && Itv.Boolean.is_true not_underrun then
      {report_issue_type= None; propagate= false} (* iu < 0 or il >= su, definitely an error *)
    else if Itv.Boolean.is_false not_overrun || Itv.Boolean.is_false not_underrun then
      {report_issue_type= Some IssueType.buffer_overrun_l1; propagate= false}
      (* su <= iu < +oo, most probably an error *)
    else if
      Bound.is_not_infty (ItvPure.ub real_idx) && Bound.le (ItvPure.ub size) (ItvPure.ub real_idx)
    then {report_issue_type= Some IssueType.buffer_overrun_l2; propagate= false}
      (* symbolic il >= sl, probably an error *)
    else if
      Bound.is_symbolic (ItvPure.lb real_idx) && Bound.le (ItvPure.lb size) (ItvPure.lb real_idx)
    then {report_issue_type= Some IssueType.buffer_overrun_s2; propagate= true}
    else
      (* other symbolic bounds are probably too noisy *)
      let is_symbolic =
        ItvPure.is_symbolic c.offset || ItvPure.is_symbolic c.idx || ItvPure.is_symbolic size
      in
      let report_issue_type =
        if Config.bo_debug <= 3 && is_symbolic then None
        else if filter1 ~real_idx c then Some IssueType.buffer_overrun_l5
        else if filter2 ~real_idx c then Some IssueType.buffer_overrun_l4
        else Some IssueType.buffer_overrun_l3
      in
      {report_issue_type; propagate= is_symbolic}


  let subst :
         (Symb.Symbol.t -> Bound.t bottom_lifted)
      -> Relation.SubstMap.t
      -> Relation.astate
      -> t
      -> t option =
   fun eval_sym rel_map caller_relation c ->
    match
      (ItvPure.subst c.offset eval_sym, ItvPure.subst c.idx eval_sym, ItvPure.subst c.size eval_sym)
    with
    | NonBottom offset, NonBottom idx, NonBottom size ->
        let idx_sym_exp = Relation.SubstMap.symexp_subst_opt rel_map c.idx_sym_exp in
        let size_sym_exp = Relation.SubstMap.symexp_subst_opt rel_map c.size_sym_exp in
        let relation = Relation.instantiate rel_map ~caller:caller_relation ~callee:c.relation in
        Some {offset; idx; size; idx_sym_exp; size_sym_exp; relation}
    | _ ->
        None


  let forget_locs : AbsLoc.PowLoc.t -> t -> t =
   fun locs c -> {c with relation= Relation.forget_locs locs c.relation}
end

module BinaryOperationCondition = struct
  type binop_t = Plus | Minus | Mult [@@deriving compare]

  let equal_binop = [%compare.equal: binop_t]

  let binop_to_string = function Plus -> "+" | Minus -> "-" | Mult -> "*"

  type t =
    { binop: binop_t
    ; typ: Typ.ikind
    ; integer_widths: Typ.IntegerWidths.t
    ; lhs: ItvPure.astate
    ; rhs: ItvPure.astate }

  let get_symbols c = ItvPure.get_symbols c.lhs @ ItvPure.get_symbols c.rhs

  let subst eval_sym c =
    match (ItvPure.subst c.lhs eval_sym, ItvPure.subst c.rhs eval_sym) with
    | NonBottom lhs, NonBottom rhs ->
        Some {c with lhs; rhs}
    | _, _ ->
        None


  let have_similar_bounds {binop= binop1; typ= typ1; lhs= lhs1; rhs= rhs1}
      {binop= binop2; typ= typ2; lhs= lhs2; rhs= rhs2} =
    equal_binop binop1 binop2 && Typ.equal_ikind typ1 typ2
    && ItvPure.have_similar_bounds lhs1 lhs2
    && ItvPure.have_similar_bounds rhs1 rhs2


  let has_infty {lhs; rhs} = ItvPure.has_infty lhs || ItvPure.has_infty rhs

  let xcompare ~lhs:{binop= binop1; typ= typ1; lhs= lhs1; rhs= rhs1}
      ~rhs:{binop= binop2; typ= typ2; lhs= lhs2; rhs= rhs2} =
    if not (equal_binop binop1 binop2 && Typ.equal_ikind typ1 typ2) then `NotComparable
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


  let pp fmt {binop; typ; integer_widths; lhs; rhs} =
    F.fprintf fmt "(%a %s %a):" ItvPure.pp lhs (binop_to_string binop) ItvPure.pp rhs ;
    match typ with
    | Typ.IBool ->
        F.fprintf fmt "bool"
    | _ ->
        F.fprintf fmt "%s%d"
          (if Typ.ikind_is_unsigned typ then "unsigned" else "signed")
          (Typ.width_of_ikind integer_widths typ)


  let pp_description = pp

  let is_mult_one binop lhs rhs =
    equal_binop binop Mult && (ItvPure.is_one lhs || ItvPure.is_one rhs)


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


  let check ({binop; typ; integer_widths; lhs; rhs} as c) =
    if is_mult_one binop lhs rhs then {report_issue_type= None; propagate= false}
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
        let lb, ub = Typ.range_of_ikind integer_widths typ in
        (Bound.of_big_int lb, Bound.of_big_int ub)
      in
      let check_underflow, check_overflow = should_check c in
      if
        (* typ_lb <= v_lb and v_ub <= typ_ub, not an error *)
        ((not check_underflow) || Bound.le typ_lb v_lb)
        && ((not check_overflow) || Bound.le v_ub typ_ub)
      then {report_issue_type= None; propagate= false}
      else if
        (* v_ub < typ_lb or typ_ub < v_lb, definitely an error *)
        (check_underflow && Bound.lt v_ub typ_lb) || (check_overflow && Bound.lt typ_ub v_lb)
      then {report_issue_type= Some IssueType.integer_overflow_l1; propagate= false}
      else if
        (* -oo != v_lb < typ_lb or typ_ub < v_ub != +oo, probably an error *)
        (check_underflow && Bound.lt v_lb typ_lb && Bound.is_not_infty v_lb)
        || (check_overflow && Bound.lt typ_ub v_ub && Bound.is_not_infty v_ub)
      then {report_issue_type= Some IssueType.integer_overflow_l2; propagate= false}
      else
        let is_symbolic = ItvPure.is_symbolic v in
        let report_issue_type =
          if Config.bo_debug <= 3 && is_symbolic then None else Some IssueType.integer_overflow_l5
        in
        {report_issue_type; propagate= is_symbolic}


  let make integer_widths bop ~lhs ~rhs =
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
      Some {binop; typ; integer_widths; lhs; rhs}
end

module Condition = struct
  type t =
    | AllocSize of AllocSizeCondition.t
    | ArrayAccess of {is_collection_add: bool; c: ArrayAccessCondition.t}
    | BinaryOperation of BinaryOperationCondition.t

  let make_alloc_size = Option.map ~f:(fun c -> AllocSize c)

  let make_array_access ~is_collection_add =
    Option.map ~f:(fun c -> ArrayAccess {is_collection_add; c})


  let make_binary_operation = Option.map ~f:(fun c -> BinaryOperation c)

  let get_symbols = function
    | AllocSize c ->
        AllocSizeCondition.get_symbols c
    | ArrayAccess {c} ->
        ArrayAccessCondition.get_symbols c
    | BinaryOperation c ->
        BinaryOperationCondition.get_symbols c


  let subst eval_sym rel_map caller_relation = function
    | AllocSize c ->
        AllocSizeCondition.subst eval_sym c |> make_alloc_size
    | ArrayAccess {is_collection_add; c} ->
        ArrayAccessCondition.subst eval_sym rel_map caller_relation c
        |> make_array_access ~is_collection_add
    | BinaryOperation c ->
        BinaryOperationCondition.subst eval_sym c |> make_binary_operation


  let have_similar_bounds c1 c2 =
    match (c1, c2) with
    | AllocSize c1, AllocSize c2 ->
        AllocSizeCondition.have_similar_bounds c1 c2
    | ArrayAccess {c= c1}, ArrayAccess {c= c2} ->
        ArrayAccessCondition.have_similar_bounds c1 c2
    | BinaryOperation c1, BinaryOperation c2 ->
        BinaryOperationCondition.have_similar_bounds c1 c2
    | _ ->
        false


  let has_infty = function
    | ArrayAccess {c} ->
        ArrayAccessCondition.has_infty c
    | BinaryOperation c ->
        BinaryOperationCondition.has_infty c
    | _ ->
        false


  let xcompare ~lhs ~rhs =
    match (lhs, rhs) with
    | AllocSize lhs, AllocSize rhs ->
        AllocSizeCondition.xcompare ~lhs ~rhs
    | ArrayAccess {is_collection_add= b1; c= lhs}, ArrayAccess {is_collection_add= b2; c= rhs}
      when Bool.equal b1 b2 ->
        ArrayAccessCondition.xcompare ~lhs ~rhs
    | BinaryOperation lhs, BinaryOperation rhs ->
        BinaryOperationCondition.xcompare ~lhs ~rhs
    | _ ->
        `NotComparable


  let pp fmt = function
    | AllocSize c ->
        AllocSizeCondition.pp fmt c
    | ArrayAccess {c} ->
        ArrayAccessCondition.pp fmt c
    | BinaryOperation c ->
        BinaryOperationCondition.pp fmt c


  let pp_description fmt = function
    | AllocSize c ->
        AllocSizeCondition.pp_description fmt c
    | ArrayAccess {c} ->
        ArrayAccessCondition.pp_description fmt c
    | BinaryOperation c ->
        BinaryOperationCondition.pp_description fmt c


  let check = function
    | AllocSize c ->
        AllocSizeCondition.check c
    | ArrayAccess {is_collection_add; c} ->
        ArrayAccessCondition.check ~is_collection_add c
    | BinaryOperation c ->
        BinaryOperationCondition.check c


  let forget_locs locs x =
    match x with
    | ArrayAccess {is_collection_add; c} ->
        ArrayAccess {is_collection_add; c= ArrayAccessCondition.forget_locs locs c}
    | AllocSize _ | BinaryOperation _ ->
        x
end

module ConditionTrace = struct
  type intra_cond_trace = Intra | Inter of {call_site: Location.t; callee_pname: Typ.Procname.t}
  [@@deriving compare]

  type 'cond_trace t0 =
    {cond_trace: 'cond_trace; issue_location: Location.t; val_traces: ValTraceSet.t}
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
          let pname = Typ.Procname.to_string callee_pname in
          F.fprintf fmt " by call to %s at %a (%a)" pname Location.pp_file_pos call_site
            ValTraceSet.pp ct.val_traces
      | Intra ->
          F.fprintf fmt " (%a)" ValTraceSet.pp ct.val_traces


  let pp_description : F.formatter -> t -> unit =
   fun fmt ct ->
    match ct.cond_trace with
    | Inter {callee_pname}
      when Config.bo_debug >= 1 || not (SourceFile.is_cpp_model ct.issue_location.Location.file) ->
        F.fprintf fmt " by call to %a " MF.pp_monospaced (Typ.Procname.to_string callee_pname)
    | _ ->
        ()


  let get_val_traces {val_traces} = val_traces

  let get_report_location : t -> Location.t =
   fun ct -> match ct.cond_trace with Intra -> ct.issue_location | Inter {call_site} -> call_site


  let make : Location.t -> ValTraceSet.t -> t =
   fun issue_location val_traces -> {issue_location; cond_trace= Intra; val_traces}


  let make_call_and_subst ~traces_caller ~callee_pname call_site ct =
    let val_traces =
      ValTraceSet.instantiate ~traces_caller ~traces_callee:ct.val_traces call_site
    in
    {ct with cond_trace= Inter {callee_pname; call_site}; val_traces}


  let has_unknown ct = ValTraceSet.has_unknown ct.val_traces

  let check issue_type_u5 : _ t0 -> IssueType.t option =
   fun ct -> if has_unknown ct then Some issue_type_u5 else None


  let check_buffer_overrun ct = check IssueType.buffer_overrun_u5 ct

  let check_integer_overflow ct = check IssueType.integer_overflow_u5 ct

  let for_summary : _ t0 -> summary_t = fun ct -> {ct with cond_trace= ()}
end

module Reported = struct
  type t = IssueType.t [@@deriving compare]

  let make issue_type = issue_type

  let equal = [%compare.equal: t]
end

module ConditionWithTrace = struct
  type 'cond_trace t0 =
    {cond: Condition.t; trace: 'cond_trace ConditionTrace.t0; reported: Reported.t option}

  let make cond trace = {cond; trace; reported= None}

  let pp fmt {cond; trace} = F.fprintf fmt "%a %a" Condition.pp cond ConditionTrace.pp trace

  let pp_summary fmt {cond; trace} =
    F.fprintf fmt "%a %a" Condition.pp cond ConditionTrace.pp_summary trace


  let have_similar_bounds {cond= cond1} {cond= cond2} = Condition.have_similar_bounds cond1 cond2

  let xcompare ~lhs ~rhs =
    match Condition.xcompare ~lhs:lhs.cond ~rhs:rhs.cond with
    | `Equal ->
        if ConditionTrace.compare lhs.trace rhs.trace <= 0 then `LeftSubsumesRight
        else `RightSubsumesLeft
    | (`LeftSubsumesRight | `RightSubsumesLeft | `NotComparable) as cmp ->
        cmp


  let subst (eval_sym, trace_of_sym) rel_map caller_relation callee_pname call_site cwt =
    let symbols = Condition.get_symbols cwt.cond in
    if List.is_empty symbols then
      L.(die InternalError)
        "Trying to substitute a non-symbolic condition %a from %a at %a. Why was it propagated in \
         the first place?"
        pp_summary cwt Typ.Procname.pp callee_pname Location.pp call_site ;
    match Condition.subst eval_sym rel_map caller_relation cwt.cond with
    | None ->
        None
    | Some cond ->
        let traces_caller =
          List.fold symbols ~init:ValTraceSet.empty ~f:(fun val_traces symbol ->
              ValTraceSet.join (trace_of_sym symbol) val_traces )
        in
        let trace =
          ConditionTrace.make_call_and_subst ~traces_caller ~callee_pname call_site cwt.trace
        in
        Some {cond; trace; reported= cwt.reported}


  let set_u5 {cond; trace} issue_type =
    if
      ( IssueType.equal issue_type IssueType.buffer_overrun_l3
      || IssueType.equal issue_type IssueType.buffer_overrun_l4
      || IssueType.equal issue_type IssueType.buffer_overrun_l5 )
      && Condition.has_infty cond
    then Option.value (ConditionTrace.check_buffer_overrun trace) ~default:issue_type
    else if IssueType.equal issue_type IssueType.integer_overflow_l5 && Condition.has_infty cond
    then Option.value (ConditionTrace.check_integer_overflow trace) ~default:issue_type
    else issue_type


  let check_aux cwt =
    let ({report_issue_type; propagate} as checked) = Condition.check cwt.cond in
    match report_issue_type with
    | None ->
        checked
    | Some issue_type ->
        let issue_type = set_u5 cwt issue_type in
        (* Only report if the precision has improved.
        This is approximated by: only report if the issue_type has changed. *)
        let report_issue_type =
          match cwt.reported with
          | Some reported when Reported.equal reported issue_type ->
              (* already reported and the precision hasn't improved *) None
          | _ ->
              (* either never reported or already reported but the precision has improved *)
              Some issue_type
        in
        {report_issue_type; propagate}


  let check ~report cwt =
    let {report_issue_type; propagate} = check_aux cwt in
    match report_issue_type with
    | Some issue_type ->
        report cwt.cond cwt.trace issue_type ;
        if propagate then Some {cwt with reported= Some (Reported.make issue_type)} else None
    | None ->
        Option.some_if propagate cwt


  let forget_locs locs cwt = {cwt with cond= Condition.forget_locs locs cwt.cond}

  let for_summary {cond; trace; reported} =
    {cond; trace= ConditionTrace.for_summary trace; reported}
end

module ConditionSet = struct
  type 'cond_trace t0 = 'cond_trace ConditionWithTrace.t0 list

  type t = ConditionTrace.intra_cond_trace t0

  type summary_t = unit t0

  (* invariant: join_one of one of the elements should return the original list *)

  let empty = []

  let try_merge ~existing ~new_ =
    (* we don't want to remove issues that would end up in a higher bucket,
     e.g. [a, b] < [c, d] is subsumed by [a, +oo] < [c, d] but the latter is less precise *)
    if ConditionWithTrace.have_similar_bounds existing new_ then
      match ConditionWithTrace.xcompare ~lhs:existing ~rhs:new_ with
      | `LeftSubsumesRight ->
          `DoNotAddAndStop
      | `RightSubsumesLeft ->
          `RemoveExistingAndContinue
      | `NotComparable ->
          `KeepExistingAndContinue
    else `KeepExistingAndContinue


  let join_one condset new_ =
    let rec aux ~new_ acc ~same = function
      | [] ->
          if Config.bo_debug >= 3 then
            L.(debug BufferOverrun Verbose)
              "[InferboPO] Adding new condition %a@." ConditionWithTrace.pp new_ ;
          if same then new_ :: condset else new_ :: acc
      | existing :: rest as existings -> (
        match try_merge ~existing ~new_ with
        | `DoNotAddAndStop ->
            if Config.bo_debug >= 3 then
              L.(debug BufferOverrun Verbose)
                "[InferboPO] Not adding condition %a (because of existing %a)@."
                ConditionWithTrace.pp new_ ConditionWithTrace.pp existing ;
            if same then condset else List.rev_append acc existings
        | `RemoveExistingAndContinue ->
            if Config.bo_debug >= 3 then
              L.(debug BufferOverrun Verbose)
                "[InferboPO] Removing condition %a (because of new %a)@." ConditionWithTrace.pp
                existing ConditionWithTrace.pp new_ ;
            aux ~new_ acc ~same:false rest
        | `KeepExistingAndContinue ->
            aux ~new_ (existing :: acc) ~same rest )
    in
    aux ~new_ [] ~same:true condset


  let join condset1 condset2 = List.fold_left ~f:join_one condset1 ~init:condset2

  let add_opt location val_traces condset = function
    | None ->
        condset
    | Some cond ->
        let trace = ConditionTrace.make location val_traces in
        let cwt = ConditionWithTrace.make cond trace in
        join [cwt] condset


  let add_array_access location ~offset ~idx ~size ~is_collection_add ~idx_sym_exp ~size_sym_exp
      ~relation val_traces condset =
    ArrayAccessCondition.make ~offset ~idx ~size ~idx_sym_exp ~size_sym_exp ~relation
    |> Condition.make_array_access ~is_collection_add
    |> add_opt location val_traces condset


  let add_alloc_size location ~length val_traces condset =
    AllocSizeCondition.make ~length |> Condition.make_alloc_size
    |> add_opt location val_traces condset


  let add_binary_operation integer_type_widths location bop ~lhs ~rhs val_traces condset =
    BinaryOperationCondition.make integer_type_widths bop ~lhs ~rhs
    |> Condition.make_binary_operation
    |> add_opt location val_traces condset


  let subst condset eval_sym_trace rel_subst_map caller_relation callee_pname call_site =
    let subst_add_cwt condset cwt =
      match
        ConditionWithTrace.subst eval_sym_trace rel_subst_map caller_relation callee_pname
          call_site cwt
      with
      | None ->
          condset
      | Some cwt ->
          join_one condset cwt
    in
    List.fold condset ~f:subst_add_cwt ~init:[]


  let check_all ~report condset = List.filter_map condset ~f:(ConditionWithTrace.check ~report)

  let pp_summary : F.formatter -> _ t0 -> unit =
   fun fmt condset ->
    let pp_sep fmt () = F.fprintf fmt ", @," in
    F.fprintf fmt "@[<v 0>Safety conditions:@," ;
    F.fprintf fmt "@[<hov 2>{ " ;
    F.pp_print_list ~pp_sep ConditionWithTrace.pp_summary fmt condset ;
    F.fprintf fmt " }@]" ;
    F.fprintf fmt "@]"


  let pp : Format.formatter -> t -> unit =
   fun fmt condset ->
    let pp_sep fmt () = F.fprintf fmt ",@;" in
    F.fprintf fmt "Safety conditions:@;@[<hov 2>{ %a }@]"
      (IList.pp_print_list ~max:30 ~pp_sep ConditionWithTrace.pp)
      condset


  let forget_locs : AbsLoc.PowLoc.t -> 'a t0 -> 'a t0 =
   fun locs x -> List.map x ~f:(ConditionWithTrace.forget_locs locs)


  let for_summary : _ t0 -> summary_t =
   fun condset -> List.map condset ~f:ConditionWithTrace.for_summary
end

let description cond trace =
  F.asprintf "%a%a" Condition.pp_description cond ConditionTrace.pp_description trace
