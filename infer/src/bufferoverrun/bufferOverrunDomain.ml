(*
 * Copyright (c) 2016-present, Programming Research Laboratory (ROPAS)
 *                             Seoul National University, Korea
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open AbsLoc
open! AbstractDomain.Types
module BoField = BufferOverrunField
module F = Format
module L = Logging
module OndemandEnv = BufferOverrunOndemandEnv
module SPath = Symb.SymbolPath
module Trace = BufferOverrunTrace
module TraceSet = Trace.Set
module LoopHeadLoc = Location

module ItvThresholds = AbstractDomain.FiniteSet (struct
  include Z

  let pp = pp_print
end)

module ItvUpdatedBy = struct
  type t = Addition | Multiplication | Top

  let leq ~lhs ~rhs =
    match (lhs, rhs) with
    | Addition, _ ->
        true
    | _, Addition ->
        false
    | Multiplication, _ ->
        true
    | _, Multiplication ->
        false
    | Top, Top ->
        true


  let join x y = if leq ~lhs:x ~rhs:y then y else x

  let widen ~prev ~next ~num_iters:_ = join prev next

  let pp fmt = function
    | Addition ->
        F.pp_print_string fmt "+"
    | Multiplication ->
        F.pp_print_string fmt "*"
    | Top ->
        F.pp_print_string fmt "?"


  let bottom = Addition
end

(* ModeledRange represents how many times the interval value can be updated by modeled functions.
   This domain is to support the case where there are mismatches between value of a control variable
   and actual number of loop iterations.  For example,

   [while((c = file_channel.read(buf)) != -1) { ... }]

   the loop will iterates as the file size, but the control variable [c] does not have that value.
   In these cases, it assigns a symbolic value of the file size to the modeled range of [c], then
   which it is used when calculating the overall cost. *)
module ModeledRange = struct
  include AbstractDomain.BottomLifted (struct
    include Bounds.NonNegativeBound

    let pp = pp ~hum:true
  end)

  let of_modeled_function pname location bound =
    let pname = Procname.to_simplified_string pname in
    NonBottom (Bounds.NonNegativeBound.of_modeled_function pname location bound)


  let of_big_int ~trace z = NonBottom (Bounds.NonNegativeBound.of_big_int ~trace z)
end

type eval_sym_trace =
  { eval_sym: Bounds.Bound.eval_sym
  ; eval_locpath: PowLoc.eval_locpath
  ; eval_func_ptrs: FuncPtr.Set.eval_func_ptrs
  ; trace_of_sym: Symb.Symbol.t -> Trace.Set.t }

module Val = struct
  type t =
    { itv: Itv.t
    ; itv_thresholds: ItvThresholds.t
    ; itv_updated_by: ItvUpdatedBy.t
    ; modeled_range: ModeledRange.t
    ; powloc: PowLoc.t
    ; arrayblk: ArrayBlk.t
    ; func_ptrs: FuncPtr.Set.t
    ; traces: TraceSet.t }
  [@@deriving abstract_domain]

  (* Overwrite [widen] to use the interval threshold always. This function is originally constructed
     by [deriving_inline]. *)
  let widen ~prev ~next ~num_iters =
    if phys_equal prev next then prev
    else
      let traces = TraceSet.widen ~prev:prev.traces ~next:next.traces ~num_iters in
      let func_ptrs = FuncPtr.Set.widen ~prev:prev.func_ptrs ~next:next.func_ptrs ~num_iters in
      let arrayblk = ArrayBlk.widen ~prev:prev.arrayblk ~next:next.arrayblk ~num_iters in
      let powloc = PowLoc.widen ~prev:prev.powloc ~next:next.powloc ~num_iters in
      let modeled_range =
        ModeledRange.widen ~prev:prev.modeled_range ~next:next.modeled_range ~num_iters
      in
      let itv_updated_by =
        ItvUpdatedBy.widen ~prev:prev.itv_updated_by ~next:next.itv_updated_by ~num_iters
      in
      let itv_thresholds =
        ItvThresholds.widen ~prev:prev.itv_thresholds ~next:next.itv_thresholds ~num_iters
      in
      let itv =
        Itv.widen_thresholds
          ~thresholds:(ItvThresholds.elements itv_thresholds)
          ~prev:prev.itv ~next:next.itv ~num_iters
      in
      if
        phys_equal traces next.traces
        && phys_equal func_ptrs next.func_ptrs
        && phys_equal arrayblk next.arrayblk && phys_equal powloc next.powloc
        && phys_equal modeled_range next.modeled_range
        && phys_equal itv_updated_by next.itv_updated_by
        && phys_equal itv_thresholds next.itv_thresholds
        && phys_equal itv next.itv
      then next
      else if
        phys_equal traces prev.traces
        && phys_equal func_ptrs prev.func_ptrs
        && phys_equal arrayblk prev.arrayblk && phys_equal powloc prev.powloc
        && phys_equal modeled_range prev.modeled_range
        && phys_equal itv_updated_by prev.itv_updated_by
        && phys_equal itv_thresholds prev.itv_thresholds
        && phys_equal itv prev.itv
      then prev
      else {itv; itv_thresholds; itv_updated_by; modeled_range; powloc; arrayblk; func_ptrs; traces}


  let bot : t =
    { itv= Itv.bot
    ; itv_thresholds= ItvThresholds.empty
    ; itv_updated_by= ItvUpdatedBy.bottom
    ; modeled_range= ModeledRange.bottom
    ; powloc= PowLoc.bot
    ; arrayblk= ArrayBlk.bot
    ; func_ptrs= FuncPtr.Set.bottom
    ; traces= TraceSet.bottom }


  let unknown : t =
    { itv= Itv.top
    ; itv_thresholds= ItvThresholds.empty
    ; itv_updated_by= ItvUpdatedBy.Top
    ; modeled_range= ModeledRange.bottom
    ; powloc= PowLoc.unknown
    ; arrayblk= ArrayBlk.unknown
    ; func_ptrs= FuncPtr.Set.empty
    ; traces= TraceSet.bottom }


  let default = if Config.bo_bottom_as_default then bot else unknown

  let pp fmt x =
    let itv_thresholds_pp fmt itv_thresholds =
      if Config.bo_debug >= 3 && not (ItvThresholds.is_empty itv_thresholds) then
        F.fprintf fmt " (thresholds:%a)" ItvThresholds.pp itv_thresholds
    in
    let itv_updated_by_pp fmt itv_updated_by =
      if Config.bo_debug >= 3 then F.fprintf fmt "(updated by %a)" ItvUpdatedBy.pp itv_updated_by
    in
    let modeled_range_pp fmt range =
      if not (ModeledRange.is_bottom range) then
        F.fprintf fmt " (modeled_range:%a)" ModeledRange.pp range
    in
    let func_ptrs_pp fmt func_ptrs =
      if not (FuncPtr.Set.is_bottom func_ptrs) then
        F.fprintf fmt ", func_ptrs:%a" FuncPtr.Set.pp func_ptrs
    in
    let trace_pp fmt traces =
      if Config.bo_debug >= 3 then F.fprintf fmt ", %a" TraceSet.pp traces
    in
    F.fprintf fmt "(%a%a%a%a, %a, %a%a%a)" Itv.pp x.itv itv_thresholds_pp x.itv_thresholds
      itv_updated_by_pp x.itv_updated_by modeled_range_pp x.modeled_range PowLoc.pp x.powloc
      ArrayBlk.pp x.arrayblk func_ptrs_pp x.func_ptrs trace_pp x.traces


  let unknown_from : Typ.t -> callee_pname:_ -> location:_ -> t =
   fun typ ~callee_pname ~location ->
    let is_int = Typ.is_int typ in
    let traces = Trace.(Set.singleton_final location (UnknownFrom callee_pname)) in
    { unknown with
      powloc= (if is_int then PowLoc.bot else PowLoc.unknown)
    ; arrayblk= (if is_int then ArrayBlk.bottom else ArrayBlk.unknown)
    ; traces }


  let get_itv : t -> Itv.t = fun x -> x.itv

  let get_itv_updated_by : t -> ItvUpdatedBy.t = fun x -> x.itv_updated_by

  let get_modeled_range : t -> ModeledRange.t = fun x -> x.modeled_range

  let get_pow_loc : t -> PowLoc.t = fun x -> x.powloc

  let get_array_blk : t -> ArrayBlk.t = fun x -> x.arrayblk

  let get_array_locs : t -> PowLoc.t = fun x -> ArrayBlk.get_pow_loc x.arrayblk

  let get_all_locs : t -> PowLoc.t = fun x -> PowLoc.join x.powloc (get_array_locs x)

  let get_func_ptrs : t -> FuncPtr.Set.t = fun x -> x.func_ptrs

  let get_traces : t -> TraceSet.t = fun x -> x.traces

  let of_itv ?(traces = TraceSet.bottom) itv = {bot with itv; traces}

  let of_int n = of_itv (Itv.of_int n)

  let of_big_int n = of_itv (Itv.of_big_int n)

  let of_int_lit n = of_itv (Itv.of_int_lit n)

  let of_loc ?(traces = TraceSet.bottom) x = {bot with powloc= PowLoc.singleton x; traces}

  let of_pow_loc ~traces powloc = {bot with powloc; traces}

  let of_c_array_alloc :
      Allocsite.t -> stride:int option -> offset:Itv.t -> size:Itv.t -> traces:TraceSet.t -> t =
   fun allocsite ~stride ~offset ~size ~traces ->
    let stride = Option.value_map stride ~default:Itv.nat ~f:Itv.of_int in
    {bot with arrayblk= ArrayBlk.make_c allocsite ~offset ~size ~stride; traces}


  let of_java_array_alloc : Allocsite.t -> length:Itv.t -> traces:TraceSet.t -> t =
   fun allocsite ~length ~traces -> {bot with arrayblk= ArrayBlk.make_java allocsite ~length; traces}


  let of_literal_string : IntegerWidths.t -> string -> t =
   fun integer_type_widths s ->
    let allocsite = Allocsite.literal_string s in
    let stride = Some (integer_type_widths.char_width / 8) in
    let offset = Itv.zero in
    let size = Itv.of_int (String.length s + 1) in
    of_c_array_alloc allocsite ~stride ~offset ~size ~traces:TraceSet.bottom


  let of_func_ptrs func_ptrs = {bot with func_ptrs}

  let deref_of_literal_string s =
    let max_char = String.fold s ~init:0 ~f:(fun acc c -> max acc (Char.to_int c)) in
    of_itv (Itv.set_lb_zero (Itv.of_int max_char))


  let set_itv_updated_by itv_updated_by x =
    {x with itv_updated_by= ItvUpdatedBy.join x.itv_updated_by itv_updated_by}


  let set_itv_updated_by_addition = set_itv_updated_by ItvUpdatedBy.Addition

  let set_itv_updated_by_multiplication = set_itv_updated_by ItvUpdatedBy.Multiplication

  let set_itv_updated_by_unknown = set_itv_updated_by ItvUpdatedBy.Top

  let set_modeled_range range x = {x with modeled_range= range}

  let unknown_bit : t -> t = fun x -> {x with itv= Itv.top}

  let neg : t -> t = fun x -> {x with itv= Itv.neg x.itv}

  let lnot : t -> t = fun x -> {x with itv= Itv.lnot x.itv |> Itv.of_bool}

  let lift_itv : ?may_ptr:bool -> (Itv.t -> Itv.t -> Itv.t) -> ?f_trace:_ -> t -> t -> t =
    let no_ptr {powloc; arrayblk} = PowLoc.is_bot powloc && ArrayBlk.is_bot arrayblk in
    fun ?(may_ptr = false) f ?f_trace x y ->
      let itv = if (not may_ptr) || (no_ptr x && no_ptr y) then f x.itv y.itv else Itv.top in
      let itv_thresholds = ItvThresholds.join x.itv_thresholds y.itv_thresholds in
      let itv_updated_by = ItvUpdatedBy.join x.itv_updated_by y.itv_updated_by in
      let modeled_range = ModeledRange.join x.modeled_range y.modeled_range in
      let traces =
        match f_trace with
        | Some f_trace ->
            f_trace x.traces y.traces
        | None -> (
          match (Itv.eq itv x.itv, Itv.eq itv y.itv) with
          | true, false ->
              x.traces
          | false, true ->
              y.traces
          | true, true | false, false ->
              TraceSet.join x.traces y.traces )
      in
      {bot with itv; itv_thresholds; itv_updated_by; modeled_range; traces}


  let lift_cmp_itv : (Itv.t -> Itv.t -> Boolean.t) -> Boolean.EqualOrder.t -> t -> t -> t =
   fun cmp_itv cmp_loc x y ->
    let b =
      match
        ( x.itv
        , PowLoc.is_bot x.powloc
        , ArrayBlk.is_bot x.arrayblk
        , y.itv
        , PowLoc.is_bot y.powloc
        , ArrayBlk.is_bot y.arrayblk )
      with
      | NonBottom _, true, true, NonBottom _, true, true ->
          cmp_itv x.itv y.itv
      | Bottom, false, true, Bottom, false, true ->
          PowLoc.lift_cmp cmp_loc x.powloc y.powloc
      | Bottom, true, false, Bottom, true, false ->
          ArrayBlk.lift_cmp_itv cmp_itv cmp_loc x.arrayblk y.arrayblk
      | _ ->
          Boolean.Top
    in
    let itv = Itv.of_bool b in
    {bot with itv; traces= TraceSet.join x.traces y.traces}


  let plus_a = lift_itv Itv.plus

  let minus_a = lift_itv Itv.minus

  let get_range_of_iterator : t -> t =
   fun i ->
    let itv = Itv.get_range_of_iterator i.itv in
    of_itv itv ~traces:i.traces


  let mult = lift_itv Itv.mult

  let div = lift_itv Itv.div

  let mod_sem = lift_itv Itv.mod_sem

  let shiftlt = lift_itv Itv.shiftlt

  let shiftrt = lift_itv Itv.shiftrt

  let band_sem = lift_itv ~may_ptr:true Itv.band_sem

  let lt_sem : t -> t -> t = lift_cmp_itv Itv.lt_sem Boolean.EqualOrder.strict_cmp

  let gt_sem : t -> t -> t = lift_cmp_itv Itv.gt_sem Boolean.EqualOrder.strict_cmp

  let le_sem : t -> t -> t = lift_cmp_itv Itv.le_sem Boolean.EqualOrder.loose_cmp

  let ge_sem : t -> t -> t = lift_cmp_itv Itv.ge_sem Boolean.EqualOrder.loose_cmp

  let eq_sem : t -> t -> t = lift_cmp_itv Itv.eq_sem Boolean.EqualOrder.eq

  let ne_sem : t -> t -> t = lift_cmp_itv Itv.ne_sem Boolean.EqualOrder.ne

  let land_sem : t -> t -> t = lift_cmp_itv Itv.land_sem Boolean.EqualOrder.top

  let lor_sem : t -> t -> t = lift_cmp_itv Itv.lor_sem Boolean.EqualOrder.top

  let lift_prune1 : (Itv.t -> Itv.t) -> t -> t =
   fun f x ->
    let itv = f x.itv in
    if (not (Itv.is_bottom x.itv)) && Itv.is_bottom itv then
      (* Prune produced bottom interval, return bottom value in order to detect that prune
         pairs are not reachable (see PrunePairs.is_reachable). *)
      {bot with traces= x.traces}
    else {x with itv}


  let lift_prune_length1 : (Itv.t -> Itv.t) -> t -> t =
   fun f x -> {x with arrayblk= ArrayBlk.transform_length ~f x.arrayblk}


  let lift_prune2 :
      (Itv.t -> Itv.t -> Itv.t) -> (ArrayBlk.t -> ArrayBlk.t -> ArrayBlk.t) -> t -> t -> t =
   fun f g x y ->
    let itv =
      let pruned_itv = f x.itv y.itv in
      if
        Itv.is_bottom pruned_itv
        && (not (Itv.is_bottom x.itv))
        && Itv.is_bottom y.itv
        && not (PowLoc.is_bot (get_all_locs y))
      then x.itv
      else pruned_itv
    in
    let itv_thresholds =
      Option.value_map (Itv.get_const y.itv) ~default:x.itv_thresholds ~f:(fun z ->
          x.itv_thresholds
          |> ItvThresholds.add Z.(z - one)
          |> ItvThresholds.add z
          |> ItvThresholds.add Z.(z + one) )
    in
    let arrayblk = g x.arrayblk y.arrayblk in
    if
      phys_equal itv x.itv
      && phys_equal itv_thresholds x.itv_thresholds
      && phys_equal arrayblk x.arrayblk
    then (* x hasn't changed, don't join traces *)
      x
    else {x with itv; itv_thresholds; arrayblk; traces= TraceSet.join x.traces y.traces}


  let prune_eq_zero : t -> t = lift_prune1 Itv.prune_eq_zero

  let prune_ne_zero : t -> t = lift_prune1 Itv.prune_ne_zero

  let prune_ge_one : t -> t = lift_prune1 Itv.prune_ge_one

  let prune_length_le : t -> Itv.t -> t =
   fun x y -> lift_prune_length1 (fun x -> Itv.prune_le x y) x


  let prune_length_lt : t -> Itv.t -> t =
   fun x y -> lift_prune_length1 (fun x -> Itv.prune_lt x y) x


  let prune_length_eq : t -> Itv.t -> t =
   fun x y -> lift_prune_length1 (fun x -> Itv.prune_eq x y) x


  let prune_length_eq_zero : t -> t = fun x -> prune_length_eq x Itv.zero

  let prune_length_ge_one : t -> t = lift_prune_length1 Itv.prune_ge_one

  let prune_binop : Binop.t -> t -> t -> t =
   fun c -> lift_prune2 (Itv.prune_binop c) (ArrayBlk.prune_binop c)


  let is_null : t -> bool =
   fun x -> Itv.is_false x.itv && PowLoc.is_bot x.powloc && ArrayBlk.is_bot x.arrayblk


  let prune_eq : t -> t -> t =
   fun x y ->
    if is_null y then {x with itv= Itv.zero; powloc= PowLoc.bot; arrayblk= ArrayBlk.bot}
    else lift_prune2 Itv.prune_eq ArrayBlk.prune_eq x y


  let prune_ne : t -> t -> t = lift_prune2 Itv.prune_ne ArrayBlk.prune_ne

  let prune_lt : t -> t -> t = prune_binop Binop.Lt

  let is_pointer_to_non_array x = (not (PowLoc.is_bot x.powloc)) && ArrayBlk.is_bot x.arrayblk

  (* In the pointer arithmetics, it returns top, if we cannot
     precisely follow the physical memory model, e.g., (&x + 1). *)
  let lift_pi : (ArrayBlk.t -> Itv.t -> ArrayBlk.t) -> t -> t -> t =
   fun f x y ->
    let traces = TraceSet.join x.traces y.traces in
    if is_pointer_to_non_array x then {bot with itv= Itv.top; traces}
    else {bot with arrayblk= f x.arrayblk y.itv; traces}


  let plus_pi : t -> t -> t = fun x y -> lift_pi ArrayBlk.plus_offset x y

  let minus_pi : t -> t -> t = fun x y -> lift_pi ArrayBlk.minus_offset x y

  let minus_pp : t -> t -> t =
   fun x y ->
    let itv =
      if is_pointer_to_non_array x && is_pointer_to_non_array y then Itv.top
      else ArrayBlk.diff x.arrayblk y.arrayblk
    in
    {bot with itv; traces= TraceSet.join x.traces y.traces}


  let get_symbols : t -> Itv.SymbolSet.t =
   fun x -> Itv.SymbolSet.union (Itv.get_symbols x.itv) (ArrayBlk.get_symbols x.arrayblk)


  let normalize : t -> t =
   fun x -> {x with itv= Itv.normalize x.itv; arrayblk= ArrayBlk.normalize x.arrayblk}


  let subst : t -> eval_sym_trace -> Location.t -> t =
   fun x {eval_sym; eval_locpath; eval_func_ptrs; trace_of_sym} location ->
    let symbols = get_symbols x in
    let traces_caller =
      Itv.SymbolSet.fold
        (fun symbol traces -> TraceSet.join (trace_of_sym symbol) traces)
        symbols TraceSet.bottom
    in
    let traces = TraceSet.call location ~traces_caller ~traces_callee:x.traces in
    let powloc = PowLoc.subst x.powloc eval_locpath in
    let powloc_from_arrayblk, arrayblk = ArrayBlk.subst x.arrayblk eval_sym eval_locpath in
    { x with
      itv= Itv.subst x.itv eval_sym
    ; powloc= PowLoc.join powloc powloc_from_arrayblk
    ; arrayblk
    ; func_ptrs= FuncPtr.Set.subst x.func_ptrs eval_func_ptrs
    ; traces }
    (* normalize bottom *)
    |> normalize


  let add_assign_trace_elem location locs x =
    let traces = Trace.(Set.add_elem location (Assign locs)) x.traces in
    {x with traces}


  let array_sizeof {arrayblk} =
    if ArrayBlk.is_bot arrayblk then Itv.top else ArrayBlk.get_size arrayblk


  let set_array_length : Location.t -> length:t -> t -> t =
   fun location ~length v ->
    { v with
      arrayblk= ArrayBlk.set_length length.itv v.arrayblk
    ; traces= Trace.(Set.add_elem location SetArraySize) length.traces }


  let transform_array_length : Location.t -> f:(Itv.t -> Itv.t) -> t -> t =
   fun location ~f v ->
    { v with
      arrayblk= ArrayBlk.transform_length ~f v.arrayblk
    ; traces= Trace.(Set.add_elem location Through) v.traces }


  let set_array_offset : Location.t -> Itv.t -> t -> t =
   fun location offset v ->
    { v with
      arrayblk= ArrayBlk.set_offset offset v.arrayblk
    ; traces= Trace.(Set.add_elem location Through) v.traces }


  let set_array_stride : Z.t -> t -> t =
   fun new_stride v ->
    PhysEqual.optim1 v ~res:{v with arrayblk= ArrayBlk.set_stride new_stride v.arrayblk}


  let unknown_locs = of_pow_loc PowLoc.unknown ~traces:TraceSet.bottom

  let is_bot x =
    Itv.is_bottom x.itv && PowLoc.is_bot x.powloc && ArrayBlk.is_bot x.arrayblk
    && FuncPtr.Set.is_bottom x.func_ptrs


  let is_unknown v =
    Itv.is_top v.itv && PowLoc.is_unknown v.powloc && ArrayBlk.is_unknown v.arrayblk


  let is_mone x = Itv.is_mone (get_itv x)

  let is_incr_of l {itv} = Option.exists (Loc.get_path l) ~f:(fun path -> Itv.is_incr_of path itv)

  let cast typ v = {v with powloc= PowLoc.cast typ v.powloc}

  let of_path tenv ~may_last_field integer_type_widths location typ path =
    let traces_of_loc l =
      let trace = if Loc.is_global l then Trace.Global l else Trace.Parameter l in
      TraceSet.singleton location trace
    in
    let itv_val ~non_int =
      let l = Loc.of_path path in
      let traces = traces_of_loc l in
      let unsigned = Typ.is_unsigned_int typ in
      of_itv ~traces (Itv.of_normal_path ~unsigned ~non_int path)
    in
    let ptr_to_c_array_alloc deref_path size =
      let allocsite = Allocsite.make_symbol deref_path in
      let offset = Itv.zero in
      let traces = traces_of_loc (Loc.of_path deref_path) in
      of_c_array_alloc allocsite ~stride:None ~offset ~size ~traces
    in
    let ptr_to_normal_c_array elt =
      let deref_kind = SPath.Deref_CPointer in
      let deref_path = SPath.deref ~deref_kind path in
      let l = Loc.of_path deref_path in
      let traces = traces_of_loc l in
      let arrayblk =
        let allocsite = Allocsite.make_symbol deref_path in
        let stride =
          match elt with
          | Some {Typ.desc= Tint ikind} ->
              Itv.of_int (IntegerWidths.width_of_ikind integer_type_widths ikind)
          | _ ->
              Itv.nat
        in
        let is_void = Typ.is_pointer_to_void typ in
        let offset =
          if SPath.is_cpp_vector_elem path then Itv.zero else Itv.of_offset_path ~is_void path
        in
        let size = Itv.of_length_path ~is_void path in
        ArrayBlk.make_c allocsite ~stride ~offset ~size
      in
      {bot with arrayblk; traces}
    in
    let is_java = Language.curr_language_is Java in
    L.d_printfln_escaped "Val.of_path %a : %a%s%s" SPath.pp_partial path (Typ.pp Pp.text) typ
      (if may_last_field then ", may_last_field" else "")
      (if is_java then ", is_java" else "") ;
    match path with
    | BoField.Field {fn; typ} when BufferOverrunField.is_cpp_vector_elem fn ->
        ptr_to_normal_c_array typ
    | _ -> (
      match typ.Typ.desc with
      | Tint (IBool | IChar | ISChar | IUChar | IUShort) ->
          let v = itv_val ~non_int:is_java in
          if is_java then set_itv_updated_by_unknown v else set_itv_updated_by_addition v
      | Tfloat _ | Tfun | TVar _ ->
          itv_val ~non_int:true |> set_itv_updated_by_unknown
      | Tint _ | Tvoid ->
          itv_val ~non_int:false |> set_itv_updated_by_addition
      | Tptr ({desc= Tfun}, _) ->
          of_func_ptrs (FuncPtr.Set.of_path path)
      | Tptr ({desc= Tstruct name}, _)
        when PatternMatch.is_subtype tenv name StdTyp.Name.Objc.ns_enumerator ->
          (* NOTE: It generates a value of NSEnumerator specifically.  Especially, it assigns zero to
             the offset, rather than a symbol, to avoid precision loss by limited handling of symbolic
             values in the domain.  Although this is an unsound design choice, we expect it should not
             that harmful for calculating WCET. *)
          let allocsite = SPath.deref ~deref_kind:Deref_CPointer path |> Allocsite.make_symbol in
          let size = Itv.of_length_path ~is_void:false path in
          {bot with arrayblk= ArrayBlk.make_c allocsite ~offset:Itv.zero ~size ~stride:Itv.one}
      | Tptr (elt, _) ->
          if is_java || SPath.is_this path then
            let deref_kind =
              if is_java then SPath.Deref_JavaPointer else SPath.Deref_COneValuePointer
            in
            let deref_path = SPath.deref ~deref_kind path in
            let l = Loc.of_path deref_path in
            let traces = traces_of_loc l in
            {bot with powloc= PowLoc.singleton l; traces}
          else ptr_to_normal_c_array (Some elt)
      | Tstruct typename -> (
        match BufferOverrunTypModels.dispatch tenv typename with
        | Some (CArray {deref_kind; length}) ->
            let deref_path = SPath.deref ~deref_kind path in
            let size = Itv.of_int_lit length in
            ptr_to_c_array_alloc deref_path size
        | Some CppStdVector ->
            let l = Loc.of_path (SPath.deref ~deref_kind:Deref_CPointer path) in
            let traces = traces_of_loc l in
            of_loc ~traces l
        | Some JavaCollection ->
            let deref_path = SPath.deref ~deref_kind:Deref_ArrayIndex path in
            let l = Loc.of_path deref_path in
            let traces = traces_of_loc l in
            let allocsite = Allocsite.make_symbol deref_path in
            let length = Itv.of_length_path ~is_void:false path in
            of_java_array_alloc allocsite ~length ~traces
        | Some JavaInteger ->
            itv_val ~non_int:false
        | None ->
            let l = Loc.of_path path in
            let traces = traces_of_loc l in
            of_loc ~traces l )
      | Tarray {length; stride} ->
          let deref_path = SPath.deref ~deref_kind:Deref_ArrayIndex path in
          let l = Loc.of_path deref_path in
          let traces = traces_of_loc l in
          let allocsite = Allocsite.make_symbol deref_path in
          let size =
            match length with
            | None (* IncompleteArrayType, no-size flexible array *) ->
                Itv.of_length_path ~is_void:false path
            | Some length
              when may_last_field && (IntLit.iszero length || IntLit.isone length)
                   (* 0/1-sized flexible array *) ->
                Itv.of_length_path ~is_void:false path
            | Some length ->
                Itv.of_big_int (IntLit.to_big_int length)
          in
          if is_java then of_java_array_alloc allocsite ~length:size ~traces
          else
            let stride = Option.map stride ~f:(fun n -> IntLit.to_int_exn n) in
            let offset = Itv.zero in
            of_c_array_alloc allocsite ~stride ~offset ~size ~traces )


  let on_demand : default:t -> ?typ:Typ.t -> OndemandEnv.t -> Loc.t -> t =
   fun ~default ?typ {tenv; typ_of_param_path; may_last_field; entry_location; integer_type_widths}
       l ->
    let do_on_demand path typ =
      let may_last_field = may_last_field path in
      of_path tenv ~may_last_field integer_type_widths entry_location typ path
    in
    match Loc.get_literal_string l with
    | Some s ->
        deref_of_literal_string s
    | None -> (
      match Loc.get_literal_string_strlen l with
      | Some s ->
          of_itv (Itv.of_int (String.length s))
      | None -> (
        match l with
        | Field {fn} when Fieldname.equal fn BufferOverrunField.java_linked_list_index ->
            L.d_printfln_escaped "Val.on_demand for %a as zero" Loc.pp l ;
            of_itv Itv.zero
        | _ -> (
          match Loc.get_path l with
          | None ->
              L.d_printfln_escaped "Val.on_demand for %a -> no path" Loc.pp l ;
              default
          | Some path -> (
            match typ_of_param_path path with
            | None -> (
              match typ with
              | Some typ ->
                  L.d_printfln_escaped "Val.on_demand for %a -> typ=%a" Loc.pp l (Typ.pp Pp.text)
                    typ ;
                  do_on_demand path typ
              | _ ->
                  L.d_printfln_escaped "Val.on_demand for %a -> no type" Loc.pp l ;
                  default )
            | Some typ ->
                L.d_printfln_escaped "Val.on_demand for %a" Loc.pp l ;
                do_on_demand path typ ) ) ) )


  module Itv = struct
    let zero_255 = of_itv Itv.zero_255

    let m1_255 = of_itv Itv.m1_255

    let nat = of_itv Itv.nat

    let pos = of_itv Itv.pos

    let top = of_itv Itv.top

    let unknown_bool = of_itv Itv.unknown_bool

    let zero = of_itv Itv.zero

    let one = of_itv Itv.one
  end
end

module StackLocs = struct
  include AbstractDomain.FiniteSet (Loc)

  let bot = empty
end

(* MultiLocs denotes whether abstract locations represent one or multiple concrete locations.  If
   the value is true, the abstract location may represent multiple concrete locations, thus it
   should be updated weakly. *)
module MultiLocs = AbstractDomain.BooleanOr

module MVal = struct
  include AbstractDomain.Pair (MultiLocs) (Val)

  let pp fmt (represents_multiple_values, v) =
    if represents_multiple_values then F.fprintf fmt "M" ;
    Val.pp fmt v


  let on_demand ~default ?typ oenv l =
    (Loc.represents_multiple_values l, Val.on_demand ~default ?typ oenv l)


  let get_rep_multi (represents_multiple_values, _) = represents_multiple_values

  let get_val (_, v) = v

  let is_incr_of l (m, v) = (not m) && Val.is_incr_of l v
end

module MemPure = struct
  include AbstractDomain.Map (Loc) (MVal)

  let bot = empty

  let get_linked_list_index loc mem =
    let linked_list_index = Loc.append_field loc BufferOverrunField.java_linked_list_index in
    Option.map (find_opt linked_list_index mem) ~f:(fun (_, v) -> (linked_list_index, v))


  let get_objc_iterator_offset loc mem =
    match find_opt loc mem with
    | Some (_, array_v) -> (
        let elem_loc = Val.get_all_locs array_v in
        let array_set = PowLoc.get_parent_field elem_loc |> PowLoc.to_set in
        match LocSet.max_elt_opt array_set with
        | Some array_loc ->
            let offset = Loc.append_field array_loc BufferOverrunField.objc_iterator_offset in
            if Loc.is_unknown offset then None
            else Option.map (find_opt offset mem) ~f:(fun (_, v) -> (offset, v))
        | _ ->
            None )
    | _ ->
        None


  let range :
         filter_loc:(Loc.t -> LoopHeadLoc.t option)
      -> node_id:ProcCfg.Normal.Node.id
      -> t
      -> Polynomials.NonNegativePolynomial.t =
   fun ~filter_loc ~node_id mem ->
    fold
      (fun loc (_, v) acc ->
        match filter_loc loc with
        | Some loop_head_loc -> (
            let loc, v = Option.value (get_linked_list_index loc mem) ~default:(loc, v) in
            let loc, v = Option.value (get_objc_iterator_offset loc mem) ~default:(loc, v) in
            let itv_updated_by = Val.get_itv_updated_by v in
            match itv_updated_by with
            | Addition | Multiplication ->
                (* TODO take range of multiplied one with log scale *)
                let itv = Val.get_itv v in
                if Itv.has_only_non_int_symbols itv then acc
                else
                  let range1 =
                    match Val.get_modeled_range v with
                    | NonBottom range ->
                        Polynomials.NonNegativePolynomial.of_non_negative_bound range
                    | Bottom ->
                        Itv.range loop_head_loc itv |> Itv.ItvRange.to_top_lifted_polynomial
                  in
                  if Polynomials.NonNegativePolynomial.is_top range1 then
                    L.d_printfln_escaped "Range of %a (loc:%a) became top at %a." Itv.pp itv Loc.pp
                      loc ProcCfg.Normal.Node.pp_id node_id ;
                  let range = Polynomials.NonNegativePolynomial.mult acc range1 in
                  if
                    (not (Polynomials.NonNegativePolynomial.is_top acc))
                    && Polynomials.NonNegativePolynomial.is_top range
                  then
                    L.d_printfln_escaped "Multiplication of %a and %a (loc:%a) became top at %a."
                      Polynomials.NonNegativePolynomial.pp acc Polynomials.NonNegativePolynomial.pp
                      range1 Loc.pp loc ProcCfg.Normal.Node.pp_id node_id ;
                  range
            | Top ->
                acc )
        | None ->
            acc )
      mem Polynomials.NonNegativePolynomial.one


  let join oenv astate1 astate2 =
    if phys_equal astate1 astate2 then astate1
    else
      merge
        (fun l v1_opt v2_opt ->
          match (v1_opt, v2_opt) with
          | Some v1, Some v2 ->
              Some (MVal.join v1 v2)
          | Some v1, None | None, Some v1 ->
              (* Since abstract state doesn't contain complete information, use Val.top
                      if a location is missing in one state being joined to be sound. *)
              let v2 = MVal.on_demand ~default:Val.default oenv l in
              Some (MVal.join v1 v2)
          | None, None ->
              None )
        astate1 astate2


  let widen oenv ~prev ~next ~num_iters =
    if phys_equal prev next then prev
    else
      merge
        (fun l v1_opt v2_opt ->
          match (v1_opt, v2_opt) with
          | Some v1, Some v2 ->
              Some (MVal.widen ~prev:v1 ~next:v2 ~num_iters)
          | Some v1, None ->
              (* Since abstract state doesn't contain complete information, use Val.top
                      if a location is missing in one state being widened to be sound. *)
              let v2 = MVal.on_demand ~default:Val.default oenv l in
              Some (MVal.widen ~prev:v1 ~next:v2 ~num_iters)
          | None, Some v2 ->
              (* Since abstract state doesn't contain complete information, use Val.top
                 if a location is missing in one state being widened to be sound. *)
              let v1 = MVal.on_demand ~default:Val.default oenv l in
              Some (MVal.widen ~prev:v1 ~next:v2 ~num_iters)
          | None, None ->
              None )
        prev next


  let is_rep_multi_loc l m =
    Option.value_map (find_opt l m) ~f:MVal.get_rep_multi
      ~default:(Loc.represents_multiple_values l)


  (** Collect the location that was increased by one, i.e., [x -> x+1] *)
  let get_incr_locs m =
    fold (fun l v acc -> if MVal.is_incr_of l v then PowLoc.add l acc else acc) m PowLoc.bot


  let find_opt l m = Option.map (find_opt l m) ~f:MVal.get_val

  let strong_update l v m = add l (Loc.represents_multiple_values l, v) m

  let add ?(represents_multiple_values = false) l ({Val.powloc; arrayblk} as v) m =
    let v =
      if Loc.is_unknown l then
        (* We do not add the other locations except the unknown itself in its value.  This spoiled
           other values raising out of memory sometimes, rather than being helpful for analysis
           precision. *)
        { v with
          Val.powloc= (if PowLoc.is_bot powloc then powloc else PowLoc.unknown)
        ; arrayblk= (if ArrayBlk.is_bottom arrayblk then arrayblk else ArrayBlk.unknown) }
      else v
    in
    let f = function
      | None ->
          Some (represents_multiple_values || Loc.represents_multiple_values l, v)
      | Some (represents_multiple_values', v') ->
          let represents_multiple_values =
            represents_multiple_values || represents_multiple_values'
          in
          let v = if represents_multiple_values then Val.join v' v else v in
          Some (represents_multiple_values, v)
    in
    update l f m


  let fold f m init = fold (fun k range acc -> f k (MVal.get_val range) acc) m init
end

module AliasTarget = struct
  (* [Eq]: The value of alias target is exactly the same to the alias key.

     [Le]: The value of alias target is less than or equal to the alias key.  For example, if there
     is an alias between [%r] and [size(x)+i] with the [Le] type, which means [size(x)+i <= %r]. *)
  type alias_typ = Eq | Le [@@deriving compare, equal]

  let alias_typ_pp fmt = function
    | Eq ->
        F.pp_print_string fmt "="
    | Le ->
        F.pp_print_string fmt ">="


  type t =
    | Simple of {i: IntLit.t; java_tmp: Loc.t option}
    | Empty
    | Size of {alias_typ: alias_typ; i: IntLit.t; java_tmp: Loc.t option}
    | Fgets
    | IteratorSimple of {i: IntLit.t; java_tmp: Loc.t option}
    | IteratorOffset of {alias_typ: alias_typ; i: IntLit.t; java_tmp: Loc.t option}
    | IteratorHasNext of {java_tmp: Loc.t option}
    | IteratorNextObject of {objc_tmp: AbsLoc.Loc.t option}
    | Top
  [@@deriving compare, equal]

  let top = Top

  let is_top = function Top -> true | _ -> false

  let pp_with_key ~pp_lhs ~pp_rhs =
    let pp_intlit fmt i =
      if not (IntLit.iszero i) then
        if IntLit.isnegative i then F.fprintf fmt "-%a" IntLit.pp (IntLit.neg i)
        else F.fprintf fmt "+%a" IntLit.pp i
    in
    let pp_java_tmp fmt java_tmp = Option.iter java_tmp ~f:(F.fprintf fmt "=%a" Loc.pp) in
    fun fmt -> function
      | Simple {i; java_tmp} ->
          F.fprintf fmt "%t%a=%t%a" pp_lhs pp_java_tmp java_tmp pp_rhs pp_intlit i
      | Empty ->
          F.fprintf fmt "%t=empty(%t)" pp_lhs pp_rhs
      | Size {alias_typ; i; java_tmp} ->
          F.fprintf fmt "%t%a%asize(%t)%a" pp_lhs pp_java_tmp java_tmp alias_typ_pp alias_typ pp_rhs
            pp_intlit i
      | Fgets ->
          F.fprintf fmt "%t=fgets(%t)" pp_lhs pp_rhs
      | IteratorSimple {i; java_tmp} ->
          F.fprintf fmt "iterator offset(%t%a)=%t%a" pp_lhs pp_java_tmp java_tmp pp_rhs pp_intlit i
      | IteratorOffset {alias_typ; i; java_tmp} ->
          F.fprintf fmt "iterator offset(%t%a)%alength(%t)%a" pp_lhs pp_java_tmp java_tmp
            alias_typ_pp alias_typ pp_rhs pp_intlit i
      | IteratorHasNext {java_tmp} ->
          F.fprintf fmt "%t%a=hasNext(%t)" pp_lhs pp_java_tmp java_tmp pp_rhs
      | IteratorNextObject {objc_tmp} ->
          F.fprintf fmt "%t%a=nextObject(%t)" pp_lhs pp_java_tmp objc_tmp pp_rhs
      | Top ->
          F.fprintf fmt "%t=?%t" pp_lhs pp_rhs


  let pp =
    let pp_underscore fmt = F.pp_print_string fmt "_" in
    pp_with_key ~pp_lhs:pp_underscore ~pp_rhs:pp_underscore


  let get_locs = function
    | Simple {java_tmp= Some tmp}
    | Size {java_tmp= Some tmp}
    | IteratorSimple {java_tmp= Some tmp}
    | IteratorOffset {java_tmp= Some tmp}
    | IteratorHasNext {java_tmp= Some tmp}
    | IteratorNextObject {objc_tmp= Some tmp} ->
        PowLoc.singleton tmp
    | Simple {java_tmp= None}
    | Size {java_tmp= None}
    | Empty
    | Fgets
    | IteratorSimple {java_tmp= None}
    | IteratorOffset {java_tmp= None}
    | IteratorHasNext {java_tmp= None}
    | IteratorNextObject {objc_tmp= None}
    | Top ->
        PowLoc.bot


  let use_loc l x = PowLoc.mem l (get_locs x)

  let loc_map x ~f =
    match x with
    | Simple {i; java_tmp} ->
        Simple {i; java_tmp= Option.bind java_tmp ~f}
    | Empty ->
        Empty
    | Size {alias_typ; i; java_tmp} ->
        Size {alias_typ; i; java_tmp= Option.bind java_tmp ~f}
    | Fgets ->
        Fgets
    | IteratorSimple {i; java_tmp} ->
        IteratorSimple {i; java_tmp= Option.bind java_tmp ~f}
    | IteratorOffset {alias_typ; i; java_tmp} ->
        IteratorOffset {alias_typ; i; java_tmp= Option.bind java_tmp ~f}
    | IteratorHasNext {java_tmp} ->
        IteratorHasNext {java_tmp= Option.bind java_tmp ~f}
    | IteratorNextObject {objc_tmp} ->
        IteratorNextObject {objc_tmp= Option.bind objc_tmp ~f}
    | Top ->
        Top


  let leq ~lhs ~rhs =
    equal lhs rhs
    ||
    match (lhs, rhs) with
    | _, Top ->
        true
    | Top, _ ->
        false
    | ( Size {alias_typ= _; i= i1; java_tmp= java_tmp1}
      , Size {alias_typ= Le; i= i2; java_tmp= java_tmp2} )
    | ( IteratorOffset {alias_typ= _; i= i1; java_tmp= java_tmp1}
      , IteratorOffset {alias_typ= Le; i= i2; java_tmp= java_tmp2} ) ->
        (* (a=size(l)+2) <= (a>=size(l)+1)  *)
        (* (a>=size(l)+2) <= (a>=size(l)+1)  *)
        IntLit.geq i1 i2 && Option.equal Loc.equal java_tmp1 java_tmp2
    | _, _ ->
        false


  let join =
    let java_tmp_eq loc1 loc2 = Option.equal Loc.equal loc1 loc2 in
    fun x y ->
      if equal x y then x
      else
        match (x, y) with
        | ( Size {alias_typ= _; i= i1; java_tmp= java_tmp1}
          , Size {alias_typ= _; i= i2; java_tmp= java_tmp2} )
          when java_tmp_eq java_tmp1 java_tmp2 ->
            (* (a=size(l)+1) join (a=size(l)+2) is (a>=size(l)+1) *)
            (* (a=size(l)+1) join (a>=size(l)+2) is (a>=size(l)+1) *)
            Size {alias_typ= Le; i= IntLit.min i1 i2; java_tmp= java_tmp1}
        | ( IteratorOffset {alias_typ= _; i= i1; java_tmp= java_tmp1}
          , IteratorOffset {alias_typ= _; i= i2; java_tmp= java_tmp2} )
          when java_tmp_eq java_tmp1 java_tmp2 ->
            IteratorOffset {alias_typ= Le; i= IntLit.min i1 i2; java_tmp= java_tmp1}
        | _, _ ->
            Top


  let widen ~prev ~next ~num_iters:_ =
    if equal prev next then prev
    else
      match (prev, next) with
      | Size {alias_typ= Eq}, Size {alias_typ= _}
      | IteratorOffset {alias_typ= Eq}, IteratorOffset {alias_typ= _} ->
          join prev next
      | Size {alias_typ= Le; i= i1}, Size {alias_typ= _; i= i2}
      | IteratorOffset {alias_typ= Le; i= i1}, IteratorOffset {alias_typ= _; i= i2}
        when IntLit.eq i1 i2 ->
          join prev next
      | _, _ ->
          Top


  let is_unknown x = PowLoc.exists Loc.is_unknown (get_locs x)

  let is_size = function Size _ | IteratorOffset _ -> true | _ -> false

  let incr_size_alias x =
    match x with
    | Size {alias_typ; i} ->
        Size {alias_typ; i= IntLit.(add i minus_one); java_tmp= None}
    | IteratorOffset {alias_typ; i; java_tmp} ->
        IteratorOffset {alias_typ; i= IntLit.(add i minus_one); java_tmp}
    | _ ->
        x


  let incr_or_not_size_alias x =
    match x with
    | Size {i} ->
        Size {alias_typ= Le; i; java_tmp= None}
    | IteratorOffset {i; java_tmp} ->
        IteratorOffset {alias_typ= Le; i; java_tmp}
    | _ ->
        x


  let set_java_tmp loc = function
    | Size a ->
        Size {a with java_tmp= Some loc}
    | IteratorSimple a ->
        IteratorSimple {a with java_tmp= Some loc}
    | IteratorOffset a ->
        IteratorOffset {a with java_tmp= Some loc}
    | IteratorHasNext _ ->
        IteratorHasNext {java_tmp= Some loc}
    | IteratorNextObject _ ->
        IteratorNextObject {objc_tmp= Some loc}
    | _ as alias ->
        alias
end

module KeyLhs = struct
  type t = IdentKey of Ident.t | LocKey of Loc.t [@@deriving compare]

  let of_id id = IdentKey id

  let of_loc l = LocKey l

  let pp f = function IdentKey id -> Ident.pp f id | LocKey l -> Loc.pp f l

  let use_loc l = function LocKey l' -> Loc.equal l l' | IdentKey _ -> false
end

module KeyRhs = Loc

module AliasTargets = struct
  include AbstractDomain.SafeInvertedMap (KeyRhs) (AliasTarget)

  let pp_with_lhs ~pp_lhs fmt x =
    let pp_sep fmt () = F.fprintf fmt ", @," in
    let pp1 fmt (rhs, v) =
      AliasTarget.pp_with_key ~pp_lhs ~pp_rhs:(fun fmt -> KeyRhs.pp fmt rhs) fmt v
    in
    F.pp_print_list ~pp_sep pp1 fmt (bindings x)


  let pp = pp_with_lhs ~pp_lhs:(fun fmt -> F.pp_print_string fmt "_")

  let forget l x =
    let not_use_l k v = not (KeyRhs.equal l k || AliasTarget.use_loc l v) in
    filter not_use_l x


  let forget_size_alias arr_locs x =
    let not_in_arr_locs k v = not (PowLoc.mem k arr_locs && AliasTarget.is_size v) in
    filter not_in_arr_locs x


  let incr_size_alias loc x = update loc (Option.map ~f:AliasTarget.incr_size_alias) x

  let incr_or_not_size_alias loc x = update loc (Option.map ~f:AliasTarget.incr_or_not_size_alias) x

  let subst ~subst_loc x =
    let accum_substed rhs tgt acc =
      Option.value_map (subst_loc rhs) ~default:acc ~f:(fun rhs ->
          add rhs (AliasTarget.loc_map tgt ~f:subst_loc) acc )
    in
    fold accum_substed x empty


  let exists2 f x y = exists (fun k v -> exists (f k v) y) x

  let find_simple_alias x =
    let exception Found of KeyRhs.t in
    let is_simple_zero rhs = function
      | AliasTarget.Simple {i} when IntLit.iszero i ->
          raise (Found rhs)
      | _ ->
          ()
    in
    match iter is_simple_zero x with () -> None | exception Found rhs -> Some rhs


  let find_size_alias x =
    let exception Found of KeyRhs.t in
    let is_size rhs = function AliasTarget.Size _ -> raise (Found rhs) | _ -> () in
    match iter is_size x with () -> None | exception Found rhs -> Some rhs
end

module AliasMap = struct
  module M = AbstractDomain.SafeInvertedMap (KeyLhs) (AliasTargets)

  type t = M.t

  let leq = M.leq

  let join = M.join

  let widen = M.widen

  let pp : F.formatter -> t -> unit =
   fun fmt x ->
    let pp_sep fmt () = F.fprintf fmt ", @," in
    let pp1 fmt (lhs, v) = AliasTargets.pp_with_lhs ~pp_lhs:(fun fmt -> KeyLhs.pp fmt lhs) fmt v in
    F.pp_print_list ~pp_sep pp1 fmt (M.bindings x)


  let empty = M.empty

  let is_empty = M.is_empty

  let add_alias ~lhs ~rhs v m =
    let add_to_tgts = function
      | None ->
          Some (AliasTargets.singleton rhs v)
      | Some tgts ->
          Some (AliasTargets.add rhs v tgts)
    in
    M.update lhs add_to_tgts m


  let add_aliases ~lhs tgts m =
    AliasTargets.fold (fun rhs v acc -> add_alias ~lhs ~rhs v acc) tgts m


  let remove = M.remove

  let find k m = M.find_opt k m |> Option.value ~default:AliasTargets.empty

  let find_id : Ident.t -> t -> AliasTargets.t = fun id x -> find (KeyLhs.of_id id) x

  let find_loc : Loc.t -> t -> AliasTargets.t =
   fun loc x -> find (KeyLhs.LocKey loc) x |> AliasTargets.map (AliasTarget.set_java_tmp loc)


  let has_objc_collection_size_alias : Loc.t -> t -> bool =
   fun loc x ->
    AliasTargets.find_size_alias (find_loc loc x)
    |> Option.exists ~f:Loc.is_objc_collection_internal_array


  let load : Ident.t -> Loc.t -> AliasTarget.t -> t -> t =
   fun id loc tgt x ->
    if Loc.is_unknown loc || AliasTarget.is_unknown tgt then x
    else
      let tgts =
        match tgt with
        | AliasTarget.Simple {i}
          when IntLit.iszero i
               && (Language.curr_language_is Java || has_objc_collection_size_alias loc x) ->
            find_loc loc x |> AliasTargets.add loc tgt
        | _ ->
            AliasTargets.singleton loc tgt
      in
      add_aliases ~lhs:(KeyLhs.of_id id) tgts x


  let forget : Loc.t -> t -> t =
   fun l x ->
    let forget1 k v = if KeyLhs.use_loc l k then AliasTargets.top else AliasTargets.forget l v in
    M.mapi forget1 x


  let store : Loc.t -> Ident.t -> t -> t =
   fun l id x ->
    let tgts = find_id id x in
    if Loc.is_frontend_tmp l then add_aliases ~lhs:(KeyLhs.of_loc l) tgts x
    else
      let accum_java_tmp_alias rhs tgt acc =
        match tgt with
        | AliasTarget.Simple {i} when IntLit.iszero i && Loc.is_frontend_tmp rhs ->
            add_alias ~lhs:(KeyLhs.of_id id) ~rhs:l (AliasTarget.Simple {i; java_tmp= Some rhs}) acc
            |> add_alias ~lhs:(KeyLhs.of_loc rhs) ~rhs:l (AliasTarget.Simple {i; java_tmp= None})
        | AliasTarget.IteratorNextObject _ | AliasTarget.IteratorSimple _ ->
            add_alias ~lhs:(KeyLhs.of_loc l) ~rhs tgt acc
        | _ ->
            acc
      in
      AliasTargets.fold accum_java_tmp_alias tgts x


  let add_size_alias ~lhs ~lhs_v ~arr ~arr_size x =
    add_alias ~lhs:(KeyLhs.of_loc lhs) ~rhs:arr
      (AliasTarget.Size {alias_typ= Eq; i= IntLit.sub lhs_v arr_size; java_tmp= None})
      x


  let incr_size_alias loc x = M.map (AliasTargets.incr_size_alias loc) x

  let incr_or_not_size_alias loc x = M.map (AliasTargets.incr_or_not_size_alias loc) x

  let forget_size_alias arr_locs x = M.map (AliasTargets.forget_size_alias arr_locs) x

  let incr_iterator_simple_alias ~prev loc n x =
    let accum_tgt ~lhs ~rhs tgt acc =
      if Loc.equal rhs loc then
        match tgt with
        | AliasTarget.IteratorSimple {i; java_tmp} ->
            add_alias ~lhs ~rhs (AliasTarget.IteratorSimple {i= IntLit.sub i n; java_tmp}) acc
        | _ ->
            acc
      else acc
    in
    let accum_tgts lhs tgts acc =
      AliasTargets.fold (fun rhs tgts acc -> accum_tgt ~lhs ~rhs tgts acc) tgts acc
    in
    M.fold accum_tgts prev x


  let incr_iterator_simple_alias_on_call {eval_locpath} ~callee_locs x =
    let accum_increased_alias callee_loc acc =
      Option.value_map (Loc.get_path callee_loc) ~default:acc ~f:(fun callee_path ->
          match PowLoc.is_singleton_or_more (eval_locpath callee_path) with
          | IContainer.Singleton loc ->
              incr_iterator_simple_alias ~prev:x loc IntLit.one acc
          | IContainer.Empty | IContainer.More ->
              acc )
    in
    PowLoc.fold accum_increased_alias callee_locs x


  let store_n ~prev loc id n x =
    let accum_size_alias rhs tgt acc =
      match tgt with
      | AliasTarget.Size {alias_typ; i} ->
          add_alias ~lhs:(KeyLhs.of_loc loc) ~rhs
            (AliasTarget.Size {alias_typ; i= IntLit.add i n; java_tmp= None})
            acc
      | _ ->
          acc
    in
    let tgts = find_id id prev in
    let x = AliasTargets.fold accum_size_alias tgts x in
    match AliasTargets.find_simple_alias tgts with
    | Some loc' when Loc.equal loc loc' ->
        incr_iterator_simple_alias ~prev loc n x
    | _ ->
        x


  let add_iterator_simple_alias id int x =
    add_alias ~lhs:(KeyLhs.of_id id) ~rhs:int
      (AliasTarget.IteratorSimple {i= IntLit.zero; java_tmp= None})
      x


  let add_iterator_offset_alias id arr x =
    add_alias ~lhs:(KeyLhs.of_id id) ~rhs:arr
      (AliasTarget.IteratorOffset {alias_typ= Eq; i= IntLit.zero; java_tmp= None})
      x


  let incr_iterator_offset_alias =
    let apply_i ~f = function
      | AliasTarget.IteratorSimple ({i} as tgt) ->
          AliasTarget.IteratorSimple {tgt with i= f i}
      | AliasTarget.IteratorOffset ({i} as tgt) ->
          AliasTarget.IteratorOffset {tgt with i= f i}
      | _ ->
          assert false
    in
    let java_tmp_none = function
      | AliasTarget.IteratorSimple tgt ->
          AliasTarget.IteratorSimple {tgt with java_tmp= None}
      | AliasTarget.IteratorOffset tgt ->
          AliasTarget.IteratorOffset {tgt with java_tmp= None}
      | _ ->
          assert false
    in
    fun id x ->
      let accum_incr_iterator_offset_alias rhs tgt acc =
        match tgt with
        | AliasTarget.IteratorSimple {java_tmp} | AliasTarget.IteratorOffset {java_tmp} ->
            let tgt = apply_i tgt ~f:IntLit.(add one) in
            let acc = add_alias ~lhs:(KeyLhs.of_id id) ~rhs tgt acc in
            Option.value_map java_tmp ~default:x ~f:(fun java_tmp ->
                add_alias ~lhs:(KeyLhs.of_loc java_tmp) ~rhs (java_tmp_none tgt) acc )
        | _ ->
            acc
      in
      match M.find_opt (KeyLhs.of_id id) x with
      | Some tgts ->
          AliasTargets.fold accum_incr_iterator_offset_alias tgts x
      | _ ->
          x


  let add_iterator_has_next_alias ~ret_id ~iterator x =
    let accum_has_next_alias _rhs tgt acc =
      match tgt with
      | AliasTarget.IteratorSimple {java_tmp= Some java_tmp}
      | AliasTarget.IteratorOffset {java_tmp= Some java_tmp} ->
          add_alias ~lhs:(KeyLhs.of_id ret_id) ~rhs:java_tmp
            (AliasTarget.IteratorHasNext {java_tmp= None})
            acc
      | _ ->
          acc
    in
    match M.find_opt (KeyLhs.of_id iterator) x with
    | Some tgts ->
        AliasTargets.fold accum_has_next_alias tgts x
    | _ ->
        x


  let add_iterator_next_object_alias ~ret_id ~iterator x =
    let open IOption.Let_syntax in
    M.find_opt (KeyLhs.of_id iterator) x
    >>= AliasTargets.find_simple_alias
    >>| (fun rhs ->
          add_alias ~lhs:(KeyLhs.of_id ret_id) ~rhs
            (AliasTarget.IteratorNextObject {objc_tmp= None})
            x )
    |> Option.value ~default:x
end

module AliasRet = struct
  include AliasTargets

  let pp : F.formatter -> t -> unit =
   fun fmt x ->
    F.pp_print_string fmt "ret=" ;
    pp fmt x
end

module CppIterBeginOrEndKind = struct
  type t = Begin | End [@@deriving equal]

  let get_binop = function Begin -> Binop.Gt | End -> Binop.Lt

  let pp f = function Begin -> F.pp_print_string f "begin" | End -> F.pp_print_string f "end"
end

module CppIterBeginOrEndValue = AbstractDomain.Flat (CppIterBeginOrEndKind)

module PVar = struct
  include Pvar

  let pp = Pvar.pp Pp.text
end

module CppIterBeginOrEnd = struct
  include AbstractDomain.InvertedMap (PVar) (CppIterBeginOrEndValue)

  let pp : F.formatter -> t -> unit = fun fmt x -> F.fprintf fmt "cpp_iter_begin_or_end=%a" pp x
end

module CppIteratorCmpValue = struct
  type t = V of {iter_lhs: Pvar.t; iter_rhs: Pvar.t; begin_or_end: CppIterBeginOrEndKind.t} | Top

  let pp f = function
    | V {iter_lhs; iter_rhs; begin_or_end} ->
        F.fprintf f "{iter_lhs:%a, iter_rhs:%a begin_or_end:%a}" (Pvar.pp Pp.text) iter_lhs
          (Pvar.pp Pp.text) iter_rhs CppIterBeginOrEndKind.pp begin_or_end
    | Top ->
        F.pp_print_string f "top"


  let leq ~lhs ~rhs =
    match (lhs, rhs) with
    | V lhs, V rhs ->
        Pvar.equal lhs.iter_lhs rhs.iter_lhs
        && Pvar.equal lhs.iter_rhs rhs.iter_rhs
        && CppIterBeginOrEndKind.equal lhs.begin_or_end rhs.begin_or_end
    | Top, V _ ->
        false
    | (V _ | Top), Top ->
        true


  let join x y =
    match (x, y) with
    | V x, V y ->
        if
          Pvar.equal x.iter_lhs y.iter_lhs && Pvar.equal x.iter_rhs y.iter_rhs
          && CppIterBeginOrEndKind.equal x.begin_or_end y.begin_or_end
        then V x
        else Top
    | _, Top | Top, _ ->
        Top


  let widen ~prev ~next ~num_iters:_ = join prev next
end

module CppIteratorCmp = struct
  include AbstractDomain.InvertedMap (Ident) (CppIteratorCmpValue)

  let pp : F.formatter -> t -> unit = fun fmt x -> F.fprintf fmt "cpp_iter=%a" pp x
end

module Alias = struct
  type t =
    { map: AliasMap.t
    ; ret: AliasRet.t
          (** The list of addresses that were assigned to a return variable of a function. All
              locations reachable from these addresses need to be kept in the abstract state. *)
    ; cpp_iterator_cmp: CppIteratorCmp.t
    ; cpp_iter_begin_or_end: CppIterBeginOrEnd.t }
  [@@deriving abstract_domain]

  let pp fmt x =
    F.fprintf fmt "@[<hov 2>{ %a%s%a, %a, %a }@]" AliasMap.pp x.map
      (if AliasMap.is_empty x.map then "" else ", ")
      AliasRet.pp x.ret CppIteratorCmp.pp x.cpp_iterator_cmp CppIterBeginOrEnd.pp
      x.cpp_iter_begin_or_end


  let init : t =
    { map= AliasMap.empty
    ; ret= AliasRet.empty
    ; cpp_iterator_cmp= CppIteratorCmp.empty
    ; cpp_iter_begin_or_end= CppIterBeginOrEnd.empty }


  let lift_map : (AliasMap.t -> AliasMap.t) -> t -> t = fun f a -> {a with map= f a.map}

  let bind_map : (AliasMap.t -> 'a) -> t -> 'a = fun f a -> f a.map

  let find_id : Ident.t -> t -> AliasTargets.t = fun x -> bind_map (AliasMap.find_id x)

  let find_loc : Loc.t -> t -> AliasTargets.t = fun x -> bind_map (AliasMap.find_loc x)

  let find_ret : t -> AliasTargets.t = fun x -> x.ret

  let load : Ident.t -> Loc.t -> AliasTarget.t -> t -> t =
   fun id loc tgt -> lift_map (AliasMap.load id loc tgt)


  let store_simple : Loc.t -> Exp.t -> t -> t =
   fun loc e prev ->
    let a = lift_map (AliasMap.forget loc) prev in
    match e with
    | Exp.Var l ->
        let a = lift_map (AliasMap.store loc l) a in
        if Loc.is_return loc then {a with ret= find_id l a} else a
    | Exp.BinOp (Binop.PlusA _, Exp.Var id, Exp.Const (Const.Cint i))
    | Exp.BinOp (Binop.PlusA _, Exp.Const (Const.Cint i), Exp.Var id) ->
        lift_map (AliasMap.load id loc (AliasTarget.Simple {i= IntLit.neg i; java_tmp= None})) a
        |> lift_map (AliasMap.store_n ~prev:prev.map loc id i)
    | Exp.BinOp (Binop.MinusA _, Exp.Var id, Exp.Const (Const.Cint i)) ->
        lift_map (AliasMap.load id loc (AliasTarget.Simple {i; java_tmp= None})) a
        |> lift_map (AliasMap.store_n ~prev:prev.map loc id (IntLit.neg i))
    | _ ->
        a


  let fgets : Ident.t -> PowLoc.t -> t -> t =
   fun id locs a ->
    let a = PowLoc.fold (fun loc acc -> lift_map (AliasMap.forget loc) acc) locs a in
    match PowLoc.is_singleton_or_more locs with
    | IContainer.Singleton loc ->
        load id loc AliasTarget.Fgets a
    | _ ->
        a


  let update_size_alias locs a ~f = PowLoc.fold f locs a

  let incr_size_alias : PowLoc.t -> t -> t =
   fun locs a ->
    update_size_alias locs a ~f:(fun loc acc -> lift_map (AliasMap.incr_size_alias loc) acc)


  let incr_or_not_size_alias : PowLoc.t -> t -> t =
   fun locs a ->
    update_size_alias locs a ~f:(fun loc acc -> lift_map (AliasMap.incr_or_not_size_alias loc) acc)


  let add_size_alias : Loc.t -> IntLit.t -> (Loc.t * IntLit.t) list -> t -> t =
   fun loc i arr_locs prev ->
    let accum_size_alias acc (arr_loc, arr_size) =
      lift_map (AliasMap.add_size_alias ~lhs:loc ~lhs_v:i ~arr:arr_loc ~arr_size) acc
    in
    List.fold arr_locs ~init:(lift_map (AliasMap.forget loc) prev) ~f:accum_size_alias


  let add_iterator_simple_alias : Ident.t -> PowLoc.t -> t -> t =
   fun id int_locs a ->
    let accum_iterator_simple_alias int_loc acc =
      lift_map (AliasMap.add_iterator_simple_alias id int_loc) acc
    in
    PowLoc.fold accum_iterator_simple_alias int_locs a


  let add_iterator_offset_alias : Ident.t -> PowLoc.t -> t -> t =
   fun id arr_locs a ->
    let accum_iterator_offset_alias arr_loc acc =
      lift_map (AliasMap.add_iterator_offset_alias id arr_loc) acc
    in
    PowLoc.fold accum_iterator_offset_alias arr_locs a


  let incr_iterator_offset_alias : Ident.t -> t -> t =
   fun id a -> lift_map (AliasMap.incr_iterator_offset_alias id) a


  let add_iterator_has_next_alias : ret_id:Ident.t -> iterator:Ident.t -> t -> t =
   fun ~ret_id ~iterator a -> lift_map (AliasMap.add_iterator_has_next_alias ~ret_id ~iterator) a


  let add_iterator_next_object_alias : ret_id:Ident.t -> iterator:Ident.t -> t -> t =
   fun ~ret_id ~iterator a -> lift_map (AliasMap.add_iterator_next_object_alias ~ret_id ~iterator) a


  let remove_key : KeyLhs.t -> t -> t = fun key -> lift_map (AliasMap.remove key)

  let forget_size_alias arr_locs = lift_map (AliasMap.forget_size_alias arr_locs)

  let incr_iterator_simple_alias_on_call eval_sym_trace ~callee_locs =
    lift_map (AliasMap.incr_iterator_simple_alias_on_call eval_sym_trace ~callee_locs)


  let add_cpp_iterator_cmp_alias id ~iter_lhs ~iter_rhs begin_or_end alias =
    { alias with
      cpp_iterator_cmp=
        CppIteratorCmp.add id
          (CppIteratorCmpValue.V {iter_lhs; iter_rhs; begin_or_end})
          alias.cpp_iterator_cmp }


  let find_cpp_iterator_alias id {cpp_iterator_cmp} =
    match CppIteratorCmp.find_opt id cpp_iterator_cmp with
    | Some (V {iter_lhs; iter_rhs; begin_or_end}) ->
        Some (iter_lhs, iter_rhs, CppIterBeginOrEndKind.get_binop begin_or_end)
    | Some Top | None ->
        None


  let add_cpp_iter_begin_alias pvar alias =
    { alias with
      cpp_iter_begin_or_end=
        CppIterBeginOrEnd.add pvar CppIterBeginOrEndValue.(v Begin) alias.cpp_iter_begin_or_end }


  let add_cpp_iter_end_alias pvar alias =
    { alias with
      cpp_iter_begin_or_end=
        CppIterBeginOrEnd.add pvar CppIterBeginOrEndValue.(v End) alias.cpp_iter_begin_or_end }


  let find_cpp_iter_begin_or_end_alias pvar {cpp_iter_begin_or_end} =
    CppIterBeginOrEnd.find_opt pvar cpp_iter_begin_or_end
end

module CoreVal = struct
  type t = Val.t

  let compare x y =
    let r = Itv.compare (Val.get_itv x) (Val.get_itv y) in
    if r <> 0 then r
    else
      let r = PowLoc.compare (Val.get_pow_loc x) (Val.get_pow_loc y) in
      if r <> 0 then r else ArrayBlk.compare (Val.get_array_blk x) (Val.get_array_blk y)


  let pp fmt x =
    F.fprintf fmt "(%a, %a, %a)" Itv.pp (Val.get_itv x) PowLoc.pp (Val.get_pow_loc x) ArrayBlk.pp
      (Val.get_array_blk x)


  let is_symbolic v =
    let itv = Val.get_itv v in
    if Itv.is_bottom itv then ArrayBlk.is_symbolic (Val.get_array_blk v) else Itv.is_symbolic itv


  let is_bot = Val.is_bot
end

module PruningExp = struct
  type t = Unknown | Binop of {bop: Binop.t; lhs: CoreVal.t; rhs: CoreVal.t} [@@deriving compare]

  let leq ~lhs ~rhs =
    match (lhs, rhs) with
    | _, Unknown ->
        true
    | Unknown, _ ->
        false
    | Binop {bop= bop1; lhs= lhs1; rhs= rhs1}, Binop {bop= bop2; lhs= lhs2; rhs= rhs2} ->
        Binop.equal bop1 bop2 && Val.leq ~lhs:lhs1 ~rhs:lhs2 && Val.leq ~lhs:rhs1 ~rhs:rhs2


  let join x y =
    match (x, y) with
    | Binop {bop= bop1; lhs= lhs1; rhs= rhs1}, Binop {bop= bop2; lhs= lhs2; rhs= rhs2}
      when Binop.equal bop1 bop2 ->
        Binop {bop= bop1; lhs= Val.join lhs1 lhs2; rhs= Val.join rhs1 rhs2}
    | _, _ ->
        Unknown


  let widen ~prev ~next ~num_iters =
    match (prev, next) with
    | Binop {bop= bop1; lhs= lhs1; rhs= rhs1}, Binop {bop= bop2; lhs= lhs2; rhs= rhs2}
      when Binop.equal bop1 bop2 ->
        Binop
          { bop= bop1
          ; lhs= Val.widen ~prev:lhs1 ~next:lhs2 ~num_iters
          ; rhs= Val.widen ~prev:rhs1 ~next:rhs2 ~num_iters }
    | _, _ ->
        Unknown


  let pp fmt x =
    match x with
    | Unknown ->
        F.pp_print_string fmt "Unknown"
    | Binop {bop; lhs; rhs} ->
        F.fprintf fmt "(%a %s %a)" CoreVal.pp lhs (Binop.str Pp.text bop) CoreVal.pp rhs


  let make bop ~lhs ~rhs = Binop {bop; lhs; rhs}

  let is_unknown = function Unknown -> true | Binop _ -> false

  let is_symbolic = function
    | Unknown ->
        false
    | Binop {lhs; rhs} ->
        CoreVal.is_symbolic lhs || CoreVal.is_symbolic rhs


  let is_empty =
    let le_false v = Itv.leq ~lhs:(Val.get_itv v) ~rhs:Itv.zero in
    function
    | Unknown ->
        false
    | Binop {bop= Lt; lhs; rhs} ->
        le_false (Val.lt_sem lhs rhs)
    | Binop {bop= Gt; lhs; rhs} ->
        le_false (Val.gt_sem lhs rhs)
    | Binop {bop= Le; lhs; rhs} ->
        le_false (Val.le_sem lhs rhs)
    | Binop {bop= Ge; lhs; rhs} ->
        le_false (Val.ge_sem lhs rhs)
    | Binop {bop= Eq; lhs; rhs} ->
        le_false (Val.eq_sem lhs rhs)
    | Binop {bop= Ne; lhs; rhs} ->
        le_false (Val.ne_sem lhs rhs)
    | Binop _ ->
        assert false


  let subst x eval_sym_trace location =
    match x with
    | Unknown ->
        Unknown
    | Binop {bop; lhs; rhs} ->
        Binop
          { bop
          ; lhs= Val.subst lhs eval_sym_trace location
          ; rhs= Val.subst rhs eval_sym_trace location }
end

module PrunedVal = struct
  type t = {v: CoreVal.t; pruning_exp: PruningExp.t} [@@deriving compare]

  let leq ~lhs ~rhs =
    Val.leq ~lhs:lhs.v ~rhs:rhs.v && PruningExp.leq ~lhs:lhs.pruning_exp ~rhs:rhs.pruning_exp


  let join x y = {v= Val.join x.v y.v; pruning_exp= PruningExp.join x.pruning_exp y.pruning_exp}

  let widen ~prev ~next ~num_iters =
    { v= Val.widen ~prev:prev.v ~next:next.v ~num_iters
    ; pruning_exp= PruningExp.widen ~prev:prev.pruning_exp ~next:next.pruning_exp ~num_iters }


  let pp fmt x =
    CoreVal.pp fmt x.v ;
    if not (PruningExp.is_unknown x.pruning_exp) then
      F.fprintf fmt " by %a" PruningExp.pp x.pruning_exp


  let make v pruning_exp = {v; pruning_exp}

  let get_val x = x.v

  let subst {v; pruning_exp} eval_sym_trace location =
    { v= Val.subst v eval_sym_trace location
    ; pruning_exp= PruningExp.subst pruning_exp eval_sym_trace location }


  let is_symbolic {v; pruning_exp} = CoreVal.is_symbolic v || PruningExp.is_symbolic pruning_exp

  let is_bot {v; pruning_exp} = CoreVal.is_bot v || PruningExp.is_empty pruning_exp
end

(* [PrunePairs] is a map from abstract locations to abstract values that represents pruned results
   in the latest pruning.  It uses [InvertedMap] because more pruning means smaller abstract
   states. *)
module PrunePairs = struct
  include AbstractDomain.InvertedMap (Loc) (PrunedVal)

  let forget locs x = filter (fun l _ -> not (PowLoc.mem l locs)) x

  let subst x ({eval_locpath} as eval_sym_trace) location =
    let open Result.Monad_infix in
    let subst1 l pruned_val acc =
      acc
      >>= fun acc ->
      match PowLoc.is_singleton_or_more (PowLoc.subst_loc l eval_locpath) with
      | Singleton loc ->
          Ok (add loc (PrunedVal.subst pruned_val eval_sym_trace location) acc)
      | Empty ->
          Error `SubstBottom
      | More ->
          Error `SubstFail
    in
    fold subst1 x (Ok empty)


  let is_reachable x = not (exists (fun _ v -> PrunedVal.is_bot v) x)
end

module LatestPrune = struct
  (* Latest p: The pruned pairs 'p' has pruning information (which
     abstract locations are updated by which abstract values) in the
     latest pruning.

     TrueBranch (x, p): After a pruning, the variable 'x' is assigned
     by 1.  There is no other memory updates after the latest pruning.

     FalseBranch (x, p): After a pruning, the variable 'x' is assigned
     by 0.  There is no other memory updates after the latest pruning.

     V (x, ptrue, pfalse): After two non-sequential prunings ('ptrue'
     and 'pfalse'), the variable 'x' is assigned by 1 and 0,
     respectively.  There is no other memory updates after the latest
     prunings.

     VRet (x, ptrue, pfalse): Similar to V, but this is for return
     values of functions.

     Top: No information about the latest pruning. *)
  type t =
    | Latest of PrunePairs.t
    | TrueBranch of Pvar.t * PrunePairs.t
    | FalseBranch of Pvar.t * PrunePairs.t
    | V of Pvar.t * PrunePairs.t * PrunePairs.t
    | VRet of Ident.t * PrunePairs.t * PrunePairs.t
    | Top

  let pvar_pp = Pvar.pp Pp.text

  let pp fmt = function
    | Top ->
        ()
    | Latest p ->
        F.fprintf fmt "LatestPrune: latest %a" PrunePairs.pp p
    | TrueBranch (v, p) ->
        F.fprintf fmt "LatestPrune: true(%a) %a" pvar_pp v PrunePairs.pp p
    | FalseBranch (v, p) ->
        F.fprintf fmt "LatestPrune: false(%a) %a" pvar_pp v PrunePairs.pp p
    | V (v, p1, p2) ->
        F.fprintf fmt "LatestPrune: v(%a) %a / %a" pvar_pp v PrunePairs.pp p1 PrunePairs.pp p2
    | VRet (v, p1, p2) ->
        F.fprintf fmt "LatestPrune: ret(%a) %a / %a" Ident.pp v PrunePairs.pp p1 PrunePairs.pp p2


  let leq ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      match (lhs, rhs) with
      | _, Top ->
          true
      | Top, _ ->
          false
      | Latest p1, Latest p2 ->
          PrunePairs.leq ~lhs:p1 ~rhs:p2
      | TrueBranch (x1, p1), TrueBranch (x2, p2)
      | FalseBranch (x1, p1), FalseBranch (x2, p2)
      | TrueBranch (x1, p1), V (x2, p2, _)
      | FalseBranch (x1, p1), V (x2, _, p2) ->
          Pvar.equal x1 x2 && PrunePairs.leq ~lhs:p1 ~rhs:p2
      | V (x1, ptrue1, pfalse1), V (x2, ptrue2, pfalse2) ->
          Pvar.equal x1 x2
          && PrunePairs.leq ~lhs:ptrue1 ~rhs:ptrue2
          && PrunePairs.leq ~lhs:pfalse1 ~rhs:pfalse2
      | VRet (x1, ptrue1, pfalse1), VRet (x2, ptrue2, pfalse2) ->
          Ident.equal x1 x2
          && PrunePairs.leq ~lhs:ptrue1 ~rhs:ptrue2
          && PrunePairs.leq ~lhs:pfalse1 ~rhs:pfalse2
      | _, _ ->
          false


  let join x y =
    match (x, y) with
    | _, _ when leq ~lhs:x ~rhs:y ->
        y
    | _, _ when leq ~lhs:y ~rhs:x ->
        x
    | Latest p1, Latest p2 ->
        Latest (PrunePairs.join p1 p2)
    | FalseBranch (x1, p1), FalseBranch (x2, p2) when Pvar.equal x1 x2 ->
        FalseBranch (x1, PrunePairs.join p1 p2)
    | TrueBranch (x1, p1), TrueBranch (x2, p2) when Pvar.equal x1 x2 ->
        TrueBranch (x1, PrunePairs.join p1 p2)
    | FalseBranch (x', pfalse), TrueBranch (y', ptrue)
    | TrueBranch (x', ptrue), FalseBranch (y', pfalse)
      when Pvar.equal x' y' ->
        V (x', ptrue, pfalse)
    | TrueBranch (x1, ptrue1), V (x2, ptrue2, pfalse)
    | V (x2, ptrue2, pfalse), TrueBranch (x1, ptrue1)
      when Pvar.equal x1 x2 ->
        V (x1, PrunePairs.join ptrue1 ptrue2, pfalse)
    | FalseBranch (x1, pfalse1), V (x2, ptrue, pfalse2)
    | V (x2, ptrue, pfalse2), FalseBranch (x1, pfalse1)
      when Pvar.equal x1 x2 ->
        V (x1, ptrue, PrunePairs.join pfalse1 pfalse2)
    | V (x1, ptrue1, pfalse1), V (x2, ptrue2, pfalse2) when Pvar.equal x1 x2 ->
        V (x1, PrunePairs.join ptrue1 ptrue2, PrunePairs.join pfalse1 pfalse2)
    | VRet (x1, ptrue1, pfalse1), VRet (x2, ptrue2, pfalse2) when Ident.equal x1 x2 ->
        VRet (x1, PrunePairs.join ptrue1 ptrue2, PrunePairs.join pfalse1 pfalse2)
    | _, _ ->
        Top


  let widen ~prev ~next ~num_iters =
    match (prev, next) with
    | Latest prev, Latest next ->
        Latest (PrunePairs.widen ~prev ~next ~num_iters)
    | TrueBranch (pvar, prev), TrueBranch (pvar', next) when Pvar.equal pvar pvar' ->
        TrueBranch (pvar, PrunePairs.widen ~prev ~next ~num_iters)
    | FalseBranch (pvar, prev), FalseBranch (pvar', next) when Pvar.equal pvar pvar' ->
        FalseBranch (pvar, PrunePairs.widen ~prev ~next ~num_iters)
    | V (pvar, prev_true, prev_false), V (pvar', next_true, next_false) when Pvar.equal pvar pvar'
      ->
        V
          ( pvar
          , PrunePairs.widen ~prev:prev_true ~next:next_true ~num_iters
          , PrunePairs.widen ~prev:prev_false ~next:next_false ~num_iters )
    | VRet (id, prev_true, prev_false), VRet (id', next_true, next_false) when Ident.equal id id' ->
        VRet
          ( id
          , PrunePairs.widen ~prev:prev_true ~next:next_true ~num_iters
          , PrunePairs.widen ~prev:prev_false ~next:next_false ~num_iters )
    | _, _ ->
        Top


  let top = Top

  let is_top = function Top -> true | _ -> false

  let forget locs =
    let is_mem_locs x = PowLoc.mem (Loc.of_pvar x) locs in
    function
    | Latest p ->
        Latest (PrunePairs.forget locs p)
    | TrueBranch (x, p) ->
        if is_mem_locs x then Top else TrueBranch (x, PrunePairs.forget locs p)
    | FalseBranch (x, p) ->
        if is_mem_locs x then Top else FalseBranch (x, PrunePairs.forget locs p)
    | V (x, ptrue, pfalse) ->
        if is_mem_locs x then Top
        else V (x, PrunePairs.forget locs ptrue, PrunePairs.forget locs pfalse)
    | VRet (x, ptrue, pfalse) ->
        VRet (x, PrunePairs.forget locs ptrue, PrunePairs.forget locs pfalse)
    | Top ->
        Top


  let replace ~from ~to_ x =
    match x with
    | TrueBranch (x, p) when Pvar.equal x from ->
        TrueBranch (to_, p)
    | FalseBranch (x, p) when Pvar.equal x from ->
        FalseBranch (to_, p)
    | V (x, ptrue, pfalse) when Pvar.equal x from ->
        V (to_, ptrue, pfalse)
    | _ ->
        x


  let subst ~ret_id ({eval_locpath} as eval_sym_trace) location =
    let open Result.Monad_infix in
    let subst_pvar x =
      match PowLoc.is_singleton_or_more (PowLoc.subst_loc (Loc.of_pvar x) eval_locpath) with
      | Empty ->
          Error `SubstBottom
      | Singleton (BoField.Prim (Loc.Var (Var.ProgramVar x'))) ->
          Ok x'
      | Singleton _ | More ->
          Error `SubstFail
    in
    function
    | Latest p ->
        PrunePairs.subst p eval_sym_trace location >>| fun p' -> Latest p'
    | TrueBranch (x, p) ->
        subst_pvar x
        >>= fun x' -> PrunePairs.subst p eval_sym_trace location >>| fun p' -> TrueBranch (x', p')
    | FalseBranch (x, p) ->
        subst_pvar x
        >>= fun x' -> PrunePairs.subst p eval_sym_trace location >>| fun p' -> FalseBranch (x', p')
    | V (x, ptrue, pfalse) when Pvar.is_return x ->
        PrunePairs.subst ptrue eval_sym_trace location
        >>= fun ptrue' ->
        PrunePairs.subst pfalse eval_sym_trace location
        >>| fun pfalse' -> VRet (ret_id, ptrue', pfalse')
    | V (x, ptrue, pfalse) ->
        subst_pvar x
        >>= fun x' ->
        PrunePairs.subst ptrue eval_sym_trace location
        >>= fun ptrue' ->
        PrunePairs.subst pfalse eval_sym_trace location >>| fun pfalse' -> V (x', ptrue', pfalse')
    | VRet _ | Top ->
        Ok Top
end

module Reachability = struct
  module M = PrettyPrintable.MakePPSet (PrunedVal)

  type t = M.t

  let equal = M.equal

  let pp = M.pp

  (* It keeps only symbolic pruned values, because non-symbolic ones are useless to see the
     reachability. *)
  let add v x = if PrunedVal.is_symbolic v then M.add v x else x

  let of_latest_prune latest_prune =
    let of_prune_pairs p = PrunePairs.fold (fun _ v acc -> add v acc) p M.empty in
    match latest_prune with
    | LatestPrune.Latest p | LatestPrune.TrueBranch (_, p) | LatestPrune.FalseBranch (_, p) ->
        of_prune_pairs p
    | LatestPrune.V (_, ptrue, pfalse) | LatestPrune.VRet (_, ptrue, pfalse) ->
        M.inter (of_prune_pairs ptrue) (of_prune_pairs pfalse)
    | LatestPrune.Top ->
        M.empty


  let make latest_prune = of_latest_prune latest_prune

  let add_latest_prune latest_prune x = M.union x (of_latest_prune latest_prune)

  let subst x eval_sym_trace location =
    let exception Unreachable in
    let subst1 x acc =
      let v = PrunedVal.subst x eval_sym_trace location in
      if PrunedVal.is_bot v then raise Unreachable else add v acc
    in
    match M.fold subst1 x M.empty with x -> `Reachable x | exception Unreachable -> `Unreachable
end

module MemReach = struct
  type 'has_oenv t0 =
    { stack_locs: StackLocs.t
    ; mem_pure: MemPure.t
    ; alias: Alias.t
    ; latest_prune: LatestPrune.t
    ; oenv: ('has_oenv, OndemandEnv.t) GOption.t
    ; find_global_array: ('has_oenv, Loc.t -> Val.t option) GOption.t }

  type no_oenv_t = GOption.none t0

  type t = GOption.some t0

  let leq ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      StackLocs.leq ~lhs:lhs.stack_locs ~rhs:rhs.stack_locs
      && MemPure.leq ~lhs:lhs.mem_pure ~rhs:rhs.mem_pure
      && Alias.leq ~lhs:lhs.alias ~rhs:rhs.alias
      && LatestPrune.leq ~lhs:lhs.latest_prune ~rhs:rhs.latest_prune


  let widen ~prev ~next ~num_iters =
    if phys_equal prev next then prev
    else (
      assert (phys_equal prev.oenv next.oenv) ;
      let oenv = GOption.value prev.oenv in
      { stack_locs= StackLocs.widen ~prev:prev.stack_locs ~next:next.stack_locs ~num_iters
      ; mem_pure= MemPure.widen oenv ~prev:prev.mem_pure ~next:next.mem_pure ~num_iters
      ; alias= Alias.widen ~prev:prev.alias ~next:next.alias ~num_iters
      ; latest_prune= LatestPrune.widen ~prev:prev.latest_prune ~next:next.latest_prune ~num_iters
      ; oenv= prev.oenv
      ; find_global_array= prev.find_global_array } )


  let join : t -> t -> t =
   fun x y ->
    assert (phys_equal x.oenv y.oenv) ;
    let oenv = GOption.value x.oenv in
    { stack_locs= StackLocs.join x.stack_locs y.stack_locs
    ; mem_pure= MemPure.join oenv x.mem_pure y.mem_pure
    ; alias= Alias.join x.alias y.alias
    ; latest_prune= LatestPrune.join x.latest_prune y.latest_prune
    ; oenv= x.oenv
    ; find_global_array= x.find_global_array }


  let pp : F.formatter -> _ t0 -> unit =
   fun fmt x ->
    F.fprintf fmt "StackLocs:@;%a@;MemPure:@;%a@;Alias:@;%a@;%a" StackLocs.pp x.stack_locs
      MemPure.pp x.mem_pure Alias.pp x.alias LatestPrune.pp x.latest_prune


  let unset_oenv : t -> no_oenv_t =
   fun x -> {x with oenv= GOption.GNone; find_global_array= GOption.GNone}


  let is_stack_loc : Loc.t -> _ t0 -> bool = fun l m -> StackLocs.mem l m.stack_locs

  let is_rep_multi_loc : Loc.t -> _ t0 -> bool = fun l m -> MemPure.is_rep_multi_loc l m.mem_pure

  let find_opt : Loc.t -> _ t0 -> Val.t option = fun l m -> MemPure.find_opt l m.mem_pure

  let find_stack : Loc.t -> _ t0 -> Val.t = fun l m -> Option.value (find_opt l m) ~default:Val.bot

  (** Find heap values from: (1) given abstract memory [m] (2) initializers of global array values
      (3) otherwise, it generates symbolic or unknown values ondemand *)
  let find_heap_default : default:Val.t -> ?typ:Typ.t -> Loc.t -> _ t0 -> Val.t =
   fun ~default ?typ l m ->
    let ondemand_fallback () =
      GOption.value_map m.oenv ~default ~f:(fun oenv -> Val.on_demand ~default ?typ oenv l)
    in
    IOption.value_default_f (find_opt l m) ~f:(fun () ->
        GOption.value_map_f m.find_global_array ~default:ondemand_fallback
          ~f:(fun find_global_array ->
            IOption.value_default_f (find_global_array l) ~f:ondemand_fallback ) )


  let find_heap : ?typ:Typ.t -> Loc.t -> _ t0 -> Val.t =
   fun ?typ l m ->
    find_heap_default
      ~default:(if Config.bo_bottom_as_default then Val.Itv.top else Val.unknown)
      ?typ l m


  let find : ?typ:Typ.t -> Loc.t -> _ t0 -> Val.t =
   fun ?typ l m -> if is_stack_loc l m then find_stack l m else find_heap ?typ l m


  let find_set : ?typ:Typ.t -> PowLoc.t -> _ t0 -> Val.t =
   fun ?typ locs m ->
    let find_join loc acc = Val.join acc (find ?typ loc m) in
    PowLoc.fold find_join locs Val.bot


  let find_alias_id : Ident.t -> _ t0 -> AliasTargets.t = fun k m -> Alias.find_id k m.alias

  let find_alias_loc : Loc.t -> _ t0 -> AliasTargets.t = fun k m -> Alias.find_loc k m.alias

  let find_simple_alias : Ident.t -> _ t0 -> (Loc.t * IntLit.t) list =
    let accum_simple_alias l tgt acc =
      match tgt with AliasTarget.Simple {i} -> (l, i) :: acc | _ -> acc
    in
    fun k m -> AliasTargets.fold accum_simple_alias (Alias.find_id k m.alias) []


  let find_size_alias :
      Ident.t -> _ t0 -> (AliasTarget.alias_typ * Loc.t * IntLit.t * Loc.t option) list =
    let accum_size_alias l tgt acc =
      match tgt with
      | AliasTarget.Size {alias_typ; i; java_tmp} ->
          (alias_typ, l, i, java_tmp) :: acc
      | _ ->
          acc
    in
    fun k m -> AliasTargets.fold accum_size_alias (Alias.find_id k m.alias) []


  let find_ret_alias : _ t0 -> AliasTargets.t = fun m -> Alias.find_ret m.alias

  type get_summary = Procname.t -> no_oenv_t option

  let init : get_summary -> OndemandEnv.t -> t =
   fun get_summary oenv ->
    let find_global_array loc =
      let open IOption.Let_syntax in
      let* pname = Loc.get_global_array_initializer loc in
      let* m = get_summary pname in
      match loc with
      | BoField.Field {prefix; fn} when Loc.is_global prefix ->
          (* This case handles field access of global array:
             n$0 = *x[n].field *)
          let+ v = find_opt prefix m in
          let locs = Val.get_all_locs v |> PowLoc.append_field ~fn in
          find_set locs m
      | _ ->
          find_opt loc m
    in
    { stack_locs= StackLocs.bot
    ; mem_pure= MemPure.bot
    ; alias= Alias.init
    ; latest_prune= LatestPrune.top
    ; oenv= GOption.GSome oenv
    ; find_global_array= GOption.GSome find_global_array }


  let load_alias : Ident.t -> Loc.t -> AliasTarget.t -> t -> t =
   fun id loc tgt m -> {m with alias= Alias.load id loc tgt m.alias}


  let store_simple_alias : Loc.t -> Exp.t -> t -> t =
   fun loc e m ->
    match e with
    | Exp.Const (Const.Cint i) when IntLit.iszero i || IntLit.isone i ->
        let arr_locs =
          let add_arr l v acc =
            let size = Val.array_sizeof v in
            if Itv.is_zero size then (l, IntLit.zero) :: acc
            else if Itv.is_one size then (l, IntLit.one) :: acc
            else acc
          in
          MemPure.fold add_arr m.mem_pure []
        in
        {m with alias= Alias.add_size_alias loc i arr_locs m.alias}
    | _ ->
        {m with alias= Alias.store_simple loc e m.alias}


  let fgets_alias : Ident.t -> PowLoc.t -> t -> t =
   fun id locs m -> {m with alias= Alias.fgets id locs m.alias}


  let incr_size_alias locs m = {m with alias= Alias.incr_size_alias locs m.alias}

  let incr_or_not_size_alias locs m = {m with alias= Alias.incr_or_not_size_alias locs m.alias}

  let add_iterator_alias_common ~cond ~alias_add id m =
    let locs =
      let accum_loc l v acc = if cond v then PowLoc.add l acc else acc in
      MemPure.fold accum_loc m.mem_pure PowLoc.bot
    in
    {m with alias= alias_add id locs m.alias}


  let add_iterator_simple_alias =
    add_iterator_alias_common
      ~cond:(fun v -> Itv.is_zero (Val.get_itv v))
      ~alias_add:Alias.add_iterator_simple_alias


  let add_iterator_offset_alias =
    add_iterator_alias_common
      ~cond:(fun v -> Itv.is_zero (Val.array_sizeof v))
      ~alias_add:Alias.add_iterator_offset_alias


  let add_iterator_alias id m = add_iterator_offset_alias id m |> add_iterator_simple_alias id

  let incr_iterator_offset_alias id m = {m with alias= Alias.incr_iterator_offset_alias id m.alias}

  let add_iterator_has_next_alias ~ret_id ~iterator m =
    {m with alias= Alias.add_iterator_has_next_alias ~ret_id ~iterator m.alias}


  let add_iterator_next_object_alias ~ret_id ~iterator m =
    {m with alias= Alias.add_iterator_next_object_alias ~ret_id ~iterator m.alias}


  let incr_iterator_simple_alias_on_call eval_sym_trace ~callee_exit_mem m =
    let callee_locs = MemPure.get_incr_locs callee_exit_mem.mem_pure in
    {m with alias= Alias.incr_iterator_simple_alias_on_call eval_sym_trace ~callee_locs m.alias}


  let add_stack_loc : Loc.t -> t -> t = fun k m -> {m with stack_locs= StackLocs.add k m.stack_locs}

  let add_stack : ?represents_multiple_values:bool -> Loc.t -> Val.t -> t -> t =
   fun ?represents_multiple_values k v m ->
    { m with
      stack_locs= StackLocs.add k m.stack_locs
    ; mem_pure= MemPure.add ?represents_multiple_values k v m.mem_pure }


  let replace_stack : Loc.t -> Val.t -> t -> t =
   fun k v m -> {m with mem_pure= MemPure.add k v m.mem_pure}


  let strong_update_heap : Loc.t -> Val.t -> t -> t =
   fun x v m ->
    if Loc.is_unknown x && not Config.bo_bottom_as_default then m
    else {m with mem_pure= MemPure.strong_update x v m.mem_pure}


  let add_heap : ?represents_multiple_values:bool -> Loc.t -> Val.t -> t -> t =
   fun ?represents_multiple_values x v m ->
    if Loc.is_unknown x && not Config.bo_bottom_as_default then m
    else {m with mem_pure= MemPure.add ?represents_multiple_values x v m.mem_pure}


  let add_heap_set : ?represents_multiple_values:bool -> PowLoc.t -> Val.t -> t -> t =
   fun ?represents_multiple_values locs v m ->
    PowLoc.fold (fun l acc -> add_heap ?represents_multiple_values l v acc) locs m


  let add_unknown_from :
      Ident.t * Typ.t -> callee_pname:Procname.t option -> location:Location.t -> t -> t =
   fun (id, typ) ~callee_pname ~location m ->
    let val_unknown = Val.unknown_from typ ~callee_pname ~location in
    add_stack (Loc.of_id id) val_unknown m |> add_heap Loc.unknown val_unknown


  let strong_update : PowLoc.t -> Val.t -> t -> t =
   fun locs v m ->
    let strong_update1 l m =
      if is_stack_loc l m then replace_stack l v m else strong_update_heap l v m
    in
    PowLoc.fold strong_update1 locs m


  let transformi_mem : f:(Loc.t -> Val.t -> Val.t) -> PowLoc.t -> t -> t =
   fun ~f locs m ->
    let transform_mem1 l m =
      let add, find =
        if is_stack_loc l m then (replace_stack, find_stack)
        else
          ( add_heap ~represents_multiple_values:false
          , find_heap_default ~default:Val.default ?typ:None )
      in
      add l (f l (find l m)) m
    in
    PowLoc.fold transform_mem1 locs m


  let transform_mem : f:(Val.t -> Val.t) -> PowLoc.t -> t -> t =
   fun ~f -> transformi_mem ~f:(fun _ v -> f v)


  (* TODO: it would probably make sense to use bot as a default value if locs contains a
     single location that contain a star field (such location represents multiple values
     but as opposed to a location representing all elements of an array, it would probably
     make sense if their first of such location in a subprogram would be strong).*)
  let weak_update locs v m = transform_mem ~f:(fun v' -> Val.join v' v) locs m

  let update_mem : ?force_strong_update:Bool.t -> PowLoc.t -> Val.t -> t -> t =
   fun ?(force_strong_update = false) ploc v s ->
    if force_strong_update || can_strong_update ploc then strong_update ploc v s
    else (
      L.d_printfln_escaped "Weak update for %a <- %a" PowLoc.pp ploc Val.pp v ;
      weak_update ploc v s )


  let set_prune_pairs : PrunePairs.t -> t -> t =
   fun prune_pairs m -> {m with latest_prune= LatestPrune.Latest prune_pairs}


  let apply_latest_prune : Exp.t -> t -> t * PrunePairs.t =
    let apply_prunes prunes m =
      let apply1 l v acc = update_mem (PowLoc.singleton l) (PrunedVal.get_val v) acc in
      PrunePairs.fold apply1 prunes m
    in
    fun e m ->
      match (m.latest_prune, e) with
      | LatestPrune.V (x, prunes, _), Exp.Var r
      | LatestPrune.V (x, _, prunes), Exp.UnOp (Unop.LNot, Exp.Var r, _) ->
          let pruned_val_meet _rhs v1 _v2 =
            (* NOTE: We need the pruned values, but for now we don't have the meet operation on
               value. *)
            Some v1
          in
          let apply_simple_alias1 ((m_acc, prunes_acc) as acc) = function
            | BoField.Prim (Loc.Var (Var.ProgramVar y)), i when Pvar.equal x y && IntLit.iszero i ->
                (apply_prunes prunes m_acc, PrunePairs.union pruned_val_meet prunes_acc prunes)
            | _ ->
                acc
          in
          List.fold (find_simple_alias r m) ~init:(m, PrunePairs.empty) ~f:apply_simple_alias1
      | LatestPrune.VRet (x, prunes, _), Exp.Var r
      | LatestPrune.VRet (x, _, prunes), Exp.UnOp (Unop.LNot, Exp.Var r, _) ->
          if Ident.equal x r then (apply_prunes prunes m, prunes) else (m, PrunePairs.empty)
      | _ ->
          (m, PrunePairs.empty)


  let update_latest_prune : updated_locs:PowLoc.t -> Exp.t -> Exp.t -> t -> t =
   fun ~updated_locs e1 e2 m ->
    match (e1, e2, m.latest_prune) with
    | Lvar x, Const (Const.Cint i), LatestPrune.Latest p ->
        if IntLit.isone i then {m with latest_prune= LatestPrune.TrueBranch (x, p)}
        else if IntLit.iszero i then {m with latest_prune= LatestPrune.FalseBranch (x, p)}
        else {m with latest_prune= LatestPrune.forget updated_locs m.latest_prune}
    | Lvar return, _, _ when Pvar.is_return return ->
        let tgts = Alias.find_ret m.alias in
        let replace_latest_prune l tgt acc =
          match (l, tgt) with
          | BoField.Prim (Loc.Var (ProgramVar pvar)), AliasTarget.Simple {i} when IntLit.iszero i ->
              {acc with latest_prune= LatestPrune.replace ~from:pvar ~to_:return m.latest_prune}
          | _ ->
              acc
        in
        AliasTargets.fold replace_latest_prune tgts m
    | _, _, _ ->
        {m with latest_prune= LatestPrune.forget updated_locs m.latest_prune}


  let get_latest_prune : _ t0 -> LatestPrune.t = fun {latest_prune} -> latest_prune

  let set_latest_prune : LatestPrune.t -> t -> t = fun latest_prune x -> {x with latest_prune}

  (** Get set of locations containing the input locations locs and all the locations reachable from
      these input locations. If a location represents a record, expand it to all its direct and
      indirect fields. That is, a location representing a record field "X1..XN.F" is reachable if
      there exists I in 1 .. 2 for which the location "X1..XI" is either an input location or it is
      reachable. If expand_ptrs_arrs is true, follow pointers and arrays. *)
  let expand_reachable_locs : locs:LocSet.t -> expand_ptrs_arrs:bool -> _ t0 -> LocSet.t =
   fun ~locs ~expand_ptrs_arrs m ->
    let add_reachable1 ~root loc v acc =
      if Loc.equal root loc then
        if expand_ptrs_arrs then LocSet.union acc (Val.get_all_locs v |> PowLoc.to_set) else acc
      else if Loc.is_trans_field_of ~loc:root ~field_loc:loc then LocSet.add loc acc
      else acc
    in
    let rec add_from_locs heap locs acc = LocSet.fold (add_from_loc heap) locs acc
    and add_from_loc heap loc acc =
      if LocSet.mem loc acc then acc
      else
        let reachable_locs = MemPure.fold (add_reachable1 ~root:loc) heap LocSet.empty in
        add_from_locs heap reachable_locs (LocSet.add loc acc)
    in
    add_from_locs m.mem_pure locs LocSet.empty


  let get_reachable_locs_from_aux : f:(Pvar.t -> bool) -> LocSet.t -> _ t0 -> LocSet.t =
    let add_param_locs ~f mem acc =
      let add_loc loc _ acc = if Loc.exists_pvar ~f loc then LocSet.add loc acc else acc in
      MemPure.fold add_loc mem acc
    in
    fun ~f locs m ->
      let locs = add_param_locs ~f m.mem_pure locs in
      expand_reachable_locs ~locs ~expand_ptrs_arrs:true m


  let get_reachable_locs_from : (Pvar.t * Typ.t) list -> LocSet.t -> _ t0 -> LocSet.t =
   fun formals locs m ->
    let is_formal pvar = List.exists formals ~f:(fun (formal, _) -> Pvar.equal pvar formal) in
    get_reachable_locs_from_aux ~f:is_formal locs m


  let range :
         filter_loc:(Loc.t -> LoopHeadLoc.t option)
      -> node_id:Procdesc.Node.id
      -> t
      -> Polynomials.NonNegativePolynomial.t =
   fun ~filter_loc ~node_id {mem_pure} -> MemPure.range ~filter_loc ~node_id mem_pure


  let forget_unreachable_locs : formals:(Pvar.t * Typ.t) list -> t -> t =
   fun ~formals m ->
    let is_reachable =
      let reachable_locs =
        let f pvar =
          Pvar.is_return pvar || Pvar.is_global pvar
          || List.exists formals ~f:(fun (formal, _) -> Pvar.equal formal pvar)
        in
        get_reachable_locs_from_aux ~f LocSet.empty m
      in
      fun l -> LocSet.mem l reachable_locs
    in
    let stack_locs = StackLocs.filter is_reachable m.stack_locs in
    let mem_pure = MemPure.filter (fun l _ -> is_reachable l) m.mem_pure in
    {m with stack_locs; mem_pure}


  let forget_size_alias arr_locs m = {m with alias= Alias.forget_size_alias arr_locs m.alias}

  let remove_vars : Var.t list -> t -> t =
   fun vars m ->
    let remove l key m =
      { m with
        stack_locs= StackLocs.remove l m.stack_locs
      ; mem_pure= MemPure.remove l m.mem_pure
      ; alias= Alias.remove_key key m.alias }
    in
    List.fold vars ~init:m ~f:(fun m var ->
        match (var : Var.t) with
        | ProgramVar pvar ->
            if Config.bo_exit_frontend_gener_vars && Pvar.is_frontend_tmp pvar then
              let locs =
                expand_reachable_locs
                  ~locs:(LocSet.singleton (Loc.of_pvar pvar))
                  ~expand_ptrs_arrs:false m
              in
              LocSet.fold (fun l m -> remove l (KeyLhs.of_loc l) m) locs m
            else m
        | LogicalVar id ->
            remove (Loc.of_id id) (KeyLhs.of_id id) m )


  (* unsound *)
  let set_first_idx_of_null : Loc.t -> Val.t -> t -> t =
   fun loc idx m -> update_mem (PowLoc.singleton (Loc.of_c_strlen loc)) idx m


  (* unsound *)
  let unset_first_idx_of_null : Loc.t -> Val.t -> t -> t =
   fun loc idx m ->
    let old_c_strlen = find_heap (Loc.of_c_strlen loc) m in
    let idx_itv = Val.get_itv idx in
    if Boolean.is_true (Itv.lt_sem idx_itv (Val.get_itv old_c_strlen)) then m
    else
      let new_c_strlen = Val.of_itv ~traces:(Val.get_traces idx) (Itv.incr idx_itv) in
      set_first_idx_of_null loc new_c_strlen m


  let add_cpp_iter_begin_alias pvar m = {m with alias= Alias.add_cpp_iter_begin_alias pvar m.alias}

  let add_cpp_iter_end_alias pvar m = {m with alias= Alias.add_cpp_iter_end_alias pvar m.alias}

  let find_cpp_iterator_alias id m = Alias.find_cpp_iterator_alias id m.alias

  let find_cpp_iter_begin_or_end_alias id m = Alias.find_cpp_iter_begin_or_end_alias id m.alias

  let add_cpp_iterator_cmp_alias id ~iter_lhs ~iter_rhs m =
    let open IOption.Let_syntax in
    (let* begin_or_end_iter_v = find_cpp_iter_begin_or_end_alias iter_rhs m in
     let+ begin_or_end = CppIterBeginOrEndValue.get begin_or_end_iter_v in
     {m with alias= Alias.add_cpp_iterator_cmp_alias id ~iter_lhs ~iter_rhs begin_or_end m.alias} )
    |> Option.value ~default:m


  let propagate_cpp_iter_begin_or_end_alias ~new_pvar ~existing_pvar m =
    let open IOption.Let_syntax in
    (let* begin_or_end_iter_v = find_cpp_iter_begin_or_end_alias existing_pvar m in
     let+ begin_or_end = CppIterBeginOrEndValue.get begin_or_end_iter_v in
     match (begin_or_end : CppIterBeginOrEndKind.t) with
     | Begin ->
         add_cpp_iter_begin_alias new_pvar m
     | End ->
         add_cpp_iter_end_alias new_pvar m )
    |> Option.value ~default:m
end

module Mem = struct
  type 'has_oenv t0 = Unreachable | ExcRaised | Reachable of 'has_oenv MemReach.t0

  type no_oenv_t = GOption.none t0

  type t = GOption.some t0

  let unreachable : t = Unreachable

  let is_reachable = function Reachable _ -> true | _ -> false

  let exc_raised : t = ExcRaised

  let leq ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      match (lhs, rhs) with
      | Unreachable, _ ->
          true
      | _, Unreachable ->
          false
      | ExcRaised, _ ->
          true
      | _, ExcRaised ->
          false
      | Reachable lhs, Reachable rhs ->
          MemReach.leq ~lhs ~rhs


  let join x y =
    if phys_equal x y then x
    else
      match (x, y) with
      | Unreachable, m | m, Unreachable ->
          m
      | ExcRaised, m | m, ExcRaised ->
          m
      | Reachable m1, Reachable m2 ->
          PhysEqual.optim2 ~res:(Reachable (MemReach.join m1 m2)) x y


  let widen ~prev:prev0 ~next:next0 ~num_iters =
    if phys_equal prev0 next0 then prev0
    else
      match (prev0, next0) with
      | Unreachable, m | m, Unreachable ->
          m
      | ExcRaised, m | m, ExcRaised ->
          m
      | Reachable prev, Reachable next ->
          PhysEqual.optim2 ~res:(Reachable (MemReach.widen ~prev ~next ~num_iters)) prev0 next0


  let map ~f x =
    match x with
    | Unreachable | ExcRaised ->
        x
    | Reachable m ->
        let m' = f m in
        if phys_equal m' m then x else Reachable m'


  type get_summary = Procname.t -> no_oenv_t option

  let init : get_summary -> OndemandEnv.t -> t =
   fun get_summary oenv ->
    let get_summary pname =
      match get_summary pname with Some (Reachable m) -> Some m | _ -> None
    in
    Reachable (MemReach.init get_summary oenv)


  let f_lift_default : default:'a -> ('h MemReach.t0 -> 'a) -> 'h t0 -> 'a =
   fun ~default f m -> match m with Unreachable | ExcRaised -> default | Reachable m' -> f m'


  let is_stack_loc : Loc.t -> _ t0 -> bool =
   fun k -> f_lift_default ~default:false (MemReach.is_stack_loc k)


  let is_rep_multi_loc : Loc.t -> _ t0 -> bool =
   fun k -> f_lift_default ~default:false (MemReach.is_rep_multi_loc k)


  let find : Loc.t -> _ t0 -> Val.t = fun k -> f_lift_default ~default:Val.default (MemReach.find k)

  let find_stack : Loc.t -> _ t0 -> Val.t =
   fun k -> f_lift_default ~default:Val.bot (MemReach.find_stack k)


  let find_set : ?typ:Typ.t -> PowLoc.t -> _ t0 -> Val.t =
   fun ?typ k -> f_lift_default ~default:Val.bot (MemReach.find_set ?typ k)


  let find_opt : Loc.t -> _ t0 -> Val.t option =
   fun k -> f_lift_default ~default:None (MemReach.find_opt k)


  let find_alias_id : Ident.t -> _ t0 -> AliasTargets.t =
   fun k -> f_lift_default ~default:AliasTargets.empty (MemReach.find_alias_id k)


  let find_alias_loc : Loc.t -> _ t0 -> AliasTargets.t =
   fun k -> f_lift_default ~default:AliasTargets.empty (MemReach.find_alias_loc k)


  let find_simple_alias : Ident.t -> _ t0 -> (Loc.t * IntLit.t) list =
   fun k -> f_lift_default ~default:[] (MemReach.find_simple_alias k)


  let find_size_alias :
      Ident.t -> _ t0 -> (AliasTarget.alias_typ * Loc.t * IntLit.t * Loc.t option) list =
   fun k -> f_lift_default ~default:[] (MemReach.find_size_alias k)


  let find_ret_alias : _ t0 -> AliasTargets.t bottom_lifted =
   fun m ->
    match m with
    | Unreachable | ExcRaised ->
        Bottom
    | Reachable m' ->
        NonBottom (MemReach.find_ret_alias m')


  let load_alias : Ident.t -> Loc.t -> AliasTarget.t -> t -> t =
   fun id loc tgt -> map ~f:(MemReach.load_alias id loc tgt)


  let load_simple_alias : Ident.t -> Loc.t -> t -> t =
   fun id loc -> load_alias id loc (AliasTarget.Simple {i= IntLit.zero; java_tmp= None})


  let load_empty_alias : Ident.t -> Loc.t -> t -> t =
   fun id loc -> load_alias id loc AliasTarget.Empty


  let load_size_alias : Ident.t -> Loc.t -> t -> t =
   fun id loc -> load_alias id loc (AliasTarget.Size {alias_typ= Eq; i= IntLit.zero; java_tmp= None})


  let store_simple_alias : Loc.t -> Exp.t -> t -> t =
   fun loc e -> map ~f:(MemReach.store_simple_alias loc e)


  let fgets_alias : Ident.t -> PowLoc.t -> t -> t =
   fun id locs -> map ~f:(MemReach.fgets_alias id locs)


  let incr_size_alias locs = map ~f:(MemReach.incr_size_alias locs)

  let incr_or_not_size_alias locs = map ~f:(MemReach.incr_or_not_size_alias locs)

  let add_iterator_alias : Ident.t -> t -> t = fun id -> map ~f:(MemReach.add_iterator_alias id)

  let incr_iterator_offset_alias : Exp.t -> t -> t =
   fun iterator m ->
    match iterator with Exp.Var id -> map ~f:(MemReach.incr_iterator_offset_alias id) m | _ -> m


  let add_iterator_has_next_alias : Ident.t -> Exp.t -> t -> t =
   fun ret_id iterator m ->
    match iterator with
    | Exp.Var iterator ->
        map ~f:(MemReach.add_iterator_has_next_alias ~ret_id ~iterator) m
    | _ ->
        m


  let add_iterator_next_object_alias : ret_id:Ident.t -> iterator:Ident.t -> t -> t =
   fun ~ret_id ~iterator m -> map ~f:(MemReach.add_iterator_next_object_alias ~ret_id ~iterator) m


  let incr_iterator_simple_alias_on_call eval_sym_trace ~callee_exit_mem m =
    match (callee_exit_mem, m) with
    | Reachable callee_exit_mem, Reachable m ->
        Reachable (MemReach.incr_iterator_simple_alias_on_call eval_sym_trace ~callee_exit_mem m)
    | _, _ ->
        m


  let add_stack_loc : Loc.t -> t -> t = fun k -> map ~f:(MemReach.add_stack_loc k)

  let add_stack : ?represents_multiple_values:bool -> Loc.t -> Val.t -> t -> t =
   fun ?represents_multiple_values k v ->
    map ~f:(MemReach.add_stack ?represents_multiple_values k v)


  let add_heap : ?represents_multiple_values:bool -> Loc.t -> Val.t -> t -> t =
   fun ?represents_multiple_values k v -> map ~f:(MemReach.add_heap ?represents_multiple_values k v)


  let add_heap_set : ?represents_multiple_values:bool -> PowLoc.t -> Val.t -> t -> t =
   fun ?represents_multiple_values ploc v ->
    map ~f:(MemReach.add_heap_set ?represents_multiple_values ploc v)


  let add_unknown_from : Ident.t * Typ.t -> callee_pname:Procname.t -> location:Location.t -> t -> t
      =
   fun ret ~callee_pname ~location ->
    map ~f:(MemReach.add_unknown_from ret ~callee_pname:(Some callee_pname) ~location)


  let add_unknown : Ident.t * Typ.t -> location:Location.t -> t -> t =
   fun ret ~location -> map ~f:(MemReach.add_unknown_from ret ~callee_pname:None ~location)


  let strong_update : PowLoc.t -> Val.t -> t -> t = fun p v -> map ~f:(MemReach.strong_update p v)

  let get_reachable_locs_from : (Pvar.t * Typ.t) list -> LocSet.t -> _ t0 -> LocSet.t =
   fun formals locs ->
    f_lift_default ~default:LocSet.empty (MemReach.get_reachable_locs_from formals locs)


  let update_mem : ?force_strong_update:Bool.t -> PowLoc.t -> Val.t -> t -> t =
   fun ?(force_strong_update = false) ploc v ->
    map ~f:(MemReach.update_mem ~force_strong_update ploc v)


  let transform_mem : f:(Val.t -> Val.t) -> PowLoc.t -> t -> t =
   fun ~f ploc -> map ~f:(MemReach.transform_mem ~f ploc)


  let remove_vars : Var.t list -> t -> t = fun vars -> map ~f:(MemReach.remove_vars vars)

  let set_prune_pairs : PrunePairs.t -> t -> t =
   fun prune_pairs -> map ~f:(MemReach.set_prune_pairs prune_pairs)


  let apply_latest_prune : Exp.t -> t -> t * PrunePairs.t =
   fun e -> function
    | (Unreachable | ExcRaised) as x ->
        (x, PrunePairs.empty)
    | Reachable m ->
        let m, prune_pairs = MemReach.apply_latest_prune e m in
        (Reachable m, prune_pairs)


  let update_latest_prune : updated_locs:PowLoc.t -> Exp.t -> Exp.t -> t -> t =
   fun ~updated_locs e1 e2 -> map ~f:(MemReach.update_latest_prune ~updated_locs e1 e2)


  let get_latest_prune : _ t0 -> LatestPrune.t =
   fun m -> f_lift_default ~default:LatestPrune.Top MemReach.get_latest_prune m


  let set_latest_prune : LatestPrune.t -> t -> t =
   fun latest_prune m -> map ~f:(MemReach.set_latest_prune latest_prune) m


  let forget_unreachable_locs : formals:(Pvar.t * Typ.t) list -> t -> t =
   fun ~formals -> map ~f:(MemReach.forget_unreachable_locs ~formals)


  let forget_size_alias arr_locs = map ~f:(MemReach.forget_size_alias arr_locs)

  let unset_oenv = function
    | (Unreachable | ExcRaised) as x ->
        x
    | Reachable m ->
        Reachable (MemReach.unset_oenv m)


  let set_first_idx_of_null loc idx = map ~f:(MemReach.set_first_idx_of_null loc idx)

  let unset_first_idx_of_null loc idx = map ~f:(MemReach.unset_first_idx_of_null loc idx)

  let get_c_strlen locs m =
    let get_c_strlen' loc acc =
      match loc with
      | BoField.Prim (Loc.Allocsite _) ->
          Val.join acc (find (Loc.of_c_strlen loc) m)
      | _ ->
          acc
    in
    PowLoc.fold get_c_strlen' locs Val.bot


  let pp f m =
    match m with
    | Unreachable ->
        F.fprintf f "%s_unreachable" SpecialChars.up_tack
    | ExcRaised ->
        F.pp_print_string f (SpecialChars.up_tack ^ " by exception")
    | Reachable m ->
        MemReach.pp f m


  let find_cpp_iterator_alias id m =
    f_lift_default ~default:None (MemReach.find_cpp_iterator_alias id) m


  let add_cpp_iterator_cmp_alias id ~iter_lhs ~iter_rhs m =
    map m ~f:(MemReach.add_cpp_iterator_cmp_alias id ~iter_lhs ~iter_rhs)


  let add_cpp_iter_begin_alias pvar m = map m ~f:(MemReach.add_cpp_iter_begin_alias pvar)

  let add_cpp_iter_end_alias pvar m = map m ~f:(MemReach.add_cpp_iter_end_alias pvar)

  let propagate_cpp_iter_begin_or_end_alias ~new_pvar ~existing_pvar m =
    map m ~f:(MemReach.propagate_cpp_iter_begin_or_end_alias ~new_pvar ~existing_pvar)
end
