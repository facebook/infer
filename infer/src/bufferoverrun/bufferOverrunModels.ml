(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd
open AbsLoc
open! AbstractDomain.Types
module L = Logging
module Dom = BufferOverrunDomain
module PO = BufferOverrunProofObligations
module Trace = BufferOverrunTrace
module TraceSet = Trace.Set

module Make (BoUtils : BufferOverrunUtils.S) = struct
  module CFG = BoUtils.CFG
  module Sem = BoUtils.Sem

  type exec_fun =
    Typ.Procname.t -> (Ident.t * Typ.t) option -> CFG.node -> Location.t -> Dom.Mem.astate
    -> Dom.Mem.astate

  type check_fun =
    Typ.Procname.t -> CFG.node -> Location.t -> Dom.Mem.astate -> PO.ConditionSet.t
    -> PO.ConditionSet.t

  type model = {exec: exec_fun; check: check_fun}

  type declare_local_fun =
    decl_local:BoUtils.Exec.decl_local -> Typ.Procname.t -> CFG.node -> Location.t -> Loc.t
    -> inst_num:int -> dimension:int -> Dom.Mem.astate -> Dom.Mem.astate * int

  type declare_symbolic_fun =
    decl_sym_val:BoUtils.Exec.decl_sym_val -> Typ.Procname.t -> Tenv.t -> CFG.node -> Location.t
    -> depth:int -> Loc.t -> inst_num:int -> new_sym_num:BoUtils.counter
    -> new_alloc_num:BoUtils.counter -> Dom.Mem.astate -> Dom.Mem.astate

  type typ_model = {declare_local: declare_local_fun; declare_symbolic: declare_symbolic_fun}

  let no_check _pname _node _location _mem cond_set = cond_set

  (* NOTE: heuristic *)
  let get_malloc_info : Exp.t -> Typ.t * Int.t option * Exp.t = function
    | Exp.BinOp (Binop.Mult, Exp.Sizeof {typ; nbytes}, length)
    | Exp.BinOp (Binop.Mult, length, Exp.Sizeof {typ; nbytes}) ->
        (typ, nbytes, length)
    | Exp.Sizeof {typ; nbytes} ->
        (typ, nbytes, Exp.one)
    | x ->
        (Typ.mk (Typ.Tint Typ.IChar), Some 1, x)


  let check_alloc_size size_exp pname _node location mem cond_set =
    let _, _, length0 = get_malloc_info size_exp in
    let v_length = Sem.eval length0 mem in
    match Dom.Val.get_itv v_length with
    | Bottom ->
        cond_set
    | NonBottom length ->
        let traces = Dom.Val.get_traces v_length in
        PO.ConditionSet.add_alloc_size pname location ~length traces cond_set


  let set_uninitialized node (typ: Typ.t) ploc mem =
    match typ.desc with
    | Tint _ | Tfloat _ ->
        Dom.Mem.weak_update_heap ploc Dom.Val.Itv.top mem
    | _ ->
        L.(debug BufferOverrun Verbose)
          "/!\\ Do not know how to uninitialize type %a at %a@\n" (Typ.pp Pp.text) typ Location.pp
          (CFG.loc node) ;
        mem


  let malloc size_exp =
    let exec pname ret node location mem =
      match ret with
      | Some (id, _) ->
          let typ, stride, length0 = get_malloc_info size_exp in
          let length = Sem.eval length0 mem in
          let traces = TraceSet.add_elem (Trace.ArrDecl location) (Dom.Val.get_traces length) in
          let v =
            Sem.eval_array_alloc pname node typ ?stride Itv.zero (Dom.Val.get_itv length) 0 1
            |> Dom.Val.set_traces traces
          in
          mem |> Dom.Mem.add_stack (Loc.of_id id) v
          |> set_uninitialized node typ (Dom.Val.get_array_locs v)
      | _ ->
          L.(debug BufferOverrun Verbose)
            "/!\\ Do not know where to model malloc at %a@\n" Location.pp (CFG.loc node) ;
          mem
    and check = check_alloc_size size_exp in
    {exec; check}


  let realloc = malloc

  let inferbo_min e1 e2 =
    let exec _pname ret _node _location mem =
      match ret with
      | Some (id, _) ->
          let i1 = Sem.eval e1 mem |> Dom.Val.get_itv in
          let i2 = Sem.eval e2 mem |> Dom.Val.get_itv in
          let v = Itv.min_sem i1 i2 |> Dom.Val.of_itv in
          mem |> Dom.Mem.add_stack (Loc.of_id id) v
      | _ ->
          mem
    in
    {exec; check= no_check}


  let inferbo_set_size e1 e2 =
    let exec _pname _ret _node _location mem =
      let locs = Sem.eval_locs e1 mem |> Dom.Val.get_pow_loc in
      let size = Sem.eval e2 mem |> Dom.Val.get_itv in
      let arr = Dom.Mem.find_heap_set locs mem in
      let arr = Dom.Val.set_array_size size arr in
      Dom.Mem.strong_update_heap locs arr mem
    and check = check_alloc_size e2 in
    {exec; check}


  let model_by_value value _pname ret _node _location mem =
    match ret with
    | Some (id, _) ->
        Dom.Mem.add_stack (Loc.of_id id) value mem
    | None ->
        L.(debug BufferOverrun Verbose)
          "/!\\ Do not know where to model value %a@\n" Dom.Val.pp value ;
        mem


  let by_value value = {exec= model_by_value value; check= no_check}

  let bottom =
    let exec _pname _ret _node _location _mem = Bottom in
    {exec; check= no_check}


  let infer_print e =
    let exec _pname _ret _node location mem =
      L.(debug BufferOverrun Medium)
        "@[<v>=== Infer Print === at %a@,%a@]%!" Location.pp location Dom.Val.pp (Sem.eval e mem) ;
      mem
    in
    {exec; check= no_check}


  let set_array_length array length_exp =
    let exec pname _ret node _location mem =
      match array with
      | Exp.Lvar array_pvar, {Typ.desc= Typ.Tarray (typ, _, stride0)} ->
          let length = Sem.eval length_exp mem |> Dom.Val.get_itv in
          let stride = Option.map ~f:IntLit.to_int stride0 in
          let v = Sem.eval_array_alloc pname node typ ?stride Itv.zero length 0 1 in
          mem |> Dom.Mem.add_stack (Loc.of_pvar array_pvar) v
          |> set_uninitialized node typ (Dom.Val.get_array_locs v)
      | _ ->
          L.(die InternalError) "Unexpected type of first argument for __set_array_length()"
    and check = check_alloc_size length_exp in
    {exec; check}


  module Split = struct
    let std_vector ~adds_at_least_one (vector_exp, vector_typ) location mem =
      let traces = BufferOverrunTrace.(Call location |> singleton |> Set.singleton) in
      let increment_itv = if adds_at_least_one then Itv.pos else Itv.nat in
      let increment = Dom.Val.of_itv ~traces increment_itv in
      let vector_type_name = Option.value_exn (vector_typ |> Typ.strip_ptr |> Typ.name) in
      let size_field = Typ.Fieldname.Clang.from_class_name vector_type_name "infer_size" in
      let vector_size_locs =
        Sem.eval vector_exp mem |> Dom.Val.get_all_locs |> PowLoc.append_field ~fn:size_field
      in
      Dom.Mem.transform_mem ~f:(Dom.Val.plus increment) vector_size_locs mem
  end

  module Boost = struct
    module Split = struct
      let std_vector vector_arg =
        let exec _pname _ret _node location mem =
          Split.std_vector ~adds_at_least_one:true vector_arg location mem
        in
        {exec; check= no_check}
    end
  end

  module Folly = struct
    module Split = struct
      let std_vector vector_arg ignore_empty_opt =
        let exec _pname _ret _node location mem =
          let adds_at_least_one =
            match ignore_empty_opt with
            | Some ignore_empty_exp ->
                Sem.eval ignore_empty_exp mem |> Dom.Val.get_itv |> Itv.is_false
            | None ->
                (* default: ignore_empty is false *)
                true
          in
          Split.std_vector ~adds_at_least_one vector_arg location mem
        in
        {exec; check= no_check}
    end
  end

  module Procname = struct
    let dispatch : model ProcnameDispatcher.dispatcher =
      let open ProcnameDispatcher.Procname in
      make_dispatcher
        [ -"__inferbo_min" <>$ capt_exp $+ capt_exp $!--> inferbo_min
        ; -"__inferbo_set_size" <>$ capt_exp $+ capt_exp $!--> inferbo_set_size
        ; -"__exit" <>--> bottom
        ; -"exit" <>--> bottom
        ; -"fgetc" <>--> by_value Dom.Val.Itv.m1_255
        ; -"infer_print" <>$ capt_exp $!--> infer_print
        ; -"malloc" <>$ capt_exp $+...$--> malloc
        ; -"__new_array" <>$ capt_exp $+...$--> malloc
        ; -"realloc" <>$ any_arg $+ capt_exp $+...$--> realloc
        ; -"__set_array_length" <>$ capt_arg $+ capt_exp $!--> set_array_length
        ; -"strlen" <>--> by_value Dom.Val.Itv.nat
        ; -"boost" &:: "split" $ capt_arg_of_typ (-"std" &:: "vector") $+ any_arg $+ any_arg
          $+? any_arg $--> Boost.Split.std_vector
        ; -"folly" &:: "split" $ any_arg $+ any_arg $+ capt_arg_of_typ (-"std" &:: "vector")
          $+? capt_exp $--> Folly.Split.std_vector ]
  end

  module TypName = struct
    let dispatch : typ_model ProcnameDispatcher.typ_dispatcher =
      ProcnameDispatcher.TypName.make_dispatcher []
  end
end
