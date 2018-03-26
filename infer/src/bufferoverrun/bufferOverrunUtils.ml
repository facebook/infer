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
module Sem = BufferOverrunSemantics
module Trace = BufferOverrunTrace
module TraceSet = Trace.Set

module Exec = struct
  let load_val id val_ mem =
    let locs = val_ |> Dom.Val.get_all_locs in
    let v = Dom.Mem.find_heap_set locs mem in
    let mem = Dom.Mem.add_stack (Loc.of_id id) v mem in
    if PowLoc.is_singleton locs then Dom.Mem.load_simple_alias id (PowLoc.min_elt locs) mem
    else mem


  type decl_local =
    Typ.Procname.t -> node_hash:int -> Location.t -> Loc.t -> Typ.t -> inst_num:int
    -> dimension:int -> Dom.Mem.astate -> Dom.Mem.astate * int

  let decl_local_array
      : decl_local:decl_local -> Typ.Procname.t -> node_hash:int -> Location.t -> Loc.t -> Typ.t
        -> length:IntLit.t option -> ?stride:int -> inst_num:int -> dimension:int -> Dom.Mem.astate
        -> Dom.Mem.astate * int =
   fun ~decl_local pname ~node_hash location loc typ ~length ?stride ~inst_num ~dimension mem ->
    let size = Option.value_map ~default:Itv.top ~f:Itv.of_int_lit length in
    let arr =
      Sem.eval_array_alloc pname ~node_hash typ ~stride ~offset:Itv.zero ~size ~inst_num ~dimension
      |> Dom.Val.add_trace_elem (Trace.ArrDecl location)
    in
    let mem =
      if Int.equal dimension 1 then Dom.Mem.add_stack loc arr mem else Dom.Mem.add_heap loc arr mem
    in
    let loc = Loc.of_allocsite (Sem.get_allocsite pname ~node_hash ~inst_num ~dimension) in
    let mem, _ =
      decl_local pname ~node_hash location loc typ ~inst_num ~dimension:(dimension + 1) mem
    in
    (mem, inst_num + 1)


  type decl_sym_val =
    Typ.Procname.t -> Tenv.t -> node_hash:int -> Location.t -> depth:int -> Loc.t -> Typ.t
    -> Dom.Mem.astate -> Dom.Mem.astate

  let decl_sym_arr
      : decl_sym_val:decl_sym_val -> Typ.Procname.t -> Tenv.t -> node_hash:int -> Location.t
        -> depth:int -> Loc.t -> Typ.t -> ?offset:Itv.t -> ?size:Itv.t -> inst_num:int
        -> new_sym_num:Itv.Counter.t -> new_alloc_num:Itv.Counter.t -> Dom.Mem.astate
        -> Dom.Mem.astate =
   fun ~decl_sym_val pname tenv ~node_hash location ~depth loc typ ?offset ?size ~inst_num
       ~new_sym_num ~new_alloc_num mem ->
    let option_value opt_x default_f = match opt_x with Some x -> x | None -> default_f () in
    let itv_make_sym () = Itv.make_sym pname new_sym_num in
    let offset = option_value offset itv_make_sym in
    let size = option_value size itv_make_sym in
    let alloc_num = Itv.Counter.next new_alloc_num in
    let elem = Trace.SymAssign (loc, location) in
    let arr =
      Sem.eval_array_alloc pname ~node_hash typ ~stride:None ~offset ~size ~inst_num
        ~dimension:alloc_num
      |> Dom.Val.add_trace_elem elem
    in
    let mem = Dom.Mem.add_heap loc arr mem in
    let deref_loc =
      Loc.of_allocsite (Sem.get_allocsite pname ~node_hash ~inst_num ~dimension:alloc_num)
    in
    decl_sym_val pname tenv ~node_hash location ~depth deref_loc typ mem


  let init_array_fields tenv pname ~node_hash typ locs ?dyn_length mem =
    let rec init_field locs dimension ?dyn_length (mem, inst_num) (field_name, field_typ, _) =
      let field_loc = PowLoc.append_field locs ~fn:field_name in
      let mem =
        match field_typ.Typ.desc with
        | Tarray {elt= typ; length= Some length; stride} ->
            let length = Itv.of_int_lit length in
            let length =
              Option.value_map dyn_length ~default:length ~f:(fun dyn_length ->
                  let i = Dom.Val.get_itv (Sem.eval dyn_length mem) in
                  Itv.plus i length )
            in
            let stride = Option.map stride ~f:IntLit.to_int in
            let v =
              Sem.eval_array_alloc pname ~node_hash typ ~stride ~offset:Itv.zero ~size:length
                ~inst_num ~dimension
            in
            Dom.Mem.strong_update_heap field_loc v mem
        | _ ->
            init_fields field_typ field_loc dimension ?dyn_length mem
      in
      (mem, inst_num + 1)
    and init_fields typ locs dimension ?dyn_length mem =
      match typ.Typ.desc with
      | Tstruct typename -> (
        match Tenv.lookup tenv typename with
        | Some str ->
            let f = init_field locs (dimension + 1) in
            IList.fold_last ~f ~f_last:(f ?dyn_length) ~init:(mem, 1) str.Typ.Struct.fields |> fst
        | None ->
            mem )
      | _ ->
          mem
    in
    init_fields typ locs 1 ?dyn_length mem


  let rec set_dyn_length tenv typ locs dyn_length mem =
    match typ.Typ.desc with
    | Tstruct typename -> (
      match Tenv.lookup tenv typename with
      | Some {fields} when not (List.is_empty fields)
        -> (
          let field_name, field_typ, _ = List.last_exn fields in
          let field_loc = PowLoc.append_field locs ~fn:field_name in
          match field_typ.Typ.desc with
          | Tarray {length= Some length} ->
              let length = Itv.plus (Itv.of_int_lit length) dyn_length in
              let v = Dom.Mem.find_set field_loc mem |> Dom.Val.set_array_size length in
              Dom.Mem.strong_update_heap field_loc v mem
          | _ ->
              set_dyn_length tenv field_typ field_loc dyn_length mem )
      | _ ->
          mem )
    | _ ->
        mem
end

module Check = struct
  let array_access ~arr ~idx ~is_plus pname location cond_set =
    let arr_blk = Dom.Val.get_array_blk arr in
    let arr_traces = Dom.Val.get_traces arr in
    let size = ArrayBlk.sizeof arr_blk in
    let offset = ArrayBlk.offsetof arr_blk in
    let idx_itv = Dom.Val.get_itv idx in
    let idx_traces = Dom.Val.get_traces idx in
    let idx_in_blk = (if is_plus then Itv.plus else Itv.minus) offset idx_itv in
    L.(debug BufferOverrun Verbose) "@[<v 2>Add condition :@," ;
    L.(debug BufferOverrun Verbose) "array: %a@," ArrayBlk.pp arr_blk ;
    L.(debug BufferOverrun Verbose) "  idx: %a@," Itv.pp idx_in_blk ;
    L.(debug BufferOverrun Verbose) "@]@." ;
    match (size, idx_in_blk) with
    | NonBottom size, NonBottom idx ->
        let traces = TraceSet.merge ~arr_traces ~idx_traces location in
        PO.ConditionSet.add_array_access pname location ~size ~idx traces cond_set
    | _ ->
        cond_set


  let lindex ~array_exp ~index_exp mem pname location cond_set =
    let locs = Sem.eval_locs array_exp mem |> Dom.Val.get_all_locs in
    let arr = Dom.Mem.find_set locs mem in
    let idx = Sem.eval index_exp mem in
    array_access ~arr ~idx ~is_plus:true pname location cond_set
end
