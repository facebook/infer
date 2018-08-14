(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open AbsLoc
open! AbstractDomain.Types
module L = Logging
module Dom = BufferOverrunDomain
module Relation = BufferOverrunDomainRelation
module PO = BufferOverrunProofObligations
module Sem = BufferOverrunSemantics
module Trace = BufferOverrunTrace
module TraceSet = Trace.Set

module Exec = struct
  let get_alist_size alist mem =
    let size_powloc = Dom.Val.get_pow_loc alist in
    Dom.Mem.find_heap_set size_powloc mem


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
    let offset = Itv.zero in
    let size = Option.value_map ~default:Itv.top ~f:Itv.of_int_lit length in
    let allocsite = Allocsite.make pname ~node_hash ~inst_num ~dimension in
    let arr =
      Dom.Val.of_array_alloc allocsite ~stride ~offset ~size
      |> Dom.Val.add_trace_elem (Trace.ArrDecl location)
    in
    let mem =
      if Int.equal dimension 1 then Dom.Mem.add_stack loc arr mem else Dom.Mem.add_heap loc arr mem
    in
    let mem = Dom.Mem.init_array_relation allocsite ~offset ~size ~size_exp_opt:None mem in
    let loc = Loc.of_allocsite allocsite in
    let mem, _ =
      decl_local pname ~node_hash location loc typ ~inst_num ~dimension:(dimension + 1) mem
    in
    (mem, inst_num + 1)


  let decl_local_arraylist
      : Typ.Procname.t -> node_hash:int -> Location.t -> Loc.t -> inst_num:int -> dimension:int
        -> Dom.Mem.astate -> Dom.Mem.astate * int =
   fun pname ~node_hash location loc ~inst_num ~dimension mem ->
    let allocsite = Allocsite.make pname ~node_hash ~inst_num ~dimension in
    let alloc_loc = Loc.of_allocsite allocsite in
    let alist =
      Dom.Val.of_pow_loc (PowLoc.singleton alloc_loc)
      |> Dom.Val.add_trace_elem (Trace.ArrDecl location)
    in
    let size = Dom.Val.of_int 0 in
    let mem =
      if Int.equal dimension 1 then Dom.Mem.add_stack loc alist mem
      else Dom.Mem.add_heap loc alist mem |> Dom.Mem.add_heap alloc_loc size
    in
    (mem, inst_num + 1)


  type decl_sym_val =
    Typ.Procname.t -> Itv.SymbolPath.partial -> Tenv.t -> node_hash:int -> Location.t -> depth:int
    -> Loc.t -> Typ.t -> Dom.Mem.astate -> Dom.Mem.astate

  let decl_sym_arr
      : decl_sym_val:decl_sym_val -> Typ.Procname.t -> Itv.SymbolTable.t -> Itv.SymbolPath.partial
        -> Tenv.t -> node_hash:int -> Location.t -> depth:int -> Loc.t -> Typ.t -> ?offset:Itv.t
        -> ?size:Itv.t -> inst_num:int -> new_sym_num:Itv.Counter.t -> new_alloc_num:Itv.Counter.t
        -> Dom.Mem.astate -> Dom.Mem.astate =
   fun ~decl_sym_val pname symbol_table path tenv ~node_hash location ~depth loc typ ?offset ?size
       ~inst_num ~new_sym_num ~new_alloc_num mem ->
    let option_value opt_x default_f = match opt_x with Some x -> x | None -> default_f () in
    let offset =
      option_value offset (fun () ->
          Itv.make_sym pname symbol_table (Itv.SymbolPath.offset path) new_sym_num )
    in
    let size =
      option_value size (fun () ->
          Itv.make_sym ~unsigned:true pname symbol_table (Itv.SymbolPath.length path) new_sym_num
      )
    in
    let alloc_num = Itv.Counter.next new_alloc_num in
    let elem = Trace.SymAssign (loc, location) in
    let allocsite = Allocsite.make pname ~node_hash ~inst_num ~dimension:alloc_num in
    let arr =
      Dom.Val.of_array_alloc allocsite ~stride:None ~offset ~size |> Dom.Val.add_trace_elem elem
    in
    let mem =
      mem |> Dom.Mem.add_heap loc arr |> Dom.Mem.init_param_relation loc
      |> Dom.Mem.init_array_relation allocsite ~offset ~size ~size_exp_opt:None
    in
    let deref_loc =
      Loc.of_allocsite (Allocsite.make pname ~node_hash ~inst_num ~dimension:alloc_num)
    in
    let path = Itv.SymbolPath.index path in
    decl_sym_val pname path tenv ~node_hash location ~depth deref_loc typ mem


  let decl_sym_java_ptr
      : decl_sym_val:decl_sym_val -> Typ.Procname.t -> Itv.SymbolPath.partial -> Tenv.t
        -> node_hash:int -> Location.t -> depth:int -> Loc.t -> Typ.t -> inst_num:int
        -> new_alloc_num:Itv.Counter.t -> Dom.Mem.astate -> Dom.Mem.astate =
   fun ~decl_sym_val pname path tenv ~node_hash location ~depth loc typ ~inst_num ~new_alloc_num
       mem ->
    let alloc_num = Itv.Counter.next new_alloc_num in
    let elem = Trace.SymAssign (loc, location) in
    let allocsite = Allocsite.make pname ~node_hash ~inst_num ~dimension:alloc_num in
    let alloc_loc = Loc.of_allocsite allocsite in
    let v = Dom.Val.of_pow_loc (PowLoc.singleton alloc_loc) |> Dom.Val.add_trace_elem elem in
    let mem = Dom.Mem.add_heap loc v mem in
    decl_sym_val pname path tenv ~node_hash location ~depth alloc_loc typ mem


  let decl_sym_arraylist
      : Typ.Procname.t -> Itv.SymbolTable.t -> Itv.SymbolPath.partial -> Location.t -> Loc.t
        -> new_sym_num:Itv.Counter.t -> Dom.Mem.astate -> Dom.Mem.astate =
   fun pname symbol_table path location loc ~new_sym_num mem ->
    let size =
      Itv.make_sym ~unsigned:true pname symbol_table (Itv.SymbolPath.length path) new_sym_num
      |> Dom.Val.of_itv |> Dom.Val.add_trace_elem (Trace.SymAssign (loc, location))
    in
    Dom.Mem.add_heap loc size mem


  let init_array_fields tenv pname ~node_hash typ locs ?dyn_length mem =
    let rec init_field locs dimension ?dyn_length (mem, inst_num) (field_name, field_typ, _) =
      let field_loc = PowLoc.append_field locs ~fn:field_name in
      let mem =
        match field_typ.Typ.desc with
        | Tarray {length= Some length; stride} ->
            let length = Itv.of_int_lit length in
            let length =
              Option.value_map dyn_length ~default:length ~f:(fun dyn_length ->
                  let i = Dom.Val.get_itv (Sem.eval dyn_length mem) in
                  Itv.plus i length )
            in
            let stride = Option.map stride ~f:IntLit.to_int_exn in
            let allocsite = Allocsite.make pname ~node_hash ~inst_num ~dimension in
            let offset, size = (Itv.zero, length) in
            let v = Dom.Val.of_array_alloc allocsite ~stride ~offset ~size in
            mem |> Dom.Mem.strong_update_heap field_loc v
            |> Dom.Mem.init_array_relation allocsite ~offset ~size ~size_exp_opt:None
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
  let check_access ~size ~idx ~size_sym_exp ~idx_sym_exp ~relation ~arr ~idx_traces
      ?(is_arraylist_add= false) pname location cond_set =
    let arr_traces = Dom.Val.get_traces arr in
    match (size, idx) with
    | NonBottom length, NonBottom idx ->
        let traces = TraceSet.merge ~arr_traces ~idx_traces location in
        PO.ConditionSet.add_array_access pname location ~size:length ~idx ~size_sym_exp
          ~idx_sym_exp ~relation ~is_arraylist_add traces cond_set
    | _ ->
        cond_set


  let array_access ~arr ~idx ~idx_sym_exp ~relation ~is_plus pname location cond_set =
    let arr_blk = Dom.Val.get_array_blk arr in
    let size = ArrayBlk.sizeof arr_blk in
    let size_sym_exp = Relation.SymExp.of_sym (Dom.Val.get_size_sym arr) in
    let offset = ArrayBlk.offsetof arr_blk in
    let offset_sym_exp = Relation.SymExp.of_sym (Dom.Val.get_offset_sym arr) in
    let idx_itv = Dom.Val.get_itv idx in
    let idx_traces = Dom.Val.get_traces idx in
    let idx_in_blk = (if is_plus then Itv.plus else Itv.minus) offset idx_itv in
    let idx_sym_exp =
      Option.map2 offset_sym_exp idx_sym_exp ~f:(fun offset_sym_exp idx_sym_exp ->
          let op = if is_plus then Relation.SymExp.plus else Relation.SymExp.minus in
          op idx_sym_exp offset_sym_exp )
    in
    L.(debug BufferOverrun Verbose)
      "@[<v 2>Add condition :@,array: %a@,  idx: %a@,@]@." ArrayBlk.pp arr_blk Itv.pp idx_in_blk ;
    check_access ~size ~idx:idx_in_blk ~size_sym_exp ~idx_sym_exp ~relation ~arr ~idx_traces pname
      location cond_set


  let arraylist_access ~array_exp ~index_exp ?(is_arraylist_add= false) mem pname location cond_set =
    let idx = Sem.eval index_exp mem in
    let arr = Sem.eval array_exp mem in
    let idx_traces = Dom.Val.get_traces idx in
    let size = Exec.get_alist_size arr mem |> Dom.Val.get_itv in
    let idx = Dom.Val.get_itv idx in
    let relation = Dom.Mem.get_relation mem in
    check_access ~size ~idx ~size_sym_exp:None ~idx_sym_exp:None ~relation ~arr ~idx_traces
      ~is_arraylist_add pname location cond_set


  let lindex ~array_exp ~index_exp mem pname location cond_set =
    let idx = Sem.eval index_exp mem in
    let arr = Sem.eval_arr array_exp mem in
    let idx_sym_exp = Relation.SymExp.of_exp ~get_sym_f:(Sem.get_sym_f mem) index_exp in
    let relation = Dom.Mem.get_relation mem in
    array_access ~arr ~idx ~idx_sym_exp ~relation ~is_plus:true pname location cond_set
end
