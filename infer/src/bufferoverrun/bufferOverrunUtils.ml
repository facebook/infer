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
    Dom.Mem.find_set size_powloc mem


  let load_val id val_ mem =
    let locs = val_ |> Dom.Val.get_all_locs in
    let v = Dom.Mem.find_set locs mem in
    let mem = Dom.Mem.add_stack (Loc.of_id id) v mem in
    if not v.represents_multiple_values then
      match PowLoc.is_singleton_or_more locs with
      | IContainer.Singleton loc ->
          Dom.Mem.load_simple_alias id loc mem
      | _ ->
          mem
    else mem


  type decl_local =
       Typ.Procname.t
    -> node_hash:int
    -> Location.t
    -> Loc.t
    -> Typ.t
    -> inst_num:int
    -> represents_multiple_values:bool
    -> dimension:int
    -> Dom.Mem.astate
    -> Dom.Mem.astate * int

  let decl_local_array :
         decl_local:decl_local
      -> Typ.Procname.t
      -> node_hash:int
      -> Location.t
      -> Loc.t
      -> Typ.t
      -> length:IntLit.t option
      -> ?stride:int
      -> inst_num:int
      -> represents_multiple_values:bool
      -> dimension:int
      -> Dom.Mem.astate
      -> Dom.Mem.astate * int =
   fun ~decl_local pname ~node_hash location loc typ ~length ?stride ~inst_num
       ~represents_multiple_values ~dimension mem ->
    let offset = Itv.zero in
    let size = Option.value_map ~default:Itv.top ~f:Itv.of_int_lit length in
    let path = Loc.get_path loc in
    let allocsite = Allocsite.make pname ~node_hash ~inst_num ~dimension ~path in
    let mem =
      let arr =
        Dom.Val.of_array_alloc allocsite ~stride ~offset ~size
        |> Dom.Val.add_trace_elem (Trace.ArrDecl location)
        |> Dom.Val.sets_represents_multiple_values ~represents_multiple_values
      in
      if Int.equal dimension 1 then Dom.Mem.add_stack loc arr mem else Dom.Mem.add_heap loc arr mem
    in
    let mem = Dom.Mem.init_array_relation allocsite ~offset ~size ~size_exp_opt:None mem in
    let loc = Loc.of_allocsite allocsite in
    let mem, _ =
      decl_local pname ~node_hash location loc typ ~inst_num ~represents_multiple_values:true
        ~dimension:(dimension + 1) mem
    in
    (mem, inst_num + 1)


  let decl_local_collection :
         Typ.Procname.t
      -> node_hash:int
      -> Location.t
      -> Loc.t
      -> inst_num:int
      -> represents_multiple_values:bool
      -> dimension:int
      -> Dom.Mem.astate
      -> Dom.Mem.astate * int =
   fun pname ~node_hash location loc ~inst_num ~represents_multiple_values ~dimension mem ->
    let path = Loc.get_path loc in
    let allocsite = Allocsite.make pname ~node_hash ~inst_num ~dimension ~path in
    let alloc_loc = Loc.of_allocsite allocsite in
    let mem =
      let alist =
        Dom.Val.of_pow_loc (PowLoc.singleton alloc_loc)
        |> Dom.Val.add_trace_elem (Trace.ArrDecl location)
        |> Dom.Val.sets_represents_multiple_values ~represents_multiple_values
      in
      if Int.equal dimension 1 then Dom.Mem.add_stack loc alist mem
      else
        let size = Dom.Val.Itv.zero in
        Dom.Mem.add_heap loc alist mem |> Dom.Mem.add_heap alloc_loc size
    in
    (mem, inst_num + 1)


  type decl_sym_val =
       Typ.Procname.t
    -> Itv.SymbolPath.partial
    -> Tenv.t
    -> node_hash:int
    -> Location.t
    -> represents_multiple_values:bool
    -> depth:int
    -> Loc.t
    -> Typ.t
    -> Dom.Mem.astate
    -> Dom.Mem.astate

  let decl_sym_arr :
         decl_sym_val:decl_sym_val
      -> Symb.SymbolPath.c_sym_array_kind
      -> Typ.Procname.t
      -> Itv.SymbolTable.t
      -> Itv.SymbolPath.partial
      -> Tenv.t
      -> node_hash:int
      -> Location.t
      -> represents_multiple_values:bool
      -> depth:int
      -> Loc.t
      -> Typ.t
      -> ?offset:Itv.t
      -> ?size:Itv.t
      -> ?stride:int
      -> inst_num:int
      -> new_sym_num:Counter.t
      -> new_alloc_num:Counter.t
      -> Dom.Mem.astate
      -> Dom.Mem.astate =
   fun ~decl_sym_val array_kind pname symbol_table path tenv ~node_hash location
       ~represents_multiple_values ~depth loc typ ?offset ?size ?stride ~inst_num ~new_sym_num
       ~new_alloc_num mem ->
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
    let deref_path = Itv.SymbolPath.index ~array_kind path in
    let allocsite =
      let alloc_num = Counter.next new_alloc_num in
      Allocsite.make pname ~node_hash ~inst_num ~dimension:alloc_num ~path:(Some deref_path)
    in
    let mem =
      let arr =
        let elem = Trace.SymAssign (loc, location) in
        Dom.Val.of_array_alloc allocsite ~stride ~offset ~size
        |> Dom.Val.add_trace_elem elem
        |> Dom.Val.sets_represents_multiple_values ~represents_multiple_values
      in
      mem |> Dom.Mem.add_heap loc arr |> Dom.Mem.init_param_relation loc
      |> Dom.Mem.init_array_relation allocsite ~offset ~size ~size_exp_opt:None
    in
    let deref_loc = Loc.of_allocsite allocsite in
    let represents_multiple_values =
      match array_kind with CSymArray_Array -> true | CSymArray_Pointer -> false
      (* unsound but avoids many FPs for non-array pointers *)
    in
    decl_sym_val pname deref_path tenv ~node_hash location ~represents_multiple_values ~depth
      deref_loc typ mem


  let decl_sym_java_ptr :
         decl_sym_val:decl_sym_val
      -> Typ.Procname.t
      -> Itv.SymbolPath.partial
      -> Tenv.t
      -> node_hash:int
      -> Location.t
      -> represents_multiple_values:bool
      -> depth:int
      -> Loc.t
      -> Typ.t
      -> inst_num:int
      -> new_alloc_num:Counter.t
      -> Dom.Mem.astate
      -> Dom.Mem.astate =
   fun ~decl_sym_val pname path tenv ~node_hash location ~represents_multiple_values ~depth loc typ
       ~inst_num ~new_alloc_num mem ->
    let alloc_num = Counter.next new_alloc_num in
    let elem = Trace.SymAssign (loc, location) in
    let allocsite =
      Allocsite.make pname ~node_hash ~inst_num ~dimension:alloc_num ~path:(Some path)
    in
    let alloc_loc = Loc.of_allocsite allocsite in
    let v =
      Dom.Val.of_pow_loc (PowLoc.singleton alloc_loc)
      |> Dom.Val.add_trace_elem elem
      |> Dom.Val.sets_represents_multiple_values ~represents_multiple_values
    in
    let mem = Dom.Mem.add_heap loc v mem in
    decl_sym_val pname path tenv ~node_hash location ~represents_multiple_values ~depth alloc_loc
      typ mem


  let decl_sym_collection :
         Typ.Procname.t
      -> Itv.SymbolTable.t
      -> Itv.SymbolPath.partial
      -> Location.t
      -> represents_multiple_values:bool
      -> Loc.t
      -> new_sym_num:Counter.t
      -> Dom.Mem.astate
      -> Dom.Mem.astate =
   fun pname symbol_table path location ~represents_multiple_values loc ~new_sym_num mem ->
    let size =
      Itv.make_sym ~unsigned:true pname symbol_table (Itv.SymbolPath.length path) new_sym_num
      |> Dom.Val.of_itv
      |> Dom.Val.add_trace_elem (Trace.SymAssign (loc, location))
      |> Dom.Val.sets_represents_multiple_values ~represents_multiple_values
    in
    Dom.Mem.add_heap loc size mem


  let init_array_fields tenv integer_type_widths pname path ~node_hash typ locs ?dyn_length mem =
    let rec init_field path locs dimension ?dyn_length (mem, inst_num) (field_name, field_typ, _) =
      let field_path = Option.map path ~f:(fun path -> Symb.SymbolPath.field path field_name) in
      let field_loc = PowLoc.append_field locs ~fn:field_name in
      let mem =
        match field_typ.Typ.desc with
        | Tarray {length= Some length; stride} ->
            let length = Itv.of_int_lit length in
            let length =
              Option.value_map dyn_length ~default:length ~f:(fun dyn_length ->
                  let i = Dom.Val.get_itv (Sem.eval integer_type_widths dyn_length mem) in
                  Itv.plus i length )
            in
            let stride = Option.map stride ~f:IntLit.to_int_exn in
            let allocsite =
              Allocsite.make pname ~node_hash ~inst_num ~dimension ~path:field_path
            in
            let offset, size = (Itv.zero, length) in
            let v = Dom.Val.of_array_alloc allocsite ~stride ~offset ~size in
            mem |> Dom.Mem.strong_update field_loc v
            |> Dom.Mem.init_array_relation allocsite ~offset ~size ~size_exp_opt:None
        | _ ->
            init_fields field_path field_typ field_loc dimension ?dyn_length mem
      in
      (mem, inst_num + 1)
    and init_fields path typ locs dimension ?dyn_length mem =
      match typ.Typ.desc with
      | Tstruct typename -> (
        match Tenv.lookup tenv typename with
        | Some str ->
            let f = init_field path locs (dimension + 1) in
            IList.fold_last ~f ~f_last:(f ?dyn_length) ~init:(mem, 1) str.Typ.Struct.fields |> fst
        | None ->
            mem )
      | _ ->
          mem
    in
    init_fields path typ locs 1 ?dyn_length mem


  let rec set_dyn_length tenv typ locs dyn_length mem =
    match typ.Typ.desc with
    | Tstruct typename -> (
      match Tenv.lookup tenv typename with
      | Some {fields} when not (List.is_empty fields) -> (
          let field_name, field_typ, _ = List.last_exn fields in
          let field_loc = PowLoc.append_field locs ~fn:field_name in
          match field_typ.Typ.desc with
          | Tarray {length= Some length} ->
              let length = Itv.plus (Itv.of_int_lit length) dyn_length in
              let v = Dom.Mem.find_set field_loc mem |> Dom.Val.set_array_size length in
              Dom.Mem.strong_update field_loc v mem
          | _ ->
              set_dyn_length tenv field_typ field_loc dyn_length mem )
      | _ ->
          mem )
    | _ ->
        mem
end

module Check = struct
  let check_access ~size ~idx ~size_sym_exp ~idx_sym_exp ~relation ~arr ~idx_traces
      ?(is_collection_add = false) location cond_set =
    match (size, idx) with
    | NonBottom length, NonBottom idx ->
        let offset =
          match ArrayBlk.offsetof (Dom.Val.get_array_blk arr) with
          | Bottom ->
              (* Java's collection has no offset. *)
              Itv.ItvPure.zero
          | NonBottom offset ->
              offset
        in
        let arr_traces = Dom.Val.get_traces arr in
        let traces = TraceSet.merge ~arr_traces ~idx_traces location in
        PO.ConditionSet.add_array_access location ~size:length ~offset ~idx ~size_sym_exp
          ~idx_sym_exp ~relation ~is_collection_add traces cond_set
    | _ ->
        cond_set


  let array_access ~arr ~idx ~idx_sym_exp ~relation ~is_plus location cond_set =
    let arr_blk = Dom.Val.get_array_blk arr in
    let size = ArrayBlk.sizeof arr_blk in
    let size_sym_exp = Relation.SymExp.of_sym (Dom.Val.get_size_sym arr) in
    let idx_itv = Dom.Val.get_itv idx in
    let idx_traces = Dom.Val.get_traces idx in
    let idx = if is_plus then idx_itv else Itv.neg idx_itv in
    let idx_sym_exp =
      let offset_sym_exp = Relation.SymExp.of_sym (Dom.Val.get_offset_sym arr) in
      Option.map2 offset_sym_exp idx_sym_exp ~f:(fun offset_sym_exp idx_sym_exp ->
          let op = if is_plus then Relation.SymExp.plus else Relation.SymExp.minus in
          op idx_sym_exp offset_sym_exp )
    in
    L.(debug BufferOverrun Verbose)
      "@[<v 2>Add condition :@,array: %a@,  idx: %a + %a@,@]@." ArrayBlk.pp arr_blk Itv.pp
      (ArrayBlk.offsetof arr_blk) Itv.pp idx ;
    check_access ~size ~idx ~size_sym_exp ~idx_sym_exp ~relation ~arr ~idx_traces location cond_set


  let collection_access integer_type_widths ~array_exp ~index_exp ?(is_collection_add = false) mem
      location cond_set =
    let idx = Sem.eval integer_type_widths index_exp mem in
    let arr = Sem.eval integer_type_widths array_exp mem in
    let idx_traces = Dom.Val.get_traces idx in
    let size = Exec.get_alist_size arr mem |> Dom.Val.get_itv in
    let idx = Dom.Val.get_itv idx in
    let relation = Dom.Mem.get_relation mem in
    check_access ~size ~idx ~size_sym_exp:None ~idx_sym_exp:None ~relation ~arr ~idx_traces
      ~is_collection_add location cond_set


  let lindex integer_type_widths ~array_exp ~index_exp mem location cond_set =
    let idx = Sem.eval integer_type_widths index_exp mem in
    let arr = Sem.eval_arr integer_type_widths array_exp mem in
    let idx_sym_exp =
      Relation.SymExp.of_exp ~get_sym_f:(Sem.get_sym_f integer_type_widths mem) index_exp
    in
    let relation = Dom.Mem.get_relation mem in
    array_access ~arr ~idx ~idx_sym_exp ~relation ~is_plus:true location cond_set


  let array_access_byte ~arr ~idx ~relation ~is_plus location cond_set =
    let arr_blk = Dom.Val.get_array_blk arr in
    let size = ArrayBlk.sizeof_byte arr_blk in
    let idx_itv = Dom.Val.get_itv idx in
    let idx_traces = Dom.Val.get_traces idx in
    let idx = if is_plus then idx_itv else Itv.neg idx_itv in
    L.(debug BufferOverrun Verbose)
      "@[<v 2>Add condition :@,array: %a@,  idx: %a + %a@,@]@." ArrayBlk.pp arr_blk Itv.pp
      (ArrayBlk.offsetof arr_blk) Itv.pp idx ;
    check_access ~size ~idx ~size_sym_exp:None ~idx_sym_exp:None ~relation ~arr ~idx_traces
      location cond_set ~is_collection_add:true


  let lindex_byte integer_type_widths ~array_exp ~byte_index_exp mem location cond_set =
    let idx = Sem.eval integer_type_widths byte_index_exp mem in
    let arr = Sem.eval_arr integer_type_widths array_exp mem in
    let relation = Dom.Mem.get_relation mem in
    array_access_byte ~arr ~idx ~relation ~is_plus:true location cond_set


  let binary_operation integer_type_widths bop ~lhs ~rhs location cond_set =
    let lhs_itv = Dom.Val.get_itv lhs in
    let rhs_itv = Dom.Val.get_itv rhs in
    match (lhs_itv, rhs_itv) with
    | NonBottom lhs_itv, NonBottom rhs_itv ->
        let traces =
          TraceSet.join (Dom.Val.get_traces lhs) (Dom.Val.get_traces rhs)
          |> TraceSet.add_elem (Trace.Binop location)
        in
        L.(debug BufferOverrun Verbose)
          "@[<v 2>Add condition :@,bop:%s@,  lhs: %a@,  rhs: %a@,@]@." (Binop.str Pp.text bop)
          Itv.ItvPure.pp lhs_itv Itv.ItvPure.pp rhs_itv ;
        PO.ConditionSet.add_binary_operation integer_type_widths location bop ~lhs:lhs_itv
          ~rhs:rhs_itv traces cond_set
    | _, _ ->
        cond_set
end
