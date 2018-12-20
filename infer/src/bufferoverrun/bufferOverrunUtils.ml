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
  let load_locs id locs mem =
    let v = Dom.Mem.find_set locs mem in
    let mem = Dom.Mem.add_stack (Loc.of_id id) v mem in
    if not v.represents_multiple_values then
      match PowLoc.is_singleton_or_more locs with
      | IContainer.Singleton loc ->
          Dom.Mem.load_simple_alias id loc mem
      | _ ->
          mem
    else mem


  let load_val id v mem = load_locs id (Dom.Val.get_all_locs v) mem

  type decl_local =
       Typ.Procname.t
    -> node_hash:int
    -> Location.t
    -> Loc.t
    -> Typ.t
    -> inst_num:int
    -> represents_multiple_values:bool
    -> dimension:int
    -> Dom.Mem.t
    -> Dom.Mem.t * int

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
      -> Dom.Mem.t
      -> Dom.Mem.t * int =
   fun ~decl_local pname ~node_hash location loc typ ~length ?stride ~inst_num
       ~represents_multiple_values ~dimension mem ->
    let size = Option.value_map ~default:Itv.top ~f:Itv.of_int_lit length in
    let path = Loc.get_path loc in
    let allocsite =
      let represents_multiple_values = represents_multiple_values || not (Itv.is_one size) in
      Allocsite.make pname ~node_hash ~inst_num ~dimension ~path ~represents_multiple_values
    in
    let mem =
      let arr, offset_opt =
        let traces = Trace.(Set.singleton location ArrayDeclaration) in
        match Typ.Procname.get_language pname with
        | Language.Clang ->
            let offset = Itv.zero in
            (Dom.Val.of_c_array_alloc allocsite ~stride ~offset ~size ~traces, Some offset)
        | Language.Java ->
            (Dom.Val.of_java_array_alloc allocsite ~length:size ~traces, None)
      in
      let arr = Dom.Val.sets_represents_multiple_values arr ~represents_multiple_values in
      let mem = Dom.Mem.init_array_relation allocsite ~offset_opt ~size ~size_exp_opt:None mem in
      if Int.equal dimension 1 then Dom.Mem.add_stack loc arr mem else Dom.Mem.add_heap loc arr mem
    in
    let loc = Loc.of_allocsite allocsite in
    let mem, _ =
      decl_local pname ~node_hash location loc typ ~inst_num ~represents_multiple_values:true
        ~dimension:(dimension + 1) mem
    in
    (mem, inst_num + 1)


  let init_c_array_fields tenv integer_type_widths pname path ~node_hash typ locs ?dyn_length mem =
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
              let represents_multiple_values = not (Itv.is_one length) in
              Allocsite.make pname ~node_hash ~inst_num ~dimension ~path:field_path
                ~represents_multiple_values
            in
            let offset, size = (Itv.zero, length) in
            let v =
              let traces = TraceSet.empty (* TODO: location of field declaration *) in
              Dom.Val.of_c_array_alloc allocsite ~stride ~offset ~size ~traces
            in
            mem |> Dom.Mem.strong_update field_loc v
            |> Dom.Mem.init_array_relation allocsite ~offset_opt:(Some offset) ~size
                 ~size_exp_opt:None
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


  let rec set_dyn_length location tenv typ locs dyn_length mem =
    match typ.Typ.desc with
    | Tstruct typename -> (
      match Tenv.lookup tenv typename with
      | Some {fields} when not (List.is_empty fields) -> (
          let field_name, field_typ, _ = List.last_exn fields in
          let field_loc = PowLoc.append_field locs ~fn:field_name in
          match field_typ.Typ.desc with
          | Tarray {length= Some length} ->
              let length = Itv.plus (Itv.of_int_lit length) dyn_length |> Dom.Val.of_itv in
              let v =
                Dom.Mem.find_set field_loc mem |> Dom.Val.set_array_length location ~length
              in
              Dom.Mem.strong_update field_loc v mem
          | _ ->
              set_dyn_length location tenv field_typ field_loc dyn_length mem )
      | _ ->
          mem )
    | _ ->
        mem


  let get_max_char s = String.fold s ~init:0 ~f:(fun acc c -> max acc (Char.to_int c))

  let decl_string pname ~node_hash integer_type_widths location locs s mem =
    let stride = Some (Typ.width_of_ikind integer_type_widths IChar / 8) in
    let offset = Itv.zero in
    let size = Itv.of_int (String.length s + 1) in
    let traces = Trace.Set.singleton location Trace.ArrayDeclaration in
    let char_itv = Itv.join Itv.zero (Itv.of_int (get_max_char s)) in
    let decl loc mem =
      let allocsite =
        let deref_kind = Symb.SymbolPath.Deref_ArrayIndex in
        let path = Loc.get_path loc in
        let deref_path = Option.map ~f:(fun path -> Symb.SymbolPath.deref ~deref_kind path) path in
        Allocsite.make pname ~node_hash ~inst_num:0 ~dimension:1 ~path:deref_path
          ~represents_multiple_values:true
      in
      let v = Dom.Val.of_c_array_alloc allocsite ~stride ~offset ~size ~traces in
      mem
      |> Dom.Mem.update_mem (PowLoc.singleton loc) v
      |> Dom.Mem.add_heap (Loc.of_allocsite allocsite) (Dom.Val.of_itv char_itv)
    in
    PowLoc.fold decl locs mem
end

module Check = struct
  let check_access ~size ~idx ~size_sym_exp ~idx_sym_exp ~relation ~arr ~idx_traces ~last_included
      ~latest_prune location cond_set =
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
        PO.ConditionSet.add_array_access location ~size:length ~offset ~idx ~size_sym_exp
          ~idx_sym_exp ~relation ~last_included ~idx_traces ~arr_traces ~latest_prune cond_set
    | _ ->
        cond_set


  let array_access ~arr ~idx ~idx_sym_exp ~relation ~is_plus ~last_included ~latest_prune location
      cond_set =
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
    check_access ~size ~idx ~size_sym_exp ~idx_sym_exp ~relation ~arr ~idx_traces ~last_included
      ~latest_prune location cond_set


  let lindex integer_type_widths ~array_exp ~index_exp ~last_included mem location cond_set =
    let idx = Sem.eval integer_type_widths index_exp mem in
    let arr = Sem.eval_arr integer_type_widths array_exp mem in
    let idx_sym_exp =
      Relation.SymExp.of_exp ~get_sym_f:(Sem.get_sym_f integer_type_widths mem) index_exp
    in
    let relation = Dom.Mem.get_relation mem in
    let latest_prune = Dom.Mem.get_latest_prune mem in
    array_access ~arr ~idx ~idx_sym_exp ~relation ~is_plus:true ~last_included ~latest_prune
      location cond_set


  let array_access_byte ~arr ~idx ~relation ~is_plus ~last_included location cond_set =
    let arr_blk = Dom.Val.get_array_blk arr in
    let size = ArrayBlk.sizeof_byte arr_blk in
    let idx_itv = Dom.Val.get_itv idx in
    let idx_traces = Dom.Val.get_traces idx in
    let idx = if is_plus then idx_itv else Itv.neg idx_itv in
    L.(debug BufferOverrun Verbose)
      "@[<v 2>Add condition :@,array: %a@,  idx: %a + %a@,@]@." ArrayBlk.pp arr_blk Itv.pp
      (ArrayBlk.offsetof arr_blk) Itv.pp idx ;
    check_access ~size ~idx ~size_sym_exp:None ~idx_sym_exp:None ~relation ~arr ~idx_traces
      ~last_included location cond_set


  let lindex_byte integer_type_widths ~array_exp ~byte_index_exp ~last_included mem location
      cond_set =
    let idx = Sem.eval integer_type_widths byte_index_exp mem in
    let arr = Sem.eval_arr integer_type_widths array_exp mem in
    let relation = Dom.Mem.get_relation mem in
    let latest_prune = Dom.Mem.get_latest_prune mem in
    array_access_byte ~arr ~idx ~relation ~is_plus:true ~last_included ~latest_prune location
      cond_set


  let binary_operation integer_type_widths bop ~lhs ~rhs ~latest_prune location cond_set =
    let lhs_itv = Dom.Val.get_itv lhs in
    let rhs_itv = Dom.Val.get_itv rhs in
    match (lhs_itv, rhs_itv) with
    | NonBottom lhs_itv, NonBottom rhs_itv ->
        L.(debug BufferOverrun Verbose)
          "@[<v 2>Add condition :@,bop:%s@,  lhs: %a@,  rhs: %a@,@]@." (Binop.str Pp.text bop)
          Itv.ItvPure.pp lhs_itv Itv.ItvPure.pp rhs_itv ;
        PO.ConditionSet.add_binary_operation integer_type_widths location bop ~lhs:lhs_itv
          ~rhs:rhs_itv ~lhs_traces:(Dom.Val.get_traces lhs) ~rhs_traces:(Dom.Val.get_traces rhs)
          ~latest_prune cond_set
    | _, _ ->
        cond_set
end
