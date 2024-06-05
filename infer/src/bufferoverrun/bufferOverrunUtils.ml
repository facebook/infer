(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open AbsLoc
open! AbstractDomain.Types
module BoSummary = BufferOverrunAnalysisSummary
module L = Logging
module Dom = BufferOverrunDomain
module PO = BufferOverrunProofObligations
module Sem = BufferOverrunSemantics
module Trace = BufferOverrunTrace
module TraceSet = Trace.Set
module TypModels = BufferOverrunTypModels

module ModelEnv = struct
  type model_env =
    { pname: Procname.t
    ; caller_pname: Procname.t option
    ; node_hash: int
    ; location: Location.t
    ; tenv: Tenv.t
    ; integer_type_widths: IntegerWidths.t
    ; get_summary: BoSummary.get_summary }

  let mk_model_env pname ?caller_pname ~node_hash location tenv integer_type_widths get_summary =
    {pname; caller_pname; node_hash; location; tenv; integer_type_widths; get_summary}
end

module Exec = struct
  open ModelEnv

  let load_locs ~represents_multiple_values ~modeled_range id typ locs mem =
    let set_modeled_range v =
      Option.value_map modeled_range ~default:v ~f:(fun modeled_range ->
          Dom.Val.set_modeled_range modeled_range v )
    in
    let v = Dom.Mem.find_set ~typ locs mem |> set_modeled_range in
    let mem = Dom.Mem.add_stack (Loc.of_id id) v mem in
    let mem =
      if represents_multiple_values then Dom.Mem.add_heap_set ~represents_multiple_values locs v mem
      else mem
    in
    match PowLoc.is_singleton_or_more locs with
    | IContainer.Singleton loc ->
        Dom.Mem.load_simple_alias id loc mem
    | _ ->
        mem


  let rec decl_local_loc ({tenv} as model_env) loc typ ~inst_num ~represents_multiple_values
      ~dimension mem =
    match typ.Typ.desc with
    | Typ.Tarray {elt= typ; length; stride} ->
        let stride = Option.map ~f:IntLit.to_int_exn stride in
        decl_local_array model_env loc typ ~length ?stride ~inst_num ~represents_multiple_values
          ~dimension mem
    | Typ.Tstruct typname -> (
      match TypModels.dispatch tenv typname with
      | Some (CArray {element_typ; length}) ->
          decl_local_array model_env loc element_typ ~length:(Some length) ~inst_num
            ~represents_multiple_values ~dimension mem
      | Some CppStdVector | Some JavaCollection | Some JavaInteger | None ->
          (mem, inst_num) )
    | _ ->
        (mem, inst_num)


  and decl_local_array ({pname; caller_pname; node_hash; location} as model_env) loc typ ~length
      ?stride ~inst_num ~represents_multiple_values ~dimension mem =
    let size = Option.value_map ~default:Itv.top ~f:Itv.of_int_lit length in
    let path = Loc.get_path loc in
    let allocsite =
      let represents_multiple_values = represents_multiple_values || not (Itv.is_one size) in
      Allocsite.make pname ~caller_pname ~node_hash ~inst_num ~dimension ~path
        ~represents_multiple_values
    in
    let mem =
      let arr =
        let traces = Trace.(Set.singleton location ArrayDeclaration) in
        match Procname.get_language pname with
        | Language.Clang ->
            let offset = Itv.zero in
            Dom.Val.of_c_array_alloc allocsite ~stride ~offset ~size ~traces
        | Language.Java ->
            Dom.Val.of_java_array_alloc allocsite ~length:size ~traces
        | Language.CIL ->
            (* cil todo *)
            Dom.Val.of_java_array_alloc allocsite ~length:size ~traces
        | Language.Erlang ->
            L.die InternalError "Erlang not supported"
        | Language.Hack ->
            L.die InternalError "Hack not supported"
        | Language.Python ->
            L.die InternalError "Python not supported"
      in
      if Int.equal dimension 1 then Dom.Mem.add_stack ~represents_multiple_values loc arr mem
      else Dom.Mem.add_heap ~represents_multiple_values loc arr mem
    in
    let loc = Loc.of_allocsite allocsite in
    let mem, _ =
      decl_local_loc model_env loc typ ~inst_num ~represents_multiple_values:true
        ~dimension:(dimension + 1) mem
    in
    (mem, inst_num + 1)


  let decl_local model_env (mem, inst_num) (loc, typ) =
    decl_local_loc model_env loc typ ~inst_num ~represents_multiple_values:false ~dimension:1 mem


  let init_c_array_fields {pname; caller_pname; node_hash; tenv; integer_type_widths} path typ locs
      ?dyn_length mem =
    let rec init_field path locs dimension ?dyn_length (mem, inst_num)
        {Struct.name= field_name; typ= field_typ} =
      let field_path =
        Option.map path ~f:(fun path -> Symb.SymbolPath.append_field path field_name)
      in
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
              Allocsite.make pname ~caller_pname ~node_hash ~inst_num ~dimension ~path:field_path
                ~represents_multiple_values
            in
            let offset, size = (Itv.zero, length) in
            let v =
              let traces = TraceSet.bottom (* TODO: location of field declaration *) in
              Dom.Val.of_c_array_alloc allocsite ~stride ~offset ~size ~traces
            in
            mem |> Dom.Mem.strong_update field_loc v
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
            IList.fold_last ~f ~f_last:(f ?dyn_length) ~init:(mem, 1) str.Struct.fields |> fst
        | None ->
            mem )
      | _ ->
          mem
    in
    init_fields path typ locs 1 ?dyn_length mem


  let rec set_dyn_length ({location; tenv} as model_env) typ locs dyn_length mem =
    match typ.Typ.desc with
    | Tstruct typename -> (
      match Tenv.lookup tenv typename with
      | Some {fields} when not (List.is_empty fields) -> (
          let {Struct.name= field_name; typ= field_typ} = List.last_exn fields in
          let field_loc = PowLoc.append_field locs ~fn:field_name in
          match field_typ.Typ.desc with
          | Tarray {length= Some length} ->
              let length = Itv.plus (Itv.of_int_lit length) dyn_length |> Dom.Val.of_itv in
              let v = Dom.Mem.find_set field_loc mem |> Dom.Val.set_array_length location ~length in
              Dom.Mem.strong_update field_loc v mem
          | _ ->
              set_dyn_length model_env field_typ field_loc dyn_length mem )
      | _ ->
          mem )
    | _ ->
        mem


  let get_min_max_char s ~init_char =
    let init_i = Char.to_int init_char in
    String.fold s ~init:(init_i, init_i) ~f:(fun (min_acc, max_acc) c ->
        let i = Char.to_int c in
        (min min_acc i, max max_acc i) )


  let decl_string {pname; caller_pname; node_hash; location; integer_type_widths} ~do_alloc locs s
      mem =
    let size =
      let s_length =
        if Language.curr_language_is Java then String.length s else String.length s + 1
      in
      Itv.of_int s_length
    in
    let traces = Trace.Set.singleton location Trace.ArrayDeclaration in
    let char_itv =
      let itv =
        if Int.equal (String.length s) 0 then Itv.bot
        else
          let min, max = get_min_max_char s ~init_char:s.[0] in
          Itv.join (Itv.of_int min) (Itv.of_int max)
      in
      if Language.curr_language_is Java then itv else Itv.join Itv.zero itv
    in
    let decl loc mem =
      (* It doesn't allocate if the character pointer is in stack, because they are already
         allocated at the entry of the function. *)
      let deref_loc, mem =
        if do_alloc then
          let allocsite =
            let deref_kind = Symb.SymbolPath.Deref_ArrayIndex in
            let path = Loc.get_path loc in
            let deref_path =
              Option.map ~f:(fun path -> Symb.SymbolPath.deref ~deref_kind path) path
            in
            Allocsite.make pname ~caller_pname ~node_hash ~inst_num:0 ~dimension:1 ~path:deref_path
              ~represents_multiple_values:true
          in
          let v =
            if Language.curr_language_is Java then
              Dom.Val.of_java_array_alloc allocsite ~length:size ~traces
            else
              let stride = Some (IntegerWidths.width_of_ikind integer_type_widths IChar / 8) in
              let offset = Itv.zero in
              Dom.Val.of_c_array_alloc allocsite ~stride ~offset ~size ~traces
          in
          (Loc.of_allocsite allocsite, Dom.Mem.update_mem (PowLoc.singleton loc) v mem)
        else (loc, mem)
      in
      mem
      |> Dom.Mem.add_heap deref_loc (Dom.Val.of_itv char_itv)
      |> Dom.Mem.set_first_idx_of_null deref_loc
           (Dom.Val.of_itv ~traces (Itv.of_int (String.length s)))
    in
    PowLoc.fold decl locs mem


  let set_c_strlen ~tgt ~src mem =
    let traces = TraceSet.join (Dom.Val.get_traces tgt) (Dom.Val.get_traces src) in
    let src_itv = Dom.Val.get_itv src in
    let set_c_strlen1 allocsite arrinfo acc =
      let loc = Loc.of_allocsite allocsite in
      let idx = Dom.Val.of_itv ~traces (ArrayBlk.ArrInfo.get_offset arrinfo) in
      if Itv.leq ~lhs:Itv.zero ~rhs:src_itv then Dom.Mem.set_first_idx_of_null loc idx acc
      else Dom.Mem.unset_first_idx_of_null loc idx acc
    in
    ArrayBlk.fold set_c_strlen1 (Dom.Val.get_array_blk tgt) mem
end

module Check = struct
  let check_access ~size ~idx ~offset ~arr_traces ~idx_traces ~last_included ~latest_prune location
      cond_set =
    match (size, idx) with
    | NonBottom length, NonBottom idx ->
        PO.ConditionSet.add_array_access location ~size:length ~offset ~idx ~last_included
          ~idx_traces ~arr_traces ~latest_prune cond_set
    | _ ->
        cond_set


  let log_array_access allocsite size offset idx =
    L.(debug BufferOverrun Verbose)
      "@[<v 2>Add condition :@,array: %a@,  size: %a@,  idx: %a + %a@,@]@." Allocsite.pp allocsite
      Itv.pp size Itv.ItvPure.pp offset Itv.pp idx


  let offsetof arr_info =
    match ArrayBlk.ArrInfo.get_offset arr_info with
    | Bottom ->
        (* Java's collection has no offset. *)
        Itv.ItvPure.zero
    | NonBottom offset ->
        offset


  let array_access ~arr ~idx ~is_plus ~last_included ~latest_prune location cond_set =
    let idx_traces = Dom.Val.get_traces idx in
    let idx =
      let idx_itv = Dom.Val.get_itv idx in
      if is_plus then idx_itv else Itv.neg idx_itv
    in
    let arr_traces = Dom.Val.get_traces arr in
    let array_access1 allocsite arr_info acc =
      let size = ArrayBlk.ArrInfo.get_size arr_info in
      let offset = offsetof arr_info in
      log_array_access allocsite size offset idx ;
      check_access ~size ~idx ~offset ~arr_traces ~idx_traces ~last_included ~latest_prune location
        acc
    in
    ArrayBlk.fold array_access1 (Dom.Val.get_array_blk arr) cond_set


  let lindex integer_type_widths ~array_exp ~index_exp ~last_included mem location cond_set =
    let idx = Sem.eval integer_type_widths index_exp mem in
    let arr =
      if Language.curr_language_is Java then
        let arr_locs = Sem.eval_locs array_exp mem in
        if PowLoc.is_bot arr_locs then Dom.Val.Itv.top else Dom.Mem.find_set arr_locs mem
      else Sem.eval_arr integer_type_widths array_exp mem
    in
    let latest_prune = Dom.Mem.get_latest_prune mem in
    array_access ~arr ~idx ~is_plus:true ~last_included ~latest_prune location cond_set


  let array_access_byte ~arr ~idx ~is_plus ~last_included ~latest_prune location cond_set =
    let idx_traces = Dom.Val.get_traces idx in
    let idx =
      let idx_itv = Dom.Val.get_itv idx in
      if is_plus then idx_itv else Itv.neg idx_itv
    in
    let arr_traces = Dom.Val.get_traces arr in
    let array_access_byte1 allocsite arr_info acc =
      let size = ArrayBlk.ArrInfo.byte_size arr_info in
      let offset = offsetof arr_info in
      log_array_access allocsite size offset idx ;
      check_access ~size ~idx ~offset ~arr_traces ~idx_traces ~last_included ~latest_prune location
        acc
    in
    ArrayBlk.fold array_access_byte1 (Dom.Val.get_array_blk arr) cond_set


  let lindex_byte integer_type_widths ~array_exp ~byte_index_exp ~last_included mem location
      cond_set =
    let idx = Sem.eval integer_type_widths byte_index_exp mem in
    let arr = Sem.eval_arr integer_type_widths array_exp mem in
    let latest_prune = Dom.Mem.get_latest_prune mem in
    array_access_byte ~arr ~idx ~is_plus:true ~last_included ~latest_prune location cond_set


  let binary_operation integer_type_widths pname bop ~lhs ~rhs ~latest_prune location cond_set =
    let lhs_itv = Dom.Val.get_itv lhs in
    let rhs_itv = Dom.Val.get_itv rhs in
    match (lhs_itv, rhs_itv) with
    | NonBottom lhs_itv, NonBottom rhs_itv ->
        L.(debug BufferOverrun Verbose)
          "@[<v 2>Add condition :@,bop:%s@,  lhs: %a@,  rhs: %a@,@]@." (Binop.str Pp.text bop)
          Itv.ItvPure.pp lhs_itv Itv.ItvPure.pp rhs_itv ;
        PO.ConditionSet.add_binary_operation integer_type_widths location pname bop ~lhs:lhs_itv
          ~rhs:rhs_itv ~lhs_traces:(Dom.Val.get_traces lhs) ~rhs_traces:(Dom.Val.get_traces rhs)
          ~latest_prune cond_set
    | _, _ ->
        cond_set
end

type get_formals = Procname.t -> (Pvar.t * Typ.t) list option

module ReplaceCallee = struct
  type replaced = {pname: Procname.t; args: (Exp.t * Typ.t) list; is_args_ref: bool}

  let is_cpp_constructor_with_types get_formals class_typ param_ref_typs pname =
    let num_params = List.length param_ref_typs in
    match pname with
    | Procname.ObjC_Cpp {kind= CPPConstructor _; parameters}
      when Int.equal (List.length parameters) num_params -> (
      match get_formals pname |> Option.map ~f:(List.map ~f:snd) with
      | Some (this_typ :: formal_typs) -> (
          Typ.is_ptr_to_ignore_quals class_typ ~ptr:this_typ
          &&
          match
            List.for_all2 param_ref_typs formal_typs ~f:(fun param_ref_typ formal_typ ->
                Typ.equal_ignore_quals formal_typ param_ref_typ
                || Typ.is_ptr_to_ignore_quals formal_typ ~ptr:param_ref_typ )
          with
          | List.Or_unequal_lengths.Ok b ->
              b
          | List.Or_unequal_lengths.Unequal_lengths ->
              false )
      | _ ->
          false )
    | _ ->
        false


  module CacheForMakeShared = struct
    let results : Procname.t option Procname.LRUHash.t lazy_t =
      lazy (Procname.LRUHash.create ~initial_size:128 ~max_size:200)


    let add pname value = Procname.LRUHash.replace (Lazy.force results) pname value

    let find_opt pname = Procname.LRUHash.find_opt (Lazy.force results) pname

    let clear () = if Lazy.is_val results then Procname.LRUHash.clear (Lazy.force results)
  end

  let get_cpp_constructor_of_make_shared =
    let rec strip_ttype = function
      | [] ->
          Some []
      | Typ.TType x :: tl ->
          Option.map (strip_ttype tl) ~f:(fun tl -> x :: tl)
      | _ ->
          None
    in
    fun tenv get_formals pname ->
      IOption.value_default_f (CacheForMakeShared.find_opt pname) ~f:(fun () ->
          let result =
            match pname with
            | Procname.C ({c_template_args= Typ.Template {args}} as name)
              when Procname.C.is_make_shared name -> (
              match strip_ttype args with
              | Some (class_typ_templ :: param_typs_templ) ->
                  let open IOption.Let_syntax in
                  let* class_name = Typ.name class_typ_templ in
                  let* {Struct.methods} = Tenv.lookup tenv class_name in
                  (* NOTE: This drops the last void type off. *)
                  let* param_typs_templ = List.drop_last param_typs_templ in
                  List.find methods
                    ~f:(is_cpp_constructor_with_types get_formals class_typ_templ param_typs_templ)
              | _ ->
                  None )
            | _ ->
                None
          in
          CacheForMakeShared.add pname result ;
          result )


  let replace_make_shared tenv get_formals pname args =
    match get_cpp_constructor_of_make_shared tenv get_formals pname with
    | Some constr ->
        (* NOTE: This replaces the pointer to the target object.  In the parameters of
           [std::make_shared], the pointer is on the last place.  On the other hand, it is on the
           first place in the constructor's parameters. *)
        let args = IList.move_last_to_first args in
        {pname= constr; args; is_args_ref= true}
    | None ->
        {pname; args; is_args_ref= false}
end

let clear_cache () = ReplaceCallee.CacheForMakeShared.clear ()
