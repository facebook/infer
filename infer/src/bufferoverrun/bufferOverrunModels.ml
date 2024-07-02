(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open AbsLoc
open! AbstractDomain.Types
module L = Logging
module BoField = BufferOverrunField
module BoUtils = BufferOverrunUtils
module Dom = BufferOverrunDomain
module PO = BufferOverrunProofObligations
module Sem = BufferOverrunSemantics
module Trace = BufferOverrunTrace
open BoUtils.ModelEnv
open ProcnameDispatcher.Call.FuncArg

type exec_fun = model_env -> ret:Ident.t * Typ.t -> Dom.Mem.t -> Dom.Mem.t

type check_fun = model_env -> Dom.Mem.t -> PO.ConditionSet.checked_t -> PO.ConditionSet.checked_t

type model = {exec: exec_fun; check: check_fun}

let no_check _model_env _mem cond_set = cond_set

let no_model =
  let exec {pname; location} ~ret mem =
    L.d_printfln_escaped "No model for %a" Procname.pp pname ;
    Dom.Mem.add_unknown_from ret ~callee_pname:pname ~location mem
  in
  {exec; check= no_check}


let at ?(size = Int64.zero) array_exp index_exp =
  (* TODO? use size *)
  let exec {integer_type_widths} ~ret:(id, _) mem =
    L.d_printfln_escaped "Using model std::array<_, %Ld>::at" size ;
    Dom.Mem.add_stack (Loc.of_id id)
      (Sem.eval_lindex integer_type_widths array_exp index_exp mem)
      mem
  and check {location; integer_type_widths} mem cond_set =
    BoUtils.Check.lindex integer_type_widths ~array_exp ~index_exp ~last_included:false mem location
      cond_set
  in
  {exec; check}


let eval_binop ~f e1 e2 =
  let exec {integer_type_widths} ~ret:(id, _) mem =
    let i1 = Sem.eval integer_type_widths e1 mem |> Dom.Val.get_itv in
    let i2 = Sem.eval integer_type_widths e2 mem |> Dom.Val.get_itv in
    let v = f i1 i2 |> Dom.Val.of_itv in
    Dom.Mem.add_stack (Loc.of_id id) v mem
  in
  {exec; check= no_check}


let get_malloc_info_opt = function
  | Exp.BinOp (Binop.Mult _, Exp.Sizeof {typ; nbytes}, length)
  | Exp.BinOp (Binop.Mult _, length, Exp.Sizeof {typ; nbytes}) ->
      Some (typ, nbytes, length, None)
  (* In Java all arrays are dynamically allocated *)
  | Exp.Sizeof {typ; nbytes; dynamic_length= Some arr_length} when Language.curr_language_is Java ->
      Some (typ, nbytes, arr_length, Some arr_length)
  | Exp.Sizeof {typ; nbytes; dynamic_length} ->
      Some (typ, nbytes, Exp.one, dynamic_length)
  | _ ->
      None


let get_malloc_info : Exp.t -> Typ.t * Int.t option * Exp.t * Exp.t option =
 fun x ->
  get_malloc_info_opt x
  |> IOption.if_none_eval ~f:(fun () -> (Typ.mk (Typ.Tint Typ.IChar), Some 1, x, None))


let check_alloc_size ~can_be_zero size_exp {location; integer_type_widths} mem cond_set =
  let _, _, length0, _ = get_malloc_info size_exp in
  let v_length = Sem.eval integer_type_widths length0 mem in
  match Dom.Val.get_itv v_length with
  | Bottom ->
      cond_set
  | NonBottom length ->
      let traces = Dom.Val.get_traces v_length in
      let latest_prune = Dom.Mem.get_latest_prune mem in
      PO.ConditionSet.add_alloc_size location ~can_be_zero ~length traces latest_prune cond_set


let fgets str_exp num_exp =
  let exec {integer_type_widths} ~ret:(id, _) mem =
    let str_v = Sem.eval integer_type_widths str_exp mem in
    let num_v = Sem.eval integer_type_widths num_exp mem in
    let traces = Trace.Set.join (Dom.Val.get_traces str_v) (Dom.Val.get_traces num_v) in
    let update_strlen1 allocsite arrinfo acc =
      let strlen =
        let offset = ArrayBlk.ArrInfo.get_offset arrinfo in
        let num = Dom.Val.get_itv num_v in
        Itv.plus offset (Itv.set_lb_zero (Itv.decr num))
      in
      Dom.Mem.set_first_idx_of_null (Loc.of_allocsite allocsite) (Dom.Val.of_itv ~traces strlen) acc
    in
    mem
    |> Dom.Mem.update_mem (Sem.eval_locs str_exp mem) Dom.Val.Itv.zero_255
    |> ArrayBlk.fold update_strlen1 (Dom.Val.get_array_blk str_v)
    |> Dom.Mem.add_stack (Loc.of_id id) {str_v with itv= Itv.zero}
    |> Dom.Mem.fgets_alias id (Dom.Val.get_all_locs str_v)
  and check {location; integer_type_widths} mem cond_set =
    BoUtils.Check.lindex_byte integer_type_widths ~array_exp:str_exp ~byte_index_exp:num_exp
      ~last_included:true mem location cond_set
  in
  {exec; check}


let malloc ~can_be_zero size_exp =
  let exec ({pname; caller_pname; node_hash; location; tenv; integer_type_widths} as model_env)
      ~ret:(id, _) mem =
    let size_exp = BiabductionProp.exp_normalize_noabs tenv size_exp in
    let typ, stride, length0, dyn_length = get_malloc_info size_exp in
    let length = Sem.eval integer_type_widths length0 mem in
    let traces = Trace.(Set.add_elem location ArrayDeclaration) (Dom.Val.get_traces length) in
    let path =
      Dom.Mem.find_simple_alias id mem
      |> List.find_map ~f:(fun (rhs, i) -> if IntLit.iszero i then Loc.get_path rhs else None)
    in
    let offset, size = (Itv.zero, Dom.Val.get_itv length) in
    let represents_multiple_values = not (Itv.is_one size) in
    let allocsite =
      Allocsite.make pname ~caller_pname ~node_hash ~inst_num:0 ~dimension:1 ~path
        ~represents_multiple_values
    in
    let mem =
      if Config.bo_bottom_as_default then mem
      else
        let loc = Loc.of_allocsite allocsite in
        match Dom.Mem.find_opt loc mem with
        | None ->
            (* If the allocsite is not present in the abstract state, write bot as a value of
               allocsite. This effectively means that the first update of allocsite is strong
               (the new value will be joined with bot).
               That is, it relies on the assumption that an allocsite is fully initialized before
               being read (e.g., all elements of an array are initialized before the array is read).*)
            Dom.Mem.strong_update (PowLoc.singleton (Loc.of_allocsite allocsite)) Dom.Val.bot mem
        | _ ->
            (* If the allocsite is already in the abstract state, it means that we are in a loop.
               The abstract allocsite represents multiple concrete allocsites and we need to retain
               values already in the abstract state. The subsequent update of the allocsite will be
               weak (the new value will be joined with the value in the abstract state). *)
            mem
    in
    if Language.curr_language_is Java then
      let internal_arr =
        let allocsite =
          Allocsite.make pname ~caller_pname ~node_hash ~inst_num:1 ~dimension:1 ~path:None
            ~represents_multiple_values
        in
        Dom.Val.of_java_array_alloc allocsite ~length:size ~traces
      in
      let arr_loc = Loc.of_allocsite allocsite in
      mem
      |> Dom.Mem.add_heap arr_loc internal_arr
      |> Dom.Mem.add_stack (Loc.of_id id) (Dom.Val.of_pow_loc ~traces (PowLoc.singleton arr_loc))
    else
      let v = Dom.Val.of_c_array_alloc allocsite ~stride ~offset ~size ~traces in
      mem
      |> Dom.Mem.add_stack (Loc.of_id id) v
      |> BoUtils.Exec.init_c_array_fields model_env path typ (Dom.Val.get_array_locs v) ?dyn_length
  and check = check_alloc_size ~can_be_zero size_exp in
  {exec; check}


let calloc size_exp stride_exp =
  let byte_size_exp = Exp.BinOp (Binop.Mult (Some Typ.size_t), size_exp, stride_exp) in
  malloc byte_size_exp


let memcpy dest_exp src_exp size_exp =
  let exec _ ~ret:_ mem =
    let dest_loc = Sem.eval_locs dest_exp mem in
    let v = Dom.Mem.find_set (Sem.eval_locs src_exp mem) mem in
    Dom.Mem.update_mem dest_loc v mem
  and check {location; integer_type_widths} mem cond_set =
    BoUtils.Check.lindex_byte integer_type_widths ~array_exp:dest_exp ~byte_index_exp:size_exp
      ~last_included:true mem location cond_set
    |> BoUtils.Check.lindex_byte integer_type_widths ~array_exp:src_exp ~byte_index_exp:size_exp
         ~last_included:true mem location
  in
  {exec; check}


let memset arr_exp size_exp =
  let exec _ ~ret:_ mem = mem
  and check {location; integer_type_widths} mem cond_set =
    BoUtils.Check.lindex_byte integer_type_widths ~array_exp:arr_exp ~byte_index_exp:size_exp
      ~last_included:true mem location cond_set
  in
  {exec; check}


let strlen arr_exp =
  let exec _ ~ret:(id, _) mem =
    let v = Sem.eval_string_len arr_exp mem in
    Dom.Mem.add_stack (Loc.of_id id) v mem
  in
  {exec; check= no_check}


let strcpy dest_exp src_exp =
  let exec {integer_type_widths} ~ret:(id, _) mem =
    let src_loc = Sem.eval_locs src_exp mem in
    let dest_loc = Sem.eval_locs dest_exp mem in
    mem
    |> Dom.Mem.update_mem dest_loc (Dom.Mem.find_set src_loc mem)
    |> Dom.Mem.update_mem (PowLoc.of_c_strlen dest_loc) (Dom.Mem.get_c_strlen src_loc mem)
    |> Dom.Mem.add_stack (Loc.of_id id) (Sem.eval integer_type_widths dest_exp mem)
  and check {integer_type_widths; location} mem cond_set =
    let access_last_char =
      let idx = Dom.Mem.get_c_strlen (Sem.eval_locs src_exp mem) mem in
      let latest_prune = Dom.Mem.get_latest_prune mem in
      fun arr cond_set ->
        BoUtils.Check.array_access ~arr ~idx ~is_plus:true ~last_included:false ~latest_prune
          location cond_set
    in
    cond_set
    |> access_last_char (Sem.eval integer_type_widths dest_exp mem)
    |> access_last_char (Sem.eval integer_type_widths src_exp mem)
  in
  {exec; check}


let strncpy dest_exp src_exp size_exp =
  let {exec= memcpy_exec; check= memcpy_check} = memcpy dest_exp src_exp size_exp in
  let exec model_env ~ret mem =
    let dest_strlen_loc = PowLoc.of_c_strlen (Sem.eval_locs dest_exp mem) in
    let strlen = Dom.Mem.find_set (PowLoc.of_c_strlen (Sem.eval_locs src_exp mem)) mem in
    mem |> memcpy_exec model_env ~ret |> Dom.Mem.update_mem dest_strlen_loc strlen
  in
  {exec; check= memcpy_check}


let strcat dest_exp src_exp =
  let exec {integer_type_widths} ~ret:(id, _) mem =
    let src_loc = Sem.eval_locs src_exp mem in
    let dest_loc = Sem.eval_locs dest_exp mem in
    let new_contents =
      let src_contents = Dom.Mem.find_set src_loc mem in
      let dest_contents = Dom.Mem.find_set dest_loc mem in
      Dom.Val.join dest_contents src_contents
    in
    let src_strlen = Dom.Mem.get_c_strlen src_loc mem in
    let new_strlen =
      let dest_strlen = Dom.Mem.get_c_strlen dest_loc mem in
      Dom.Val.plus_a dest_strlen src_strlen
    in
    mem
    |> Dom.Mem.update_mem dest_loc new_contents
    |> Dom.Mem.update_mem (PowLoc.of_c_strlen dest_loc) new_strlen
    |> Dom.Mem.add_stack (Loc.of_id id) (Sem.eval integer_type_widths dest_exp mem)
  and check {integer_type_widths; location} mem cond_set =
    let access_last_char arr idx cond_set =
      let latest_prune = Dom.Mem.get_latest_prune mem in
      BoUtils.Check.array_access ~arr ~idx ~is_plus:true ~last_included:false ~latest_prune location
        cond_set
    in
    let src_strlen =
      let str_loc = Sem.eval_locs src_exp mem in
      Dom.Mem.get_c_strlen str_loc mem
    in
    let new_strlen =
      let dest_strlen =
        let dest_loc = Sem.eval_locs dest_exp mem in
        Dom.Mem.get_c_strlen dest_loc mem
      in
      Dom.Val.plus_a dest_strlen src_strlen
    in
    cond_set
    |> access_last_char (Sem.eval integer_type_widths dest_exp mem) new_strlen
    |> access_last_char (Sem.eval integer_type_widths src_exp mem) src_strlen
  in
  {exec; check}


let realloc src_exp size_exp =
  let exec ({location; tenv; integer_type_widths} as model_env) ~ret:(id, _) mem =
    let size_exp = BiabductionProp.exp_normalize_noabs tenv size_exp in
    let typ, _, length0, dyn_length = get_malloc_info size_exp in
    let length = Sem.eval integer_type_widths length0 mem in
    let v = Sem.eval integer_type_widths src_exp mem |> Dom.Val.set_array_length location ~length in
    let mem = Dom.Mem.add_stack (Loc.of_id id) v mem in
    Option.value_map dyn_length ~default:mem ~f:(fun dyn_length ->
        let dyn_length = Dom.Val.get_itv (Sem.eval integer_type_widths dyn_length mem) in
        BoUtils.Exec.set_dyn_length model_env typ (Dom.Val.get_array_locs v) dyn_length mem )
  and check = check_alloc_size ~can_be_zero:false size_exp in
  {exec; check}


let placement_new size_exp {exp= src_exp1; typ= t1} src_arg2_opt =
  match (t1.Typ.desc, src_arg2_opt) with
  | Tint _, None | Tint _, Some {typ= {Typ.desc= Tint _}} ->
      malloc ~can_be_zero:true (Exp.BinOp (Binop.PlusA (Some Typ.size_t), size_exp, src_exp1))
  | Tstruct (CppClass {name}), None
    when [%equal: string list] (QualifiedCppName.to_list name) ["std"; "nothrow_t"] ->
      malloc ~can_be_zero:true size_exp
  | _, _ ->
      let exec {integer_type_widths} ~ret:(id, _) mem =
        let src_exp =
          if Typ.is_pointer_to_void t1 then src_exp1
          else
            match src_arg2_opt with
            | Some {exp= src_exp2; typ= t2} when Typ.is_pointer_to_void t2 ->
                src_exp2
            | _ ->
                (* TODO: Raise an exception when given unexpected arguments.  Before that, we need
                   to fix the frontend to parse user defined `new` correctly. *)
                L.d_error "Unexpected types of arguments for __placement_new" ;
                src_exp1
        in
        let v = Sem.eval integer_type_widths src_exp mem in
        Dom.Mem.add_stack (Loc.of_id id) v mem
      in
      {exec; check= no_check}


let strndup src_exp length_exp =
  let exec ({pname; caller_pname; node_hash; location; integer_type_widths} as model_env)
      ~ret:((id, _) as ret) mem =
    let v =
      let src_strlen = Dom.Mem.get_c_strlen (Sem.eval_locs src_exp mem) mem in
      let length = Sem.eval integer_type_widths length_exp mem in
      let size = Itv.incr (Itv.min_sem (Dom.Val.get_itv src_strlen) (Dom.Val.get_itv length)) in
      let allocsite =
        let represents_multiple_values = not (Itv.is_one size) in
        Allocsite.make pname ~caller_pname ~node_hash ~inst_num:0 ~dimension:1 ~path:None
          ~represents_multiple_values
      in
      let traces =
        Trace.Set.join (Dom.Val.get_traces src_strlen) (Dom.Val.get_traces length)
        |> Trace.Set.add_elem location Trace.Through
        |> Trace.Set.add_elem location ArrayDeclaration
      in
      Dom.Val.of_c_array_alloc allocsite
        ~stride:(Some (integer_type_widths.char_width / 8))
        ~offset:Itv.zero ~size ~traces
    in
    mem
    |> Dom.Mem.add_stack (Loc.of_id id) v
    |> (strncpy (Exp.Var id) src_exp length_exp).exec model_env ~ret
  in
  {exec; check= no_check}


let set_size {integer_type_widths; location} array_v size_exp mem =
  let locs = Dom.Val.get_pow_loc array_v in
  let length = Sem.eval integer_type_widths size_exp mem in
  Dom.Mem.transform_mem ~f:(Dom.Val.set_array_length location ~length) locs mem


let model_by_value value id mem = Dom.Mem.add_stack (Loc.of_id id) value mem

let cast exp size_exp =
  let exec {integer_type_widths} ~ret:(ret_id, _) mem =
    let v = Sem.eval integer_type_widths exp mem in
    let v = match size_exp with Exp.Sizeof {typ} -> Dom.Val.cast typ v | _ -> v in
    model_by_value v ret_id mem
  in
  {exec; check= no_check}


let id exp =
  let exec {integer_type_widths} ~ret:(ret_id, _) mem =
    let v = Sem.eval integer_type_widths exp mem in
    model_by_value v ret_id mem
  in
  {exec; check= no_check}


let by_value =
  let exec ~value _ ~ret:(ret_id, _) mem = model_by_value value ret_id mem in
  fun value -> {exec= exec ~value; check= no_check}


let bottom =
  let exec _model_env ~ret:_ _mem = Dom.Mem.unreachable in
  {exec; check= no_check}


let infer_print e =
  let exec {location; integer_type_widths} ~ret:_ mem =
    L.(debug BufferOverrun Medium)
      "@[<v>=== Infer Print === at %a@,%a@]%!" Location.pp location Dom.Val.pp
      (Sem.eval integer_type_widths e mem) ;
    mem
  in
  {exec; check= no_check}


let load_size_alias id arr_locs mem =
  match PowLoc.is_singleton_or_more arr_locs with
  | IContainer.Singleton loc ->
      Dom.Mem.load_size_alias id loc mem
  | IContainer.Empty | IContainer.More ->
      mem


(* Java only *)
let get_array_length array_exp =
  let exec _ ~ret:(ret_id, _) mem =
    let arr_locs = Sem.eval_locs array_exp mem in
    let result = Sem.eval_array_locs_length arr_locs mem in
    model_by_value result ret_id mem |> load_size_alias ret_id arr_locs
  in
  {exec; check= no_check}


(* Clang only *)
let set_array_length {exp; typ} length_exp =
  let exec {pname; caller_pname; node_hash; location; integer_type_widths} ~ret:_ mem =
    match (exp, typ) with
    | Exp.Lvar array_pvar, {Typ.desc= Typ.Tarray {stride}} ->
        let length = Sem.eval integer_type_widths length_exp mem in
        let stride = Option.map ~f:IntLit.to_int_exn stride in
        let path = Some (Symb.SymbolPath.of_pvar array_pvar) in
        let traces = Trace.(Set.add_elem location ArrayDeclaration) (Dom.Val.get_traces length) in
        let size = Dom.Val.get_itv length in
        let allocsite =
          let represents_multiple_values = not (Itv.is_one size) in
          Allocsite.make pname ~caller_pname ~node_hash ~inst_num:0 ~dimension:1 ~path
            ~represents_multiple_values
        in
        let v = Dom.Val.of_c_array_alloc allocsite ~stride ~offset:Itv.zero ~size ~traces in
        Dom.Mem.add_stack (Loc.of_pvar array_pvar) v mem
    | _ ->
        L.(die InternalError) "Unexpected type of first argument for __set_array_length() "
  and check = check_alloc_size ~can_be_zero:false length_exp in
  {exec; check}


let copy array_v ret_id mem =
  let dest_loc = Loc.of_id ret_id |> PowLoc.singleton in
  Dom.Mem.update_mem dest_loc array_v mem


(** Returns a fixed-size array with a given length backed by the specified array. *)
let copyOf array_exp length_exp =
  let exec ({integer_type_widths} as model) ~ret:(id, _) mem =
    let array_v = Sem.eval integer_type_widths array_exp mem in
    copy array_v id mem |> set_size model array_v length_exp
  in
  {exec; check= no_check}


(** Creates a new array with the values from the given array.*)
let create_copy_array src_exp =
  let exec {integer_type_widths} ~ret:(id, _) mem =
    let array_v = Sem.eval integer_type_widths src_exp mem in
    copy array_v id mem
  in
  {exec; check= no_check}


module StdArray = struct
  let constructor _size =
    let exec _model_env ~ret:_ mem = mem (* initialize? *) in
    {exec; check= no_check}


  let at size {exp= array_exp} {exp= index_exp} = at ~size array_exp index_exp

  let begin_ _size {exp= array_exp} =
    let exec {location; integer_type_widths} ~ret:(id, _) mem =
      let v =
        Sem.eval integer_type_widths array_exp mem |> Dom.Val.set_array_offset location Itv.zero
      in
      Dom.Mem.add_stack (Loc.of_id id) v mem
    in
    {exec; check= no_check}


  let end_ size {exp= array_exp} =
    let exec {location; integer_type_widths} ~ret:(id, _) mem =
      let v =
        let offset = Itv.of_int_lit (IntLit.of_int64 size) in
        Sem.eval integer_type_widths array_exp mem |> Dom.Val.set_array_offset location offset
      in
      Dom.Mem.add_stack (Loc.of_id id) v mem
    in
    {exec; check= no_check}


  let back size {exp= array_exp} =
    let exec {location; integer_type_widths} ~ret:(id, _) mem =
      let v =
        let offset = Itv.of_int_lit (IntLit.of_int64 Int64.(size - one)) in
        Sem.eval integer_type_widths array_exp mem |> Dom.Val.set_array_offset location offset
      in
      Dom.Mem.add_stack (Loc.of_id id) v mem
    in
    {exec; check= no_check}
end

module Iterator = struct
  (* This is used in C++11 where there is an intermediate step for linking iterator to a temp *)
  let new_ beg_exp temp_exp =
    let exec _ ~ret:_ mem =
      let locs_b = Sem.eval_locs beg_exp mem in
      let locs_t = Sem.eval_locs temp_exp mem in
      let v = Dom.Mem.find_set locs_t mem |> Dom.Val.get_itv |> Dom.Val.of_itv in
      let mem = Dom.Mem.update_mem locs_b v mem in
      match (beg_exp, temp_exp) with
      | Exp.Lvar new_pvar, Exp.Lvar existing_pvar ->
          Dom.Mem.propagate_cpp_iter_begin_or_end_alias ~new_pvar ~existing_pvar mem
      | _ ->
          mem
    in
    {exec; check= no_check}


  let copy dest src =
    let exec _ ~ret:_ mem =
      let dest_loc = Sem.eval_locs dest mem in
      let v = Dom.Mem.find_set (Sem.eval_locs src mem) mem in
      Dom.Mem.update_mem dest_loc v mem
    in
    {exec; check= no_check}


  let begin_ exp =
    let exec _ ~ret:_ mem =
      let locs = Sem.eval_locs exp mem in
      let mem = Dom.Mem.update_mem locs Dom.Val.Itv.zero mem in
      match exp with Exp.Lvar pvar -> Dom.Mem.add_cpp_iter_begin_alias pvar mem | _ -> mem
    in
    {exec; check= no_check}


  let end_ ~size_exec temp_exp =
    let exec model_env ~ret:((ret_id, _) as ret) mem =
      let mem = size_exec model_env ~ret mem in
      let locs = Sem.eval_locs temp_exp mem in
      let v = Dom.Mem.find (Loc.of_id ret_id) mem in
      let mem = Dom.Mem.update_mem locs v mem in
      match temp_exp with Exp.Lvar pvar -> Dom.Mem.add_cpp_iter_end_alias pvar mem | _ -> mem
    in
    {exec; check= no_check}


  let iterator_ne lhs_exp rhs_exp =
    let exec {integer_type_widths} ~ret:(ret_id, _) mem =
      let v = Sem.eval integer_type_widths (Exp.BinOp (Binop.Ne, lhs_exp, rhs_exp)) mem in
      let mem =
        match (lhs_exp, rhs_exp) with
        | Exp.Lvar iter_lhs, Exp.Lvar iter_rhs ->
            Dom.Mem.add_cpp_iterator_cmp_alias ret_id ~iter_lhs ~iter_rhs mem
        | _, _ ->
            mem
      in
      model_by_value v ret_id mem
    in
    {exec; check= no_check}


  let iterator_incr iterator_exp =
    let exec _ ~ret:_ mem =
      let locs = Sem.eval_locs iterator_exp mem in
      let v = Dom.Mem.find_set locs mem |> Dom.Val.get_itv |> Itv.incr |> Dom.Val.of_itv in
      Dom.Mem.update_mem locs v mem
    in
    {exec; check= no_check}
end

module ArrObjCommon = struct
  let deref_of {integer_type_widths} exp ~fn ?fn_typ mem =
    let typ = Option.map fn_typ ~f:Typ.mk_ptr in
    Dom.Val.get_all_locs (Sem.eval_arr integer_type_widths exp mem) |> PowLoc.append_field ?typ ~fn


  let eval_size model_env exp ~fn mem =
    Sem.eval_array_locs_length (deref_of model_env exp ~fn mem) mem


  let size_exec exp ~fn ?fn_typ ({integer_type_widths} as model_env) ~ret:(id, _) mem =
    let locs = Sem.eval integer_type_widths exp mem |> Dom.Val.get_all_locs in
    match PowLoc.is_singleton_or_more locs with
    | Singleton (BoField.Prim (Loc.Allocsite (Allocsite.LiteralString s))) ->
        model_by_value (Dom.Val.of_int (String.length s)) id mem
    | _ ->
        let arr_locs = deref_of model_env exp ~fn ?fn_typ mem in
        let mem = Dom.Mem.add_stack (Loc.of_id id) (Sem.eval_array_locs_length arr_locs mem) mem in
        load_size_alias id arr_locs mem


  let at arr_exp ~fn ?fn_typ index_exp =
    let exec ({pname; location} as model_env) ~ret:(id, typ) mem =
      let array_v =
        let locs = deref_of model_env arr_exp ~fn ?fn_typ mem in
        if PowLoc.is_bot locs then Dom.Val.unknown_from typ ~callee_pname:(Some pname) ~location
        else Dom.Mem.find_set locs mem
      in
      Dom.Mem.add_stack (Loc.of_id id) array_v mem
    and check ({location; integer_type_widths} as model_env) mem cond_set =
      let idx = Sem.eval integer_type_widths index_exp mem in
      let arr = Dom.Mem.find_set (deref_of model_env arr_exp ~fn mem) mem in
      let latest_prune = Dom.Mem.get_latest_prune mem in
      BoUtils.Check.array_access ~arr ~idx ~is_plus:true ~last_included:false ~latest_prune location
        cond_set
    in
    {exec; check}


  let copy_constructor model_env deref_of_tgt ~fn ?fn_typ src_exp mem =
    let deref_of_src = deref_of model_env src_exp ~fn ?fn_typ mem in
    Dom.Mem.update_mem deref_of_tgt (Dom.Mem.find_set deref_of_src mem) mem


  let constructor_from_char_ptr ({integer_type_widths} as model_env) tgt_deref ~fn ?char_typ src mem
      =
    let elem_locs = PowLoc.append_field ?typ:(Option.map char_typ ~f:Typ.mk_ptr) tgt_deref ~fn in
    match src with
    | Exp.Const (Const.Cstr s) ->
        BoUtils.Exec.decl_string model_env ~do_alloc:true elem_locs s mem
    | _ ->
        let v = Sem.eval integer_type_widths src mem in
        Dom.Mem.update_mem elem_locs v mem


  let check_index ~last_included get_array_locs arr_id index_exp {location; integer_type_widths} mem
      cond_set =
    let arr =
      let arr_locs = get_array_locs arr_id mem in
      Dom.Mem.find_set arr_locs mem
    in
    let idx = Sem.eval integer_type_widths index_exp mem in
    let latest_prune = Dom.Mem.get_latest_prune mem in
    BoUtils.Check.array_access ~arr ~idx ~is_plus:true ~last_included ~latest_prune location
      cond_set
end

module StdVector = struct
  let append_field loc ~vec_typ ~elt_typ =
    Loc.append_field ~typ:(Typ.mk_ptr elt_typ) loc (BufferOverrunField.cpp_vector_elem ~vec_typ)


  let append_fields locs ~vec_typ ~elt_typ =
    PowLoc.append_field ~typ:(Typ.mk_ptr elt_typ) locs
      ~fn:(BufferOverrunField.cpp_vector_elem ~vec_typ)


  let deref_of model_env elt_typ {exp= vec_exp; typ= vec_typ} mem =
    let fn = BufferOverrunField.cpp_vector_elem ~vec_typ in
    ArrObjCommon.deref_of model_env vec_exp ~fn ~fn_typ:(Typ.mk_ptr elt_typ) mem


  let set_size {location} locs new_size mem =
    Dom.Mem.transform_mem locs mem ~f:(fun v ->
        Dom.Val.set_array_length location ~length:new_size v )


  (* The (3) constructor in https://en.cppreference.com/w/cpp/container/vector/vector *)
  let constructor_size elt_typ {exp= vec_exp; typ= vec_typ} size_exp =
    let {exec= malloc_exec; check} = malloc ~can_be_zero:true size_exp in
    let exec ({pname; caller_pname; node_hash; integer_type_widths; location} as model_env)
        ~ret:((id, _) as ret) mem =
      let mem = malloc_exec model_env ~ret mem in
      let vec_locs = Sem.eval_locs vec_exp mem in
      let deref_of_vec =
        Allocsite.make pname ~caller_pname ~node_hash ~inst_num:1 ~dimension:1 ~path:None
          ~represents_multiple_values:false
        |> Loc.of_allocsite
      in
      let array_v =
        Sem.eval integer_type_widths (Exp.Var id) mem
        |> Dom.Val.add_assign_trace_elem location vec_locs
      in
      mem
      |> Dom.Mem.update_mem vec_locs (Dom.Val.of_loc deref_of_vec)
      |> Dom.Mem.add_heap (append_field deref_of_vec ~vec_typ ~elt_typ) array_v
    in
    {exec; check}


  (* The (1) constructor in https://en.cppreference.com/w/cpp/container/vector/vector *)
  let constructor_empty elt_typ vec = constructor_size elt_typ vec Exp.zero

  (* The (5) constructor in https://en.cppreference.com/w/cpp/container/vector/vector *)
  let constructor_copy elt_typ {exp= vec_exp; typ= vec_typ} src_exp =
    let exec ({integer_type_widths} as model_env) ~ret:_ mem =
      let vec_locs, traces =
        let v = Sem.eval integer_type_widths vec_exp mem in
        (Dom.Val.get_all_locs v, Dom.Val.get_traces v)
      in
      let deref_of_vec = append_fields vec_locs ~vec_typ ~elt_typ in
      let fn = BufferOverrunField.cpp_vector_elem ~vec_typ in
      mem
      |> Dom.Mem.update_mem vec_locs (Dom.Val.of_pow_loc ~traces deref_of_vec)
      |> ArrObjCommon.copy_constructor model_env deref_of_vec ~fn ~fn_typ:(Typ.mk_ptr elt_typ)
           src_exp
    in
    {exec; check= no_check}


  (* The (10) constructor in https://en.cppreference.com/w/cpp/container/vector/vector *)
  let constructor_initializer_list elt_typ {exp= vec_exp; typ= vec_typ} lst_exp =
    let exec {pname; caller_pname; node_hash; location} ~ret:_ mem =
      let arr_blk =
        let internal_locs =
          Sem.eval_locs lst_exp mem |> PowLoc.append_field ~fn:BoField.cpp_collection_internal_array
        in
        Dom.Mem.find_set internal_locs mem |> Dom.Val.get_array_blk
      in
      let deref_of_vec =
        Allocsite.make pname ~caller_pname ~node_hash ~inst_num:1 ~dimension:1 ~path:None
          ~represents_multiple_values:false
        |> Loc.of_allocsite
      in
      let mem =
        let vec_locs = Sem.eval_locs vec_exp mem in
        Dom.Mem.update_mem vec_locs (Dom.Val.of_loc deref_of_vec) mem
      in
      let vec_arr_ploc = append_field deref_of_vec ~vec_typ ~elt_typ |> PowLoc.singleton in
      let traces = Trace.Set.singleton location ArrayDeclaration in
      let arr_as =
        Allocsite.make pname ~caller_pname ~node_hash ~inst_num:0 ~dimension:1 ~path:None
          ~represents_multiple_values:true
      in
      let v =
        Dom.Val.of_c_array_alloc arr_as ~stride:None ~offset:Itv.zero
          ~size:(ArrayBlk.get_size arr_blk) ~traces
      in
      let vec_elt_locs = arr_as |> Loc.of_allocsite |> PowLoc.singleton in
      let lst_elem_v = Dom.Mem.find_set (ArrayBlk.get_pow_loc arr_blk) mem in
      mem |> Dom.Mem.strong_update vec_arr_ploc v |> Dom.Mem.update_mem vec_elt_locs lst_elem_v
    in
    {exec; check= no_check}


  let at elt_typ {exp= vec_exp; typ= vec_typ} index_exp =
    ArrObjCommon.at vec_exp
      ~fn:(BufferOverrunField.cpp_vector_elem ~vec_typ)
      ~fn_typ:(Typ.mk_ptr elt_typ) index_exp


  let empty elt_typ vec_arg =
    let exec model_env ~ret:(id, _) mem =
      let deref_of_vec = deref_of model_env elt_typ vec_arg mem in
      let array_v = Dom.Mem.find_set deref_of_vec mem in
      let traces = Dom.Val.get_traces array_v in
      let size = ArrayBlk.get_size (Dom.Val.get_array_blk array_v) in
      let empty = Dom.Val.of_itv ~traces (Itv.of_bool (Itv.le_sem size Itv.zero)) in
      let mem = model_by_value empty id mem in
      match PowLoc.is_singleton_or_more deref_of_vec with
      | IContainer.Singleton loc ->
          Dom.Mem.load_empty_alias id loc mem
      | IContainer.(Empty | More) ->
          mem
    in
    {exec; check= no_check}


  let data elt_typ vec_arg =
    let exec model_env ~ret:(id, _) mem =
      let arr = Dom.Mem.find_set (deref_of model_env elt_typ vec_arg mem) mem in
      model_by_value arr id mem
    in
    {exec; check= no_check}


  let add_n_element ~n elt_typ vec_arg elt_exp =
    let exec ({integer_type_widths} as model_env) ~ret:_ mem =
      let arr_locs = deref_of model_env elt_typ vec_arg mem in
      let mem =
        let new_size =
          Sem.eval integer_type_widths n mem
          |> Dom.Val.plus_a (Sem.eval_array_locs_length arr_locs mem)
        in
        set_size model_env arr_locs new_size mem
      in
      let elt_locs = Dom.Val.get_all_locs (Dom.Mem.find_set arr_locs mem) in
      let elt_v = Dom.Mem.find_set (Sem.eval_locs elt_exp mem) mem in
      Dom.Mem.update_mem elt_locs elt_v mem
    in
    {exec; check= no_check}


  let push_back = add_n_element ~n:Exp.one

  let insert_n elt_typ vec_arg n elt_exp = add_n_element ~n elt_typ vec_arg elt_exp

  let insert_initializer_list elt_typ vec_arg lst_exp =
    let exec model_env ~ret:_ mem =
      let arr_locs = deref_of model_env elt_typ vec_arg mem in
      let elt_locs = Dom.Val.get_all_locs (Dom.Mem.find_set arr_locs mem) in
      let internal_arr =
        let internal_array_locs =
          Sem.eval_locs lst_exp mem |> PowLoc.append_field ~fn:BoField.cpp_collection_internal_array
        in
        Dom.Mem.find_set internal_array_locs mem
      in
      let arr_blk = Dom.Val.get_array_blk internal_arr in
      let lst_elet = Dom.Mem.find_set (ArrayBlk.get_pow_loc arr_blk) mem in
      let add_cnt = arr_blk |> ArrayBlk.get_size |> Dom.Val.of_itv in
      let new_size = add_cnt |> Dom.Val.plus_a (Sem.eval_array_locs_length arr_locs mem) in
      mem |> set_size model_env arr_locs new_size |> Dom.Mem.update_mem elt_locs lst_elet
    in
    {exec; check= no_check}


  let size elt_typ {exp= vec_exp; typ= vec_typ} =
    let exec =
      ArrObjCommon.size_exec vec_exp
        ~fn:(BufferOverrunField.cpp_vector_elem ~vec_typ)
        ~fn_typ:(Typ.mk_ptr elt_typ)
    in
    {exec; check= no_check}


  let resize elt_typ vec_arg size_exp =
    let exec ({integer_type_widths} as model_env) ~ret:_ mem =
      let arr_locs = deref_of model_env elt_typ vec_arg mem in
      let new_size = Sem.eval integer_type_widths size_exp mem in
      set_size model_env arr_locs new_size mem
    in
    {exec; check= no_check}


  module Iterator = struct
    let end_ elt_typ vec temp_exp = Iterator.end_ ~size_exec:(size elt_typ vec).exec temp_exp
  end
end

module Split = struct
  let std_vector model_env ~adds_at_least_one ({typ= vector_typ} as vec_arg) mem =
    match vector_typ with
    | Typ.
        { desc=
            Tptr
              ( {desc= Tstruct (CppClass {template_spec_info= Template {args= TType elt_typ :: _}})}
              , _ ) } ->
        let arr_locs = StdVector.deref_of model_env elt_typ vec_arg mem in
        let size = if adds_at_least_one then Dom.Val.Itv.pos else Dom.Val.Itv.nat in
        StdVector.set_size model_env arr_locs size mem
    | _ ->
        Logging.d_printfln_escaped "Could not find element type of vector" ;
        mem
end

module Boost = struct
  module Split = struct
    let std_vector vector_arg =
      let exec model_env ~ret:_ mem =
        Split.std_vector model_env ~adds_at_least_one:true vector_arg mem
      in
      {exec; check= no_check}
  end
end

module Folly = struct
  module Split = struct
    let std_vector vector_arg ignore_empty_opt =
      let exec ({integer_type_widths} as model_env) ~ret:_ mem =
        let adds_at_least_one =
          match ignore_empty_opt with
          | Some ignore_empty_exp ->
              Sem.eval integer_type_widths ignore_empty_exp mem |> Dom.Val.get_itv |> Itv.is_false
          | None ->
              (* default: ignore_empty is false *)
              true
        in
        Split.std_vector model_env ~adds_at_least_one vector_arg mem
      in
      {exec; check= no_check}
  end
end

module StdBasicString = struct
  let constructor_from_char_ptr char_typ {exp= tgt_exp; typ= tgt_typ} src ~len_opt =
    let exec ({pname; caller_pname; node_hash} as model_env) ~ret mem =
      let mem =
        Option.value_map len_opt ~default:mem ~f:(fun len ->
            let {exec= malloc_exec} = malloc ~can_be_zero:true len in
            malloc_exec model_env ~ret mem )
      in
      let tgt_locs = Sem.eval_locs tgt_exp mem in
      let tgt_deref =
        let allocsite =
          Allocsite.make pname ~caller_pname ~node_hash ~inst_num:1 ~dimension:1 ~path:None
            ~represents_multiple_values:false
        in
        PowLoc.singleton (Loc.of_allocsite allocsite)
      in
      let mem =
        Dom.Mem.update_mem tgt_locs (Dom.Val.of_pow_loc ~traces:Trace.Set.bottom tgt_deref) mem
      in
      let fn = BufferOverrunField.cpp_vector_elem ~vec_typ:tgt_typ in
      ArrObjCommon.constructor_from_char_ptr model_env tgt_deref src ~fn ~char_typ mem
    in
    let check ({location; integer_type_widths} as model_env) mem cond_set =
      Option.value_map len_opt ~default:cond_set ~f:(fun len ->
          let {check= malloc_check} = malloc ~can_be_zero:true len in
          let cond_set = malloc_check model_env mem cond_set in
          BoUtils.Check.lindex integer_type_widths ~array_exp:src ~index_exp:len ~last_included:true
            mem location cond_set )
    in
    {exec; check}


  (* The (4) constructor in https://en.cppreference.com/w/cpp/string/basic_string/basic_string *)
  let constructor_from_char_ptr_with_len char_typ tgt_arg src len =
    constructor_from_char_ptr char_typ tgt_arg src ~len_opt:(Some len)


  (* The (5) constructor in https://en.cppreference.com/w/cpp/string/basic_string/basic_string *)
  let constructor_from_char_ptr_without_len char_typ tgt_arg src =
    constructor_from_char_ptr char_typ tgt_arg src ~len_opt:None


  (* The (7) constructor in https://en.cppreference.com/w/cpp/string/basic_string/basic_string *)
  let copy_constructor = StdVector.constructor_copy

  let empty = StdVector.empty

  let length = StdVector.size
end

(** Java's integers are modeled with an indirection to a memory location that holds the actual
    integer value *)
module JavaInteger = struct
  let intValue exp =
    let exec _ ~ret:(id, _) mem =
      let powloc = Sem.eval_locs exp mem in
      let v = if PowLoc.is_bot powloc then Dom.Val.Itv.top else Dom.Mem.find_set powloc mem in
      model_by_value v id mem
    in
    {exec; check= no_check}


  let valueOf exp =
    let exec {pname; caller_pname; node_hash; location; integer_type_widths} ~ret:(id, _) mem =
      let represents_multiple_values = false in
      let int_allocsite =
        Allocsite.make pname ~caller_pname ~node_hash ~inst_num:0 ~dimension:0 ~path:None
          ~represents_multiple_values
      in
      let v = Sem.eval integer_type_widths exp mem in
      let int_loc = Loc.of_allocsite int_allocsite in
      mem |> Dom.Mem.add_heap int_loc v
      |> Dom.Mem.add_stack (Loc.of_id id)
           ( int_loc |> PowLoc.singleton
           |> Dom.Val.of_pow_loc ~traces:Trace.(Set.singleton location JavaIntDecleration) )
    in
    {exec; check= no_check}
end

module type Lang = sig
  val collection_internal_array_field : Fieldname.t
end

(* Abstract Collections are represented like arrays. But we don't care about the elements.
   - when they are constructed, we set the size to 0
   - each time we add an element, we increase the length of the array
   - each time we delete an element, we decrease the length of the array *)
module AbstractCollection (Lang : Lang) = struct
  let create_collection {pname; caller_pname; node_hash; location} ~ret:(id, _) mem ~length =
    let represents_multiple_values = true in
    let traces = Trace.(Set.singleton location ArrayDeclaration) in
    let coll_allocsite =
      Allocsite.make pname ~caller_pname ~node_hash ~inst_num:0 ~dimension:1 ~path:None
        ~represents_multiple_values
    in
    let internal_array =
      let allocsite =
        Allocsite.make pname ~caller_pname ~node_hash ~inst_num:1 ~dimension:1 ~path:None
          ~represents_multiple_values
      in
      Dom.Val.of_java_array_alloc allocsite ~length ~traces
    in
    let coll_loc = Loc.of_allocsite coll_allocsite in
    let internal_array_loc = Loc.append_field coll_loc Lang.collection_internal_array_field in
    mem
    |> Dom.Mem.add_heap internal_array_loc internal_array
    |> Dom.Mem.add_stack (Loc.of_id id) (coll_loc |> PowLoc.singleton |> Dom.Val.of_pow_loc ~traces)


  let new_collection =
    let exec = create_collection ~length:Itv.zero in
    {exec; check= no_check}


  let allocate size_exp =
    let exec ({integer_type_widths} as env) ~ret mem =
      let length = Sem.eval integer_type_widths size_exp mem |> Dom.Val.get_itv in
      create_collection env ~ret mem ~length
    in
    {exec; check= no_check}


  let eval_collection_internal_array_locs coll_exp mem =
    Sem.eval_locs coll_exp mem |> PowLoc.append_field ~fn:Lang.collection_internal_array_field


  let get_collection_internal_array_locs coll_id mem =
    let coll = Dom.Mem.find (Loc.of_id coll_id) mem in
    Dom.Val.get_all_locs coll |> PowLoc.append_field ~fn:Lang.collection_internal_array_field


  let get_collection_internal_elements_locs coll_id mem =
    let arr_locs = get_collection_internal_array_locs coll_id mem in
    Dom.Mem.find_set arr_locs mem |> Dom.Val.get_all_locs


  let eval_collection_length coll_exp mem =
    let arr_locs = eval_collection_internal_array_locs coll_exp mem in
    Sem.eval_array_locs_length arr_locs mem


  let change_size_by ~size_f coll_id {location} ~ret:_ mem =
    let arr_locs = get_collection_internal_array_locs coll_id mem in
    let mem = Dom.Mem.forget_size_alias arr_locs mem in
    Dom.Mem.transform_mem ~f:(Dom.Val.transform_array_length location ~f:size_f) arr_locs mem


  let change_size_by_incr coll_id {location} ~ret:_ mem =
    let arr_locs = get_collection_internal_array_locs coll_id mem in
    Dom.Mem.transform_mem ~f:(Dom.Val.transform_array_length location ~f:Itv.incr) arr_locs mem
    |> Dom.Mem.incr_size_alias arr_locs


  let change_size_by_incr_or_not coll_id {location} ~ret:_ mem =
    let arr_locs = get_collection_internal_array_locs coll_id mem in
    Dom.Mem.transform_mem
      ~f:(Dom.Val.transform_array_length location ~f:(Itv.plus Itv.zero_one))
      arr_locs mem
    |> Dom.Mem.incr_or_not_size_alias arr_locs


  let set_elem_exec {integer_type_widths} coll_id elem_exp mem =
    let locs = get_collection_internal_elements_locs coll_id mem in
    let v = Sem.eval integer_type_widths elem_exp mem in
    Dom.Mem.update_mem locs v mem


  let add coll_id elem_exp =
    let exec env ~ret mem =
      change_size_by_incr coll_id env ~ret mem |> set_elem_exec env coll_id elem_exp
    in
    {exec; check= no_check}


  let of_list list =
    let exec env ~ret:((id, _) as ret) mem =
      let mem = new_collection.exec env ~ret mem in
      List.fold_left list ~init:mem ~f:(fun acc {exp= elem_exp} ->
          (add id elem_exp).exec env ~ret acc )
    in
    {exec; check= no_check}


  let singleton_collection =
    let exec env ~ret:((id, _) as ret) mem =
      let {exec= new_exec; check= _} = new_collection in
      let mem = new_exec env ~ret mem in
      change_size_by_incr id ~ret env mem
    in
    {exec; check= no_check}


  (** increase the size by [0, 1] because put replaces the value rather than add a new one when the
      key is found in the map *)
  let put coll_id = {exec= change_size_by_incr_or_not coll_id; check= no_check}

  let put_with_elem coll_id elem_exp =
    let put_model = put coll_id in
    let exec env ~ret mem = put_model.exec env ~ret mem |> set_elem_exec env coll_id elem_exp in
    {put_model with exec}


  (* The return value is set by [set_itv_updated_by_addition] in order to be sure that it can be
     used as a control variable value in the cost checker. *)
  let size coll_exp =
    let exec _ ~ret:(ret_id, _) mem =
      let result = eval_collection_length coll_exp mem |> Dom.Val.set_itv_updated_by_addition in
      let mem = model_by_value result ret_id mem in
      load_size_alias ret_id (eval_collection_internal_array_locs coll_exp mem) mem
    in
    {exec; check= no_check}


  let iterator coll_exp =
    let exec {integer_type_widths} ~ret:(ret_id, _) mem =
      let itr = Sem.eval integer_type_widths coll_exp mem in
      model_by_value itr ret_id mem |> Dom.Mem.add_iterator_alias ret_id
    in
    {exec; check= no_check}


  let init_with_capacity size_exp =
    let exec _ ~ret:_ mem = mem and check = check_alloc_size ~can_be_zero:true size_exp in
    {exec; check}


  let init_with_arg lhs_id rhs_exp =
    let exec model_env ~ret mem =
      let length = eval_collection_length rhs_exp mem |> Dom.Val.get_itv in
      change_size_by ~size_f:(fun _ -> length) lhs_id model_env ~ret mem
    and check = check_alloc_size ~can_be_zero:true rhs_exp in
    {exec; check}


  (* The return value is set by [set_itv_updated_by_addition] in order to be sure that it can be
     used as a control variable value in the cost checker. *)
  let hasNext iterator =
    let exec _ ~ret:(ret_id, _) mem =
      (* Set the size of the iterator to be [0, size], so that range
         will be size of the collection. *)
      let collection_size =
        let arr_locs = eval_collection_internal_array_locs iterator mem in
        Sem.conservative_array_length arr_locs mem
      in
      model_by_value collection_size ret_id mem
      |> Dom.Mem.add_iterator_has_next_alias ret_id iterator
    in
    {exec; check= no_check}


  let get_elem iterator =
    let exec {integer_type_widths} ~ret:(id, _) mem =
      let traces = Sem.eval integer_type_widths iterator mem |> Dom.Val.get_traces in
      let locs = eval_collection_internal_array_locs iterator mem in
      model_by_value (Dom.Val.of_pow_loc ~traces locs) id mem
    in
    {exec; check= no_check}


  let next iterator =
    let exec {integer_type_widths} ~ret:(id, _) mem =
      let traces = Sem.eval integer_type_widths iterator mem |> Dom.Val.get_traces in
      let locs = eval_collection_internal_array_locs iterator mem in
      model_by_value (Dom.Val.of_pow_loc ~traces locs) id mem
      |> Dom.Mem.incr_iterator_offset_alias iterator
    in
    {exec; check= no_check}


  let addAll coll_id coll_to_add =
    let exec model_env ~ret mem =
      let to_add_length = eval_collection_length coll_to_add mem |> Dom.Val.get_itv in
      change_size_by ~size_f:(Itv.plus to_add_length) coll_id model_env ~ret mem
    in
    {exec; check= no_check}


  (** Returns a view of the portion of this list between the specified fromIndex, inclusive, and
      toIndex, exclusive. Simply model it as creating a new list with length toIndex - fromIndex. *)
  let subList from_exp to_exp =
    let exec ({integer_type_widths} as model) ~ret mem =
      let from_idx = Sem.eval integer_type_widths from_exp mem in
      let to_idx = Sem.eval integer_type_widths to_exp mem in
      let length = Itv.minus (Dom.Val.get_itv to_idx) (Dom.Val.get_itv from_idx) in
      create_collection model ~ret mem ~length
    in
    {exec; check= no_check}


  (** increase the size by [0, |collection_to_add|] because put replaces the value rather than add a
      new one when the key is found in the map *)
  let putAll coll_id coll_to_add =
    let exec model_env ~ret mem =
      let to_add_length =
        eval_collection_length coll_to_add mem |> Dom.Val.get_itv |> Itv.set_lb_zero
      in
      change_size_by ~size_f:(Itv.plus to_add_length) coll_id model_env ~ret mem
    in
    {exec; check= no_check}


  let check_index ~last_included arr_id index_exp =
    ArrObjCommon.check_index ~last_included get_collection_internal_array_locs arr_id index_exp


  let add_at_index (coll_id : Ident.t) index_exp =
    {exec= change_size_by_incr coll_id; check= check_index ~last_included:true coll_id index_exp}


  let remove_at_index coll_id index_exp =
    { exec= change_size_by ~size_f:Itv.decr_length coll_id
    ; check= check_index ~last_included:false coll_id index_exp }


  let addAll_at_index coll_id index_exp coll_to_add =
    let exec model_env ~ret mem =
      let to_add_length = eval_collection_length coll_to_add mem |> Dom.Val.get_itv in
      change_size_by ~size_f:(Itv.plus to_add_length) coll_id model_env ~ret mem
    in
    {exec; check= check_index ~last_included:true coll_id index_exp}


  let set_at_index coll_id index_exp elem_exp =
    let exec env ~ret:_ mem = set_elem_exec env coll_id elem_exp mem in
    {exec; check= check_index ~last_included:false coll_id index_exp}


  let get_any_index coll_id =
    let exec _model_env ~ret:(ret_id, _) mem =
      let locs = get_collection_internal_elements_locs coll_id mem in
      let v = Dom.Mem.find_set locs mem in
      model_by_value v ret_id mem
    in
    {exec; check= no_check}


  let get_at_index coll_id index_exp =
    let {exec} = get_any_index coll_id in
    {exec; check= check_index ~last_included:false coll_id index_exp}
end

module Collection = AbstractCollection (struct
  let collection_internal_array_field = BufferOverrunField.java_collection_internal_array
end)

module Container = struct
  include AbstractCollection (struct
    let collection_internal_array_field = BufferOverrunField.cpp_collection_internal_array
  end)

  let integer_size size =
    let exec _ ~ret:(ret_id, _) mem =
      let size_v = IntLit.of_int64 size |> Itv.of_int_lit |> Dom.Val.of_itv in
      model_by_value size_v ret_id mem
    in
    {exec; check= no_check}


  let constructor_size {exp= vec_exp} size_exp =
    let exec ({pname; caller_pname; node_hash; integer_type_widths; location} as model_env)
        ~ret:((id, _) as ret) mem =
      let mem = (malloc ~can_be_zero:true size_exp).exec model_env ~ret mem in
      let vec_locs = Sem.eval_locs vec_exp mem in
      let deref_of_vec =
        Allocsite.make pname ~caller_pname ~node_hash ~inst_num:0 ~dimension:1 ~path:None
          ~represents_multiple_values:false
        |> Loc.of_allocsite
      in
      let array_v =
        Sem.eval integer_type_widths (Exp.Var id) mem
        |> Dom.Val.add_assign_trace_elem location vec_locs
      in
      let internal_array_loc =
        PowLoc.append_field vec_locs ~fn:BufferOverrunField.cpp_collection_internal_array
      in
      mem
      |> Dom.Mem.update_mem vec_locs (Dom.Val.of_loc deref_of_vec)
      |> Dom.Mem.add_heap_set internal_array_loc array_v
    in
    {exec; check= no_check}


  let constructor_empty vec = constructor_size vec Exp.zero

  module Iterator = struct
    let end_ vec temp_exp = Iterator.end_ ~size_exec:(size vec).exec temp_exp
  end
end

module StdInitializerList = struct
  let create_lst {pname; caller_pname; node_hash; location} ~ret:(id, _) mem arrblk ~arr =
    let represents_multiple_values = true in
    let traces = Trace.(Set.singleton location ArrayDeclaration) in
    let lst_allocsite =
      Allocsite.make pname ~caller_pname ~node_hash ~inst_num:0 ~dimension:1 ~path:None
        ~represents_multiple_values
    in
    let lst_loc = Loc.of_allocsite lst_allocsite in
    let internal_array_loc =
      Loc.append_field lst_loc BufferOverrunField.cpp_collection_internal_array
    in
    let arr_as =
      Allocsite.make pname ~caller_pname ~node_hash ~inst_num:1 ~dimension:1 ~path:None
        ~represents_multiple_values
    in
    let internal_array =
      let offset = ArrayBlk.get_offset arrblk in
      let size = ArrayBlk.get_size arrblk in
      let stride = None in
      Dom.Val.of_c_array_alloc arr_as ~offset ~size ~stride ~traces
    in
    mem
    |> Dom.Mem.add_heap (Loc.of_allocsite arr_as) arr
    |> Dom.Mem.add_heap internal_array_loc internal_array
    |> Dom.Mem.add_stack (Loc.of_id id) (lst_loc |> PowLoc.singleton |> Dom.Val.of_pow_loc ~traces)


  let constructor exp =
    let exec ({integer_type_widths} as model_env) ~ret mem =
      let v = Sem.eval integer_type_widths exp mem in
      let arr = Dom.Mem.find_set (Dom.Val.get_all_locs v) mem in
      create_lst model_env ~ret mem (Dom.Val.get_array_blk v) ~arr
    in
    {exec; check= no_check}
end

module NSCollection = struct
  include AbstractCollection (struct
    let collection_internal_array_field = BufferOverrunField.objc_collection_internal_array
  end)

  let create_collection {pname; caller_pname; node_hash; location; integer_type_widths}
      ~ret:(coll_id, _) mem ~size_exp =
    let represents_multiple_values = true in
    let _, stride, length0, _ = get_malloc_info size_exp in
    let length = Sem.eval integer_type_widths length0 mem in
    let traces = Trace.(Set.add_elem location ArrayDeclaration) (Dom.Val.get_traces length) in
    let internal_array =
      let allocsite =
        Allocsite.make pname ~caller_pname ~node_hash ~inst_num:1 ~dimension:1 ~path:None
          ~represents_multiple_values
      in
      let offset, size = (Itv.zero, Dom.Val.get_itv length) in
      Dom.Val.of_c_array_alloc allocsite ~stride ~offset ~size ~traces
    in
    let coll_allocsite =
      Allocsite.make pname ~caller_pname ~node_hash ~inst_num:0 ~dimension:1 ~path:None
        ~represents_multiple_values
    in
    let coll_loc = Loc.of_allocsite coll_allocsite in
    let internal_array_loc =
      Loc.append_field coll_loc BufferOverrunField.objc_collection_internal_array
    in
    mem
    |> Dom.Mem.add_heap internal_array_loc internal_array
    |> Dom.Mem.add_stack (Loc.of_id coll_id)
         (coll_loc |> PowLoc.singleton |> Dom.Val.of_pow_loc ~traces)


  (** Creates a new array from the given c array by copying the first X elements. *)
  let create_from_array src_exp size_exp =
    let exec model_env ~ret:((id, _) as ret) mem =
      let src_arr_v = Dom.Mem.find_set (Sem.eval_locs src_exp mem) mem in
      let mem = create_collection model_env ~ret mem ~size_exp in
      let dest_arr_loc = get_collection_internal_elements_locs id mem in
      Dom.Mem.update_mem dest_arr_loc src_arr_v mem
    and check {location; integer_type_widths} mem cond_set =
      BoUtils.Check.lindex integer_type_widths ~array_exp:src_exp ~index_exp:size_exp
        ~last_included:true mem location cond_set
    in
    {exec; check}


  let new_collection =
    let exec = create_collection ~size_exp:Exp.zero in
    {exec; check= no_check}


  let new_collection_of_size coll_id size_exp =
    let exec ({integer_type_widths} as model_env) ~ret:((id, _) as ret) mem =
      let coll = Dom.Mem.find_stack (Loc.of_id coll_id) mem in
      let to_add_length = Sem.eval integer_type_widths size_exp mem |> Dom.Val.get_itv in
      change_size_by ~size_f:(fun _ -> to_add_length) coll_id model_env ~ret mem
      |> model_by_value coll id
    in
    {exec; check= no_check}


  let new_collection_by_add_all coll_exp1 coll_exp2 =
    let exec model_env ~ret:((ret_id, _) as ret) mem =
      create_collection model_env ~ret mem ~size_exp:Exp.zero
      |> (addAll ret_id coll_exp1).exec model_env ~ret
      |> (addAll ret_id coll_exp2).exec model_env ~ret
    in
    {exec; check= no_check}


  let get_first coll_id = get_at_index coll_id Exp.zero

  let remove_last coll_id = {exec= change_size_by ~size_f:Itv.decr_length coll_id; check= no_check}

  let remove_all coll_id =
    let exec model_env ~ret mem =
      change_size_by ~size_f:(fun _ -> Itv.zero) coll_id model_env ~ret mem
    in
    {exec; check= no_check}


  let of_list list =
    let exec env ~ret:((id, _) as ret) mem =
      let mem = new_collection.exec env ~ret mem in
      List.fold_left list ~init:mem ~f:(fun acc {exp= elem_exp} ->
          (add id elem_exp).exec env ~ret acc )
    in
    {exec; check= no_check}


  let copy coll_id src_exp =
    let exec model_env ~ret:((id, _) as ret) mem =
      let coll = Dom.Mem.find_stack (Loc.of_id coll_id) mem in
      let set_length = eval_collection_length src_exp mem |> Dom.Val.get_itv in
      change_size_by ~size_f:(fun _ -> set_length) coll_id model_env ~ret mem
      |> model_by_value coll id
    in
    {exec; check= no_check}


  let new_collection_by_init coll_exp =
    let exec model_env ~ret:((ret_id, _) as ret) mem =
      create_collection model_env ~ret mem ~size_exp:Exp.zero
      |> (addAll ret_id coll_exp).exec model_env ~ret
    in
    {exec; check= no_check}


  let iterator coll_exp =
    let exec {integer_type_widths= _; location} ~ret:(id, _) mem =
      let elements_locs = eval_collection_internal_array_locs coll_exp mem in
      let v = Dom.Mem.find_set elements_locs mem |> Dom.Val.set_array_offset location Itv.zero in
      model_by_value v id mem
    in
    {exec; check= no_check}


  let next_object iterator =
    let exec _ ~ret:(ret_id, _) mem =
      let iterator_locs =
        Dom.Mem.find_simple_alias iterator mem
        |> List.filter_map ~f:(fun (l, i) -> if IntLit.iszero i then Some l else None)
        |> PowLoc.of_list
      in
      let iterator_v = Dom.Mem.find_set iterator_locs mem in
      let arrayblk = ArrayBlk.plus_offset (Dom.Val.get_array_blk iterator_v) Itv.one in
      let iterator_v' = {iterator_v with arrayblk} in
      let return_v =
        (* NOTE: It joins zero to avoid unreachable nodes due to the following hack.  The
           [nextObject] returns the offset of the enumerator instead of an object elements,
           which helps the cost checker select the offset as a control variable, for example,
           [while(obj = [enumerator nextObject]) { ... }]. *)
        ArrayBlk.get_offset ~cost_mode:true arrayblk |> Itv.join Itv.zero |> Dom.Val.of_itv
      in
      model_by_value return_v ret_id mem
      |> Dom.Mem.add_heap_set iterator_locs iterator_v'
      |> Dom.Mem.add_iterator_next_object_alias ~ret_id ~iterator
    in
    {exec; check= no_check}
end

module NSURL = struct
  let get_resource_value =
    let exec _model_env ~ret:(id, _) mem = model_by_value (Dom.Val.of_itv Itv.zero_one) id mem in
    {exec; check= no_check}
end

module JavaClass = struct
  let decl_array {pname; caller_pname; node_hash; location} ~ret:(ret_id, _) length mem =
    let loc =
      Allocsite.make pname ~caller_pname ~node_hash ~inst_num:0 ~dimension:1 ~path:None
        ~represents_multiple_values:true
      |> Loc.of_allocsite
    in
    let arr_v =
      let allocsite =
        Allocsite.make pname ~caller_pname ~node_hash ~inst_num:1 ~dimension:1 ~path:None
          ~represents_multiple_values:true
      in
      let traces = Trace.(Set.singleton location ArrayDeclaration) in
      Dom.Val.of_java_array_alloc allocsite ~length ~traces
    in
    Dom.Mem.add_heap loc arr_v mem |> model_by_value (Dom.Val.of_loc loc) ret_id


  let get_fields class_name_exp =
    let exec ({tenv} as model_env) ~ret mem =
      match class_name_exp with
      | Exp.Const (Const.Cclass name) -> (
          let typ_name = Typ.Name.Java.from_string (Ident.name_to_string name) in
          match Tenv.lookup tenv typ_name with
          | Some {fields} ->
              decl_array model_env ~ret (List.length fields |> Itv.of_int) mem
          | None ->
              Logging.d_printfln_escaped "Could not find class from tenv" ;
              mem )
      | _ ->
          Logging.d_printfln_escaped "Parameter is not a class name constant" ;
          mem
    in
    {exec; check= no_check}


  let get_enum_constants class_name_exp =
    let exec ({get_summary} as model_env) ~ret mem =
      match class_name_exp with
      | Exp.Const (Const.Cclass name) -> (
          let enum_values_pname =
            let class_name_str = Ident.name_to_string name in
            let class_name = Typ.JavaClass (JavaClassName.from_string class_name_str) in
            Procname.make_java
              ~class_name:(Typ.Name.Java.from_string class_name_str)
              ~return_type:(Some Typ.(mk_ptr (mk_array (mk_ptr (mk_struct class_name)))))
              ~method_name:"values" ~parameters:[] ~kind:Procname.Java.Static
          in
          match get_summary enum_values_pname with
          | Some enum_values_mem ->
              let length =
                let ret_loc = Loc.of_pvar (Pvar.get_ret_pvar enum_values_pname) in
                let ret_v = Dom.Mem.find ret_loc enum_values_mem in
                Dom.Mem.find_set (Dom.Val.get_all_locs ret_v) enum_values_mem
                |> Dom.Val.array_sizeof
              in
              decl_array model_env ~ret length mem
          | None ->
              Logging.d_printfln_escaped "Summary of Enum.values not found" ;
              mem )
      | _ ->
          Logging.d_printfln_escaped "Parameter is not a class name constant" ;
          mem
    in
    {exec; check= no_check}
end

module JavaLinkedList = struct
  let next {exp; typ} =
    let exec _model_env ~ret:(ret_id, _) mem =
      match exp with
      | Exp.Var id -> (
        match Dom.Mem.find_simple_alias id mem with
        | [(l, zero)] when IntLit.iszero zero ->
            let fn = BufferOverrunField.java_linked_list_next typ in
            model_by_value (Loc.append_field l fn |> Dom.Val.of_loc) ret_id mem
        | _ ->
            mem )
      | _ ->
          mem
    in
    {exec; check= no_check}
end

module JavaString = struct
  let fn = BufferOverrunField.java_collection_internal_array

  let deref_of = ArrObjCommon.deref_of ~fn

  let get_char_range s =
    let min_max =
      String.fold s ~init:None ~f:(fun acc c ->
          let i = Char.to_int c in
          match acc with None -> Some (i, i) | Some (lb, ub) -> Some (min lb i, max ub i) )
    in
    match min_max with None -> Itv.bot | Some (min, max) -> Itv.(join (of_int min) (of_int max))


  let get_length_and_elem model_env exp mem =
    match exp with
    | Exp.Const (Const.Cstr s) ->
        (Dom.Val.of_int (String.length s), Dom.Val.of_itv (get_char_range s))
    | _ ->
        let arr_locs = deref_of model_env exp mem in
        let length = Sem.eval_array_locs_length arr_locs mem in
        let elem =
          let arr_locs = Dom.Val.get_all_locs (Dom.Mem.find_set arr_locs mem) in
          Dom.Mem.find_set arr_locs mem
        in
        (length, elem)


  let get_length model_env exp mem = get_length_and_elem model_env exp mem |> fst

  let concat exp1 exp2 =
    let exec ({pname; caller_pname; node_hash} as model_env) ~ret:(id, _) mem =
      let length_v, elem =
        let length1, elem1 = get_length_and_elem model_env exp1 mem in
        let length2, elem2 = get_length_and_elem model_env exp2 mem in
        (Dom.Val.plus_a length1 length2, Dom.Val.join elem1 elem2)
      in
      let length, traces = (Dom.Val.get_itv length_v, Dom.Val.get_traces length_v) in
      let arr_loc =
        Allocsite.make pname ~caller_pname ~node_hash ~inst_num:0 ~dimension:1 ~path:None
          ~represents_multiple_values:false
        |> Loc.of_allocsite
      in
      let elem_alloc =
        Allocsite.make pname ~caller_pname ~node_hash ~inst_num:1 ~dimension:1 ~path:None
          ~represents_multiple_values:true
      in
      Dom.Mem.add_stack (Loc.of_id id) (Dom.Val.of_loc arr_loc) mem
      |> Dom.Mem.add_heap (Loc.append_field arr_loc fn)
           (Dom.Val.of_java_array_alloc elem_alloc ~length ~traces)
      |> Dom.Mem.add_heap (Loc.of_allocsite elem_alloc) elem
    in
    {exec; check= no_check}


  let length exp = {exec= ArrObjCommon.size_exec exp ~fn ?fn_typ:None; check= no_check}

  (** Given a string of length n, return itv [-1, n_u-1]. *)
  let range_itv_mone model_env exp mem =
    ArrObjCommon.eval_size model_env exp ~fn mem
    |> BufferOverrunDomain.Val.get_itv |> Itv.set_lb_zero |> Itv.decr


  (** Given a string of length n, return itv [1, max(1, n_u -1)]. *)
  let range_itv_one_max_one_mone v =
    let itv_mone = BufferOverrunDomain.Val.get_itv v |> Itv.decr in
    let max_length_mone = Itv.max_sem ~use_minmax_bound:true itv_mone (Itv.of_int 1) in
    Itv.set_lb Itv.Bound.one max_length_mone


  let indexOf exp =
    let exec model_env ~ret:(ret_id, _) mem =
      (* if not found, indexOf returns -1. *)
      let v = range_itv_mone model_env exp mem |> Dom.Val.of_itv in
      model_by_value v ret_id mem
    in
    {exec; check= no_check}


  let charAt string_exp idx = ArrObjCommon.at ~fn string_exp idx

  let constructor_from_char_ptr model_env tgt_deref src mem =
    ArrObjCommon.constructor_from_char_ptr model_env tgt_deref ~fn src mem


  let malloc_and_set_length exp ({location} as model_env) ~ret:((id, _) as ret) length mem =
    let {exec} = malloc ~can_be_zero:false exp in
    let mem = exec model_env ~ret mem in
    let underlying_arr_loc = Dom.Mem.find_stack (Loc.of_id id) mem |> Dom.Val.get_pow_loc in
    Dom.Mem.transform_mem ~f:(Dom.Val.set_array_length location ~length) underlying_arr_loc mem


  (** If the expression does not match any part of the input then the resulting array has just one
      element. *)
  let split exp =
    let exec model_env ~ret mem =
      let length =
        ArrObjCommon.eval_size model_env exp ~fn mem |> range_itv_one_max_one_mone |> Dom.Val.of_itv
      in
      malloc_and_set_length exp model_env ~ret length mem
    in
    {exec; check= no_check}


  let split_with_limit limit_exp =
    let exec ({integer_type_widths} as model_env) ~ret mem =
      let dummy_exp = Exp.zero in
      let length =
        Sem.eval integer_type_widths dummy_exp mem |> range_itv_one_max_one_mone |> Dom.Val.of_itv
      in
      malloc_and_set_length limit_exp model_env ~ret length mem
    in
    {exec; check= no_check}


  (* https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#String(java.lang.String) *)
  let copy_constructor tgt_exp src_exp =
    let exec ({integer_type_widths} as model_env) ~ret:_ mem =
      let tgt_deref = Sem.eval integer_type_widths tgt_exp mem |> Dom.Val.get_all_locs in
      let elem_locs = PowLoc.append_field tgt_deref ~fn in
      match src_exp with
      | Exp.Const (Const.Cstr s) ->
          BoUtils.Exec.decl_string model_env ~do_alloc:true elem_locs s mem
      | _ ->
          ArrObjCommon.copy_constructor model_env elem_locs ~fn src_exp mem
    in
    {exec; check= no_check}


  let empty_constructor tgt_exp = copy_constructor tgt_exp (Exp.Const (Const.Cstr ""))

  (* We model Enum.name or Class.getCanonicalName as returning an arbitrary constant name, rather
     than getting real names.  We did this because we couldn't think of any big gains from getting
     the real names in terms of analysis precision. *)
  let inferbo_constant_string =
    let constant_string_val =
      Loc.of_allocsite (Allocsite.literal_string "__constant_string") |> Dom.Val.of_loc
    in
    let exec _model_env ~ret:(ret_id, _) mem = model_by_value constant_string_val ret_id mem in
    {exec; check= no_check}


  let create_with_length {pname; caller_pname; node_hash; location} ~ret:(id, _) ~length_itv mem =
    let arr_loc =
      Allocsite.make pname ~caller_pname ~node_hash ~inst_num:0 ~dimension:1 ~path:None
        ~represents_multiple_values:false
      |> Loc.of_allocsite
    in
    let elem_alloc =
      Allocsite.make pname ~caller_pname ~node_hash ~inst_num:1 ~dimension:1 ~path:None
        ~represents_multiple_values:true
    in
    let traces = Trace.(Set.singleton location ArrayDeclaration) in
    Dom.Mem.add_stack (Loc.of_id id) (Dom.Val.of_loc arr_loc) mem
    |> Dom.Mem.add_heap (Loc.append_field arr_loc fn)
         (Dom.Val.of_java_array_alloc elem_alloc ~length:length_itv ~traces)


  let substring_no_end exp begin_idx =
    let exec ({integer_type_widths} as model_env) ~ret mem =
      let begin_itv = Sem.eval integer_type_widths begin_idx mem |> Dom.Val.get_itv in
      let end_itv = get_length model_env exp mem |> Dom.Val.get_itv in
      let length_itv = Itv.minus end_itv begin_itv in
      create_with_length model_env ~ret ~length_itv mem
    in
    {exec; check= no_check}


  let substring begin_idx end_idx =
    let exec ({integer_type_widths} as model_env) ~ret mem =
      let begin_itv = Sem.eval integer_type_widths begin_idx mem |> Dom.Val.get_itv in
      let end_itv = Sem.eval integer_type_widths end_idx mem |> Dom.Val.get_itv in
      let length_itv = Itv.minus end_itv begin_itv in
      create_with_length model_env ~ret ~length_itv mem
    in
    {exec; check= no_check}


  let create_from_short_arr exp =
    let exec ({integer_type_widths} as model_env) ~ret mem =
      let mem = create_with_length model_env ~ret ~length_itv:Itv.zero mem in
      let deref_of_tgt =
        Dom.Mem.find_stack (Loc.of_id (fst ret)) mem
        |> Dom.Val.get_pow_loc |> PowLoc.append_field ~fn
      in
      let src_arr_locs = Dom.Val.get_all_locs (Sem.eval_arr integer_type_widths exp mem) in
      Dom.Mem.update_mem deref_of_tgt (Dom.Mem.find_set src_arr_locs mem) mem
    in
    {exec; check= no_check}


  let replace = id
end

module NSString = struct
  let fn = JavaString.fn

  let create_with_c_string src_exp =
    let exec model_env ~ret mem =
      let length_itv = Sem.eval_string_len src_exp mem |> Dom.Val.get_itv in
      JavaString.create_with_length model_env ~ret ~length_itv mem
    in
    {exec; check= no_check}


  let init_with_string tgt_exp src_exp =
    let {exec= copy_exec; check= _} = JavaString.copy_constructor tgt_exp src_exp in
    let exec ({integer_type_widths} as model_env) ~ret:((id, _) as ret) mem =
      let mem = copy_exec model_env ~ret mem in
      let v = Sem.eval integer_type_widths tgt_exp mem in
      model_by_value v id mem
    in
    {exec; check= no_check}


  let init_with_c_string_with_len tgt_exp src_exp len_exp =
    let exec ({integer_type_widths; location} as model_env) ~ret:(id, _) mem =
      let len_v = Sem.eval integer_type_widths len_exp mem in
      let tgt_v = Sem.eval integer_type_widths tgt_exp mem in
      let tgt_deref = Dom.Val.get_all_locs tgt_v in
      let tgt_arr_loc = JavaString.deref_of model_env tgt_exp mem in
      ArrObjCommon.constructor_from_char_ptr model_env tgt_deref ~fn src_exp mem
      |> Dom.Mem.transform_mem ~f:(Dom.Val.set_array_length location ~length:len_v) tgt_arr_loc
      |> model_by_value tgt_v id
    in
    {exec; check= no_check}


  let substring_from_index = JavaString.substring_no_end

  let length = JavaString.length

  (** For cost analysis *)
  let get_length = JavaString.get_length

  let concat = JavaString.concat

  let split exp =
    let exec model_env ~ret:((coll_id, _) as ret) mem =
      let to_add_length =
        ArrObjCommon.eval_size model_env exp ~fn mem |> JavaString.range_itv_one_max_one_mone
      in
      NSCollection.create_collection ~size_exp:Exp.zero model_env ~ret mem
      |> NSCollection.change_size_by ~size_f:(Itv.plus to_add_length) coll_id model_env ~ret
    in
    {exec; check= no_check}


  let append_string str1_exp str2_exp =
    let exec ({location} as model_env) ~ret:_ mem =
      let to_add_len = JavaString.get_length model_env str2_exp mem |> Dom.Val.get_itv in
      let arr_locs = JavaString.deref_of model_env str1_exp mem in
      let mem = Dom.Mem.forget_size_alias arr_locs mem in
      Dom.Mem.transform_mem
        ~f:(Dom.Val.transform_array_length location ~f:(Itv.plus to_add_len))
        arr_locs mem
    in
    {exec; check= no_check}
end

let objc_malloc exp =
  let can_be_zero = true in
  let exec ({tenv} as model) ~ret mem =
    match exp with
    | Exp.Sizeof {typ} when PatternMatch.ObjectiveC.implements_collection tenv (Typ.to_string typ)
      ->
        NSCollection.new_collection.exec model ~ret mem
    | Exp.Sizeof {typ}
      when PatternMatch.ObjectiveC.implements_ns_string_variants tenv (Typ.to_string typ) ->
        (NSString.create_with_c_string (Exp.Const (Const.Cstr ""))).exec model ~ret mem
    | _ ->
        (malloc ~can_be_zero exp).exec model ~ret mem
  in
  {exec; check= check_alloc_size ~can_be_zero exp}


module Preconditions = struct
  let check_argument exp =
    let exec {integer_type_widths; location} ~ret:_ mem =
      Sem.Prune.prune location integer_type_widths exp mem
    in
    {exec; check= no_check}
end

let unmodifiable _ s =
  String.is_prefix ~prefix:"unmodifiable" s
  && List.exists ~f:(fun suffix -> String.is_suffix ~suffix s) ["Set"; "Collection"; "Map"; "List"]


module InputStream = struct
  (* https://docs.oracle.com/javase/7/docs/api/java/io/InputStream.html#read(byte[],%20int,%20int) *)
  let read exp =
    let exec {integer_type_widths} ~ret:(ret_id, _) mem =
      let max = Sem.eval integer_type_widths exp mem in
      let traces = Dom.Val.get_traces max in
      let v = Dom.Val.of_itv ~traces (Itv.set_lb Itv.Bound.mone (Dom.Val.get_itv max)) in
      model_by_value v ret_id mem
    in
    {exec; check= no_check}
end

module File = struct
  let list_files exp =
    let exec ({integer_type_widths} as model_env) ~ret mem =
      let locs = Sem.eval integer_type_widths exp mem |> Dom.Val.get_pow_loc in
      match PowLoc.is_singleton_or_more locs with
      | Singleton loc ->
          Loc.append_field loc BufferOverrunField.java_list_files_length
          |> Loc.get_path
          |> Option.value_map ~default:mem ~f:(fun path ->
                 let length = Itv.of_normal_path ~unsigned:true path in
                 JavaClass.decl_array model_env ~ret length mem )
      | Empty | More ->
          mem
    in
    {exec; check= no_check}
end

module FileChannel = struct
  (* https://docs.oracle.com/javase/7/docs/api/java/io/InputStream.html#read(byte[],%20int,%20int) *)
  let read buf_exp =
    let exec {pname; location} ~ret:(ret_id, ret_typ) mem =
      let buf_locs = Sem.eval_locs buf_exp mem in
      let buf_v = Dom.Mem.find_set buf_locs mem in
      let range =
        Symb.SymbolPath.of_callsite ~ret_typ (CallSite.make pname location)
        |> Bounds.Bound.of_modeled_path Symb.Symbol.make_onevalue
        |> Dom.ModeledRange.of_modeled_function pname location
      in
      let mem = Dom.Mem.add_heap_set buf_locs (Dom.Val.set_modeled_range range buf_v) mem in
      let v =
        Dom.Val.of_itv (Itv.set_lb Itv.Bound.mone Itv.nat) |> Dom.Val.set_modeled_range range
      in
      model_by_value v ret_id mem
    in
    {exec; check= no_check}
end

module Buffer = struct
  let get buf_exp =
    let exec _ ~ret:(ret_id, _) mem =
      let range = Dom.Mem.find_set (Sem.eval_locs buf_exp mem) mem |> Dom.Val.get_modeled_range in
      let v = Dom.Val.of_itv Itv.nat |> Dom.Val.set_modeled_range range in
      model_by_value v ret_id mem
    in
    {exec; check= no_check}
end

module InferAnnotation = struct
  let assert_get index_exp coll_exp =
    match coll_exp with
    | Exp.Var coll_id ->
        Collection.get_at_index coll_id index_exp
    | _ ->
        no_model
end

let iter_begin _ str = List.exists ~f:(String.equal str) ["begin"; "cbegin"; "rbegin"; "crbegin"]

let iter_end _ str = List.exists ~f:(String.equal str) ["end"; "cend"; "rend"; "crend"]

let std_container _ str =
  List.exists ~f:(String.equal str)
    ["basic_string"; "deque"; "list"; "map"; "queue"; "set"; "unordered_map"; "unordered_set"]


(* libcpp - native library for mac *)
let std_iterator_libcpp _ str =
  List.exists ~f:(String.equal str)
    [ "__deque_iterator"
    ; "__hash_const_iterator"
    ; "__hash_map_const_iterator"
    ; "__hash_map_iterator"
    ; "__list_const_iterator"
    ; "__list_iterator"
    ; "__map_const_iterator"
    ; "__map_iterator"
    ; "__tree_const_iterator"
    ; "__wrap_iter"
    ; "reverse_iterator" ]


(* libstdcpp - native library for gnu/linux *)
let std_iterator_libstdcpp _ str =
  List.exists ~f:(String.equal str)
    [ "_Deque_iterator"
    ; "_List_const_iterator"
    ; "_List_iterator"
    ; "_Rb_tree_const_iterator"
    ; "_Rb_tree_iterator"
    ; "reverse_iterator" ]


(* libstdcpp - std::__detail:: cases *)
let std_iterator_libstdcpp_detail _ str =
  List.exists ~f:(String.equal str) ["_Node_const_iterator"; "_Node_iterator"]


module Call = struct
  let dispatch : (Tenv.t, model, unit) ProcnameDispatcher.Call.dispatcher =
    let open ProcnameDispatcher.Call in
    let int_typ = Typ.mk (Typ.Tint Typ.IInt) in
    let char_typ = Typ.mk (Typ.Tint Typ.IChar) in
    let short_typ = Typ.mk (Typ.Tint Typ.IUShort) in
    let char_ptr = Typ.mk (Typ.Tptr (char_typ, Pk_pointer)) in
    let short_array = Typ.mk (Typ.Tptr (Typ.mk_array short_typ, Pk_pointer)) in
    make_dispatcher
      [ (* Clang common models *)
        +BuiltinDecl.(match_builtin __cast) <>$ capt_exp $+ capt_exp $+...$--> cast
      ; +BuiltinDecl.(match_builtin __exit) <>--> bottom
      ; +BuiltinDecl.(match_builtin __get_array_length) <>$ capt_exp $!--> get_array_length
      ; +BuiltinDecl.(match_builtin objc_cpp_throw) <>--> bottom
      ; +BuiltinDecl.(match_builtin __new)
        <>$ any_arg_of_typ (+PatternMatch.Java.implements_collection)
        $+...$--> Collection.new_collection
      ; +BuiltinDecl.(match_builtin __new)
        <>$ any_arg_of_typ (+PatternMatch.Java.implements_map)
        $+...$--> Collection.new_collection
      ; +BuiltinDecl.(match_builtin __new)
        <>$ any_arg_of_typ (+PatternMatch.Java.implements_org_json "JSONArray")
        $+...$--> Collection.new_collection
      ; +BuiltinDecl.(match_builtin __new)
        <>$ any_arg_of_typ (+PatternMatch.Java.implements_pseudo_collection)
        $+...$--> Collection.new_collection
      ; +BuiltinDecl.(match_builtin __new) <>$ capt_exp $+...$--> malloc ~can_be_zero:true
      ; +BuiltinDecl.(match_builtin __new_array) <>$ capt_exp $+...$--> malloc ~can_be_zero:true
      ; +BuiltinDecl.(match_builtin __placement_new)
        <>$ capt_exp $+ capt_arg $+? capt_arg $!--> placement_new
      ; +BuiltinDecl.(match_builtin __set_array_length)
        <>$ capt_arg $+ capt_exp $!--> set_array_length
      ; +BuiltinDecl.(match_builtin __infer_initializer_list)
        <>$ capt_exp $+...$--> StdInitializerList.constructor
      ; -"calloc" <>$ capt_exp $+ capt_exp $!--> calloc ~can_be_zero:false
      ; -"exit" <>--> bottom
      ; -"fgetc" <>--> by_value Dom.Val.Itv.m1_255
      ; -"fgets" <>$ capt_exp $+ capt_exp $+...$--> fgets
      ; -"infer_print" <>$ capt_exp $!--> infer_print
      ; -"malloc" <>$ capt_exp $+...$--> malloc ~can_be_zero:false
      ; -"memcpy" <>$ capt_exp $+ capt_exp $+ capt_exp $+...$--> memcpy
      ; -"memmove" <>$ capt_exp $+ capt_exp $+ capt_exp $+...$--> memcpy
      ; -"memset" <>$ capt_exp $+ any_arg $+ capt_exp $!--> memset
      ; -"realloc" <>$ capt_exp $+ capt_exp $+...$--> realloc
      ; -"snprintf" <>--> by_value Dom.Val.Itv.nat
      ; -"strcat" <>$ capt_exp $+ capt_exp $+...$--> strcat
      ; -"strcpy" <>$ capt_exp $+ capt_exp $+...$--> strcpy
      ; -"strlen" <>$ capt_exp $!--> strlen
      ; -"strncpy" <>$ capt_exp $+ capt_exp $+ capt_exp $+...$--> strncpy
      ; -"strndup" <>$ capt_exp $+ capt_exp $+...$--> strndup
      ; -"vsnprintf" <>--> by_value Dom.Val.Itv.nat
      ; (* ObjC models *)
        +BuiltinDecl.(match_builtin __objc_alloc_no_fail) <>$ capt_exp $+...$--> objc_malloc
      ; -"CFArrayCreate" <>$ any_arg $+ capt_exp $+ capt_exp
        $+...$--> NSCollection.create_from_array
      ; -"CFArrayCreateCopy" <>$ any_arg $+ capt_exp $!--> create_copy_array
      ; -"CFArrayGetCount" <>$ capt_exp $!--> NSCollection.size
      ; -"CFArrayGetValueAtIndex" <>$ capt_var $+ capt_exp $!--> NSCollection.get_at_index
      ; -"CFDictionaryGetCount" <>$ capt_exp $!--> NSCollection.size
      ; -"MCFArrayGetCount" <>$ capt_exp $!--> NSCollection.size
      ; +PatternMatch.ObjectiveC.implements "NSObject" &:: "init" <>$ capt_exp $--> id
      ; +PatternMatch.ObjectiveC.implements "NSObject" &:: "copy" <>$ capt_exp $--> id
      ; +PatternMatch.ObjectiveC.implements "NSObject" &:: "mutableCopy" <>$ capt_exp $--> id
      ; +PatternMatch.ObjectiveC.implements "NSArray" &:: "array" <>--> NSCollection.new_collection
      ; +PatternMatch.ObjectiveC.implements "NSArray"
        &:: "firstObject" <>$ capt_var $!--> NSCollection.get_first
      ; +PatternMatch.ObjectiveC.implements "NSDictionary"
        &:: "initWithDictionary:" <>$ capt_var $+ capt_exp $--> NSCollection.copy
      ; +PatternMatch.ObjectiveC.implements "NSDictionary"
        &:: "dictionaryWithDictionary:" <>$ capt_exp $--> NSCollection.new_collection_by_init
      ; +PatternMatch.ObjectiveC.implements "NSSet"
        &:: "initWithArray:" <>$ capt_var $+ capt_exp $--> NSCollection.copy
      ; +PatternMatch.ObjectiveC.implements "NSArray"
        &:: "initWithArray:" <>$ capt_var $+ capt_exp $--> NSCollection.copy
      ; +PatternMatch.ObjectiveC.implements "NSArray"
        &:: "initWithArray:copyItems:" <>$ capt_var $+ capt_exp $+ any_arg $--> NSCollection.copy
      ; +PatternMatch.ObjectiveC.implements_collection
        &:: "count" <>$ capt_exp $!--> NSCollection.size
      ; +PatternMatch.ObjectiveC.implements_collection
        &:: "objectEnumerator" <>$ capt_exp $--> NSCollection.iterator
      ; +PatternMatch.ObjectiveC.conforms_to ~protocol:"NSFastEnumeration"
        &:: "objectEnumerator" <>$ capt_exp $--> NSCollection.iterator
      ; +PatternMatch.ObjectiveC.implements "NSArray"
        &:: "objectAtIndexedSubscript:" <>$ capt_var $+ capt_exp $!--> NSCollection.get_at_index
      ; +PatternMatch.ObjectiveC.implements "NSArray"
        &:: "arrayWithObjects:count:" <>$ capt_exp $+ capt_exp $--> NSCollection.create_from_array
      ; +PatternMatch.ObjectiveC.implements "NSArray"
        &:: "arrayWithObjects:" &++> NSCollection.of_list
      ; +PatternMatch.ObjectiveC.implements "NSArray"
        &:: "arrayByAddingObjectsFromArray:" <>$ capt_exp $+ capt_exp
        $--> NSCollection.new_collection_by_add_all
      ; +PatternMatch.ObjectiveC.implements "NSEnumerator"
        &:: "nextObject" <>$ capt_var $--> NSCollection.next_object
      ; +PatternMatch.ObjectiveC.implements "NSFileManager"
        &:: "contentsOfDirectoryAtURL:includingPropertiesForKeys:options:error:"
        &--> NSCollection.new_collection
      ; +PatternMatch.ObjectiveC.implements "NSKeyedUnarchiver"
        &:: "decodeObjectForKey:" $ capt_var $+...$--> NSCollection.get_any_index
      ; +PatternMatch.ObjectiveC.implements "NSMutableArray"
        &:: "initWithCapacity:" <>$ capt_var $+ capt_exp $--> NSCollection.new_collection_of_size
      ; +PatternMatch.ObjectiveC.implements "NSMutableArray"
        &:: "addObject:" <>$ capt_var $+ capt_exp $--> NSCollection.add
      ; +PatternMatch.ObjectiveC.implements "NSMutableArray"
        &:: "removeLastObject" <>$ capt_var $--> NSCollection.remove_last
      ; +PatternMatch.ObjectiveC.implements "NSMutableArray"
        &:: "insertObject:atIndex:" <>$ capt_var $+ any_arg $+ capt_exp
        $--> NSCollection.add_at_index
      ; +PatternMatch.ObjectiveC.implements "NSMutableArray"
        &:: "removeObjectAtIndex:" <>$ capt_var $+ capt_exp $--> NSCollection.remove_at_index
      ; +PatternMatch.ObjectiveC.implements "NSMutableArray"
        &:: "removeAllObjects:" <>$ capt_var $--> NSCollection.remove_all
      ; +PatternMatch.ObjectiveC.implements "NSMutableArray"
        &:: "addObjectsFromArray:" <>$ capt_var $+ capt_exp $--> NSCollection.addAll
      ; +PatternMatch.ObjectiveC.implements "NSDictionary"
        &:: "dictionary" <>--> NSCollection.new_collection
      ; +PatternMatch.ObjectiveC.implements "NSDictionary"
        &:: "dictionaryWithObjects:forKeys:count:" <>$ any_arg $+ capt_exp $+ capt_exp
        $--> NSCollection.create_from_array
      ; +PatternMatch.ObjectiveC.implements "NSDictionary"
        &:: "objectForKeyedSubscript:" <>$ capt_var $+ capt_exp $--> NSCollection.get_at_index
      ; +PatternMatch.ObjectiveC.implements "NSDictionary"
        &:: "objectForKey:" <>$ capt_var $+ capt_exp $--> NSCollection.get_at_index
      ; +PatternMatch.ObjectiveC.implements "NSDictionary"
        &:: "allKeys" <>$ capt_exp $--> create_copy_array
      ; +PatternMatch.ObjectiveC.implements "NSDictionary"
        &:: "allValues" <>$ capt_exp $--> create_copy_array
      ; +PatternMatch.ObjectiveC.implements "NSDictionary"
        &:: "keyEnumerator" <>$ capt_exp $--> NSCollection.iterator
      ; +PatternMatch.ObjectiveC.implements "NSOrderedSet"
        &:: "orderedSet" $$--> NSCollection.new_collection
      ; +PatternMatch.ObjectiveC.implements "NSOrderedSet"
        &:: "orderedSetWithArray:" $ capt_exp $--> id
      ; +PatternMatch.ObjectiveC.implements "NSOrderedSet"
        &:: "reverseObjectEnumerator" <>$ capt_exp $--> NSCollection.iterator
      ; +PatternMatch.ObjectiveC.implements "NSNumber" &:: "numberWithInt:" <>$ capt_exp $--> id
      ; +PatternMatch.ObjectiveC.implements "NSNumber" &:: "integerValue" <>$ capt_exp $--> id
      ; +PatternMatch.ObjectiveC.implements "NSAttributedString" &:: "string" <>$ capt_exp $!--> id
      ; +PatternMatch.ObjectiveC.implements "NSString"
        &:: "stringWithUTF8String:" <>$ capt_exp $!--> NSString.create_with_c_string
      ; +PatternMatch.ObjectiveC.implements_ns_string_variants
        &:: "length" <>$ capt_exp $--> NSString.length
      ; +PatternMatch.ObjectiveC.implements "NSString"
        &:: "stringByAppendingString:" <>$ capt_exp $+ capt_exp $!--> NSString.concat
      ; +PatternMatch.ObjectiveC.implements "NSString"
        &:: "substringFromIndex:" <>$ capt_exp $+ capt_exp $--> NSString.substring_from_index
      ; +PatternMatch.ObjectiveC.implements "NSString"
        &:: "appendString:" <>$ capt_exp $+ capt_exp $--> NSString.append_string
      ; +PatternMatch.ObjectiveC.implements "NSString"
        &:: "componentsSeparatedByString:" <>$ capt_exp $+ any_arg $--> NSString.split
      ; +PatternMatch.ObjectiveC.implements "NSAttributedString"
        &:: "initWithString:attributes:" <>$ capt_exp $+ capt_exp $+ any_arg
        $--> NSString.init_with_string
      ; +PatternMatch.ObjectiveC.implements_ns_string_variants
        &:: "initWithString:" <>$ capt_exp $+ capt_exp $--> NSString.init_with_string
      ; +PatternMatch.ObjectiveC.implements "NSString"
        &:: "initWithBytes:length:encoding:" <>$ capt_exp $+ capt_exp $+ capt_exp $+ any_arg
        $!--> NSString.init_with_c_string_with_len
      ; +PatternMatch.ObjectiveC.implements "NSURL"
        &:: "getResourceValue:forKey:error:" &--> NSURL.get_resource_value
      ; +PatternMatch.ObjectiveC.implements "NSURL" &:: "path" $ capt_exp $--> id
      ; (* C++ models *)
        -"boost" &:: "split"
        $ capt_arg_of_typ (-"std" &:: "vector")
        $+ any_arg $+ any_arg $+? any_arg $--> Boost.Split.std_vector
      ; -"folly" &:: "split" $ any_arg $+ any_arg
        $+ capt_arg_of_typ (-"std" &:: "vector")
        $+? capt_exp $--> Folly.Split.std_vector
      ; -"std" &:: "integral_constant" < any_typ &+ capt_int >:: "operator_int"
        &--> Container.integer_size
      ; -"std" &:: "__shared_ptr_access" &:: "operator->" $ capt_exp $--> id
      ; -"std" &:: "array" < any_typ &+ capt_int >:: "array" &--> StdArray.constructor
      ; -"std" &:: "array" < any_typ &+ capt_int >:: "size" &--> Container.integer_size
      ; -"std" &:: "array" < any_typ &+ capt_int >:: "max_size" &--> Container.integer_size
      ; -"std" &:: "array" < any_typ &+ capt_int >:: "at" $ capt_arg $+ capt_arg $!--> StdArray.at
      ; -"std" &:: "array" < any_typ &+ capt_int >:: "back" $ capt_arg $!--> StdArray.back
      ; -"std" &:: "array" < any_typ &+ capt_int >:: "begin" $ capt_arg $!--> StdArray.begin_
      ; -"std" &:: "array" < any_typ &+ capt_int >:: "cbegin" $ capt_arg $!--> StdArray.begin_
      ; -"std" &:: "array" < any_typ &+ capt_int >:: "cend" $ capt_arg $!--> StdArray.end_
      ; -"std" &:: "array" < any_typ &+ capt_int >:: "end" $ capt_arg $!--> StdArray.end_
      ; -"std" &:: "array" < any_typ &+ capt_int >:: "front" $ capt_arg $!--> StdArray.begin_
      ; -"std" &:: "array" < any_typ &+ capt_int >:: "operator[]" $ capt_arg $+ capt_arg
        $!--> StdArray.at
      ; -"std" &:: "array" &::.*--> no_model
      ; -"std" &:: "basic_string" &:: "compare" &--> by_value Dom.Val.Itv.top
      ; -"std" &:: "basic_string" < capt_typ &+...>:: "basic_string" $ capt_arg
        $+ capt_exp_of_prim_typ char_ptr
        $+ capt_exp_of_prim_typ (Typ.mk (Typ.Tint Typ.size_t))
        $--> StdBasicString.constructor_from_char_ptr_with_len
      ; -"std" &:: "basic_string" < capt_typ &+...>:: "basic_string" $ capt_arg
        $+ capt_exp_of_prim_typ char_ptr $--> StdBasicString.constructor_from_char_ptr_without_len
      ; -"std" &:: "basic_string" < capt_typ &+...>:: "basic_string" $ capt_arg
        $+ capt_exp_of_typ (-"std" &:: "basic_string")
        $--> StdBasicString.copy_constructor
      ; -"std" &:: "basic_string" < capt_typ &+...>:: "empty" $ capt_arg $--> StdBasicString.empty
      ; -"std" &:: "basic_string" < capt_typ &+...>:: "length" $ capt_arg $--> StdBasicString.length
      ; -"std" &:: "basic_string" < capt_typ &+...>:: "size" $ capt_arg $--> StdBasicString.length
      ; -"std" &:: "shared_ptr" &:: "operator->" $ capt_exp $--> id
        (*             Models for c++ iterators <begin>           *)
        (* C++11 -- macosx *)
      ; -"std" &::+ std_iterator_libcpp &::+ std_iterator_libcpp $ capt_exp $+ capt_exp
        $--> Iterator.new_
      ; -"std" &::+ std_iterator_libcpp &:: "operator=" $ capt_exp $+ capt_exp $--> Iterator.copy
        (* C++11 -- gnu/linux *)
      ; -"__gnu_cxx" &:: "__normal_iterator" &:: "__normal_iterator" $ capt_exp $+ capt_exp
        $--> Iterator.new_
      ; -"std" &::+ std_iterator_libstdcpp &:: "operator=" $ capt_exp $+ capt_exp $--> Iterator.copy
      ; -"std" &:: "__detail" &::+ std_iterator_libstdcpp_detail &::+ std_iterator_libstdcpp_detail
        $ capt_exp $+ capt_exp $--> Iterator.new_
      ; -"std" &::+ std_iterator_libstdcpp &::+ std_iterator_libstdcpp $ capt_exp $+ capt_exp
        $--> Iterator.new_
        (* begin and end models for containers *)
      ; -"std" &::+ std_container &::+ iter_begin $ any_arg $+ capt_exp $--> Iterator.begin_
      ; -"std" &::+ std_container &::+ iter_end $ capt_exp $+ capt_exp $--> Container.Iterator.end_
        (* Iterators for vectors *)
      ; -"std" &:: "vector" < capt_typ &+ any_typ >::+ iter_end $ capt_arg $+ capt_exp
        $--> StdVector.Iterator.end_
      ; -"std" &:: "vector" &::+ iter_begin $ any_arg $+ capt_exp $--> Iterator.begin_
        (*             Models for c++ iterators <end>             *)
        (*     Models for c++ operators <begin> -- libcpp/mac     *)
      ; -"std" &:: "operator!="
        $ capt_exp_of_typ (-"std" &::+ std_iterator_libcpp)
        $+ capt_exp $--> Iterator.iterator_ne
      ; -"std" &::+ std_iterator_libcpp &:: "operator++" $ capt_exp $+...$--> Iterator.iterator_incr
        (*          Models for c++ operators -- gnu/linx          *)
      ; -"std" &:: "operator!="
        $ capt_exp_of_typ (-"std" &::+ std_iterator_libstdcpp)
        $+ capt_exp $--> Iterator.iterator_ne
        (* C++11 representation of operator!= *)
      ; -"std" &::+ std_iterator_libstdcpp &:: "operator!="
        $ capt_exp_of_typ (-"std" &::+ std_iterator_libstdcpp)
        $+ capt_exp $--> Iterator.iterator_ne
      ; -"std" &::+ std_iterator_libstdcpp &:: "operator++" $ capt_exp
        $+...$--> Iterator.iterator_incr
        (*    vectors have a different namespace in gnu/linux     *)
      ; -"__gnu_cxx" &:: "operator!="
        $ capt_exp_of_typ (-"__gnu_cxx" &:: "__normal_iterator")
        $+ capt_exp $--> Iterator.iterator_ne
      ; -"__gnu_cxx" &:: "__normal_iterator" &:: "operator++" $ capt_exp $--> Iterator.iterator_incr
        (*              vectors operators for macosx              *)
      ; -"std" &:: "vector" &:: "operator!="
        $ capt_exp_of_typ (-"std" &::+ std_iterator_libstdcpp)
        $+ capt_exp $--> Iterator.iterator_ne
      ; -"std" &:: "vector" &:: "operator++" $ capt_exp $+...$--> Iterator.iterator_incr
        (*    unordered sets representation includes __detail     *)
      ; -"std" &:: "operator!="
        $ capt_exp_of_typ (-"std" &:: "__detail" &::+ std_iterator_libstdcpp_detail)
        $+ capt_exp $--> Iterator.iterator_ne
        (* C++11 representation of operator!= *)
      ; -"std" &:: "__detail" &:: "operator!="
        $ capt_exp_of_typ (-"std" &:: "__detail" &::+ std_iterator_libstdcpp_detail)
        $+ capt_exp $--> Iterator.iterator_ne
      ; -"std" &:: "__detail" &::+ std_iterator_libstdcpp_detail &:: "operator++" $ capt_exp
        $+...$--> Iterator.iterator_incr
        (*             Models for c++ operators <end>             *)
        (*      Models for c++ containers operations <begin>      *)
      ; -"std" &::+ std_container &:: "size" $ capt_exp $--> Container.size
      ; -"std" &::+ std_container &::+ std_container
        $ capt_arg_of_typ (-"std" &::+ std_container)
        $--> Container.constructor_empty
        (*       Models for c++ containers operations <end>       *)
        (*             Models for std::vector <begin>             *)
      ; -"std" &:: "vector" < capt_typ &+ any_typ >:: "data" $ capt_arg $--> StdVector.data
      ; -"std" &:: "vector" < capt_typ &+ any_typ >:: "emplace_back" $ capt_arg $+ capt_exp
        $--> StdVector.push_back
      ; -"std" &:: "vector" < capt_typ &+ any_typ >:: "empty" $ capt_arg $--> StdVector.empty
      ; -"std" &:: "vector" < capt_typ &+ any_typ >:: "operator[]"
        $ capt_arg_of_typ (-"std" &:: "vector")
        $+ capt_exp $--> StdVector.at
      ; -"std" &:: "vector" < capt_typ &+ any_typ >:: "size" $ capt_arg $--> StdVector.size
      ; -"std" &:: "vector" < capt_typ &+ any_typ >:: "push_back" $ capt_arg $+ capt_exp
        $--> StdVector.push_back
      ; -"std" &:: "vector" < capt_typ &+ any_typ >:: "insert" $ capt_arg $+ any_arg
        $+ capt_exp_of_typ (-"std" &:: "initializer_list")
        $+ any_arg $--> StdVector.insert_initializer_list
      ; -"std" &:: "vector" < capt_typ &+ any_typ >:: "insert" $ capt_arg $+ any_arg $+ capt_exp
        $+ any_arg $--> StdVector.push_back
      ; -"std" &:: "vector" < capt_typ &+ any_typ >:: "insert" $ capt_arg $+ any_arg $+ capt_exp
        $+ capt_exp $+ any_arg $--> StdVector.insert_n
      ; -"std" &:: "vector" < any_typ &+ any_typ >:: "reserve" $ any_arg $+ any_arg $--> no_model
      ; -"std" &:: "vector" < capt_typ &+ any_typ >:: "resize" $ capt_arg $+ capt_exp
        $--> StdVector.resize
      ; -"std" &:: "vector" < capt_typ &+ any_typ >:: "vector"
        $ capt_arg_of_typ (-"std" &:: "vector")
        $+ capt_exp_of_prim_typ (Typ.mk (Typ.Tint Typ.size_t))
        $+? any_arg $--> StdVector.constructor_size
      ; -"std" &:: "vector" < capt_typ &+ any_typ >:: "vector"
        $ capt_arg_of_typ (-"std" &:: "vector")
        $--> StdVector.constructor_empty
      ; -"std" &:: "vector" < capt_typ &+ any_typ >:: "vector"
        $ capt_arg_of_typ (-"std" &:: "vector")
        $+ capt_exp_of_typ (-"std" &:: "vector")
        $+? any_arg $--> StdVector.constructor_copy
      ; -"std" &:: "vector" < capt_typ &+ any_typ >:: "vector"
        $ capt_arg_of_typ (-"std" &:: "vector")
        $+ capt_exp_of_typ (-"std" &:: "initializer_list")
        $+? any_arg $--> StdVector.constructor_initializer_list
        (*             Models for std::vector <end>               *)
      ; -"google" &:: "StrLen" <>$ capt_exp $--> strlen
      ; (* Java models *)
        -"java.lang.Object" &:: "clone" <>$ capt_exp $--> id
      ; +PatternMatch.Java.implements_arrays &:: "asList" <>$ capt_exp $!--> create_copy_array
      ; +PatternMatch.Java.implements_arrays &:: "copyOf" <>$ capt_exp $+ capt_exp $+...$--> copyOf
      ; (* model sets and maps as lists *)
        +PatternMatch.Java.implements_collection
        &:: "<init>" <>$ capt_var
        $+ capt_exp_of_typ (+PatternMatch.Java.implements_collection)
        $--> Collection.init_with_arg
      ; +PatternMatch.Java.implements_collection
        &:: "<init>" <>$ any_arg $+ capt_exp $--> Collection.init_with_capacity
      ; +PatternMatch.Java.implements_collection
        &:: "add" <>$ capt_var $+ capt_exp $+ any_arg $--> Collection.add_at_index
      ; +PatternMatch.Java.implements_collection
        &:: "add" <>$ capt_var $+ capt_exp $--> Collection.add
      ; +PatternMatch.Java.implements_collection
        &:: "addAll" <>$ capt_var $+ capt_exp $+ capt_exp $--> Collection.addAll_at_index
      ; +PatternMatch.Java.implements_collection
        &:: "addAll" <>$ capt_var $+ capt_exp $--> Collection.addAll
      ; +PatternMatch.Java.implements_collection
        &:: "get" <>$ capt_var $+ capt_exp $--> Collection.get_at_index
      ; +PatternMatch.Java.implements_map &:: "get" <>$ capt_exp $+ any_arg $--> Collection.get_elem
      ; +PatternMatch.Java.implements_collection
        &:: "remove" <>$ capt_var $+ capt_exp $--> Collection.remove_at_index
      ; +PatternMatch.Java.implements_collection
        &:: "set" <>$ capt_var $+ capt_exp $+ capt_exp $--> Collection.set_at_index
      ; +PatternMatch.Java.implements_collection &:: "size" <>$ capt_exp $!--> Collection.size
      ; +PatternMatch.Java.implements_collection
        &:: "toArray" <>$ capt_exp $+...$--> create_copy_array
      ; +PatternMatch.Java.implements_collections &:: "emptyList" <>--> Collection.new_collection
      ; +PatternMatch.Java.implements_collections &:: "emptyMap" <>--> Collection.new_collection
      ; +PatternMatch.Java.implements_collections &:: "emptySet" <>--> Collection.new_collection
      ; +PatternMatch.Java.implements_collections
        &:: "singleton" <>--> Collection.singleton_collection
      ; +PatternMatch.Java.implements_collections
        &:: "singletonList" <>--> Collection.singleton_collection
      ; +PatternMatch.Java.implements_collections
        &:: "singletonList" <>--> Collection.singleton_collection
      ; +PatternMatch.Java.implements_collections
        &:: "singletonMap" <>--> Collection.singleton_collection
      ; +PatternMatch.Java.implements_collections
        &:: "singletonMap" <>--> Collection.singleton_collection
      ; +PatternMatch.Java.implements_collections
        &::+ unmodifiable <>$ capt_exp $--> Collection.iterator
      ; +PatternMatch.Java.implements_set &:: "of" &++> Collection.of_list
      ; +PatternMatch.Java.implements_google "common.collect.ImmutableSet"
        &:: "of" &++> Collection.of_list
      ; +PatternMatch.Java.implements_google "common.base.Preconditions"
        &:: "checkArgument" <>$ capt_exp $+...$--> Preconditions.check_argument
      ; +PatternMatch.Java.implements_google "common.base.Preconditions"
        &:: "checkNotNull" <>$ capt_exp $+...$--> id
      ; +PatternMatch.Java.implements_google "common.base.Preconditions"
        &:: "checkState" <>$ capt_exp $+...$--> Preconditions.check_argument
      ; +PatternMatch.Java.implements_infer_annotation "Assertions"
        &:: "assertGet" <>$ capt_exp $+ capt_exp $--> InferAnnotation.assert_get
      ; +PatternMatch.Java.implements_infer_annotation "Assertions"
        &:: "assertNotNull" <>$ capt_exp $+...$--> id
      ; +PatternMatch.Java.implements_infer_annotation "Assertions"
        &:: "assumeNotNull" <>$ capt_exp $+...$--> id
      ; +PatternMatch.Java.implements_infer_annotation "Assertions"
        &:: "nullsafeFIXME" <>$ capt_exp $+...$--> id
      ; +PatternMatch.Java.implements_infer_annotation "Assertions" &::.*--> no_model
      ; +PatternMatch.Java.implements_io "File" &:: "listFiles" <>$ capt_exp $--> File.list_files
      ; +PatternMatch.Java.implements_io "InputStream"
        &:: "read" <>$ any_arg $+ any_arg $+ any_arg $+ capt_exp $--> InputStream.read
      ; +PatternMatch.Java.implements_iterator &:: "hasNext" <>$ capt_exp $!--> Collection.hasNext
      ; +PatternMatch.Java.implements_nio "Buffer" &:: "wrap" <>$ capt_exp $--> create_copy_array
      ; +PatternMatch.Java.implements_nio "Buffer"
        &:: "allocate" <>$ capt_exp $--> Collection.allocate
      ; +PatternMatch.Java.implements_nio "Buffer"
        &:: "hasRemaining" <>$ capt_exp $!--> Collection.hasNext
      ; +PatternMatch.Java.implements_iterator &:: "next" <>$ capt_exp $!--> Collection.next
      ; +PatternMatch.Java.implements_lang "CharSequence"
        &:: "<init>" <>$ capt_exp $+ capt_exp $--> JavaString.copy_constructor
      ; +PatternMatch.Java.implements_lang "CharSequence"
        &:: "charAt" <>$ capt_exp $+ capt_exp $--> JavaString.charAt
      ; +PatternMatch.Java.implements_lang "CharSequence"
        &:: "equals"
        $ any_arg_of_typ (+PatternMatch.Java.implements_lang "CharSequence")
        $+ any_arg_of_typ (+PatternMatch.Java.implements_lang "CharSequence")
        $--> by_value Dom.Val.Itv.unknown_bool
      ; +PatternMatch.Java.implements_lang "CharSequence"
        &:: "length" <>$ capt_exp $!--> JavaString.length
      ; +PatternMatch.Java.implements_lang "CharSequence"
        &:: "substring" <>$ any_arg $+ capt_exp $+ capt_exp $--> JavaString.substring
      ; +PatternMatch.Java.implements_lang "Class"
        &:: "getCanonicalName" &::.*--> JavaString.inferbo_constant_string
      ; +PatternMatch.Java.implements_lang "Class"
        &:: "getEnumConstants" <>$ capt_exp $--> JavaClass.get_enum_constants
      ; +PatternMatch.Java.implements_lang "Class"
        &:: "getFields" <>$ capt_exp $--> JavaClass.get_fields
      ; +PatternMatch.Java.implements_lang "Enum"
        &:: "name" &::.*--> JavaString.inferbo_constant_string
      ; +PatternMatch.Java.implements_lang "Integer"
        &:: "intValue" <>$ capt_exp $--> JavaInteger.intValue
      ; +PatternMatch.Java.implements_lang "Integer"
        &:: "valueOf" <>$ capt_exp $--> JavaInteger.valueOf
      ; +PatternMatch.Java.implements_lang "Iterable"
        &:: "iterator" <>$ capt_exp $!--> Collection.iterator
      ; +PatternMatch.Java.implements_lang "Math"
        &:: "max" <>$ capt_exp $+ capt_exp
        $--> eval_binop ~f:(Itv.max_sem ~use_minmax_bound:true)
      ; +PatternMatch.Java.implements_lang "Math"
        &:: "min" <>$ capt_exp $+ capt_exp
        $--> eval_binop ~f:(Itv.min_sem ~use_minmax_bound:true)
      ; +PatternMatch.Java.implements_lang "String"
        &:: "<init>" <>$ capt_exp $--> JavaString.empty_constructor
      ; +PatternMatch.Java.implements_lang "String"
        &:: "concat" <>$ capt_exp $+ capt_exp $+...$--> JavaString.concat
      ; +PatternMatch.Java.implements_lang "String"
        &:: "valueOf" <>$ capt_exp_of_prim_typ short_array $--> JavaString.create_from_short_arr
      ; +PatternMatch.Java.implements_lang "String"
        &:: "indexOf" <>$ capt_exp $+ any_arg $+...$--> JavaString.indexOf
      ; +PatternMatch.Java.implements_lang "String"
        &:: "lastIndexOf" <>$ capt_exp $+ any_arg $+...$--> JavaString.indexOf
      ; +PatternMatch.Java.implements_lang "String"
        &:: "replace" <>$ capt_exp $+ any_arg_of_prim_typ int_typ $+ any_arg_of_prim_typ int_typ
        $--> JavaString.replace
      ; +PatternMatch.Java.implements_lang "String"
        &:: "split" <>$ any_arg $+ any_arg $+ capt_exp $--> JavaString.split_with_limit
      ; +PatternMatch.Java.implements_lang "String"
        &:: "split" <>$ capt_exp $+ any_arg $--> JavaString.split
      ; +PatternMatch.Java.implements_lang "String"
        &:: "startsWith"
        $ any_arg_of_typ (+PatternMatch.Java.implements_lang "String")
        $+ any_arg_of_typ (+PatternMatch.Java.implements_lang "String")
        $--> by_value Dom.Val.Itv.unknown_bool
      ; +PatternMatch.Java.implements_lang "String"
        &:: "substring" <>$ capt_exp $+ capt_exp $--> JavaString.substring_no_end
      ; +PatternMatch.Java.implements_lang "StringBuilder"
        &:: "append" <>$ capt_exp $+ capt_exp $+...$--> JavaString.concat
      ; +PatternMatch.Java.implements_lang "StringBuilder" &:: "toString" <>$ capt_exp $+...$--> id
      ; +PatternMatch.Java.implements_list
        &:: "listIterator" <>$ capt_exp $+...$--> Collection.iterator
      ; +PatternMatch.Java.implements_list
        &:: "subList" <>$ any_arg $+ capt_exp $+ capt_exp $--> Collection.subList
      ; +PatternMatch.Java.implements_map &:: "entrySet" <>$ capt_exp $!--> Collection.iterator
      ; +PatternMatch.Java.implements_map &:: "keySet" <>$ capt_exp $!--> Collection.iterator
      ; +PatternMatch.Java.implements_map &:: "put" <>$ capt_var $+ any_arg $+ capt_exp
        $--> Collection.put_with_elem
      ; +PatternMatch.Java.implements_map &:: "putAll" <>$ capt_var $+ capt_exp
        $--> Collection.putAll
      ; +PatternMatch.Java.implements_map &:: "size" <>$ capt_exp $!--> Collection.size
      ; +PatternMatch.Java.implements_map &:: "values" <>$ capt_exp $!--> Collection.iterator
      ; +PatternMatch.Java.implements_nio "Buffer"
        &::+ startsWith "get" <>$ capt_exp $+...$--> Buffer.get
      ; +PatternMatch.Java.implements_nio "Buffer" &:: "duplicate" <>$ capt_exp $+...$--> id
      ; +PatternMatch.Java.implements_nio "channels.FileChannel"
        &:: "read" <>$ any_arg $+ capt_exp $+ any_arg $--> FileChannel.read
      ; +PatternMatch.Java.implements_org_json "JSONArray"
        &:: "<init>" <>$ capt_var
        $+ capt_exp_of_typ (+PatternMatch.Java.implements_collection)
        $--> Collection.init_with_arg
      ; +PatternMatch.Java.implements_org_json "JSONArray"
        &:: "length" <>$ capt_exp $!--> Collection.size
      ; +PatternMatch.Java.implements_org_json "JSONArray"
        &:: "put" <>$ capt_var $+...$--> Collection.put
      ; +PatternMatch.Java.implements_pseudo_collection
        &:: "put" <>$ capt_var $+ any_arg $+ any_arg $--> Collection.put
      ; +PatternMatch.Java.implements_pseudo_collection
        &:: "size" <>$ capt_exp $!--> Collection.size
      ; (* Java linked list models *)
        +PatternMatch.Java.implements_app_activity
        &:: "getParent" <>$ capt_arg $!--> JavaLinkedList.next
      ; +PatternMatch.Java.implements_app_fragment
        &:: "getParentFragment" <>$ capt_arg $!--> JavaLinkedList.next
      ; +PatternMatch.Java.implements_graphql_story
        &:: "getAttachedStory" <>$ capt_arg $!--> JavaLinkedList.next
      ; +PatternMatch.Java.implements_psi_element
        &:: "getParent" <>$ capt_arg $!--> JavaLinkedList.next
      ; +PatternMatch.Java.implements_view_group
        &:: "getParent" <>$ capt_arg $!--> JavaLinkedList.next
      ; +PatternMatch.Java.implements_view_parent
        &:: "getParent" <>$ capt_arg $!--> JavaLinkedList.next ]
end
