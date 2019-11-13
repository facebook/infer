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
module BoUtils = BufferOverrunUtils
module Dom = BufferOverrunDomain
module PO = BufferOverrunProofObligations
module Sem = BufferOverrunSemantics
module Relation = BufferOverrunDomainRelation
module Trace = BufferOverrunTrace
open BoUtils.ModelEnv

type exec_fun = model_env -> ret:Ident.t * Typ.t -> Dom.Mem.t -> Dom.Mem.t

type check_fun = model_env -> Dom.Mem.t -> PO.ConditionSet.checked_t -> PO.ConditionSet.checked_t

type model = {exec: exec_fun; check: check_fun}

let no_check _model_env _mem cond_set = cond_set

let no_model =
  let exec {pname; location} ~ret:(id, _) mem =
    L.d_printfln_escaped "No model for %a" Typ.Procname.pp pname ;
    Dom.Mem.add_unknown_from id ~callee_pname:pname ~location mem
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


(* It returns a tuple of:
     - type of array element
     - stride of the type
     - array size
     - flexible array size *)
let get_malloc_info : Exp.t -> Typ.t * Int.t option * Exp.t * Exp.t option = function
  | Exp.BinOp (Binop.Mult _, Exp.Sizeof {typ; nbytes}, length)
  | Exp.BinOp (Binop.Mult _, length, Exp.Sizeof {typ; nbytes}) ->
      (typ, nbytes, length, None)
  (* In Java all arrays are dynamically allocated *)
  | Exp.Sizeof {typ; nbytes; dynamic_length= Some arr_length} when Language.curr_language_is Java ->
      (typ, nbytes, arr_length, Some arr_length)
  | Exp.Sizeof {typ; nbytes; dynamic_length} ->
      (typ, nbytes, Exp.one, dynamic_length)
  | x ->
      (Typ.mk (Typ.Tint Typ.IChar), Some 1, x, None)


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
        let offset = ArrayBlk.ArrInfo.offsetof arrinfo in
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
  let exec ({pname; node_hash; location; tenv; integer_type_widths} as model_env) ~ret:(id, _) mem =
    let size_exp = Prop.exp_normalize_noabs tenv Sil.sub_empty size_exp in
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
      Allocsite.make pname ~node_hash ~inst_num:0 ~dimension:1 ~path ~represents_multiple_values
    in
    let size_exp_opt =
      let size_exp = Option.value dyn_length ~default:length0 in
      Relation.SymExp.of_exp ~get_sym_f:(Sem.get_sym_f integer_type_widths mem) size_exp
    in
    if Language.curr_language_is Java then
      let internal_arr =
        let allocsite =
          Allocsite.make pname ~node_hash ~inst_num:1 ~dimension:1 ~path:None
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
      |> Dom.Mem.init_array_relation allocsite ~offset_opt:(Some offset) ~size ~size_exp_opt
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


let eval_string_len arr_exp mem = Dom.Mem.get_c_strlen (Sem.eval_locs arr_exp mem) mem

let strlen arr_exp =
  let exec _ ~ret:(id, _) mem =
    let v = eval_string_len arr_exp mem in
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
      let relation = Dom.Mem.get_relation mem in
      let latest_prune = Dom.Mem.get_latest_prune mem in
      fun arr cond_set ->
        BoUtils.Check.array_access ~arr ~idx ~idx_sym_exp:None ~relation ~is_plus:true
          ~last_included:false ~latest_prune location cond_set
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
      let relation = Dom.Mem.get_relation mem in
      let latest_prune = Dom.Mem.get_latest_prune mem in
      BoUtils.Check.array_access ~arr ~idx ~idx_sym_exp:None ~relation ~is_plus:true
        ~last_included:false ~latest_prune location cond_set
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
    let size_exp = Prop.exp_normalize_noabs tenv Sil.sub_empty size_exp in
    let typ, _, length0, dyn_length = get_malloc_info size_exp in
    let length = Sem.eval integer_type_widths length0 mem in
    let v = Sem.eval integer_type_widths src_exp mem |> Dom.Val.set_array_length location ~length in
    let mem = Dom.Mem.add_stack (Loc.of_id id) v mem in
    Option.value_map dyn_length ~default:mem ~f:(fun dyn_length ->
        let dyn_length = Dom.Val.get_itv (Sem.eval integer_type_widths dyn_length mem) in
        BoUtils.Exec.set_dyn_length model_env typ (Dom.Val.get_array_locs v) dyn_length mem )
  and check = check_alloc_size ~can_be_zero:false size_exp in
  {exec; check}


let placement_new size_exp (src_exp1, t1) src_arg2_opt =
  match (t1.Typ.desc, src_arg2_opt) with
  | Tint _, None | Tint _, Some (_, {Typ.desc= Tint _}) ->
      malloc ~can_be_zero:true (Exp.BinOp (Binop.PlusA (Some Typ.size_t), size_exp, src_exp1))
  | Tstruct (CppClass (name, _)), None
    when [%compare.equal: string list] (QualifiedCppName.to_list name) ["std"; "nothrow_t"] ->
      malloc ~can_be_zero:true size_exp
  | _, _ ->
      let exec {integer_type_widths} ~ret:(id, _) mem =
        let src_exp =
          if Typ.is_pointer_to_void t1 then src_exp1
          else
            match src_arg2_opt with
            | Some (src_exp2, t2) when Typ.is_pointer_to_void t2 ->
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
  let exec ({pname; node_hash; location; integer_type_widths} as model_env) ~ret:((id, _) as ret)
      mem =
    let v =
      let src_strlen = Dom.Mem.get_c_strlen (Sem.eval_locs src_exp mem) mem in
      let length = Sem.eval integer_type_widths length_exp mem in
      let size = Itv.incr (Itv.min_sem (Dom.Val.get_itv src_strlen) (Dom.Val.get_itv length)) in
      let allocsite =
        let represents_multiple_values = not (Itv.is_one size) in
        Allocsite.make pname ~node_hash ~inst_num:0 ~dimension:1 ~path:None
          ~represents_multiple_values
      in
      let traces =
        Trace.Set.join (Dom.Val.get_traces src_strlen) (Dom.Val.get_traces length)
        |> Trace.Set.add_elem location (Trace.through ~risky_fun:(Some Trace.strndup))
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


let by_value =
  let exec ~value _ ~ret:(ret_id, _) mem = model_by_value value ret_id mem in
  fun value -> {exec= exec ~value; check= no_check}


let by_risky_value_from lib_fun =
  let exec ~value {location} ~ret:(ret_id, _) mem =
    let traces =
      Trace.(Set.add_elem location (through ~risky_fun:(Some lib_fun))) (Dom.Val.get_traces value)
    in
    model_by_value {value with traces} ret_id mem
  in
  fun value -> {exec= exec ~value; check= no_check}


let bottom =
  let exec _model_env ~ret:_ _mem = Dom.Mem.bot in
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
let set_array_length array length_exp =
  let exec {pname; node_hash; location; integer_type_widths} ~ret:_ mem =
    match array with
    | Exp.Lvar array_pvar, {Typ.desc= Typ.Tarray {stride}} ->
        let length = Sem.eval integer_type_widths length_exp mem in
        let stride = Option.map ~f:IntLit.to_int_exn stride in
        let path = Some (Symb.SymbolPath.of_pvar array_pvar) in
        let traces = Trace.(Set.add_elem location ArrayDeclaration) (Dom.Val.get_traces length) in
        let size = Dom.Val.get_itv length in
        let allocsite =
          let represents_multiple_values = not (Itv.is_one size) in
          Allocsite.make pname ~node_hash ~inst_num:0 ~dimension:1 ~path ~represents_multiple_values
        in
        let v = Dom.Val.of_c_array_alloc allocsite ~stride ~offset:Itv.zero ~size ~traces in
        Dom.Mem.add_stack (Loc.of_pvar array_pvar) v mem
    | _ ->
        L.(die InternalError) "Unexpected type of first argument for __set_array_length() "
  and check = check_alloc_size ~can_be_zero:false length_exp in
  {exec; check}


let snprintf = by_risky_value_from Trace.snprintf Dom.Val.Itv.nat

let vsnprintf = by_risky_value_from Trace.vsnprintf Dom.Val.Itv.nat

let copy array_v ret_id mem =
  let dest_loc = Loc.of_id ret_id |> PowLoc.singleton in
  Dom.Mem.update_mem dest_loc array_v mem


(** Creates a new array with the values from the given array.*)
let create_copy_array src_exp =
  let exec {integer_type_widths} ~ret:(id, _) mem =
    let array_v = Sem.eval integer_type_widths src_exp mem in
    copy array_v id mem
  in
  {exec; check= no_check}


module CFArray = struct
  (** Creates a new array from the given array by copying the first X
     elements. *)
  let create_array src_exp size_exp =
    let {exec= malloc_exec; check= _} = malloc ~can_be_zero:true size_exp in
    let exec model_env ~ret:((id, _) as ret) mem =
      let mem = malloc_exec model_env ~ret mem in
      let dest_loc = Loc.of_id id |> PowLoc.singleton in
      let dest_arr_loc = Dom.Val.get_array_locs (Dom.Mem.find_set dest_loc mem) in
      let src_arr_v = Dom.Mem.find_set (Sem.eval_locs src_exp mem) mem in
      Dom.Mem.update_mem dest_arr_loc src_arr_v mem
    and check {location; integer_type_widths} mem cond_set =
      BoUtils.Check.lindex integer_type_widths ~array_exp:src_exp ~index_exp:size_exp
        ~last_included:true mem location cond_set
    in
    {exec; check}


  let at (array_exp, _) (index_exp, _) = at ?size:None array_exp index_exp

  let length e =
    let exec {integer_type_widths} ~ret:(ret_id, _) mem =
      let v = Sem.eval_arr integer_type_widths e mem in
      let length = Dom.Val.of_itv (ArrayBlk.sizeof (Dom.Val.get_array_blk v)) in
      Dom.Mem.add_stack (Loc.of_id ret_id) length mem
    in
    {exec; check= no_check}
end

module Split = struct
  let std_vector ~adds_at_least_one (vector_exp, vector_typ) location mem =
    let increment = if adds_at_least_one then Dom.Val.Itv.pos else Dom.Val.Itv.nat in
    let vector_type_name = Option.value_exn (vector_typ |> Typ.strip_ptr |> Typ.name) in
    let size_field = Typ.Fieldname.Clang.from_class_name vector_type_name "infer_size" in
    let vector_size_locs = Sem.eval_locs vector_exp mem |> PowLoc.append_field ~fn:size_field in
    let f_trace _ traces = Trace.(Set.add_elem location (through ~risky_fun:None)) traces in
    Dom.Mem.transform_mem ~f:(Dom.Val.plus_a ~f_trace increment) vector_size_locs mem
end

module Boost = struct
  module Split = struct
    let std_vector vector_arg =
      let exec {location} ~ret:_ mem =
        Split.std_vector ~adds_at_least_one:true vector_arg location mem
      in
      {exec; check= no_check}
  end
end

module Folly = struct
  module Split = struct
    let std_vector vector_arg ignore_empty_opt =
      let exec {location; integer_type_widths} ~ret:_ mem =
        let adds_at_least_one =
          match ignore_empty_opt with
          | Some ignore_empty_exp ->
              Sem.eval integer_type_widths ignore_empty_exp mem |> Dom.Val.get_itv |> Itv.is_false
          | None ->
              (* default: ignore_empty is false *)
              true
        in
        Split.std_vector ~adds_at_least_one vector_arg location mem
      in
      {exec; check= no_check}
  end
end

module StdArray = struct
  let constructor _size =
    let exec _model_env ~ret:_ mem = mem (* initialize? *) in
    {exec; check= no_check}


  let at size (array_exp, _) (index_exp, _) = at ~size array_exp index_exp

  let begin_ _size (array_exp, _) =
    let exec {location; integer_type_widths} ~ret:(id, _) mem =
      let v =
        Sem.eval integer_type_widths array_exp mem |> Dom.Val.set_array_offset location Itv.zero
      in
      Dom.Mem.add_stack (Loc.of_id id) v mem
    in
    {exec; check= no_check}


  let end_ size (array_exp, _) =
    let exec {location; integer_type_widths} ~ret:(id, _) mem =
      let v =
        let offset = Itv.of_int_lit (IntLit.of_int64 size) in
        Sem.eval integer_type_widths array_exp mem |> Dom.Val.set_array_offset location offset
      in
      Dom.Mem.add_stack (Loc.of_id id) v mem
    in
    {exec; check= no_check}


  let back size (array_exp, _) =
    let exec {location; integer_type_widths} ~ret:(id, _) mem =
      let v =
        let offset = Itv.of_int_lit (IntLit.of_int64 Int64.(size - one)) in
        Sem.eval integer_type_widths array_exp mem |> Dom.Val.set_array_offset location offset
      in
      Dom.Mem.add_stack (Loc.of_id id) v mem
    in
    {exec; check= no_check}
end

module ArrObjCommon = struct
  let deref_of {integer_type_widths} exp ~fn mem =
    Dom.Val.get_all_locs (Sem.eval_arr integer_type_widths exp mem) |> PowLoc.append_field ~fn


  let eval_size model_env exp ~fn mem =
    Sem.eval_array_locs_length (deref_of model_env exp ~fn mem) mem


  let size_exec exp ~fn model_env ~ret:(id, _) mem =
    let arr_locs = deref_of model_env exp ~fn mem in
    let mem = Dom.Mem.add_stack (Loc.of_id id) (Sem.eval_array_locs_length arr_locs mem) mem in
    load_size_alias id arr_locs mem


  let at arr_exp ~fn index_exp =
    let exec ({pname; location} as model_env) ~ret:(id, _) mem =
      let array_v =
        let locs = deref_of model_env arr_exp ~fn mem in
        if PowLoc.is_bot locs then Dom.Val.unknown_from ~callee_pname:(Some pname) ~location
        else Dom.Mem.find_set locs mem
      in
      Dom.Mem.add_stack (Loc.of_id id) array_v mem
    and check ({location; integer_type_widths} as model_env) mem cond_set =
      let idx = Sem.eval integer_type_widths index_exp mem in
      let arr = Dom.Mem.find_set (deref_of model_env arr_exp ~fn mem) mem in
      let relation = Dom.Mem.get_relation mem in
      let latest_prune = Dom.Mem.get_latest_prune mem in
      BoUtils.Check.array_access ~arr ~idx ~idx_sym_exp:None ~relation ~is_plus:true
        ~last_included:false ~latest_prune location cond_set
    in
    {exec; check}


  let copy_constructor model_env deref_of_tgt ~fn src_exp mem =
    let deref_of_src = deref_of model_env src_exp ~fn mem in
    Dom.Mem.update_mem deref_of_tgt (Dom.Mem.find_set deref_of_src mem) mem


  let constructor_from_char_ptr ({integer_type_widths} as model_env) tgt_deref ~fn src mem =
    let elem_locs = PowLoc.append_field tgt_deref ~fn in
    match src with
    | Exp.Const (Const.Cstr s) ->
        BoUtils.Exec.decl_string model_env ~do_alloc:true elem_locs s mem
    | _ ->
        let v = Sem.eval integer_type_widths src mem in
        Dom.Mem.update_mem elem_locs v mem
end

module StdVector = struct
  let append_field loc ~vec_typ ~elt_typ =
    Loc.append_field loc ~fn:(BufferOverrunField.cpp_vector_elem ~vec_typ ~elt_typ)


  let append_fields locs ~vec_typ ~elt_typ =
    PowLoc.append_field locs ~fn:(BufferOverrunField.cpp_vector_elem ~vec_typ ~elt_typ)


  let deref_of model_env elt_typ (vec_exp, vec_typ) mem =
    let fn = BufferOverrunField.cpp_vector_elem ~vec_typ ~elt_typ in
    ArrObjCommon.deref_of model_env vec_exp ~fn mem


  (* The (3) constructor in https://en.cppreference.com/w/cpp/container/vector/vector *)
  let constructor_size elt_typ (vec_exp, vec_typ) size_exp =
    let {exec= malloc_exec; check} = malloc ~can_be_zero:true size_exp in
    let exec ({pname; node_hash; integer_type_widths; location} as model_env) ~ret:((id, _) as ret)
        mem =
      let mem = malloc_exec model_env ~ret mem in
      let vec_locs = Sem.eval_locs vec_exp mem in
      let deref_of_vec =
        Allocsite.make pname ~node_hash ~inst_num:1 ~dimension:1 ~path:None
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
  let constructor_copy elt_typ (vec_exp, vec_typ) src_exp =
    let exec ({integer_type_widths} as model_env) ~ret:_ mem =
      let vec_locs, traces =
        let v = Sem.eval integer_type_widths vec_exp mem in
        (Dom.Val.get_all_locs v, Dom.Val.get_traces v)
      in
      let deref_of_vec = append_fields vec_locs ~vec_typ ~elt_typ in
      let fn = BufferOverrunField.cpp_vector_elem ~vec_typ ~elt_typ in
      mem
      |> Dom.Mem.update_mem vec_locs (Dom.Val.of_pow_loc ~traces deref_of_vec)
      |> ArrObjCommon.copy_constructor model_env deref_of_vec ~fn src_exp
    in
    {exec; check= no_check}


  let at elt_typ (vec_exp, vec_typ) index_exp =
    ArrObjCommon.at vec_exp ~fn:(BufferOverrunField.cpp_vector_elem ~vec_typ ~elt_typ) index_exp


  let set_size {location} locs new_size mem =
    Dom.Mem.transform_mem locs mem ~f:(fun v -> Dom.Val.set_array_length location ~length:new_size v)


  let empty elt_typ vec_arg =
    let exec model_env ~ret:(id, _) mem =
      let deref_of_vec = deref_of model_env elt_typ vec_arg mem in
      let array_v = Dom.Mem.find_set deref_of_vec mem in
      let traces = Dom.Val.get_traces array_v in
      let size = ArrayBlk.sizeof (Dom.Val.get_array_blk array_v) in
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


  let push_back elt_typ vec_arg elt_exp =
    let exec model_env ~ret:_ mem =
      let arr_locs = deref_of model_env elt_typ vec_arg mem in
      let mem =
        let new_size =
          Dom.Val.plus_a (Sem.eval_array_locs_length arr_locs mem) (Dom.Val.of_int 1)
        in
        set_size model_env arr_locs new_size mem
      in
      let elt_locs = Dom.Val.get_all_locs (Dom.Mem.find_set arr_locs mem) in
      let elt_v = Dom.Mem.find_set (Sem.eval_locs elt_exp mem) mem in
      Dom.Mem.update_mem elt_locs elt_v mem
    in
    {exec; check= no_check}


  let size elt_typ (vec_exp, vec_typ) =
    let exec =
      ArrObjCommon.size_exec vec_exp ~fn:(BufferOverrunField.cpp_vector_elem ~vec_typ ~elt_typ)
    in
    {exec; check= no_check}
end

module StdBasicString = struct
  let constructor_from_char_ptr char_typ (tgt_exp, tgt_typ) src ~len_opt =
    let exec ({pname; node_hash} as model_env) ~ret mem =
      let mem =
        Option.value_map len_opt ~default:mem ~f:(fun len ->
            let {exec= malloc_exec} = malloc ~can_be_zero:true len in
            malloc_exec model_env ~ret mem )
      in
      let tgt_locs = Sem.eval_locs tgt_exp mem in
      let tgt_deref =
        let allocsite =
          Allocsite.make pname ~node_hash ~inst_num:1 ~dimension:1 ~path:None
            ~represents_multiple_values:false
        in
        PowLoc.singleton (Loc.of_allocsite allocsite)
      in
      let mem =
        Dom.Mem.update_mem tgt_locs (Dom.Val.of_pow_loc ~traces:Trace.Set.bottom tgt_deref) mem
      in
      let fn = BufferOverrunField.cpp_vector_elem ~vec_typ:tgt_typ ~elt_typ:char_typ in
      ArrObjCommon.constructor_from_char_ptr model_env tgt_deref src ~fn mem
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
  let constructor_from_char_ptr_with_len char_typ (tgt_exp, tgt_typ) src len =
    constructor_from_char_ptr char_typ (tgt_exp, tgt_typ) src ~len_opt:(Some len)


  (* The (5) constructor in https://en.cppreference.com/w/cpp/string/basic_string/basic_string *)
  let constructor_from_char_ptr_without_len char_typ (tgt_exp, tgt_typ) src =
    constructor_from_char_ptr char_typ (tgt_exp, tgt_typ) src ~len_opt:None


  (* The (7) constructor in https://en.cppreference.com/w/cpp/string/basic_string/basic_string *)
  let copy_constructor = StdVector.constructor_copy

  let empty = StdVector.empty

  let length = StdVector.size
end

(** Java's integers are modeled with an indirection to a memory
   location that holds the actual integer value *)
module JavaInteger = struct
  let intValue exp =
    let exec _ ~ret:(id, _) mem =
      let powloc = Sem.eval_locs exp mem in
      let v = if PowLoc.is_empty powloc then Dom.Val.Itv.top else Dom.Mem.find_set powloc mem in
      model_by_value v id mem
    in
    {exec; check= no_check}


  let valueOf exp =
    let exec {pname; node_hash; location; integer_type_widths} ~ret:(id, _) mem =
      let represents_multiple_values = false in
      let int_allocsite =
        Allocsite.make pname ~node_hash ~inst_num:0 ~dimension:0 ~path:None
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

(* Java's Collections are represented like arrays. But we don't care about the elements.
- when they are constructed, we set the size to 0
- each time we add an element, we increase the length of the array
- each time we delete an element, we decrease the length of the array *)
module Collection = struct
  let create_collection {pname; node_hash; location} ~ret:(id, _) mem ~length =
    let represents_multiple_values = true in
    let traces = Trace.(Set.singleton location ArrayDeclaration) in
    let coll_allocsite =
      Allocsite.make pname ~node_hash ~inst_num:0 ~dimension:1 ~path:None
        ~represents_multiple_values
    in
    let internal_array =
      let allocsite =
        Allocsite.make pname ~node_hash ~inst_num:1 ~dimension:1 ~path:None
          ~represents_multiple_values
      in
      Dom.Val.of_java_array_alloc allocsite ~length ~traces
    in
    let coll_loc = Loc.of_allocsite coll_allocsite in
    let internal_array_loc =
      Loc.append_field coll_loc ~fn:BufferOverrunField.java_collection_internal_array
    in
    mem
    |> Dom.Mem.add_heap internal_array_loc internal_array
    |> Dom.Mem.add_stack (Loc.of_id id) (coll_loc |> PowLoc.singleton |> Dom.Val.of_pow_loc ~traces)


  (** Returns a fixed-size list with a given length backed by the specified array. *)
  let copyOf array_exp length_exp =
    let exec ({integer_type_widths} as model) ~ret:(id, _) mem =
      let array_v = Sem.eval integer_type_widths array_exp mem in
      copy array_v id mem |> set_size model array_v length_exp
    in
    {exec; check= no_check}


  let new_collection =
    let exec = create_collection ~length:Itv.zero in
    {exec; check= no_check}


  let eval_collection_internal_array_locs coll_exp mem =
    Sem.eval_locs coll_exp mem
    |> PowLoc.append_field ~fn:BufferOverrunField.java_collection_internal_array


  let get_collection_internal_array_locs coll_id mem =
    let coll = Dom.Mem.find (Loc.of_id coll_id) mem in
    Dom.Val.get_pow_loc coll
    |> PowLoc.append_field ~fn:BufferOverrunField.java_collection_internal_array


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


  let add coll_id = {exec= change_size_by_incr coll_id; check= no_check}

  let singleton_collection =
    let exec env ~ret:((id, _) as ret) mem =
      let {exec= new_exec; check= _} = new_collection in
      let mem = new_exec env ~ret mem in
      change_size_by_incr id ~ret env mem
    in
    {exec; check= no_check}


  (** increase the size by [0, 1] because put replaces the value
     rather than add a new one when the key is found in the map *)
  let put coll_id = {exec= change_size_by_incr_or_not coll_id; check= no_check}

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
      model_by_value itr ret_id mem |> Dom.Mem.add_iterator_offset_alias ret_id
    in
    {exec; check= no_check}


  let init_with_capacity size_exp =
    let exec _ ~ret:_ mem = mem and check = check_alloc_size ~can_be_zero:true size_exp in
    {exec; check}


  let init_with_arg lhs_id rhs_exp =
    let exec {integer_type_widths} ~ret:_ mem =
      let itr = Sem.eval integer_type_widths rhs_exp mem in
      model_by_value itr lhs_id mem
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


  (** Returns a view of the portion of this list between the specified
     fromIndex, inclusive, and toIndex, exclusive. Simply model it as
     creating a new list with length toIndex - fromIndex.  *)
  let subList from_exp to_exp =
    let exec ({integer_type_widths} as model) ~ret mem =
      let from_idx = Sem.eval integer_type_widths from_exp mem in
      let to_idx = Sem.eval integer_type_widths to_exp mem in
      let length = Itv.minus (Dom.Val.get_itv to_idx) (Dom.Val.get_itv from_idx) in
      create_collection model ~ret mem ~length
    in
    {exec; check= no_check}


  (** increase the size by [0, |collection_to_add|] because put replaces the value
     rather than add a new one when the key is found in the map *)
  let putAll coll_id coll_to_add =
    let exec model_env ~ret mem =
      let to_add_length =
        eval_collection_length coll_to_add mem |> Dom.Val.get_itv |> Itv.set_lb_zero
      in
      change_size_by ~size_f:(Itv.plus to_add_length) coll_id model_env ~ret mem
    in
    {exec; check= no_check}


  let check_index ~last_included coll_id index_exp {location; integer_type_widths} mem cond_set =
    let arr =
      let arr_locs = get_collection_internal_array_locs coll_id mem in
      Dom.Mem.find_set arr_locs mem
    in
    let idx = Sem.eval integer_type_widths index_exp mem in
    let idx_sym_exp =
      Relation.SymExp.of_exp ~get_sym_f:(Sem.get_sym_f integer_type_widths mem) index_exp
    in
    let relation = Dom.Mem.get_relation mem in
    let latest_prune = Dom.Mem.get_latest_prune mem in
    BoUtils.Check.array_access ~arr ~idx ~idx_sym_exp ~relation ~is_plus:true ~last_included
      ~latest_prune location cond_set


  let add_at_index (coll_id : Ident.t) index_exp =
    {exec= change_size_by_incr coll_id; check= check_index ~last_included:true coll_id index_exp}


  let remove_at_index coll_id index_exp =
    { exec= change_size_by ~size_f:Itv.decr coll_id
    ; check= check_index ~last_included:false coll_id index_exp }


  let addAll_at_index coll_id index_exp coll_to_add =
    let exec model_env ~ret mem =
      let to_add_length = eval_collection_length coll_to_add mem |> Dom.Val.get_itv in
      change_size_by ~size_f:(Itv.plus to_add_length) coll_id model_env ~ret mem
    in
    {exec; check= check_index ~last_included:true coll_id index_exp}


  let get_or_set_at_index coll_id index_exp =
    let exec _model_env ~ret:_ mem = mem in
    {exec; check= check_index ~last_included:false coll_id index_exp}
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
    let exec ({pname; node_hash} as model_env) ~ret:(id, _) mem =
      let length_v, elem =
        let length1, elem1 = get_length_and_elem model_env exp1 mem in
        let length2, elem2 = get_length_and_elem model_env exp2 mem in
        (Dom.Val.plus_a length1 length2, Dom.Val.join elem1 elem2)
      in
      let length, traces = (Dom.Val.get_itv length_v, Dom.Val.get_traces length_v) in
      let arr_loc =
        Allocsite.make pname ~node_hash ~inst_num:0 ~dimension:1 ~path:None
          ~represents_multiple_values:false
        |> Loc.of_allocsite
      in
      let elem_alloc =
        Allocsite.make pname ~node_hash ~inst_num:1 ~dimension:1 ~path:None
          ~represents_multiple_values:true
      in
      Dom.Mem.add_stack (Loc.of_id id) (Dom.Val.of_loc arr_loc) mem
      |> Dom.Mem.add_heap (Loc.append_field arr_loc ~fn)
           (Dom.Val.of_java_array_alloc elem_alloc ~length ~traces)
      |> Dom.Mem.add_heap (Loc.of_allocsite elem_alloc) elem
    in
    {exec; check= no_check}


  let length exp = {exec= ArrObjCommon.size_exec exp ~fn; check= no_check}

  (** Given a string of length n, return itv [-1, n_u-1]. *)
  let range_itv_mone model_env exp mem =
    ArrObjCommon.eval_size model_env exp ~fn mem
    |> BufferOverrunDomain.Val.get_itv |> Itv.set_lb_zero |> Itv.decr


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


  (* https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#String(byte[]) *)
  let constructor tgt_exp src =
    let exec model_env ~ret:_ mem =
      constructor_from_char_ptr model_env (Sem.eval_locs tgt_exp mem) src mem
    in
    {exec; check= no_check}


  (* https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#String(java.lang.String) *)
  let copy_constructor vec_exp src_exp =
    let exec ({integer_type_widths} as model_env) ~ret:_ mem =
      let vec_locs = Sem.eval integer_type_widths vec_exp mem |> Dom.Val.get_all_locs in
      let deref_of_vec = PowLoc.append_field vec_locs ~fn in
      ArrObjCommon.copy_constructor model_env deref_of_vec ~fn src_exp mem
    in
    {exec; check= no_check}
end

module Preconditions = struct
  let check_argument exp =
    let exec {integer_type_widths} ~ret:_ mem = Sem.Prune.prune integer_type_widths exp mem in
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

module ByteBuffer = struct
  let get_int buf_exp =
    let exec _ ~ret:(ret_id, _) mem =
      let range = Dom.Mem.find_set (Sem.eval_locs buf_exp mem) mem |> Dom.Val.get_modeled_range in
      let v = Dom.Val.of_itv Itv.nat |> Dom.Val.set_modeled_range range in
      model_by_value v ret_id mem
    in
    {exec; check= no_check}
end

module Object = struct
  let clone exp =
    let exec {integer_type_widths} ~ret:(ret_id, _) mem =
      let v = Sem.eval integer_type_widths exp mem in
      model_by_value v ret_id mem
    in
    {exec; check= no_check}
end

module Call = struct
  let dispatch : (Tenv.t, model) ProcnameDispatcher.Call.dispatcher =
    let open ProcnameDispatcher.Call in
    let mk_std_array () = -"std" &:: "array" < any_typ &+ capt_int in
    let std_array0 = mk_std_array () in
    let std_array1 = mk_std_array () in
    let std_array2 = mk_std_array () in
    let char_ptr = Typ.mk (Typ.Tptr (Typ.mk (Typ.Tint Typ.IChar), Pk_pointer)) in
    make_dispatcher
      [ -"__exit" <>--> bottom
      ; -"CFArrayCreate" <>$ any_arg $+ capt_exp $+ capt_exp $+...$--> CFArray.create_array
      ; -"CFArrayCreateCopy" <>$ any_arg $+ capt_exp $!--> create_copy_array
      ; -"MCFArrayGetCount" <>$ capt_exp $!--> CFArray.length
      ; -"CFDictionaryGetCount" <>$ capt_exp $!--> CFArray.length
      ; -"CFArrayGetCount" <>$ capt_exp $!--> CFArray.length
      ; -"CFArrayGetValueAtIndex" <>$ capt_arg $+ capt_arg $!--> CFArray.at
      ; -"exit" <>--> bottom
      ; -"__cast" <>$ capt_exp $+ capt_exp $+...$--> cast
      ; -"fgetc" <>--> by_value Dom.Val.Itv.m1_255
      ; -"fgets" <>$ capt_exp $+ capt_exp $+...$--> fgets
      ; -"infer_print" <>$ capt_exp $!--> infer_print
      ; -"malloc" <>$ capt_exp $+...$--> malloc ~can_be_zero:false
      ; -"calloc" <>$ capt_exp $+ capt_exp $!--> calloc ~can_be_zero:false
      ; -"__new"
        <>$ any_arg_of_typ (+PatternMatch.implements_pseudo_collection)
        $+...$--> Collection.new_collection
      ; -"__new"
        <>$ any_arg_of_typ (+PatternMatch.implements_collection)
        $+...$--> Collection.new_collection
      ; -"__new"
        <>$ any_arg_of_typ (+PatternMatch.implements_map)
        $+...$--> Collection.new_collection
      ; +PatternMatch.implements_map &:: "size" <>$ capt_exp $!--> Collection.size
      ; -"__new"
        <>$ any_arg_of_typ (+PatternMatch.implements_org_json "JSONArray")
        $+...$--> Collection.new_collection
      ; -"__new" <>$ capt_exp $+...$--> malloc ~can_be_zero:true
      ; -"__new_array" <>$ capt_exp $+...$--> malloc ~can_be_zero:true
      ; +PatternMatch.implements_arrays &:: "asList" <>$ capt_exp $!--> create_copy_array
      ; +PatternMatch.implements_arrays &:: "copyOf" <>$ capt_exp $+ capt_exp
        $+...$--> Collection.copyOf
      ; -"__placement_new" <>$ capt_exp $+ capt_arg $+? capt_arg $!--> placement_new
      ; -"realloc" <>$ capt_exp $+ capt_exp $+...$--> realloc
      ; -"__get_array_length" <>$ capt_exp $!--> get_array_length
      ; -"__set_array_length" <>$ capt_arg $+ capt_exp $!--> set_array_length
      ; +PatternMatch.implements_lang "CharSequence"
        &:: "length" <>$ capt_exp $!--> JavaString.length
      ; -"strlen" <>$ capt_exp $!--> strlen
      ; -"memcpy" <>$ capt_exp $+ capt_exp $+ capt_exp $+...$--> memcpy
      ; -"memmove" <>$ capt_exp $+ capt_exp $+ capt_exp $+...$--> memcpy
      ; -"memset" <>$ capt_exp $+ any_arg $+ capt_exp $!--> memset
      ; -"strcat" <>$ capt_exp $+ capt_exp $+...$--> strcat
      ; +PatternMatch.implements_lang "String"
        &:: "charAt" <>$ capt_exp $+ capt_exp $--> JavaString.charAt
      ; +PatternMatch.implements_lang "String"
        &:: "<init>" <>$ capt_exp
        $+ capt_exp_of_typ (+PatternMatch.implements_lang "String")
        $--> JavaString.copy_constructor
      ; +PatternMatch.implements_lang "String"
        &:: "<init>" <>$ capt_exp $+ capt_exp $--> JavaString.constructor
      ; +PatternMatch.implements_lang "String"
        &:: "concat" <>$ capt_exp $+ capt_exp $+...$--> JavaString.concat
      ; +PatternMatch.implements_lang "String"
        &:: "indexOf" <>$ capt_exp $+ any_arg $--> JavaString.indexOf
      ; -"strcpy" <>$ capt_exp $+ capt_exp $+...$--> strcpy
      ; -"strncpy" <>$ capt_exp $+ capt_exp $+ capt_exp $+...$--> strncpy
      ; -"snprintf" <>--> snprintf
      ; -"vsnprintf" <>--> vsnprintf
      ; -"strndup" <>$ capt_exp $+ capt_exp $+...$--> strndup
      ; -"boost" &:: "split"
        $ capt_arg_of_typ (-"std" &:: "vector")
        $+ any_arg $+ any_arg $+? any_arg $--> Boost.Split.std_vector
      ; -"folly" &:: "split" $ any_arg $+ any_arg
        $+ capt_arg_of_typ (-"std" &:: "vector")
        $+? capt_exp $--> Folly.Split.std_vector
      ; std_array0 >:: "array" &--> StdArray.constructor
      ; std_array1 >:: "begin" $ capt_arg $!--> StdArray.begin_
      ; std_array1 >:: "cbegin" $ capt_arg $!--> StdArray.begin_
      ; std_array1 >:: "end" $ capt_arg $!--> StdArray.end_
      ; std_array1 >:: "cend" $ capt_arg $!--> StdArray.end_
      ; std_array1 >:: "front" $ capt_arg $!--> StdArray.begin_
      ; std_array1 >:: "back" $ capt_arg $!--> StdArray.back
      ; std_array2 >:: "at" $ capt_arg $+ capt_arg $!--> StdArray.at
      ; std_array2 >:: "operator[]" $ capt_arg $+ capt_arg $!--> StdArray.at
      ; -"std" &:: "array" &::.*--> no_model
      ; -"std" &:: "basic_string"
        < capt_typ `T
        &+...>:: "basic_string" $ capt_arg
        $+ capt_exp_of_typ (-"std" &:: "basic_string")
        $--> StdBasicString.copy_constructor
      ; -"std" &:: "basic_string"
        < capt_typ `T
        &+...>:: "basic_string" $ capt_arg $+ capt_exp_of_prim_typ char_ptr
        $--> StdBasicString.constructor_from_char_ptr_without_len
      ; -"std" &:: "basic_string"
        < capt_typ `T
        &+...>:: "basic_string" $ capt_arg $+ capt_exp_of_prim_typ char_ptr
        $+ capt_exp_of_prim_typ (Typ.mk (Typ.Tint Typ.size_t))
        $--> StdBasicString.constructor_from_char_ptr_with_len
      ; -"std" &:: "basic_string"
        < capt_typ `T
        &+...>:: "empty" $ capt_arg $--> StdBasicString.empty
      ; -"std" &:: "basic_string"
        < capt_typ `T
        &+...>:: "length" $ capt_arg $--> StdBasicString.length
      ; -"std" &:: "basic_string"
        < capt_typ `T
        &+...>:: "size" $ capt_arg $--> StdBasicString.length
      ; -"std" &:: "basic_string" &:: "compare" &--> by_value Dom.Val.Itv.top
      ; +PatternMatch.implements_lang "String"
        &:: "equals"
        $ any_arg_of_typ (+PatternMatch.implements_lang "String")
        $+ any_arg_of_typ (+PatternMatch.implements_lang "String")
        $--> by_value Dom.Val.Itv.unknown_bool
      ; +PatternMatch.implements_lang "String"
        &:: "startsWith"
        $ any_arg_of_typ (+PatternMatch.implements_lang "String")
        $+ any_arg_of_typ (+PatternMatch.implements_lang "String")
        $--> by_value Dom.Val.Itv.unknown_bool
      ; -"std" &:: "operator=="
        $ any_arg_of_typ (-"std" &:: "basic_string")
        $+ any_arg_of_typ (-"std" &:: "basic_string")
        $--> by_value Dom.Val.Itv.unknown_bool
      ; -"std" &:: "operator=="
        $ any_arg_of_typ (-"std" &:: "basic_string")
        $+ any_arg_of_prim_typ char_ptr $--> by_value Dom.Val.Itv.unknown_bool
      ; -"std" &:: "operator==" $ any_arg_of_prim_typ char_ptr
        $+ any_arg_of_typ (-"std" &:: "basic_string")
        $--> by_value Dom.Val.Itv.unknown_bool
      ; -"std" &:: "operator!="
        $ any_arg_of_typ (-"std" &:: "basic_string")
        $+ any_arg_of_typ (-"std" &:: "basic_string")
        $--> by_value Dom.Val.Itv.unknown_bool
      ; -"std" &:: "operator!="
        $ any_arg_of_typ (-"std" &:: "basic_string")
        $+ any_arg_of_prim_typ char_ptr $--> by_value Dom.Val.Itv.unknown_bool
      ; -"std" &:: "operator!=" $ any_arg_of_prim_typ char_ptr
        $+ any_arg_of_typ (-"std" &:: "basic_string")
        $--> by_value Dom.Val.Itv.unknown_bool
      ; -"std" &:: "basic_string" &::.*--> no_model
      ; -"std" &:: "vector"
        < capt_typ `T
        &+ any_typ >:: "vector"
        $ capt_arg_of_typ (-"std" &:: "vector")
        $--> StdVector.constructor_empty
      ; -"std" &:: "vector"
        < capt_typ `T
        &+ any_typ >:: "vector"
        $ capt_arg_of_typ (-"std" &:: "vector")
        $+ capt_exp_of_prim_typ (Typ.mk (Typ.Tint Typ.size_t))
        $+? any_arg $--> StdVector.constructor_size
      ; -"std" &:: "vector"
        < capt_typ `T
        &+ any_typ >:: "vector"
        $ capt_arg_of_typ (-"std" &:: "vector")
        $+ capt_exp_of_typ (-"std" &:: "vector")
        $+? any_arg $--> StdVector.constructor_copy
      ; -"std" &:: "vector"
        < capt_typ `T
        &+ any_typ >:: "operator[]"
        $ capt_arg_of_typ (-"std" &:: "vector")
        $+ capt_exp $--> StdVector.at
      ; -"std" &:: "vector" < capt_typ `T &+ any_typ >:: "empty" $ capt_arg $--> StdVector.empty
      ; -"std" &:: "vector" < capt_typ `T &+ any_typ >:: "data" $ capt_arg $--> StdVector.data
      ; -"std" &:: "vector"
        < capt_typ `T
        &+ any_typ >:: "push_back" $ capt_arg $+ capt_exp $--> StdVector.push_back
      ; -"std" &:: "vector" < any_typ &+ any_typ >:: "reserve" $ any_arg $+ any_arg $--> no_model
      ; -"std" &:: "vector" < capt_typ `T &+ any_typ >:: "size" $ capt_arg $--> StdVector.size
      ; +PatternMatch.implements_collection
        &:: "<init>" <>$ capt_var_exn
        $+ capt_exp_of_typ (+PatternMatch.implements_collection)
        $--> Collection.init_with_arg
      ; +PatternMatch.implements_collection
        &:: "<init>" <>$ any_arg $+ capt_exp $--> Collection.init_with_capacity
        (* model sets as lists *)
      ; +PatternMatch.implements_collections &::+ unmodifiable <>$ capt_exp $--> Collection.iterator
      ; +PatternMatch.implements_collections &:: "singleton" <>--> Collection.singleton_collection
      ; +PatternMatch.implements_collections &:: "emptySet" <>--> Collection.new_collection
        (* model maps as lists *)
      ; +PatternMatch.implements_collections
        &:: "singletonMap" <>--> Collection.singleton_collection
      ; +PatternMatch.implements_collections
        &:: "singletonList" <>--> Collection.singleton_collection
      ; +PatternMatch.implements_collection
        &:: "get" <>$ capt_var_exn $+ capt_exp $--> Collection.get_or_set_at_index
      ; +PatternMatch.implements_collection
        &:: "set" <>$ capt_var_exn $+ capt_exp $+ any_arg $--> Collection.get_or_set_at_index
      ; +PatternMatch.implements_collection
        &:: "remove" <>$ capt_var_exn $+ capt_exp $--> Collection.remove_at_index
      ; +PatternMatch.implements_collection
        &:: "add" <>$ capt_var_exn $+ any_arg $--> Collection.add
      ; +PatternMatch.implements_pseudo_collection
        &:: "put" <>$ capt_var_exn $+ any_arg $+ any_arg $--> Collection.put
      ; +PatternMatch.implements_collection
        &:: "add" <>$ capt_var_exn $+ capt_exp $+ any_arg $!--> Collection.add_at_index
      ; +PatternMatch.implements_lang "Iterable"
        &:: "iterator" <>$ capt_exp $!--> Collection.iterator
      ; +PatternMatch.implements_list &:: "listIterator" <>$ capt_exp $+...$--> Collection.iterator
      ; +PatternMatch.implements_map &:: "entrySet" <>$ capt_exp $!--> Collection.iterator
      ; +PatternMatch.implements_map &:: "keySet" <>$ capt_exp $!--> Collection.iterator
      ; +PatternMatch.implements_map &:: "values" <>$ capt_exp $!--> Collection.iterator
      ; +PatternMatch.implements_map &:: "put" <>$ capt_var_exn $+ any_arg $+ any_arg
        $--> Collection.put
      ; +PatternMatch.implements_org_json "JSONArray"
        &:: "put" <>$ capt_var_exn $+...$--> Collection.put
      ; +PatternMatch.implements_map &:: "putAll" <>$ capt_var_exn $+ capt_exp
        $--> Collection.putAll
      ; +PatternMatch.implements_iterator &:: "hasNext" <>$ capt_exp $!--> Collection.hasNext
      ; +PatternMatch.implements_iterator &:: "next" <>$ capt_exp $!--> Collection.next
      ; +PatternMatch.implements_list &:: "subList" <>$ any_arg $+ capt_exp $+ capt_exp
        $--> Collection.subList
      ; +PatternMatch.implements_collection
        &:: "addAll" <>$ capt_var_exn $+ capt_exp $--> Collection.addAll
      ; +PatternMatch.implements_collection
        &:: "addAll" <>$ capt_var_exn $+ capt_exp $+ capt_exp $!--> Collection.addAll_at_index
      ; +PatternMatch.implements_collection &:: "size" <>$ capt_exp $!--> Collection.size
      ; +PatternMatch.implements_google "common.base.Preconditions"
        &:: "checkArgument" <>$ capt_exp $--> Preconditions.check_argument
      ; +PatternMatch.implements_pseudo_collection &:: "size" <>$ capt_exp $!--> Collection.size
      ; +PatternMatch.implements_org_json "JSONArray"
        &:: "length" <>$ capt_exp $!--> Collection.size
      ; +PatternMatch.implements_org_json "JSONArray"
        &:: "<init>" <>$ capt_var_exn
        $+ capt_exp_of_typ (+PatternMatch.implements_collection)
        $--> Collection.init_with_arg
      ; +PatternMatch.implements_lang "Integer"
        &:: "intValue" <>$ capt_exp $--> JavaInteger.intValue
      ; +PatternMatch.implements_lang "Integer" &:: "valueOf" <>$ capt_exp $--> JavaInteger.valueOf
      ; +PatternMatch.implements_io "InputStream"
        &:: "read" <>$ any_arg $+ any_arg $+ any_arg $+ capt_exp $--> InputStream.read
      ; +PatternMatch.implements_nio "channels.FileChannel"
        &:: "read" <>$ any_arg $+ capt_exp $+ any_arg $--> FileChannel.read
      ; +PatternMatch.implements_nio "ByteBuffer" &:: "get" <>$ capt_exp $--> ByteBuffer.get_int
      ; +PatternMatch.implements_nio "ByteBuffer"
        &:: "getShort" <>$ capt_exp $--> ByteBuffer.get_int
      ; +PatternMatch.implements_nio "ByteBuffer" &:: "getInt" <>$ capt_exp $--> ByteBuffer.get_int
      ; +PatternMatch.implements_nio "ByteBuffer" &:: "getLong" <>$ capt_exp $--> ByteBuffer.get_int
      ; -"java.lang.Object" &:: "clone" <>$ capt_exp $--> Object.clone
      ; +PatternMatch.implements_lang "Math"
        &:: "max" <>$ capt_exp $+ capt_exp
        $--> eval_binop ~f:(Itv.max_sem ~use_minmax_bound:true)
      ; +PatternMatch.implements_lang "Math"
        &:: "min" <>$ capt_exp $+ capt_exp
        $--> eval_binop ~f:(Itv.min_sem ~use_minmax_bound:true) ]
end
