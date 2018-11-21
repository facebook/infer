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
module BoUtils = BufferOverrunUtils
module Dom = BufferOverrunDomain
module PO = BufferOverrunProofObligations
module Sem = BufferOverrunSemantics
module Relation = BufferOverrunDomainRelation
module Trace = BufferOverrunTrace
module TraceSet = Trace.Set

type model_env =
  { pname: Typ.Procname.t
  ; node_hash: int
  ; location: Location.t
  ; tenv: Tenv.t
  ; integer_type_widths: Typ.IntegerWidths.t
  ; symbol_table: Itv.SymbolTable.t }

let mk_model_env pname node_hash location tenv integer_type_widths symbol_table =
  {pname; node_hash; location; tenv; integer_type_widths; symbol_table}


type exec_fun = model_env -> ret:Ident.t * Typ.t -> Dom.Mem.astate -> Dom.Mem.astate

type check_fun = model_env -> Dom.Mem.astate -> PO.ConditionSet.t -> PO.ConditionSet.t

type model = {exec: exec_fun; check: check_fun}

type declare_local_fun =
     decl_local:BoUtils.Exec.decl_local
  -> model_env
  -> Loc.t
  -> inst_num:int
  -> represents_multiple_values:bool
  -> dimension:int
  -> Dom.Mem.astate
  -> Dom.Mem.astate * int

type declare_symbolic_fun =
     decl_sym_val:BoUtils.Exec.decl_sym_val
  -> Itv.SymbolPath.partial
  -> model_env
  -> represents_multiple_values:bool
  -> depth:int
  -> Loc.t
  -> inst_num:int
  -> new_sym_num:Counter.t
  -> new_alloc_num:Counter.t
  -> Dom.Mem.astate
  -> Dom.Mem.astate

type typ_model = {declare_local: declare_local_fun; declare_symbolic: declare_symbolic_fun}

let no_check _model_env _mem cond_set = cond_set

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
  | Exp.Sizeof {typ; nbytes; dynamic_length= Some arr_length} when Language.curr_language_is Java
    ->
      (typ, nbytes, arr_length, Some arr_length)
  | Exp.Sizeof {typ; nbytes; dynamic_length} ->
      (typ, nbytes, Exp.one, dynamic_length)
  | x ->
      (Typ.mk (Typ.Tint Typ.IChar), Some 1, x, None)


let check_alloc_size size_exp {location; integer_type_widths} mem cond_set =
  let _, _, length0, _ = get_malloc_info size_exp in
  let v_length = Sem.eval integer_type_widths length0 mem in
  match Dom.Val.get_itv v_length with
  | Bottom ->
      cond_set
  | NonBottom length ->
      let alloc_trace_elem = BufferOverrunTrace.Alloc location in
      let traces =
        Dom.Val.get_traces v_length |> BufferOverrunTrace.Set.add_elem alloc_trace_elem
      in
      PO.ConditionSet.add_alloc_size location ~length traces cond_set


let set_uninitialized location (typ : Typ.t) ploc mem =
  match typ.desc with
  | Tint _ | Tfloat _ ->
      Dom.Mem.weak_update ploc Dom.Val.Itv.top mem
  | _ ->
      L.(debug BufferOverrun Verbose)
        "/!\\ Do not know how to uninitialize type %a at %a@\n" (Typ.pp Pp.text) typ Location.pp
        location ;
      mem


let malloc size_exp =
  let exec {pname; node_hash; location; tenv; integer_type_widths} ~ret:(id, _) mem =
    let size_exp = Prop.exp_normalize_noabs tenv Sil.sub_empty size_exp in
    let typ, stride, length0, dyn_length = get_malloc_info size_exp in
    let length = Sem.eval integer_type_widths length0 mem in
    let traces = TraceSet.add_elem (Trace.ArrDecl location) (Dom.Val.get_traces length) in
    let path = Option.value_map (Dom.Mem.find_simple_alias id mem) ~default:None ~f:Loc.get_path in
    let allocsite = Allocsite.make pname ~node_hash ~inst_num:0 ~dimension:1 ~path in
    let offset, size = (Itv.zero, Dom.Val.get_itv length) in
    let size_exp_opt =
      let size_exp = Option.value dyn_length ~default:length0 in
      Relation.SymExp.of_exp ~get_sym_f:(Sem.get_sym_f integer_type_widths mem) size_exp
    in
    let v = Dom.Val.of_array_alloc allocsite ~stride ~offset ~size |> Dom.Val.set_traces traces in
    mem
    |> Dom.Mem.add_stack (Loc.of_id id) v
    |> Dom.Mem.init_array_relation allocsite ~offset ~size ~size_exp_opt
    |> set_uninitialized location typ (Dom.Val.get_array_locs v)
    |> BoUtils.Exec.init_array_fields tenv integer_type_widths pname path ~node_hash typ
         (Dom.Val.get_array_locs v) ?dyn_length
  and check = check_alloc_size size_exp in
  {exec; check}


let calloc size_exp stride_exp =
  let byte_size_exp = Exp.BinOp (Binop.Mult (Some Typ.size_t), size_exp, stride_exp) in
  malloc byte_size_exp


let memcpy dest_exp src_exp size_exp =
  let exec _ ~ret:_ mem = mem
  and check {location; integer_type_widths} mem cond_set =
    BoUtils.Check.lindex_byte integer_type_widths ~array_exp:dest_exp ~byte_index_exp:size_exp mem
      location cond_set
    |> BoUtils.Check.lindex_byte integer_type_widths ~array_exp:src_exp ~byte_index_exp:size_exp
         mem location
  in
  {exec; check}


let memset arr_exp size_exp =
  let exec _ ~ret:_ mem = mem
  and check {location; integer_type_widths} mem cond_set =
    BoUtils.Check.lindex_byte integer_type_widths ~array_exp:arr_exp ~byte_index_exp:size_exp mem
      location cond_set
  in
  {exec; check}


let realloc src_exp size_exp =
  let exec {location; tenv; integer_type_widths} ~ret:(id, _) mem =
    let size_exp = Prop.exp_normalize_noabs tenv Sil.sub_empty size_exp in
    let typ, _, length0, dyn_length = get_malloc_info size_exp in
    let length = Sem.eval integer_type_widths length0 mem in
    let traces = TraceSet.add_elem (Trace.ArrDecl location) (Dom.Val.get_traces length) in
    let v =
      Sem.eval integer_type_widths src_exp mem
      |> Dom.Val.set_array_size (Dom.Val.get_itv length)
      |> Dom.Val.set_traces traces
    in
    let mem = Dom.Mem.add_stack (Loc.of_id id) v mem in
    Option.value_map dyn_length ~default:mem ~f:(fun dyn_length ->
        let dyn_length = Dom.Val.get_itv (Sem.eval integer_type_widths dyn_length mem) in
        BoUtils.Exec.set_dyn_length tenv typ (Dom.Val.get_array_locs v) dyn_length mem )
  and check = check_alloc_size size_exp in
  {exec; check}


let placement_new size_exp (src_exp1, t1) src_arg2_opt =
  match (t1.Typ.desc, src_arg2_opt) with
  | Tint _, None | Tint _, Some (_, {Typ.desc= Tint _}) ->
      malloc (Exp.BinOp (Binop.PlusA (Some Typ.size_t), size_exp, src_exp1))
  | Tstruct (CppClass (name, _)), None
    when [%compare.equal: string list] (QualifiedCppName.to_list name) ["std"; "nothrow_t"] ->
      malloc size_exp
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


let inferbo_min e1 e2 =
  let exec {integer_type_widths} ~ret:(id, _) mem =
    let i1 = Sem.eval integer_type_widths e1 mem |> Dom.Val.get_itv in
    let i2 = Sem.eval integer_type_widths e2 mem |> Dom.Val.get_itv in
    let v = Itv.min_sem i1 i2 |> Dom.Val.of_itv in
    mem |> Dom.Mem.add_stack (Loc.of_id id) v
  in
  {exec; check= no_check}


let inferbo_set_size e1 e2 =
  let exec {integer_type_widths} ~ret:_ mem =
    let locs = Sem.eval integer_type_widths e1 mem |> Dom.Val.get_pow_loc in
    let size = Sem.eval integer_type_widths e2 mem |> Dom.Val.get_itv in
    Dom.Mem.transform_mem ~f:(Dom.Val.set_array_size size) locs mem
  and check = check_alloc_size e2 in
  {exec; check}


let model_by_value value (id, _) mem = Dom.Mem.add_stack (Loc.of_id id) value mem

let by_value =
  let exec ~value _ ~ret mem = model_by_value value ret mem in
  fun value -> {exec= exec ~value; check= no_check}


let bottom =
  let exec _model_env ~ret:_ _mem = Bottom in
  {exec; check= no_check}


let infer_print e =
  let exec {location; integer_type_widths} ~ret:_ mem =
    L.(debug BufferOverrun Medium)
      "@[<v>=== Infer Print === at %a@,%a@]%!" Location.pp location Dom.Val.pp
      (Sem.eval integer_type_widths e mem) ;
    mem
  in
  {exec; check= no_check}


let get_array_length array_exp =
  let exec {integer_type_widths} ~ret mem =
    let arr = Sem.eval_arr integer_type_widths array_exp mem in
    let traces = Dom.Val.get_traces arr in
    let length = arr |> Dom.Val.get_array_blk |> ArrayBlk.sizeof in
    let result = Dom.Val.of_itv ~traces length in
    model_by_value result ret mem
  in
  {exec; check= no_check}


let set_array_length array length_exp =
  let exec {pname; node_hash; location; integer_type_widths} ~ret:_ mem =
    match array with
    | Exp.Lvar array_pvar, {Typ.desc= Typ.Tarray {elt; stride}} ->
        let length = Sem.eval integer_type_widths length_exp mem |> Dom.Val.get_itv in
        let stride = Option.map ~f:IntLit.to_int_exn stride in
        let path = Some (Symb.SymbolPath.of_pvar array_pvar) in
        let allocsite = Allocsite.make pname ~node_hash ~inst_num:0 ~dimension:1 ~path in
        let v = Dom.Val.of_array_alloc allocsite ~stride ~offset:Itv.zero ~size:length in
        mem
        |> Dom.Mem.add_stack (Loc.of_pvar array_pvar) v
        |> set_uninitialized location elt (Dom.Val.get_array_locs v)
    | _ ->
        L.(die InternalError) "Unexpected type of first argument for __set_array_length() "
  and check = check_alloc_size length_exp in
  {exec; check}


module Split = struct
  let std_vector integer_type_widths ~adds_at_least_one (vector_exp, vector_typ) location mem =
    let traces = BufferOverrunTrace.(Set.singleton (Call location)) in
    let increment_itv = if adds_at_least_one then Itv.pos else Itv.nat in
    let increment = Dom.Val.of_itv ~traces increment_itv in
    let vector_type_name = Option.value_exn (vector_typ |> Typ.strip_ptr |> Typ.name) in
    let size_field = Typ.Fieldname.Clang.from_class_name vector_type_name "infer_size" in
    let vector_size_locs =
      Sem.eval integer_type_widths vector_exp mem
      |> Dom.Val.get_all_locs
      |> PowLoc.append_field ~fn:size_field
    in
    Dom.Mem.transform_mem ~f:(Dom.Val.plus_a increment) vector_size_locs mem
end

module Boost = struct
  module Split = struct
    let std_vector vector_arg =
      let exec {location; integer_type_widths} ~ret:_ mem =
        Split.std_vector integer_type_widths ~adds_at_least_one:true vector_arg location mem
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
        Split.std_vector integer_type_widths ~adds_at_least_one vector_arg location mem
      in
      {exec; check= no_check}
  end
end

module StdArray = struct
  let typ typ length =
    let declare_local ~decl_local {pname; node_hash; location} loc ~inst_num
        ~represents_multiple_values ~dimension mem =
      (* should this be deferred to the constructor? *)
      let length = Some (IntLit.of_int64 length) in
      BoUtils.Exec.decl_local_array ~decl_local pname ~node_hash location loc typ ~length ~inst_num
        ~represents_multiple_values ~dimension mem
    in
    let declare_symbolic ~decl_sym_val path {pname; tenv; node_hash; location; symbol_table}
        ~represents_multiple_values ~depth loc ~inst_num ~new_sym_num ~new_alloc_num mem =
      let offset = Itv.zero in
      let size = Itv.of_int64 length in
      BoUtils.Exec.decl_sym_arr ~decl_sym_val Symb.SymbolPath.CSymArray_Array pname symbol_table
        path tenv ~node_hash location ~represents_multiple_values ~depth loc typ ~offset ~size
        ~inst_num ~new_sym_num ~new_alloc_num mem
    in
    {declare_local; declare_symbolic}


  let constructor _size =
    let exec _model_env ~ret:_ mem = mem (* initialize? *) in
    {exec; check= no_check}


  let at _size (array_exp, _) (index_exp, _) =
    (* TODO? use size *)
    let exec {integer_type_widths} ~ret:(id, _) mem =
      L.d_printfln_escaped "Using model std::array<_, %Ld>::at" _size ;
      BoUtils.Exec.load_val id (Sem.eval_lindex integer_type_widths array_exp index_exp mem) mem
    and check {location; integer_type_widths} mem cond_set =
      BoUtils.Check.lindex integer_type_widths ~array_exp ~index_exp mem location cond_set
    in
    {exec; check}


  let no_model =
    let exec {pname} ~ret:_ mem =
      L.d_printfln "No model for %a" Typ.Procname.pp pname ;
      mem
    in
    {exec; check= no_check}


  let no_typ_model =
    let no_model kind pname location mem =
      L.(debug BufferOverrun Verbose)
        "No %s type model in %a at %a" kind Typ.Procname.pp pname Location.pp location ;
      mem
    in
    let declare_local ~decl_local:_ {pname; location} _loc ~inst_num ~represents_multiple_values:_
        ~dimension:_ mem =
      (no_model "local" pname location mem, inst_num)
    in
    let declare_symbolic ~decl_sym_val:_ _path {pname; location} ~represents_multiple_values:_
        ~depth:_ _loc ~inst_num:_ ~new_sym_num:_ ~new_alloc_num:_ mem =
      no_model "symbolic" pname location mem
    in
    {declare_local; declare_symbolic}
end

(* Java's Collections are represented by their size. We don't care about the elements.
- when they are constructed, we set the size to 0
- each time we add an element, we increase the length of the array
- each time we delete an element, we decrease the length of the array *)
module Collection = struct
  let typ =
    let declare_local ~decl_local:_ {pname; node_hash; location} loc ~inst_num
        ~represents_multiple_values ~dimension mem =
      BoUtils.Exec.decl_local_collection pname ~node_hash location loc ~inst_num
        ~represents_multiple_values ~dimension mem
    in
    let declare_symbolic ~decl_sym_val:_ path {pname; location; symbol_table}
        ~represents_multiple_values ~depth:_ loc ~inst_num:_ ~new_sym_num ~new_alloc_num:_ mem =
      BoUtils.Exec.decl_sym_collection pname symbol_table path location ~represents_multiple_values
        loc ~new_sym_num mem
    in
    {declare_local; declare_symbolic}


  let new_list _ =
    let exec {pname; node_hash; location} ~ret:(id, _) mem =
      let loc = Loc.of_id id in
      let path =
        Option.value_map (Dom.Mem.find_simple_alias id mem) ~default:None ~f:Loc.get_path
      in
      let allocsite = Allocsite.make pname ~node_hash ~inst_num:0 ~dimension:1 ~path in
      let alloc_loc = Loc.of_allocsite allocsite in
      let init_size = Dom.Val.of_int 0 in
      let traces = TraceSet.add_elem (Trace.ArrDecl location) (Dom.Val.get_traces init_size) in
      let v = Dom.Val.of_pow_loc (PowLoc.singleton alloc_loc) |> Dom.Val.set_traces traces in
      mem |> Dom.Mem.add_stack loc v |> Dom.Mem.add_heap alloc_loc init_size
    in
    {exec; check= no_check}


  let change_size_by ~size_f alist_id _ ~ret:_ mem =
    let alist_v = Dom.Mem.find (Loc.of_id alist_id) mem in
    let locs = Dom.Val.get_pow_loc alist_v in
    Dom.Mem.transform_mem ~f:size_f locs mem


  let incr_size = Dom.Val.plus_a Dom.Val.Itv.one

  let decr_size size = Dom.Val.minus_a size Dom.Val.Itv.one

  let add alist_id = {exec= change_size_by ~size_f:incr_size alist_id; check= no_check}

  let get_size integer_type_widths alist mem =
    BoUtils.Exec.get_alist_size (Sem.eval integer_type_widths alist mem) mem


  let size array_exp =
    let exec {integer_type_widths} ~ret mem =
      let size = get_size integer_type_widths array_exp mem in
      model_by_value size ret mem
    in
    {exec; check= no_check}


  let iterator alist =
    let exec {integer_type_widths} ~ret mem =
      let itr = Sem.eval integer_type_widths alist mem in
      model_by_value itr ret mem
    in
    {exec; check= no_check}


  let hasNext iterator =
    let exec {integer_type_widths} ~ret mem =
      (* Set the size of the iterator to be [0, size-1], so that range
         will be size of the collection. *)
      let collection_size =
        get_size integer_type_widths iterator mem |> Dom.Val.get_iterator_itv
      in
      model_by_value collection_size ret mem
    in
    {exec; check= no_check}


  let addAll alist_id alist_to_add =
    let exec ({integer_type_widths} as model_env) ~ret mem =
      let to_add_length = get_size integer_type_widths alist_to_add mem in
      change_size_by ~size_f:(Dom.Val.plus_a to_add_length) alist_id model_env ~ret mem
    in
    {exec; check= no_check}


  let add_at_index (alist_id : Ident.t) index_exp =
    let check {location; integer_type_widths} mem cond_set =
      let array_exp = Exp.Var alist_id in
      BoUtils.Check.collection_access integer_type_widths ~array_exp ~index_exp
        ~is_collection_add:true mem location cond_set
    in
    {exec= change_size_by ~size_f:incr_size alist_id; check}


  let remove_at_index alist_id index_exp =
    let check {location; integer_type_widths} mem cond_set =
      let array_exp = Exp.Var alist_id in
      BoUtils.Check.collection_access integer_type_widths ~array_exp ~index_exp mem location
        cond_set
    in
    {exec= change_size_by ~size_f:decr_size alist_id; check}


  let addAll_at_index alist_id index_exp alist_to_add =
    let exec ({integer_type_widths} as model_env) ~ret mem =
      let to_add_length = get_size integer_type_widths alist_to_add mem in
      change_size_by ~size_f:(Dom.Val.plus_a to_add_length) alist_id model_env ~ret mem
    in
    let check {location; integer_type_widths} mem cond_set =
      let array_exp = Exp.Var alist_id in
      BoUtils.Check.collection_access integer_type_widths ~index_exp ~array_exp
        ~is_collection_add:true mem location cond_set
    in
    {exec; check}


  let get_or_set_at_index alist_id index_exp =
    let exec _model_env ~ret:_ mem = mem in
    let check {location; integer_type_widths} mem cond_set =
      let array_exp = Exp.Var alist_id in
      BoUtils.Check.collection_access integer_type_widths ~index_exp ~array_exp mem location
        cond_set
    in
    {exec; check}
end

module Call = struct
  let dispatch : (Tenv.t, model) ProcnameDispatcher.Call.dispatcher =
    let open ProcnameDispatcher.Call in
    let mk_std_array () = -"std" &:: "array" < any_typ &+ capt_int in
    let std_array0 = mk_std_array () in
    let std_array2 = mk_std_array () in
    make_dispatcher
      [ -"__inferbo_min" <>$ capt_exp $+ capt_exp $!--> inferbo_min
      ; -"__inferbo_set_size" <>$ capt_exp $+ capt_exp $!--> inferbo_set_size
      ; -"__exit" <>--> bottom
      ; -"exit" <>--> bottom
      ; -"fgetc" <>--> by_value Dom.Val.Itv.m1_255
      ; -"infer_print" <>$ capt_exp $!--> infer_print
      ; -"malloc" <>$ capt_exp $+...$--> malloc
      ; -"calloc" <>$ capt_exp $+ capt_exp $!--> calloc
      ; -"__new"
        <>$ capt_exp_of_typ (+PatternMatch.implements_collection)
        $+...$--> Collection.new_list
      ; -"__new" <>$ capt_exp $+...$--> malloc
      ; -"__new_array" <>$ capt_exp $+...$--> malloc
      ; -"__placement_new" <>$ capt_exp $+ capt_arg $+? capt_arg $!--> placement_new
      ; -"realloc" <>$ capt_exp $+ capt_exp $+...$--> realloc
      ; -"__get_array_length" <>$ capt_exp $!--> get_array_length
      ; -"__set_array_length" <>$ capt_arg $+ capt_exp $!--> set_array_length
      ; -"strlen" <>--> by_value Dom.Val.Itv.nat
      ; -"memcpy" <>$ capt_exp $+ capt_exp $+ capt_exp $+...$--> memcpy
      ; -"memmove" <>$ capt_exp $+ capt_exp $+ capt_exp $+...$--> memcpy
      ; -"memset" <>$ capt_exp $+ any_arg $+ capt_exp $!--> memset
      ; -"strncpy" <>$ capt_exp $+ capt_exp $+ capt_exp $+...$--> memcpy
      ; -"boost" &:: "split"
        $ capt_arg_of_typ (-"std" &:: "vector")
        $+ any_arg $+ any_arg $+? any_arg $--> Boost.Split.std_vector
      ; -"folly" &:: "split" $ any_arg $+ any_arg
        $+ capt_arg_of_typ (-"std" &:: "vector")
        $+? capt_exp $--> Folly.Split.std_vector
      ; std_array0 >:: "array" &--> StdArray.constructor
      ; std_array2 >:: "at" $ capt_arg $+ capt_arg $!--> StdArray.at
      ; std_array2 >:: "operator[]" $ capt_arg $+ capt_arg $!--> StdArray.at
      ; -"std" &:: "array" &::.*--> StdArray.no_model
      ; +PatternMatch.implements_collection
        &:: "get" <>$ capt_var_exn $+ capt_exp $--> Collection.get_or_set_at_index
      ; +PatternMatch.implements_collection
        &:: "set" <>$ capt_var_exn $+ capt_exp $+ any_arg $--> Collection.get_or_set_at_index
      ; +PatternMatch.implements_collection
        &:: "remove" <>$ capt_var_exn $+ capt_exp $--> Collection.remove_at_index
      ; +PatternMatch.implements_collection
        &:: "add" <>$ capt_var_exn $+ any_arg $--> Collection.add
      ; +PatternMatch.implements_collection
        &:: "add" <>$ capt_var_exn $+ capt_exp $+ any_arg $!--> Collection.add_at_index
      ; +PatternMatch.implements_collection &:: "iterator" <>$ capt_exp $!--> Collection.iterator
      ; +PatternMatch.implements_iterator &:: "hasNext" <>$ capt_exp $!--> Collection.hasNext
      ; +PatternMatch.implements_collection
        &:: "addAll" <>$ capt_var_exn $+ capt_exp $--> Collection.addAll
      ; +PatternMatch.implements_collection
        &:: "addAll" <>$ capt_var_exn $+ capt_exp $+ capt_exp $!--> Collection.addAll_at_index
      ; +PatternMatch.implements_collection &:: "size" <>$ capt_exp $!--> Collection.size ]
end

module TypName = struct
  let dispatch : (Tenv.t, typ_model) ProcnameDispatcher.TypName.dispatcher =
    let open ProcnameDispatcher.TypName in
    make_dispatcher
      [ -"std" &:: "array" < capt_typ `T &+ capt_int >--> StdArray.typ
      ; +PatternMatch.implements_collection &::.*--> Collection.typ
      ; +PatternMatch.implements_iterator &::.*--> Collection.typ
      ; -"std" &:: "array" &::.*--> StdArray.no_typ_model ]
end
