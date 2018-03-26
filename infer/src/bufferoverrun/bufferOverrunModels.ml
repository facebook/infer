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

  type model_env =
    { pname: Typ.Procname.t
    ; node_hash: int
    ; location: Location.t
    ; tenv: Tenv.t
    ; ret: (Ident.t * Typ.t) option }

  let mk_model_env pname node_hash location ?ret tenv = {pname; node_hash; location; tenv; ret}

  type exec_fun = model_env -> Dom.Mem.astate -> Dom.Mem.astate

  type check_fun = model_env -> Dom.Mem.astate -> PO.ConditionSet.t -> PO.ConditionSet.t

  type model = {exec: exec_fun; check: check_fun}

  type declare_local_fun =
    decl_local:BoUtils.Exec.decl_local -> model_env -> Loc.t -> inst_num:int -> dimension:int
    -> Dom.Mem.astate -> Dom.Mem.astate * int

  type declare_symbolic_fun =
    decl_sym_val:BoUtils.Exec.decl_sym_val -> model_env -> depth:int -> Loc.t -> inst_num:int
    -> new_sym_num:Itv.Counter.t -> new_alloc_num:Itv.Counter.t -> Dom.Mem.astate -> Dom.Mem.astate

  type typ_model = {declare_local: declare_local_fun; declare_symbolic: declare_symbolic_fun}

  let no_check _model_env _mem cond_set = cond_set

  (* It returns a tuple of:
     - type of array element
     - stride of the type
     - array size
     - flexible array size *)
  let get_malloc_info : Exp.t -> Typ.t * Int.t option * Exp.t * Exp.t option = function
    | Exp.BinOp (Binop.Mult, Exp.Sizeof {typ; nbytes}, length)
    | Exp.BinOp (Binop.Mult, length, Exp.Sizeof {typ; nbytes}) ->
        (typ, nbytes, length, None)
    | Exp.Sizeof {typ; nbytes; dynamic_length} ->
        (typ, nbytes, Exp.one, dynamic_length)
    | x ->
        (Typ.mk (Typ.Tint Typ.IChar), Some 1, x, None)


  let check_alloc_size size_exp {pname; location} mem cond_set =
    let _, _, length0, _ = get_malloc_info size_exp in
    let v_length = Sem.eval length0 mem in
    match Dom.Val.get_itv v_length with
    | Bottom ->
        cond_set
    | NonBottom length ->
        let traces = Dom.Val.get_traces v_length in
        PO.ConditionSet.add_alloc_size pname location ~length traces cond_set


  let set_uninitialized location (typ: Typ.t) ploc mem =
    match typ.desc with
    | Tint _ | Tfloat _ ->
        Dom.Mem.weak_update_heap ploc Dom.Val.Itv.top mem
    | _ ->
        L.(debug BufferOverrun Verbose)
          "/!\\ Do not know how to uninitialize type %a at %a@\n" (Typ.pp Pp.text) typ Location.pp
          location ;
        mem


  let malloc size_exp =
    let exec {pname; ret; node_hash; location; tenv} mem =
      match ret with
      | Some (id, _) ->
          let size_exp = Prop.exp_normalize_noabs tenv Sil.sub_empty size_exp in
          let typ, stride, length0, dyn_length = get_malloc_info size_exp in
          let length = Sem.eval length0 mem in
          let traces = TraceSet.add_elem (Trace.ArrDecl location) (Dom.Val.get_traces length) in
          let v =
            Sem.eval_array_alloc pname ~node_hash typ ~stride ~offset:Itv.zero
              ~size:(Dom.Val.get_itv length) ~inst_num:0 ~dimension:1
            |> Dom.Val.set_traces traces
          in
          mem |> Dom.Mem.add_stack (Loc.of_id id) v
          |> set_uninitialized location typ (Dom.Val.get_array_locs v)
          |> BoUtils.Exec.init_array_fields tenv pname ~node_hash typ (Dom.Val.get_array_locs v)
               ?dyn_length
      | _ ->
          L.(debug BufferOverrun Verbose)
            "/!\\ Do not know where to model malloc at %a@\n" Location.pp location ;
          mem
    and check = check_alloc_size size_exp in
    {exec; check}


  let realloc src_exp size_exp =
    let exec {ret; location; tenv} mem =
      match ret with
      | Some (id, _) ->
          let size_exp = Prop.exp_normalize_noabs tenv Sil.sub_empty size_exp in
          let typ, _, length0, dyn_length = get_malloc_info size_exp in
          let length = Sem.eval length0 mem in
          let traces = TraceSet.add_elem (Trace.ArrDecl location) (Dom.Val.get_traces length) in
          let v =
            Sem.eval src_exp mem |> Dom.Val.set_array_size (Dom.Val.get_itv length)
            |> Dom.Val.set_traces traces
          in
          let mem = Dom.Mem.add_stack (Loc.of_id id) v mem in
          Option.value_map dyn_length ~default:mem ~f:(fun dyn_length ->
              let dyn_length = Dom.Val.get_itv (Sem.eval dyn_length mem) in
              BoUtils.Exec.set_dyn_length tenv typ (Dom.Val.get_array_locs v) dyn_length mem )
      | _ ->
          mem
    and check = check_alloc_size size_exp in
    {exec; check}


  let placement_new allocated_mem_exp =
    let exec {ret} mem =
      match ret with
      | Some (id, _) ->
          let v = Sem.eval allocated_mem_exp mem in
          Dom.Mem.add_stack (Loc.of_id id) v mem
      | None ->
          mem
    in
    {exec; check= no_check}


  let inferbo_min e1 e2 =
    let exec {ret} mem =
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
    let exec _model_env mem =
      let locs = Sem.eval_locs e1 mem |> Dom.Val.get_pow_loc in
      let size = Sem.eval e2 mem |> Dom.Val.get_itv in
      let arr = Dom.Mem.find_heap_set locs mem in
      let arr = Dom.Val.set_array_size size arr in
      Dom.Mem.strong_update_heap locs arr mem
    and check = check_alloc_size e2 in
    {exec; check}


  let model_by_value value ret mem =
    match ret with
    | Some (id, _) ->
        Dom.Mem.add_stack (Loc.of_id id) value mem
    | None ->
        L.(debug BufferOverrun Verbose)
          "/!\\ Do not know where to model value %a@\n" Dom.Val.pp value ;
        mem


  let by_value value =
    let exec {ret} mem = model_by_value value ret mem in
    {exec; check= no_check}


  let bottom =
    let exec _model_env _mem = Bottom in
    {exec; check= no_check}


  let infer_print e =
    let exec {location} mem =
      L.(debug BufferOverrun Medium)
        "@[<v>=== Infer Print === at %a@,%a@]%!" Location.pp location Dom.Val.pp (Sem.eval e mem) ;
      mem
    in
    {exec; check= no_check}


  let set_array_length array length_exp =
    let exec {pname; node_hash; location} mem =
      match array with
      | Exp.Lvar array_pvar, {Typ.desc= Typ.Tarray {elt; stride}} ->
          let length = Sem.eval length_exp mem |> Dom.Val.get_itv in
          let stride = Option.map ~f:IntLit.to_int stride in
          let v =
            Sem.eval_array_alloc pname ~node_hash elt ~stride ~offset:Itv.zero ~size:length
              ~inst_num:0 ~dimension:1
          in
          mem |> Dom.Mem.add_stack (Loc.of_pvar array_pvar) v
          |> set_uninitialized location elt (Dom.Val.get_array_locs v)
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
        let exec {location} mem =
          Split.std_vector ~adds_at_least_one:true vector_arg location mem
        in
        {exec; check= no_check}
    end
  end

  module Folly = struct
    module Split = struct
      let std_vector vector_arg ignore_empty_opt =
        let exec {location} mem =
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

  module StdArray = struct
    let typ typ length =
      let declare_local ~decl_local {pname; node_hash; location} loc ~inst_num ~dimension mem =
        (* should this be deferred to the constructor? *)
        let length = Some (IntLit.of_int64 length) in
        BoUtils.Exec.decl_local_array ~decl_local pname ~node_hash location loc typ ~length
          ~inst_num ~dimension mem
      in
      let declare_symbolic ~decl_sym_val {pname; tenv; node_hash; location} ~depth loc ~inst_num
          ~new_sym_num ~new_alloc_num mem =
        let offset = Itv.zero in
        let size = Itv.of_int64 length in
        BoUtils.Exec.decl_sym_arr ~decl_sym_val pname tenv ~node_hash location ~depth loc typ
          ~offset ~size ~inst_num ~new_sym_num ~new_alloc_num mem
      in
      {declare_local; declare_symbolic}


    let constructor _size =
      let exec _model_env mem = mem (* initialize? *) in
      {exec; check= no_check}


    let at _size (array_exp, _) (index_exp, _) =
      (* TODO? use size *)
      let exec {ret} mem =
        L.(debug BufferOverrun Verbose) "Using model std::array<_, %Ld>::at" _size ;
        match ret with
        | Some (id, _) ->
            BoUtils.Exec.load_val id (Sem.eval_lindex array_exp index_exp mem) mem
        | None ->
            mem
      and check {pname; location} mem cond_set =
        BoUtils.Check.lindex ~array_exp ~index_exp mem pname location cond_set
      in
      {exec; check}


    let no_model =
      let exec {pname; location} mem =
        L.(debug BufferOverrun Verbose)
          "No model for %a at %a" Typ.Procname.pp pname Location.pp location ;
        mem
      in
      {exec; check= no_check}


    let no_typ_model =
      let no_model kind pname location mem =
        L.(debug BufferOverrun Verbose)
          "No %s type model in %a at %a" kind Typ.Procname.pp pname Location.pp location ;
        mem
      in
      let declare_local ~decl_local:_ {pname; location} _loc ~inst_num ~dimension:_ mem =
        (no_model "local" pname location mem, inst_num)
      in
      let declare_symbolic ~decl_sym_val:_ {pname; location} ~depth:_ _loc ~inst_num:_
          ~new_sym_num:_ ~new_alloc_num:_ mem =
        no_model "symbolic" pname location mem
      in
      {declare_local; declare_symbolic}
  end

  module Call = struct
    let dispatch : model ProcnameDispatcher.Call.dispatcher =
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
        ; -"__new" <>$ capt_exp $+...$--> malloc
        ; -"__new_array" <>$ capt_exp $+...$--> malloc
        ; -"__placement_new" <>$ any_arg $+ capt_exp $!--> placement_new
        ; -"realloc" <>$ capt_exp $+ capt_exp $+...$--> realloc
        ; -"__set_array_length" <>$ capt_arg $+ capt_exp $!--> set_array_length
        ; -"strlen" <>--> by_value Dom.Val.Itv.nat
        ; -"boost" &:: "split" $ capt_arg_of_typ (-"std" &:: "vector") $+ any_arg $+ any_arg
          $+? any_arg $--> Boost.Split.std_vector
        ; -"folly" &:: "split" $ any_arg $+ any_arg $+ capt_arg_of_typ (-"std" &:: "vector")
          $+? capt_exp $--> Folly.Split.std_vector
        ; std_array0 >:: "array" &--> StdArray.constructor
        ; std_array2 >:: "at" $ capt_arg $+ capt_arg $!--> StdArray.at
        ; std_array2 >:: "operator[]" $ capt_arg $+ capt_arg $!--> StdArray.at
        ; -"std" &:: "array" &::.*--> StdArray.no_model ]
  end

  module TypName = struct
    let dispatch : typ_model ProcnameDispatcher.TypName.dispatcher =
      let open ProcnameDispatcher.TypName in
      make_dispatcher
        [ -"std" &:: "array" < capt_typ `T &+ capt_int >--> StdArray.typ
        ; -"std" &:: "array" &::.*--> StdArray.no_typ_model ]
  end
end
