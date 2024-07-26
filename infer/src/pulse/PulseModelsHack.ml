(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import
open PulseModelsImport
module DSL = PulseModelsDSL

let awaitable_type_name = TextualSil.hack_awaitable_type_name

let hack_bool_type_name = TextualSil.hack_bool_type_name

let hack_int_type_name = TextualSil.hack_int_type_name

let hack_float_type_name = TextualSil.hack_float_type_name

let mixed_type_name = TextualSil.hack_mixed_type_name

let hack_string_type_name = TextualSil.hack_string_type_name

let string_val_field = Fieldname.make hack_string_type_name "val"

let read_string_value address astate = PulseArithmetic.as_constant_string astate address

let replace_backslash_with_colon s = String.tr s ~target:'\\' ~replacement:':'

let read_string_value_dsl aval : string option DSL.model_monad =
  let open PulseModelsDSL.Syntax in
  let* inner_val = eval_deref_access Read aval (FieldAccess string_val_field) in
  let operation astate = (read_string_value (fst inner_val) astate, astate) in
  let* opt_string = exec_operation operation in
  ret opt_string


let await_hack_value aval : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let val_field = Fieldname.make awaitable_type_name "val" in
  dynamic_dispatch aval
    ~cases:
      [ ( awaitable_type_name
        , fun () ->
            let* () = fst aval |> AddressAttributes.hack_async_await |> DSL.Syntax.exec_command in
            eval_deref_access Read aval (FieldAccess val_field) ) ]
    ~default:(fun () -> ret aval)


let hack_await arg : model =
  let open DSL.Syntax in
  start_model
  @@ let* rv = await_hack_value arg in
     assign_ret rv


let hack_await_static _ arg : model =
  let open DSL.Syntax in
  start_model
  @@ let* rv = await_hack_value arg in
     assign_ret rv


let make_new_awaitable av =
  let open DSL.Syntax in
  let* av = constructor awaitable_type_name [("val", av)] in
  let* () = allocation Attribute.HackAsync av in
  ret av


let deep_clean_hack_value aval : unit DSL.model_monad =
  let open DSL.Syntax in
  let* reachable_addresses =
    exec_pure_operation (fun astate ->
        AbductiveDomain.reachable_addresses_from (Seq.return (fst aval)) astate `Post )
  in
  absvalue_set_iter reachable_addresses ~f:(fun absval ->
      let* _v = await_hack_value (absval, ValueHistory.epoch) in
      let* () =
        AddressAttributes.set_hack_builder absval Attribute.Builder.Discardable |> exec_command
      in
      ret () )


(* vecs, similar treatment of Java collections, though these are value types
   Should be shared with dict (and keyset) but will generalise later.
   We have an integer size field (rather than just an empty flag) and a
   last_read field, which is 1 or 2 if we last produced the fst or snd field
   as the result of an index operation. This is used to alternate returned values
   so as to remove paths in which we return the same value repeatedly, which leads
   to false awaitable positives because the *other* value is never awaited.
   TODO: a more principled approach to collections of resources.
*)
module Vec = struct
  let type_name = TextualSil.hack_vec_type_name

  let mk_vec_field name = Fieldname.make type_name name

  let fst_field_name = "__infer_model_backing_vec_fst"

  let snd_field_name = "__infer_model_backing_vec_snd"

  let size_field_name = "__infer_model_backing_vec_size"

  let last_read_field_name = "__infer_model_backing_last_read"

  let fst_field = mk_vec_field fst_field_name

  let snd_field = mk_vec_field snd_field_name

  let size_field = mk_vec_field size_field_name

  let last_read_field = mk_vec_field last_read_field_name

  let new_vec_dsl ?(know_size = None) args : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    let actual_size = List.length args in
    let* size = match know_size with None -> eval_const_int actual_size | Some size -> ret size in
    let* last_read = mk_fresh ~model_desc:"new_vec.last_read" () in
    let* dummy = eval_const_int 9 in
    let* vec =
      constructor type_name
        [ (fst_field_name, dummy)
        ; (snd_field_name, dummy)
        ; (size_field_name, size)
        ; (last_read_field_name, last_read) ]
    in
    let* () =
      match args with
      | [] ->
          write_deref_field ~ref:vec snd_field ~obj:dummy
      | arg1 :: rest -> (
          let* () = write_deref_field ~ref:vec fst_field ~obj:arg1 in
          match rest with
          | [] ->
              ret ()
          | arg2 :: rest -> (
              let* () = write_deref_field ~ref:vec snd_field ~obj:arg2 in
              match rest with
              | [] ->
                  ret ()
              (* Do "fake" await on the values we drop on the floor. TODO: mark reachable too? *)
              | rest ->
                  list_iter rest ~f:deep_clean_hack_value ) )
    in
    ret vec


  let new_vec args : model =
    let open DSL.Syntax in
    start_model
    @@ let* vec = new_vec_dsl args in
       assign_ret vec


  (* TODO: this isn't *quite* right with respect to dummy values, but I think it's OK *)
  let vec_from_async _dummy aval : model =
    let open DSL.Syntax in
    start_model
    @@ let* fst_val = eval_deref_access Read aval (FieldAccess fst_field) in
       let* snd_val = eval_deref_access Read aval (FieldAccess snd_field) in
       let* awaited_fst_val = await_hack_value fst_val in
       let* awaited_snd_val = await_hack_value snd_val in
       let* fresh_vec = new_vec_dsl [awaited_fst_val; awaited_snd_val] in
       assign_ret fresh_vec


  let map _this arg closure =
    let open DSL.Syntax in
    start_model
    @@
    let* size_val = eval_deref_access Read arg (FieldAccess size_field) in
    let size_eq_0_case : DSL.aval DSL.model_monad =
      let* () = prune_eq_zero size_val in
      new_vec_dsl []
    in
    let size_eq_1_case : DSL.aval DSL.model_monad =
      let* () = prune_eq_int size_val IntLit.one in
      let* fst_val = eval_deref_access Read arg (FieldAccess fst_field) in
      let* mapped_fst_val = apply_hack_closure closure [fst_val] in
      new_vec_dsl [mapped_fst_val]
    in
    let size_gt_1_case : DSL.aval DSL.model_monad =
      let* () = prune_gt_int size_val IntLit.one in
      let* fst_val = eval_deref_access Read arg (FieldAccess fst_field) in
      let* snd_val = eval_deref_access Read arg (FieldAccess snd_field) in
      let* mapped_fst_val = apply_hack_closure closure [fst_val] in
      let* mapped_snd_val = apply_hack_closure closure [snd_val] in
      new_vec_dsl ~know_size:(Some size_val) [mapped_fst_val; mapped_snd_val]
    in
    let* ret = disjuncts [size_eq_0_case; size_eq_1_case; size_gt_1_case] in
    assign_ret ret


  let get_vec_dsl argv _index : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    let* ret_val = mk_fresh ~model_desc:"vec index" () in
    let* new_last_read_val = mk_fresh ~model_desc:"vec index" () in
    let* _size_val = eval_deref_access Read argv (FieldAccess size_field) in
    let* fst_val = eval_deref_access Read argv (FieldAccess fst_field) in
    let* snd_val = eval_deref_access Read argv (FieldAccess snd_field) in
    let* last_read_val = eval_deref_access Read argv (FieldAccess last_read_field) in
    let* () = write_deref_field ~ref:argv last_read_field ~obj:new_last_read_val in
    (* TODO: assert index < size_val ? *)
    let* () =
      (* Don't return dummy value *)
      prune_ne_int ret_val (IntLit.of_int 9)
    in
    (* TODO: work out how to incorporate type-based, or at least nullability, assertions on ret_val *)

    (* Temporarily removing the "or something else" case1 (which follows the Java collection models)
       because I'm unconvinced it's a net benefit, and it leads to more disjuncts.
       Will experiment later. *)
    let case2 : DSL.aval DSL.model_monad =
      (* case 2: given element is equal to fst_field *)
      let* () = prune_eq_int last_read_val IntLit.two in
      let* () = and_eq ret_val fst_val in
      let* () = and_eq_int new_last_read_val IntLit.one in
      ret ret_val
    in
    let case3 : DSL.aval DSL.model_monad =
      (* case 3: given element is equal to snd_field *)
      let* () = prune_eq_int last_read_val IntLit.one in
      let* () = and_eq ret_val snd_val in
      let* () = and_eq_int new_last_read_val IntLit.two in
      ret ret_val
    in
    disjuncts [(* case1; *) case2; case3]


  let hack_array_get_one_dim vec key : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    let* key_type = get_dynamic_type ~ask_specialization:false key in
    let* () =
      option_iter key_type ~f:(fun {Formula.typ} ->
          match typ with
          | {desc= Tstruct type_name} when not (Typ.Name.equal type_name hack_int_type_name) ->
              let* {location} = get_data in
              report (Diagnostic.DynamicTypeMismatch {location})
          | _ ->
              ret () )
    in
    let field = Fieldname.make hack_int_type_name "val" in
    let* index = eval_deref_access Read key (FieldAccess field) in
    get_vec_dsl vec index


  let hack_array_idx vec key default : unit DSL.model_monad =
    let open DSL.Syntax in
    let field = Fieldname.make hack_int_type_name "val" in
    let* index = eval_deref_access Read key (FieldAccess field) in
    let value = get_vec_dsl vec index in
    let* ret_values = disjuncts [value; ret default] in
    assign_ret ret_values


  (*
  See also $builtins.hack_array_cow_append in lib/hack/models.sil
  Model of set is very like that of append, since it ignores the index
  *)
  let hack_array_cow_set_dsl vec args : unit DSL.model_monad =
    let open DSL.Syntax in
    match args with
    | [_key; value] ->
        let* v_fst = eval_deref_access Read vec (FieldAccess fst_field) in
        let* v_snd = eval_deref_access Read vec (FieldAccess snd_field) in
        let* () = deep_clean_hack_value v_fst in
        let* new_vec = new_vec_dsl [v_snd; value] in
        let* size = eval_deref_access Read vec (FieldAccess size_field) in
        let* () = write_deref_field ~ref:new_vec size_field ~obj:size in
        (* overwrite default size of 2 *)
        assign_ret new_vec
    | _ ->
        L.d_printfln "vec hack array cow set argument error" ;
        L.internal_error "Vec.hack_array_cow_set expects 1 key and 1 value arguments" ;
        ret ()
end

let bool_val_field = Fieldname.make hack_bool_type_name "val"

let make_hack_bool bool : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let* bool = eval_const_int (if bool then 1 else 0) in
  let* boxed_bool = constructor hack_bool_type_name [("val", bool)] in
  ret boxed_bool


let aval_to_hack_int n_val : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let* ret_val = constructor hack_int_type_name [("val", n_val)] in
  ret ret_val


let aval_to_hack_bool b_val : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let* ret_val = constructor hack_bool_type_name [("val", b_val)] in
  ret ret_val


let int_to_hack_int n : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let* n_val = eval_const_int n in
  aval_to_hack_int n_val


let hack_string_dsl str_val : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let* ret_val = constructor hack_string_type_name [("val", str_val)] in
  ret ret_val


let hack_string str_val : model =
  let open DSL.Syntax in
  start_model
  @@ let* str_val = hack_string_dsl str_val in
     assign_ret str_val


let make_hack_string (s : string) : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let* str_val = eval_const_string s in
  hack_string_dsl str_val


module VecIter = struct
  let type_name = TextualSil.hack_vec_iter_type_name

  let mk_vec_iter_field name = Fieldname.make type_name name

  let vec_field_name = "__infer_model_backing_veciterator_vec"

  let index_field_name = "__infer_model_backing_veciterator_index"

  let vec_field = mk_vec_iter_field vec_field_name

  let index_field = mk_vec_iter_field index_field_name

  let iter_init_vec iteraddr keyaddr eltaddr argv : unit DSL.model_monad =
    let open DSL.Syntax in
    let* size_val = eval_deref_access Read argv (FieldAccess Vec.size_field) in
    let emptycase : DSL.aval DSL.model_monad =
      let* () = prune_eq_zero size_val in
      let* ret_val = make_hack_bool false in
      ret ret_val
    in
    let nonemptycase : DSL.aval DSL.model_monad =
      let* () = prune_positive size_val in
      let* zero = eval_const_int 0 in
      let* iter = constructor type_name [(vec_field_name, argv); (index_field_name, zero)] in
      let* () = write_deref ~ref:iteraddr ~obj:iter in
      let* hack_zero = int_to_hack_int 0 in
      let* elt = Vec.get_vec_dsl argv hack_zero in
      let* () = write_deref ~ref:eltaddr ~obj:elt in
      let* ret_val = make_hack_bool true in
      let haskey : DSL.aval DSL.model_monad =
        let* () = prune_ne_zero keyaddr in
        let* () = write_deref ~ref:keyaddr ~obj:hack_zero in
        ret ret_val
      in
      let nokey : DSL.aval DSL.model_monad =
        let* () = prune_eq_zero keyaddr in
        ret ret_val
      in
      disjuncts [haskey; nokey]
    in
    let* ret_val = disjuncts [emptycase; nonemptycase] in
    assign_ret ret_val


  let iter_next_vec iter keyaddr eltaddr : unit DSL.model_monad =
    let open DSL.Syntax in
    let* thevec = eval_deref_access Read iter (FieldAccess vec_field) in
    let* size = eval_deref_access Read thevec (FieldAccess Vec.size_field) in
    let* index = eval_deref_access Read iter (FieldAccess index_field) in
    let* succindex = eval_binop_int (Binop.PlusA None) index IntLit.one in
    (* true loop exit condition *)
    let finished1 : unit DSL.model_monad =
      let* () = prune_ge succindex size in
      let* ret_val = make_hack_bool true in
      assign_ret ret_val
    in
    (* overapproximate loop exit condition *)
    let finished2 : unit DSL.model_monad =
      let* () = prune_ge_int succindex IntLit.two in
      let* ret_val = make_hack_bool true in
      assign_ret ret_val
    in
    let not_finished : unit DSL.model_monad =
      let* () = prune_lt succindex size in
      let* () = prune_lt_int succindex IntLit.two in
      let* () = write_deref_field ~ref:iter index_field ~obj:succindex in
      let* hack_succindex = aval_to_hack_int succindex in
      let* elt = Vec.get_vec_dsl thevec hack_succindex in
      let* () = write_deref ~ref:eltaddr ~obj:elt in
      let* ret_val = make_hack_bool false in
      let haskey : unit DSL.model_monad =
        let* () = prune_positive keyaddr in
        write_deref ~ref:keyaddr ~obj:hack_succindex
      in
      let nokey : unit DSL.model_monad = prune_eq_zero keyaddr in
      let* () = disjuncts [haskey; nokey] in
      assign_ret ret_val
    in
    disjuncts [finished1; finished2; not_finished]
end

let get_static_companion_var type_name =
  let static_type_name = Typ.Name.Hack.static_companion type_name in
  Pvar.mk_global (Mangled.from_string (Typ.Name.name static_type_name))


let get_static_companion ~model_desc path location type_name astate =
  let pvar = get_static_companion_var type_name in
  let var = Var.of_pvar pvar in
  let hist = Hist.single_call path location model_desc in
  let astate, ((addr, _) as addr_hist) = AbductiveDomain.Stack.eval hist var astate in
  let static_type_name = Typ.Name.Hack.static_companion type_name in
  let typ = Typ.mk_struct static_type_name in
  let astate = PulseArithmetic.and_dynamic_type_is_unsafe addr typ location astate in
  (addr_hist, astate)


let get_static_companion_dsl ~model_desc type_name : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let* {path; location} = get_data in
  exec_operation (get_static_companion ~model_desc path location type_name)


(* NOTE: We model [lazy_class_initialize] as invoking the corresponding [sinit] procedure.  To be
   sound, we consider the cases where the initialization has been done before or not by separating
   disjuncts. *)
let lazy_class_initialize size_exp : model =
  let open DSL.Syntax in
  start_model
  @@
  let type_name =
    match size_exp with
    | Exp.Sizeof {typ= {desc= Typ.Tstruct type_name}} ->
        type_name
    | _ ->
        L.die InternalError
          "lazy_class_initialize: the Hack frontend should never generate such argument type"
  in
  let* class_object = get_static_companion_dsl ~model_desc:"lazy_class_initialize" type_name in
  let* () =
    match type_name with
    | HackClass class_name ->
        let pvar = get_static_companion_var type_name in
        let exp = Exp.Lvar pvar in
        let* static_companion = eval_read exp in
        let* is_sinit_called = is_hack_sinit_called static_companion in
        if is_sinit_called then ret ()
        else
          (* Note that we set the [HackSinitCalled] attribute even in the [not_call_sinit] case to
             avoid sinit is called later in the following instructions. *)
          let* () = set_hack_sinit_called static_companion in
          let call_sinit : unit DSL.model_monad =
            let ret_id = Ident.create_none () in
            let ret_typ = Typ.mk_ptr (Typ.mk_struct mixed_type_name) in
            let* {analysis_data= {tenv}} = get_data in
            let is_trait = Option.exists (Tenv.lookup tenv type_name) ~f:Struct.is_hack_trait in
            let pname = Procname.get_hack_static_init ~is_trait class_name in
            let typ = Typ.mk_struct type_name in
            let arg_payload =
              ValueOrigin.OnStack {var= Var.of_pvar pvar; addr_hist= static_companion}
            in
            dispatch_call (ret_id, ret_typ) pname [{exp; typ; arg_payload}]
          in
          let not_call_sinit : unit DSL.model_monad = ret () in
          disjuncts [call_sinit; not_call_sinit]
    | _ ->
        ret ()
  in
  assign_ret class_object


let get_static_class aval : model =
  let open DSL.Syntax in
  start_model
  @@ let* opt_dynamic_type_data = get_dynamic_type ~ask_specialization:true aval in
     match opt_dynamic_type_data with
     | Some {Formula.typ= {desc= Tstruct type_name}} ->
         let* class_object = get_static_companion_dsl ~model_desc:"get_static_class" type_name in
         let* () = register_class_object_for_value aval class_object in
         assign_ret class_object
     | _ ->
         let* unknown_class_object = mk_fresh ~model_desc:"get_static_class" () in
         let* () = register_class_object_for_value aval unknown_class_object in
         assign_ret unknown_class_object


let hhbc_class_get_c value : model =
  let open DSL.Syntax in
  start_model
  @@
  let default () =
    let* {location} = DSL.Syntax.get_data in
    ScubaLogging.log_message_with_location ~label:"hhbc_class_get_c argument"
      ~loc:(F.asprintf "%a" Location.pp_file_pos location)
      ~message:"hhbc_class_get_c received a non-constant-string argument." ;
    get_static_class value |> lift_to_monad
  in
  dynamic_dispatch value
    ~cases:
      [ ( hack_string_type_name
        , fun () ->
            let* opt_string = read_string_value_dsl value in
            match opt_string with
            | Some string ->
                (* namespace\\classname becomes namespace::classname *)
                let string = replace_backslash_with_colon string in
                let typ_name = Typ.HackClass (HackClassName.make string) in
                let* class_object =
                  get_static_companion_dsl ~model_desc:"hhbc_class_get_c" typ_name
                in
                assign_ret class_object
            | None ->
                default () ) ]
    ~default


module Dict = struct
  (* We model dict/shape keys as fields. This is a bit unorthodox in Pulse, but we need
     maximum precision on this ubiquitous Hack data structure. *)

  let type_name = TextualSil.hack_dict_type_name

  let field_of_string_value value : Fieldname.t option DSL.model_monad =
    let open DSL.Syntax in
    let* string = read_string_value_dsl value in
    ret (Option.map string ~f:(fun string -> TextualSil.wildcard_sil_fieldname Hack string))


  let read_dict_field_with_check dict field : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    let* () = add_dict_read_const_key dict field in
    eval_deref_access Read dict (FieldAccess field)


  type key_types = AllConstStrs | SomeOthers

  let get_bindings values : ((Fieldname.t * DSL.aval) list * key_types) DSL.model_monad =
    let open DSL.Syntax in
    let chunked = List.chunks_of ~length:2 values in
    let bindings_type = ref AllConstStrs in
    let* res =
      list_filter_map chunked ~f:(fun chunk ->
          let* res =
            match chunk with
            | [string; value] ->
                let* field = field_of_string_value string in
                ret (Option.map field ~f:(fun field -> (field, value)))
            | _ ->
                ret None
          in
          if Option.is_none res then bindings_type := SomeOthers ;
          ret res )
    in
    ret (res, !bindings_type)


  (* TODO: handle integers keys *)
  let new_dict args : model =
    let open DSL.Syntax in
    start_model
    @@ let* bindings, key_types = get_bindings args in
       let* dict = constructor type_name [] in
       let* () =
         list_iter bindings ~f:(fun (field, value) -> write_deref_field ~ref:dict field ~obj:value)
       in
       let* () =
         match key_types with
         | AllConstStrs ->
             add_dict_contain_const_keys dict
         | SomeOthers ->
             ret ()
       in
       assign_ret dict


  let dict_from_async _dummy dict : model =
    let open DSL.Syntax in
    start_model
    @@ let* fields = get_known_fields dict in
       let* new_dict = constructor type_name [] in
       let* () =
         list_iter fields ~f:(fun field_access ->
             match (field_access : Access.t) with
             | FieldAccess field_name ->
                 let* awaitable_value = read_dict_field_with_check dict field_name in
                 let* awaited_value = await_hack_value awaitable_value in
                 write_deref_field ~ref:new_dict field_name ~obj:awaited_value
             | _ ->
                 ret () )
       in
       assign_ret new_dict


  let hack_add_elem_c_dsl dict key value : unit DSL.model_monad =
    let open DSL.Syntax in
    let* field = field_of_string_value key in
    let* () =
      match field with
      | None ->
          remove_dict_contain_const_keys dict
      | Some field ->
          write_deref_field ~ref:dict ~obj:value field
    in
    assign_ret dict


  let contains_key dict key : model =
    let open DSL.Syntax in
    start_model
    @@
    let* field = field_of_string_value key in
    match field with
    | None ->
        ret ()
    | Some field ->
        let no_key : unit DSL.model_monad =
          let* ret_val = make_hack_bool false in
          assign_ret ret_val
        in
        let has_key : unit DSL.model_monad =
          let* _v =
            (* This makes the abstract value of `dict` to have `field`. *)
            eval_deref_access NoAccess dict (FieldAccess field)
          in
          let* ret_val = make_hack_bool true in
          assign_ret ret_val
        in
        disjuncts [no_key; has_key]


  (* TODO: handle the situation where we have mix of dict and vec *)
  let hack_array_cow_set_dsl dict args : unit DSL.model_monad =
    let open DSL.Syntax in
    (* args = [key1; key2; ...; key; value] *)
    let len_args = List.length args in
    match List.split_n args (len_args - 2) with
    | keys, [key; value] ->
        let* copy = deep_copy ~depth_max:1 dict in
        let* inner_dict =
          list_fold keys ~init:copy ~f:(fun dict key ->
              let* field = field_of_string_value key in
              match field with
              | Some field ->
                  let* inner_dict = read_dict_field_with_check dict field in
                  let* copied_inned_dict = deep_copy ~depth_max:1 inner_dict in
                  let* () = write_deref_field ~ref:dict field ~obj:copied_inned_dict in
                  ret copied_inned_dict
              | None ->
                  mk_fresh ~model_desc:"hack_array_cow_set_dsl" () )
        in
        let* field = field_of_string_value key in
        let* () =
          match field with
          | None ->
              let* () = remove_dict_contain_const_keys inner_dict in
              deep_clean_hack_value value
          | Some field ->
              write_deref_field field ~ref:inner_dict ~obj:value
        in
        assign_ret copy
    | _ when List.length args > 2 ->
        L.d_printfln "multidimensional copy on write not implemented yet" ;
        unreachable
    | _ ->
        L.die InternalError "should not happen"


  let hack_array_get_one_dim dict key : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    (* TODO: a key for a non-vec could be also a int *)
    let* field = field_of_string_value key in
    match field with
    | Some field ->
        read_dict_field_with_check dict field
    | None ->
        mk_fresh ~model_desc:"hack_array_get_one_dim" ()


  let hack_array_idx dict key default : unit DSL.model_monad =
    let open DSL.Syntax in
    let* field = field_of_string_value key in
    let value =
      match field with
      | Some field ->
          eval_deref_access Read dict (FieldAccess field)
      | None ->
          mk_fresh ~model_desc:"hack_array_idx" ()
    in
    let* ret_values = disjuncts [value; ret default] in
    assign_ret ret_values
end

module DictIter = struct
  let type_name = TextualSil.hack_dict_iter_type_name

  let mk_dict_iter_field name = Fieldname.make type_name name

  let dict_field_name = "__infer_model_backing_dictiterator_dict"

  let index_field_name = "__infer_model_backing_dictiterator_index"

  let dict_field = mk_dict_iter_field dict_field_name

  let index_field = mk_dict_iter_field index_field_name

  let iter_init_dict iteraddr keyaddr eltaddr argd : unit DSL.model_monad =
    let open DSL.Syntax in
    let* fields = get_known_fields argd in
    let* size_val = eval_const_int (List.length fields) in
    let emptycase : DSL.aval DSL.model_monad =
      let* () = prune_eq_zero size_val in
      let* ret_val = make_hack_bool false in
      ret ret_val
    in
    let nonemptycase : DSL.aval DSL.model_monad =
      let* () = prune_positive size_val in
      let* zero = eval_const_int 0 in
      let* iter = constructor type_name [(dict_field_name, argd); (index_field_name, zero)] in
      let* () = write_deref ~ref:iteraddr ~obj:iter in
      let* field =
        match List.hd fields with
        | None ->
            L.internal_error "iter init empty list of fields" ;
            unreachable
        | Some f ->
            ret f
      in
      let* elt = eval_deref_access Read argd field in
      let* n =
        match (field : Access.t) with
        | FieldAccess fn ->
            ret (Fieldname.get_field_name fn)
        | _ ->
            L.internal_error "dictionary non FieldAccess" ;
            unreachable
      in
      let* hack_str = make_hack_string n in
      let* () = write_deref ~ref:eltaddr ~obj:elt in
      let* ret_val = make_hack_bool true in
      let haskey : DSL.aval DSL.model_monad =
        let* () = prune_positive keyaddr in
        let* () = write_deref ~ref:keyaddr ~obj:hack_str in
        ret ret_val
      in
      let nokey : DSL.aval DSL.model_monad =
        let* () = prune_eq_zero keyaddr in
        ret ret_val
      in
      disjuncts [haskey; nokey]
    in
    let* ret_val = disjuncts [emptycase; nonemptycase] in
    assign_ret ret_val


  let iter_next_dict iter keyaddr eltaddr : unit DSL.model_monad =
    let open DSL.Syntax in
    let* thedict = eval_deref_access Read iter (FieldAccess dict_field) in
    let* fields = get_known_fields thedict in
    let* size_val = eval_const_int (List.length fields) in
    let* index = eval_deref_access Read iter (FieldAccess index_field) in
    let* succindex = eval_binop_int (Binop.PlusA None) index IntLit.one in
    (* In contrast to vecs, we don't have an overapproximate exit condition here *)
    let finished : unit DSL.model_monad =
      let* () = prune_ge succindex size_val in
      let* ret_val = make_hack_bool true in
      assign_ret ret_val
    in
    let not_finished : unit DSL.model_monad =
      let* () = prune_lt succindex size_val in
      let* () = write_deref_field ~ref:iter index_field ~obj:succindex in
      let* index_q_opt = as_constant_q succindex in
      let* key_value, elt_value =
        match index_q_opt with
        | None ->
            let* key_value = mk_fresh ~model_desc:"dict_iter_next" () in
            let* elt_value = mk_fresh ~model_desc:"dict_iter_next" () in
            ret (key_value, elt_value)
        | Some q -> (
            let* index_int =
              match QSafeCapped.to_int q with
              | None ->
                  L.internal_error "bad index in iter_next_dict" ;
                  unreachable
              | Some i ->
                  ret i
            in
            let* index_acc =
              match List.nth fields index_int with
              | None ->
                  L.internal_error "iter next out of bounds" ;
                  unreachable
              | Some ia ->
                  ret ia
            in
            match (index_acc : Access.t) with
            | FieldAccess fn ->
                let* key_value = make_hack_string (Fieldname.to_string fn) in
                let* elt_value = eval_deref_access Read thedict index_acc in
                ret (key_value, elt_value)
            | _ ->
                L.internal_error "iter next dict non field access" ;
                unreachable )
      in
      let* () = write_deref ~ref:eltaddr ~obj:elt_value in
      let* ret_val = make_hack_bool false in
      let haskey : unit DSL.model_monad =
        let* () = prune_positive keyaddr in
        write_deref ~ref:keyaddr ~obj:key_value
      in
      let nokey : unit DSL.model_monad = prune_eq_zero keyaddr in
      let* () = disjuncts [haskey; nokey] in
      assign_ret ret_val
    in
    disjuncts [finished; not_finished]
end

let hack_add_elem_c this key value : model =
  let open DSL.Syntax in
  start_model
  @@
  let default () =
    let* fresh = mk_fresh ~model_desc:"hack_add_elem_c" () in
    assign_ret fresh
  in
  dynamic_dispatch this
    ~cases:[(TextualSil.hack_dict_type_name, fun () -> Dict.hack_add_elem_c_dsl this key value)]
    ~default


let hack_array_cow_set this args : model =
  let open DSL.Syntax in
  start_model
  @@
  let default () =
    let* () = option_iter (List.last args) ~f:deep_clean_hack_value in
    let* fresh = mk_fresh ~model_desc:"hack_array_cow_set" () in
    assign_ret fresh
  in
  dynamic_dispatch this
    ~cases:
      [ (Dict.type_name, fun () -> Dict.hack_array_cow_set_dsl this args)
      ; (Vec.type_name, fun () -> Vec.hack_array_cow_set_dsl this args) ]
    ~default


let hack_array_get this args : model =
  let open DSL.Syntax in
  start_model
  @@
  let default () =
    L.d_warning "default case of hack_array_get" ;
    mk_fresh ~model_desc:"hack_array_get" ()
  in
  let hack_array_get_one_dim this key : DSL.aval DSL.model_monad =
    dynamic_dispatch this
      ~cases:
        [ (Dict.type_name, fun () -> Dict.hack_array_get_one_dim this key)
        ; (Vec.type_name, fun () -> Vec.hack_array_get_one_dim this key) ]
      ~default
  in
  let* value = list_fold args ~init:this ~f:hack_array_get_one_dim in
  assign_ret value


let hack_array_idx this key default_val : model =
  let open DSL.Syntax in
  start_model
  @@
  let default () =
    let* fresh = mk_fresh ~model_desc:"hack_array_idx" () in
    assign_ret fresh
  in
  dynamic_dispatch this
    ~cases:
      [ (Dict.type_name, fun () -> Dict.hack_array_idx this key default_val)
      ; (Vec.type_name, fun () -> Vec.hack_array_idx this key default_val) ]
    ~default


let eval_resolved_field ~model_desc typ_name fld_str =
  let open DSL.Syntax in
  let* fld_opt = tenv_resolve_fieldname typ_name fld_str in
  let name, fld =
    match fld_opt with
    | None ->
        L.d_printfln_escaped "Could not resolve the field %a.%s" Typ.Name.pp typ_name fld_str ;
        (typ_name, Fieldname.make typ_name fld_str)
    | Some fld ->
        (Fieldname.get_class_name fld, fld)
  in
  let* class_object = get_static_companion_dsl ~model_desc name in
  eval_deref_access Read class_object (FieldAccess fld)


let hack_field_get this field : model =
  let open DSL.Syntax in
  start_model
  @@ let* opt_string_field_name = get_const_string field in
     match opt_string_field_name with
     | Some string_field_name -> (
         let* opt_dynamic_type_data = get_dynamic_type ~ask_specialization:true this in
         match opt_dynamic_type_data with
         | Some {Formula.typ= {desc= Tstruct type_name}} ->
             let* aval =
               eval_resolved_field ~model_desc:"hack_field_get" type_name string_field_name
             in
             let* () =
               let field = Fieldname.make type_name string_field_name in
               let* struct_info = tenv_resolve_field_info type_name field in
               match struct_info with
               | Some {Struct.typ= field_typ} when Typ.is_pointer field_typ ->
                   option_iter
                     (Typ.name (Typ.strip_ptr field_typ))
                     ~f:(fun field_type_name -> add_static_type field_type_name aval)
               | _ ->
                   ret ()
             in
             assign_ret aval
         | _ ->
             let field = TextualSil.wildcard_sil_fieldname Hack string_field_name in
             let* aval = eval_deref_access Read this (FieldAccess field) in
             assign_ret aval )
     | None ->
         L.die InternalError "hack_field_get expect a string constant as 2nd argument"


let make_hack_random_bool : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let* any = mk_fresh ~model_desc:"make_hack_random_bool" () in
  let* boxed_bool = constructor hack_bool_type_name [("val", any)] in
  ret boxed_bool


let make_hack_unconstrained_int : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let* any = mk_fresh ~model_desc:"make_hack_unconstrained_int" () in
  let* boxed_int = constructor hack_int_type_name [("val", any)] in
  ret boxed_int


let hack_unconstrained_int : model =
  let open DSL.Syntax in
  start_model
  @@ let* rv = make_hack_unconstrained_int in
     assign_ret rv


let hhbc_not_dsl arg : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  (* this operator is always run on a HackBool argument (nonnull type) *)
  let* () = prune_ne_zero arg in
  let* int = eval_deref_access Read arg (FieldAccess bool_val_field) in
  let arg_is_true =
    let* () = prune_ne_zero int in
    make_hack_bool false
  in
  let arg_is_false =
    let* () = prune_eq_zero int in
    make_hack_bool true
  in
  disjuncts [arg_is_true; arg_is_false]


let hhbc_not arg : model =
  let open DSL.Syntax in
  start_model
  @@ let* res = hhbc_not_dsl arg in
     assign_ret res


let int_val_field = Fieldname.make hack_int_type_name "val"

let float_val_field = Fieldname.make hack_float_type_name "val"

let hhbc_cmp_same x y : model =
  L.d_printfln "hhbc_cmp_same(%a, %a)" AbstractValue.pp (fst x) AbstractValue.pp (fst y) ;
  let open DSL.Syntax in
  start_model
  @@
  let value_equality_test val1 val2 =
    let true_case =
      let* () = prune_eq val1 val2 in
      make_hack_bool true
    in
    let false_case =
      let* () = prune_ne val1 val2 in
      make_hack_bool false
    in
    disjuncts [true_case; false_case]
  in
  let* res =
    disjuncts
      [ (let* () = prune_eq_zero x in
         let* () = prune_eq_zero y in
         make_hack_bool true )
      ; (let* () = prune_eq_zero x in
         let* () = prune_ne_zero y in
         make_hack_bool false )
      ; (let* () = prune_ne_zero x in
         let* () = prune_eq_zero y in
         make_hack_bool false )
      ; (let* () = prune_ne_zero x in
         let* () = prune_ne_zero y in
         let* x_dynamic_type_data = get_dynamic_type ~ask_specialization:true x in
         let* y_dynamic_type_data = get_dynamic_type ~ask_specialization:true y in
         match (x_dynamic_type_data, y_dynamic_type_data) with
         | ( Some {Formula.typ= {desc= Tstruct x_typ_name}}
           , Some {Formula.typ= {desc= Tstruct y_typ_name}} )
           when Typ.Name.equal x_typ_name y_typ_name ->
             L.d_printfln "hhbc_cmp_same: known dynamic type" ;
             if Typ.Name.equal x_typ_name hack_int_type_name then (
               L.d_printfln "hhbc_cmp_same: both are ints" ;
               let* x_val = eval_deref_access Read x (FieldAccess int_val_field) in
               let* y_val = eval_deref_access Read y (FieldAccess int_val_field) in
               value_equality_test x_val y_val )
             else if Typ.Name.equal x_typ_name hack_float_type_name then (
               L.d_printfln "hhbc_cmp_same: both are floats" ;
               let* x_val = eval_deref_access Read x (FieldAccess float_val_field) in
               let* y_val = eval_deref_access Read y (FieldAccess float_val_field) in
               value_equality_test x_val y_val )
             else if Typ.Name.equal x_typ_name hack_bool_type_name then (
               L.d_printfln "hhbc_cmp_same: both are bools" ;
               let* x_val = eval_deref_access Read x (FieldAccess bool_val_field) in
               let* y_val = eval_deref_access Read y (FieldAccess bool_val_field) in
               value_equality_test x_val y_val )
             else if Typ.Name.equal x_typ_name hack_string_type_name then (
               L.d_printfln "hhbc_cmp_same: both are strings" ;
               let* x_val = eval_deref_access Read x (FieldAccess string_val_field) in
               let* y_val = eval_deref_access Read y (FieldAccess string_val_field) in
               disjuncts
                 [ (let* () = prune_eq x_val y_val in
                    make_hack_bool true )
                 ; (let* () = prune_ne x_val y_val in
                    make_hack_bool false ) ] )
             else (
               L.d_printfln "hhbc_cmp_same: not a known primitive type" ;
               disjuncts
                 [ (let* () = prune_eq x y in
                    (* CAUTION: Note that the pruning on a pointer may result in incorrect semantics
                       if the pointer is given as a parameter. In that case, the pruning may work as
                       a value assignment to the pointer. *)
                    make_hack_bool true )
                 ; (let* () = prune_ne x y in
                    (* TODO(dpichardie) cover the comparisons of vec, keyset, dict and
                       shape, taking into account the difference between == and ===. *)
                    (* TODO(dpichardie) cover the specificities of == that compare objects properties
                       (structural equality). *)
                    make_hack_random_bool ) ] )
         | Some {Formula.typ= x_typ}, Some {Formula.typ= y_typ} when not (Typ.equal x_typ y_typ) ->
             L.d_printfln "hhbc_cmp_same: known different dynamic types: false result" ;
             make_hack_bool false
         | _ ->
             L.d_printfln "hhbc_cmp_same: at least one unknown dynamic type: unknown result" ;
             make_hack_random_bool ) ]
  in
  assign_ret res


let hack_is_true b : model =
  let open DSL.Syntax in
  start_model
  @@
  let nullcase =
    let* () = prune_eq_zero b in
    let* zero = eval_const_int 0 in
    assign_ret zero
  in
  let nonnullcase =
    let* () = prune_ne_zero b in
    let* b_dynamic_type_data = get_dynamic_type ~ask_specialization:true b in
    match b_dynamic_type_data with
    | None ->
        let* ret = make_hack_random_bool in
        assign_ret ret
    | Some {Formula.typ= {Typ.desc= Tstruct b_typ_name}} ->
        if Typ.Name.equal b_typ_name hack_bool_type_name then
          let* b_val = eval_deref_access Read b (FieldAccess bool_val_field) in
          assign_ret b_val
        else (
          L.d_printfln "istrue got typename %a" Typ.Name.pp b_typ_name ;
          let* one = eval_const_int 1 in
          assign_ret one )
    | _ ->
        unreachable (* shouldn't happen *)
  in
  disjuncts [nullcase; nonnullcase]


let hhbc_cmp_nsame x y : model =
  let open DSL.Syntax in
  start_model
  @@ let* bool = lift_to_monad_and_get_result (hhbc_cmp_same x y) in
     let* neg_bool = hhbc_not_dsl bool in
     assign_ret neg_bool


let hhbc_cls_cns this field : model =
  let model_desc = "hhbc_cls_cns" in
  let open DSL.Syntax in
  start_model
  @@ let* dynamic_Type_data_opt = get_dynamic_type ~ask_specialization:true this in
     let* field_v =
       match dynamic_Type_data_opt with
       | Some {Formula.typ= {Typ.desc= Tstruct name}} ->
           let* opt_string_field_name = read_string_value_dsl field in
           let string_field_name =
             match opt_string_field_name with
             | Some str ->
                 str
             | None ->
                 (* we do not expect this situation to happen because hhbc_cls_cns takes as argument
                    a literal string see:
                    https://github.com/facebook/hhvm/blob/master/hphp/doc/bytecode.specification *)
                 L.internal_error "hhbc_cls_cns has been called on non-constant string" ;
                 "__dummy_constant_name__"
           in
           eval_resolved_field ~model_desc name string_field_name
       | _ ->
           mk_fresh ~model_desc ()
     in
     assign_ret field_v


let hack_get_class this : model =
  let open DSL.Syntax in
  start_model
  @@ let* typ_opt = get_dynamic_type ~ask_specialization:true this in
     let* field_v =
       match typ_opt with Some _ -> ret this | None -> mk_fresh ~model_desc:"hack_get_class" ()
     in
     assign_ret field_v


let hack_set_static_prop this prop obj : model =
  let open DSL.Syntax in
  start_model
  @@ let* opt_this = read_string_value_dsl this in
     let* opt_prop = read_string_value_dsl prop in
     match (opt_this, opt_prop) with
     | Some this, Some prop ->
         let this = replace_backslash_with_colon this in
         let name = Typ.HackClass (HackClassName.static_companion (HackClassName.make this)) in
         let* class_object = get_static_companion_dsl ~model_desc:"hack_set_static_prop" name in
         write_deref_field ~ref:class_object ~obj (Fieldname.make name prop)
     | _, _ ->
         ret ()


let hhbc_cmp_lt x y : model =
  let open DSL.Syntax in
  start_model
  @@
  let value_lt_test val1 val2 =
    let true_case =
      let* () = prune_lt val1 val2 in
      make_hack_bool true
    in
    let false_case =
      let* () = prune_ge val1 val2 in
      make_hack_bool false
    in
    disjuncts [true_case; false_case]
  in
  let* res =
    disjuncts
      [ (let* () = prune_eq x y in
         make_hack_bool false )
      ; (let* () = prune_ne x y in
         disjuncts
           [ ((* either of those is null but not both *)
              let* () = disjuncts [prune_eq_zero x; prune_eq_zero y] in
              make_hack_bool false (* should throw error/can't happen *) )
           ; (let* () = prune_ne_zero x in
              let* () = prune_ne_zero y in
              let* x_dynamic_type_data = get_dynamic_type ~ask_specialization:true x in
              let* y_dynamic_type_data = get_dynamic_type ~ask_specialization:true y in
              match (x_dynamic_type_data, y_dynamic_type_data) with
              | None, _ | _, None ->
                  L.d_printfln "random nones" ;
                  make_hack_random_bool
              | ( Some {Formula.typ= {Typ.desc= Tstruct x_typ_name}}
                , Some {Formula.typ= {Typ.desc= Tstruct y_typ_name}} )
                when Typ.Name.equal x_typ_name y_typ_name ->
                  if Typ.Name.equal x_typ_name hack_int_type_name then
                    let* x_val = eval_deref_access Read x (FieldAccess int_val_field) in
                    let* y_val = eval_deref_access Read y (FieldAccess int_val_field) in
                    value_lt_test x_val y_val
                  else if Typ.Name.equal x_typ_name hack_float_type_name then
                    let* x_val = eval_deref_access Read x (FieldAccess float_val_field) in
                    let* y_val = eval_deref_access Read y (FieldAccess float_val_field) in
                    value_lt_test x_val y_val
                  else if Typ.Name.equal x_typ_name hack_bool_type_name then
                    let* x_val = eval_deref_access Read x (FieldAccess bool_val_field) in
                    let* y_val = eval_deref_access Read y (FieldAccess bool_val_field) in
                    value_lt_test x_val y_val
                  else (
                    L.d_printfln "random somes" ;
                    make_hack_random_bool )
              | _, _ ->
                  make_hack_bool false ) ] ) ]
  in
  assign_ret res


let hhbc_cmp_gt x y : model = hhbc_cmp_lt y x

let hhbc_cmp_le x y : model =
  let open DSL.Syntax in
  start_model
  @@
  let value_le_test val1 val2 =
    let true_case =
      let* () = prune_le val1 val2 in
      make_hack_bool true
    in
    let false_case =
      let* () = prune_gt val1 val2 in
      make_hack_bool false
    in
    disjuncts [true_case; false_case]
  in
  let* res =
    disjuncts
      [ (let* () = prune_eq x y in
         make_hack_bool true )
      ; (let* () = prune_ne x y in
         disjuncts
           [ ((* either of those is null but not both *)
              let* () = disjuncts [prune_eq_zero x; prune_eq_zero y] in
              make_hack_bool false (* should throw error/can't happen *) )
           ; (let* () = prune_ne_zero x in
              let* () = prune_ne_zero y in
              let* x_dynamic_type_data = get_dynamic_type ~ask_specialization:true x in
              let* y_dynamic_type_data = get_dynamic_type ~ask_specialization:true y in
              match (x_dynamic_type_data, y_dynamic_type_data) with
              | None, _ | _, None ->
                  make_hack_random_bool
              | ( Some {Formula.typ= {Typ.desc= Tstruct x_typ_name}}
                , Some {Formula.typ= {Typ.desc= Tstruct y_typ_name}} )
                when Typ.Name.equal x_typ_name y_typ_name ->
                  if Typ.Name.equal x_typ_name hack_int_type_name then
                    let* x_val = eval_deref_access Read x (FieldAccess int_val_field) in
                    let* y_val = eval_deref_access Read y (FieldAccess int_val_field) in
                    value_le_test x_val y_val
                  else if Typ.Name.equal x_typ_name hack_float_type_name then
                    let* x_val = eval_deref_access Read x (FieldAccess float_val_field) in
                    let* y_val = eval_deref_access Read y (FieldAccess float_val_field) in
                    value_le_test x_val y_val
                  else if Typ.Name.equal x_typ_name hack_bool_type_name then
                    let* x_val = eval_deref_access Read x (FieldAccess bool_val_field) in
                    let* y_val = eval_deref_access Read y (FieldAccess bool_val_field) in
                    value_le_test x_val y_val
                  else make_hack_random_bool
              | _, _ ->
                  make_hack_bool false ) ] ) ]
  in
  assign_ret res


let hhbc_cmp_ge x y : model = hhbc_cmp_le y x

let hhbc_add x y : model =
  let open DSL.Syntax in
  start_model
  @@ let* x_dynamic_type_data = get_dynamic_type ~ask_specialization:true x in
     let* y_dynamic_type_data = get_dynamic_type ~ask_specialization:true y in
     match (x_dynamic_type_data, y_dynamic_type_data) with
     | ( Some {Formula.typ= {Typ.desc= Tstruct x_typ_name}}
       , Some {Formula.typ= {Typ.desc= Tstruct y_typ_name}} )
       when Typ.Name.equal x_typ_name y_typ_name && Typ.Name.equal x_typ_name hack_int_type_name ->
         let* x_val = eval_deref_access Read x (FieldAccess int_val_field) in
         let* y_val = eval_deref_access Read y (FieldAccess int_val_field) in
         let* sum = eval_binop (PlusA (Some IInt)) x_val y_val in
         let* res = aval_to_hack_int sum in
         assign_ret res
     | _, _ ->
         let* sum = mk_fresh ~model_desc:"hhbc_add" () in
         assign_ret sum (* unconstrained value *)


let hhbc_iter_base arg : model =
  let open DSL.Syntax in
  start_model @@ assign_ret arg


let hhbc_iter_init iteraddr keyaddr eltaddr arg : model =
  let open DSL.Syntax in
  start_model
  @@ dynamic_dispatch arg
       ~cases:
         [ (Dict.type_name, fun () -> DictIter.iter_init_dict iteraddr keyaddr eltaddr arg)
         ; (Vec.type_name, fun () -> VecIter.iter_init_vec iteraddr keyaddr eltaddr arg) ]
         (* TODO: The default is a hack to make the variadic.hack test work, should be fixed properly *)
       ~default:(fun () -> VecIter.iter_init_vec iteraddr keyaddr eltaddr arg)


let hhbc_iter_next iter keyaddr eltaddr _base : model =
  let open DSL.Syntax in
  start_model
  @@ dynamic_dispatch iter
       ~cases:
         [ (VecIter.type_name, fun () -> VecIter.iter_next_vec iter keyaddr eltaddr)
         ; (DictIter.type_name, fun () -> DictIter.iter_next_dict iter keyaddr eltaddr) ]
         (* TODO: The default is a hack to make the variadic.hack test work, should be fixed properly *)
       ~default:(fun () -> VecIter.iter_next_vec iter keyaddr eltaddr)


let hack_throw : model =
  let open DSL.Syntax in
  start_model @@ throw


module SplatedVec = struct
  let type_name = TextualSil.hack_splated_vec_type_name

  let field_name = "content"

  let field = Fieldname.make type_name field_name

  let make arg : model =
    let open DSL.Syntax in
    start_model
    @@ let* boxed = constructor type_name [(field_name, arg)] in
       assign_ret boxed


  let build_vec_for_variadic_callee args : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    match args with
    | [arg] -> (
        let* arg_dynamic_type_data = get_dynamic_type ~ask_specialization:false arg in
        match arg_dynamic_type_data with
        | Some {Formula.typ= {Typ.desc= Tstruct name}} when Typ.Name.equal name type_name ->
            eval_deref_access Read arg (FieldAccess field)
        | _ ->
            Vec.new_vec_dsl args )
    | _ ->
        Vec.new_vec_dsl args
end

let build_vec_for_variadic_callee data args astate =
  (SplatedVec.build_vec_for_variadic_callee args |> DSL.unsafe_to_astate_transformer) data astate


(* Map the kind tag values used in type structure dictionaries to their corresponding Pulse dynamic type names
   This only decodes primitive types
   See https://github.com/facebook/hhvm/blob/master/hphp/runtime/base/type-structure-kinds.h
*)
let type_struct_prim_tag_to_classname n =
  match n with
  | 0 ->
      None (* void doesn't have a tag 'cos it's represented as null *)
  | 1 ->
      Some hack_int_type_name
  | 2 ->
      Some hack_bool_type_name
  | 3 ->
      Some hack_float_type_name
  | 4 ->
      Some hack_string_type_name
  | 14 ->
      Some Dict.type_name (* really shape but the reps should be the same *)
  | 19 ->
      Some Dict.type_name (* actually dict this time *)
  | 20 ->
      Some Vec.type_name
  | _ ->
      None


let read_nullable_field_from_ts tdict =
  let open DSL.Syntax in
  let nullable_field = TextualSil.wildcard_sil_fieldname Textual.Lang.Hack "nullable" in
  let* nullable_boxed_bool = eval_deref_access Read tdict (FieldAccess nullable_field) in
  let* nullable_bool_val =
    eval_deref_access Read nullable_boxed_bool (FieldAccess bool_val_field)
  in
  as_constant_bool nullable_bool_val


let read_classname_field_from_ts tdict =
  let open DSL.Syntax in
  let classname_field = TextualSil.wildcard_sil_fieldname Textual.Lang.Hack "classname" in
  let* classname_boxed_string = eval_deref_access Read tdict (FieldAccess classname_field) in
  let* classname_string_val =
    eval_deref_access Read classname_boxed_string (FieldAccess string_val_field)
  in
  as_constant_string classname_string_val


(* returns a fresh value equated to the SIL result of the comparison *)
let check_against_type_struct v tdict : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let kind_field = TextualSil.wildcard_sil_fieldname Textual.Lang.Hack "kind" in
  let* kind_boxed_int = eval_deref_access Read tdict (FieldAccess kind_field) in
  let* kind_int_val = eval_deref_access Read kind_boxed_int (FieldAccess int_val_field) in
  let* kind_int_opt = as_constant_int kind_int_val in
  match kind_int_opt with
  | None ->
      L.d_printfln "didn't get known integer tag in check against type struct" ;
      let* md = get_data in
      L.internal_error "known tag failure tdict is %a at %a@\n" AbstractValue.pp (fst tdict)
        Location.pp_file_pos md.location ;
      (* duplicating behaviour of previous sil model instead of calling this an internal error *)
      let* one = eval_const_int 1 in
      ret one
  | Some k -> (
      let* inner_val = mk_fresh ~model_desc:"check against type struct" () in
      let* nullable_bool_opt = read_nullable_field_from_ts tdict in
      let nullable = Option.value nullable_bool_opt ~default:false in
      let* classname =
        match type_struct_prim_tag_to_classname k with
        | Some name ->
            ret (Some name)
        | None ->
            (* 101 is the magic number for "Unresolved type" in type structures.
               See https://github.com/facebook/hhvm/blob/master/hphp/runtime/base/type-structure-kinds.h *)
            if Int.(k = 101) then
              let* classname_string_opt = read_classname_field_from_ts tdict in
              ret
                (Option.map classname_string_opt ~f:(fun s ->
                     Typ.HackClass (HackClassName.make (replace_backslash_with_colon s)) ) )
            else ret None
      in
      match classname with
      | Some name ->
          L.d_printfln "type structure test against type name %a" Typ.Name.pp name ;
          let typ = Typ.mk (Typ.Tstruct name) in
          let* () = and_equal_instanceof inner_val v typ ~nullable in
          ret inner_val
      | None ->
          ret inner_val )


(* for now ignores resolve and enforce options *)
let hhbc_is_type_struct_c v tdict _resolveop _enforcekind : model =
  let open DSL.Syntax in
  start_model
  @@ let* inner_val = check_against_type_struct v tdict in
     let* wrapped_result = aval_to_hack_bool inner_val in
     assign_ret wrapped_result


let hhbc_verify_param_type_ts v tdict : model =
  let open DSL.Syntax in
  start_model
  @@ let* inner_val = check_against_type_struct v tdict in
     let* () = prune_ne_zero inner_val in
     let* zero = eval_const_int 0 in
     assign_ret zero


let hhbc_is_type_prim typname v : model =
  let open DSL.Syntax in
  start_model
  @@
  let typ = Typ.mk (Typ.Tstruct typname) in
  let model_desc = Printf.sprintf "hhbc_is_type_%s" (Typ.Name.to_string typname) in
  let* inner_val = mk_fresh ~model_desc () in
  let* rv = aval_to_hack_bool inner_val in
  let* () = and_equal_instanceof inner_val v typ ~nullable:false in
  assign_ret rv


let hhbc_is_type_str = hhbc_is_type_prim hack_string_type_name

let hhbc_is_type_bool = hhbc_is_type_prim hack_bool_type_name

let hhbc_is_type_int = hhbc_is_type_prim hack_int_type_name

let hhbc_is_type_float = hhbc_is_type_prim hack_float_type_name

let hhbc_is_type_dict = hhbc_is_type_prim Dict.type_name

let hhbc_is_type_vec = hhbc_is_type_prim Vec.type_name

let hhbc_verify_type_pred _dummy pred : model =
  let open DSL.Syntax in
  start_model
  @@ let* pred_val = eval_deref_access Read pred (FieldAccess bool_val_field) in
     let* () = prune_ne_zero pred_val in
     (* TODO: log when state is unsat at this point *)
     let* zero = eval_const_int 0 in
     assign_ret zero


let hhbc_cast_string arg : model =
  (* https://github.com/facebook/hhvm/blob/605ac5dde604ded7f25e9786032a904f28230845/hphp/doc/bytecode.specification#L1087
     Cast to string ((string),(binary)). Pushes (string)$1 onto the stack. If $1
     is an object that implements the __toString method, the string cast returns
     $1->__toString(). If $1 is an object that does not implement __toString
     method, the string cast throws a fatal error.
  *)
  let open DSL.Syntax in
  start_model
  @@ let* dynamic_type_data = get_dynamic_type ~ask_specialization:true arg in
     match dynamic_type_data with
     | Some {Formula.typ= {Typ.desc= Tstruct typ_name}}
       when Typ.Name.equal typ_name hack_string_type_name ->
         assign_ret arg
     | Some _ ->
         (* note: we do not model precisely the value returned by __toString() *)
         let* rv = make_hack_string "__infer_hack_generated_from_cast_string" in
         (* note: we do not model the case where __toString() is not implemented *)
         assign_ret rv
     | _ ->
         (* hopefully we will come back later with a dynamic type thanks to specialization *)
         let* rv = mk_fresh ~model_desc:"hhbc_is_type_struct_c" () in
         assign_ret rv


let hhbc_concat arg1 arg2 : model =
  let open DSL.Syntax in
  start_model
  @@
  let* arg1_val = eval_deref_access Read arg1 (FieldAccess string_val_field) in
  let* arg2_val = eval_deref_access Read arg2 (FieldAccess string_val_field) in
  let* res = eval_string_concat arg1_val arg2_val in
  let* res = hack_string_dsl res in
  assign_ret res


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [ -"$builtins" &:: "nondet" <>$$--> lift_model @@ Basic.nondet ~desc:"nondet"
  ; +BuiltinDecl.(match_builtin __lazy_class_initialize) <>$ capt_exp $--> lazy_class_initialize
  ; +BuiltinDecl.(match_builtin __get_lazy_class) <>$ capt_exp $--> lazy_class_initialize
  ; +BuiltinDecl.(match_builtin __hack_throw) <>--> hack_throw
  ; -"$builtins" &:: "hack_string" <>$ capt_arg_payload $--> hack_string
  ; -"$builtins" &:: "__sil_splat" <>$ capt_arg_payload $--> SplatedVec.make
  ; -"$builtins" &:: "hhbc_add_elem_c" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload
    $--> hack_add_elem_c
  ; -"$builtins" &:: "hhbc_await" <>$ capt_arg_payload $--> hack_await
  ; -"$builtins" &:: "hack_array_get" <>$ capt_arg_payload $+++$--> hack_array_get
  ; -"$builtins" &:: "hhbc_idx" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload
    $--> hack_array_idx
  ; -"$builtins" &:: "hack_array_cow_set" <>$ capt_arg_payload $+++$--> hack_array_cow_set
  ; -"$builtins" &:: "hack_new_dict" &::.*+++> Dict.new_dict
  ; -"$builtins" &:: "hhbc_new_dict" &::.*+++> Dict.new_dict
  ; -"$builtins" &:: "hhbc_new_vec" &::.*+++> Vec.new_vec
  ; -"$builtins" &:: "hhbc_not" <>$ capt_arg_payload $--> hhbc_not
  ; -"$builtins" &:: "hack_get_class" <>$ capt_arg_payload $--> hack_get_class
  ; -"$builtins" &:: "hack_field_get" <>$ capt_arg_payload $+ capt_arg_payload $--> hack_field_get
  ; -"$builtins" &:: "hhbc_cast_string" <>$ capt_arg_payload $--> hhbc_cast_string
  ; -"$builtins" &:: "hhbc_class_get_c" <>$ capt_arg_payload $--> hhbc_class_get_c
    (* we should be able to model that directly in Textual once specialization will be stronger *)
  ; -"$builtins" &:: "hhbc_cmp_same" <>$ capt_arg_payload $+ capt_arg_payload $--> hhbc_cmp_same
  ; -"$builtins" &:: "hhbc_cmp_nsame" <>$ capt_arg_payload $+ capt_arg_payload $--> hhbc_cmp_nsame
  ; -"$builtins" &:: "hhbc_cmp_eq" <>$ capt_arg_payload $+ capt_arg_payload $--> hhbc_cmp_same
  ; -"$builtins" &:: "hhbc_cmp_neq" <>$ capt_arg_payload $+ capt_arg_payload $--> hhbc_cmp_nsame
  ; -"$builtins" &:: "hhbc_cmp_lt" <>$ capt_arg_payload $+ capt_arg_payload $--> hhbc_cmp_lt
  ; -"$builtins" &:: "hhbc_cmp_gt" <>$ capt_arg_payload $+ capt_arg_payload $--> hhbc_cmp_gt
  ; -"$builtins" &:: "hhbc_cmp_ge" <>$ capt_arg_payload $+ capt_arg_payload $--> hhbc_cmp_ge
  ; -"$builtins" &:: "hhbc_concat" <>$ capt_arg_payload $+ capt_arg_payload $--> hhbc_concat
  ; -"$builtins" &:: "hhbc_cmp_le" <>$ capt_arg_payload $+ capt_arg_payload $--> hhbc_cmp_le
  ; -"$builtins" &:: "hack_is_true" <>$ capt_arg_payload $--> hack_is_true
  ; -"$builtins" &:: "hhbc_is_type_str" <>$ capt_arg_payload $--> hhbc_is_type_str
  ; -"$builtins" &:: "hhbc_is_type_bool" <>$ capt_arg_payload $--> hhbc_is_type_bool
  ; -"$builtins" &:: "hhbc_is_type_int" <>$ capt_arg_payload $--> hhbc_is_type_int
  ; -"$builtins" &:: "hhbc_is_type_dbl" <>$ capt_arg_payload $--> hhbc_is_type_float
  ; -"$builtins" &:: "hhbc_is_type_dict" <>$ capt_arg_payload $--> hhbc_is_type_dict
  ; -"$builtins" &:: "hhbc_is_type_vec" <>$ capt_arg_payload $--> hhbc_is_type_vec
  ; -"$builtins" &:: "hhbc_verify_type_pred" <>$ capt_arg_payload $+ capt_arg_payload
    $--> hhbc_verify_type_pred
  ; -"$builtins" &:: "hhbc_verify_param_type_ts" <>$ capt_arg_payload $+ capt_arg_payload
    $--> hhbc_verify_param_type_ts
  ; -"$builtins" &:: "hhbc_add" <>$ capt_arg_payload $+ capt_arg_payload $--> hhbc_add
  ; -"$builtins" &:: "hack_get_static_class" <>$ capt_arg_payload $--> get_static_class
  ; -"$builtins" &:: "hhbc_cls_cns" <>$ capt_arg_payload $+ capt_arg_payload $--> hhbc_cls_cns
  ; -"$builtins" &:: "hack_set_static_prop" <>$ capt_arg_payload $+ capt_arg_payload
    $+ capt_arg_payload $--> hack_set_static_prop
  ; -"$builtins" &:: "hhbc_is_type_struct_c" <>$ capt_arg_payload $+ capt_arg_payload
    $+ capt_arg_payload $+ capt_arg_payload $--> hhbc_is_type_struct_c
  ; -"$root" &:: "FlibSL::C::contains_key" <>$ any_arg $+ capt_arg_payload $+ capt_arg_payload
    $--> Dict.contains_key
  ; -"$root" &:: "FlibSL::Vec::map" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload
    $--> Vec.map
  ; -"$root" &:: "FlibSL::Vec::from_async" <>$ capt_arg_payload $+ capt_arg_payload
    $--> Vec.vec_from_async
  ; -"$root" &:: "FlibSL::Dict::from_async" <>$ capt_arg_payload $+ capt_arg_payload
    $--> Dict.dict_from_async
  ; -"Asio$static" &:: "awaitSynchronously" <>$ capt_arg_payload $+ capt_arg_payload
    $--> hack_await_static
  ; -"$builtins" &:: "hhbc_iter_base" <>$ capt_arg_payload $--> hhbc_iter_base
  ; -"$builtins" &:: "hhbc_iter_init" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload
    $+ capt_arg_payload $--> hhbc_iter_init
  ; -"$builtins" &:: "hhbc_iter_next" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload
    $+ capt_arg_payload $--> hhbc_iter_next
  ; -"Infer$static" &:: "newUnconstrainedInt" <>$ any_arg $--> hack_unconstrained_int ]
  |> List.map ~f:(ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist)
