(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import
open PulseModelsImport
module DSL = PulseModelsDSL

let read_boxed_string_value address astate =
  let open IOption.Let_syntax in
  let hackString = TextualSil.hack_string_type_name in
  let field = Fieldname.make hackString "val" in
  let* box_val, _ = Memory.find_edge_opt address (FieldAccess field) astate in
  let* string_val, _ = Memory.find_edge_opt box_val Dereference astate in
  AddressAttributes.get_const_string string_val astate


let read_boxed_string_value_dsl aval : string DSL.model_monad =
  (* we cut the current path if no constant string is found *)
  let open PulseModelsDSL.Syntax in
  let operation astate = (read_boxed_string_value (fst aval) astate, astate) in
  let* opt_string = exec_operation operation in
  Option.value_map opt_string ~default:unreachable ~f:ret


let payload_of_arg arg =
  let {ProcnameDispatcher.Call.FuncArg.arg_payload= addr} = arg in
  addr


let payloads_of_args args = List.map ~f:payload_of_arg args

let mk_hack_field clazz field = Fieldname.make (Typ.HackClass (HackClassName.make clazz)) field

let await_hack_value aval : unit DSL.model_monad =
  fst aval |> AddressAttributes.hack_async_await |> DSL.Syntax.exec_command


let hack_await arg : model =
  let open DSL.Syntax in
  start_model
  @@ let* () = await_hack_value arg in
     assign_ret arg


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
  let class_name = "HackVec"

  let fst_field = mk_hack_field class_name "__infer_model_backing_vec_fst"

  let snd_field = mk_hack_field class_name "__infer_model_backing_vec_snd"

  let size_field = mk_hack_field class_name "__infer_model_backing_vec_size"

  let last_read_field = mk_hack_field class_name "__infer_model_backing_last_read"

  let new_vec_dsl args : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    let actual_size = List.length args in
    let typ = Typ.mk_struct TextualSil.hack_vec_type_name in
    let* vec = mk_fresh ~model_desc:"new_vec" in
    let* () = add_dynamic_type typ vec in
    let* size = mk_fresh ~model_desc:"new_vec.size" in
    let* last_read = mk_fresh ~model_desc:"new_vec.last_read" in
    let* dummy = mk_fresh ~model_desc:"new_vec.dummy" in
    let* () = write_deref_field ~ref:vec size_field ~obj:size in
    let* () = write_deref_field ~ref:vec last_read_field ~obj:last_read in
    let* () =
      match args with
      | [] ->
          let* () = write_deref_field ~ref:vec fst_field ~obj:dummy in
          write_deref_field ~ref:vec snd_field ~obj:dummy
      | arg1 :: rest -> (
          let* () = write_deref_field ~ref:vec fst_field ~obj:arg1 in
          match rest with
          | [] ->
              write_deref_field ~ref:vec snd_field ~obj:dummy
          | arg2 :: rest -> (
              let* () = write_deref_field ~ref:vec snd_field ~obj:arg2 in
              match rest with
              | [] ->
                  ret ()
                  (* Do "fake" await on the values we drop on the floor. TODO: mark reachable too? *)
              | rest ->
                  list_iter rest ~f:await_hack_value ) )
    in
    let* () = prune_eq_int size (IntLit.of_int actual_size) in
    let* () = prune_eq_int dummy (IntLit.of_int 9) in
    ret vec


  let new_vec args : model =
    let open DSL.Syntax in
    start_model
    @@
    let values = payloads_of_args args in
    let* vec = new_vec_dsl values in
    assign_ret vec


  let vec_from_async _dummy aval : model =
    let open DSL.Syntax in
    start_model
    @@
    (* let event = Hist.call_event path location "Vec\from_async" in *)
    let () = L.d_printfln "Called vec from async" in
    let* fst_val = eval_deref_access Read aval (FieldAccess fst_field) in
    let* snd_val = eval_deref_access Read aval (FieldAccess snd_field) in
    let* () = await_hack_value fst_val in
    let* () = await_hack_value snd_val in
    let* () = allocation Attribute.HackAsync aval in
    assign_ret aval


  let get_vec_dsl argv _index : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    let* ret_val = mk_fresh ~model_desc:"vec index" in
    let* new_last_read_val = mk_fresh ~model_desc:"vec index" in
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


  let get_vec argv index : unit DSL.model_monad =
    let open DSL.Syntax in
    let* ret_val = get_vec_dsl argv index in
    assign_ret ret_val


  let hack_array_get vec args : unit DSL.model_monad =
    let open DSL.Syntax in
    match args with
    | [key] ->
        let field = mk_hack_field "HackInt" "val" in
        let* index = eval_deref_access Read key (FieldAccess field) in
        get_vec vec index
    | _ ->
        L.d_printfln "Vec.hack_array_get expects only 1 key argument" ;
        disjuncts []


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
        let* () = await_hack_value v_fst in
        let* new_vec = new_vec_dsl [v_snd; value] in
        let* size = eval_deref_access Read vec (FieldAccess size_field) in
        let* () = write_deref_field ~ref:new_vec size_field ~obj:size in
        (* overwrite default size of 2 *)
        assign_ret new_vec
    | _ ->
        L.d_printfln "Vec.hack_array_cow_set expects 1 key and 1 value arguments" ;
        unreachable
end

let bool_val_field = Fieldname.make TextualSil.hack_bool_type_name "val"

let make_hack_bool bool : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let* bool = eval_read (Const (Cint (if bool then IntLit.one else IntLit.zero))) in
  let* boxed_bool = PulseModelsCpp.constructor_dsl TextualSil.hack_bool_type_name [("val", bool)] in
  ret boxed_bool


let aval_to_hack_int n_val : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let class_name = "HackInt" in
  let typ = Typ.mk_struct (Typ.HackClass (HackClassName.make class_name)) in
  let* ret_val = mk_fresh ~model_desc:"make_int" in
  let* () = add_dynamic_type typ ret_val in
  let* () = and_positive ret_val in
  let field = mk_hack_field class_name "val" in
  let* () = write_deref_field ~ref:ret_val field ~obj:n_val in
  ret ret_val


let int_to_hack_int n : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let* n_val = mk_fresh ~model_desc:"make_int" in
  let* () = and_eq_int n_val (IntLit.of_int n) in
  aval_to_hack_int n_val


let make_zero = int_to_hack_int 0

module VecIter = struct
  let class_name = "HackVecIterator"

  let vec_field = mk_hack_field class_name "__infer_model_backing_veciterator_vec"

  let index_field = mk_hack_field class_name "__infer_model_backing_veciterator_index"

  let iter_init_vec iteraddr keyaddr eltaddr argv : unit DSL.model_monad =
    let open DSL.Syntax in
    let* size_val = eval_deref_access Read argv (FieldAccess Vec.size_field) in
    let emptycase : DSL.aval DSL.model_monad =
      let* () = prune_eq_zero size_val in
      let* ret_val = make_hack_bool false in
      ret ret_val
    in
    let nonemptycase : DSL.aval DSL.model_monad =
      let* () = prune_ne_int size_val IntLit.zero in
      let* iter = mk_fresh ~model_desc:"iter_init" in
      let* zero = mk_fresh ~model_desc:"iter_init" in
      let* () = and_eq_int zero IntLit.zero in
      let typ = Typ.mk_struct (Typ.HackClass (HackClassName.make class_name)) in
      let* () = add_dynamic_type typ iter in
      let* () = and_positive iter in
      let* () = write_deref_field ~ref:iter vec_field ~obj:argv in
      let* () = write_deref_field ~ref:iter index_field ~obj:zero in
      let* () = write_deref ~ref:iteraddr ~obj:iter in
      let* hack_zero = make_zero in
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
    (* TODO: expand DSL so can combine finshed1 and finished2 with a disjunctive formula rather than a disjunction of paths *)
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
      (* let* () = prune_eq_zero v3 in *)
      let* () = prune_lt succindex size in
      let* () = prune_lt_int succindex IntLit.two in
      let* () = write_deref_field ~ref:iter index_field ~obj:succindex in
      let* hack_succindex = aval_to_hack_int succindex in
      let* elt = Vec.get_vec_dsl thevec hack_succindex in
      let* () = write_deref ~ref:eltaddr ~obj:elt in
      let* ret_val = make_hack_bool false in
      let haskey : unit DSL.model_monad =
        let* () = prune_ne_zero keyaddr in
        write_deref ~ref:keyaddr ~obj:hack_succindex
      in
      let nokey : unit DSL.model_monad = prune_eq_zero keyaddr in
      let* () = disjuncts [haskey; nokey] in
      assign_ret ret_val
    in
    disjuncts [finished1; finished2; not_finished]
end

let get_static_companion_var type_name =
  Pvar.mk_global (Mangled.mangled (Typ.Name.name type_name) "STATIC")


let get_static_companion ~model_desc path location type_name astate =
  let pvar = get_static_companion_var type_name in
  let var = Var.of_pvar pvar in
  let hist = Hist.single_call path location model_desc in
  let astate, ((addr, _) as addr_hist) = AbductiveDomain.Stack.eval hist var astate in
  let static_type_name = Typ.Name.Hack.static_companion type_name in
  let typ = Typ.mk_struct static_type_name in
  let astate = PulseOperations.add_dynamic_type typ addr astate in
  (addr_hist, astate)


let get_static_companion_dsl ~model_desc type_name : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let* {path; location} = get_data in
  exec_operation (get_static_companion ~model_desc path location type_name)


(* NOTE: We model [lazy_class_initialize] as invoking the corresponding [sinit] procedure.  This is
   unsound in terms of that it gets non-final values. *)
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
        let ret_id = Ident.create_none () in
        let ret_typ = Typ.mk_ptr (Typ.mk_struct TextualSil.hack_mixed_type_name) in
        let pname = Procname.get_hack_static_init class_name in
        let exp = Exp.Lvar (get_static_companion_var type_name) in
        let typ = Typ.mk_struct type_name in
        let* arg_payload = eval_to_value_origin exp in
        dispatch_call (ret_id, ret_typ) pname [(exp, typ)] [{exp; typ; arg_payload}]
    | _ ->
        ret ()
  in
  assign_ret class_object


let get_static_class aval : model =
  let open DSL.Syntax in
  start_model
  @@ let* (opt_typ : Typ.t option) = get_dynamic_type ~ask_specialization:true aval in
     match opt_typ with
     | Some {desc= Tstruct type_name} ->
         let* class_object = get_static_companion_dsl ~model_desc:"get_static_class" type_name in
         assign_ret class_object
     | _ ->
         ret ()


let hhbc_class_get_c value : model =
  let open DSL.Syntax in
  start_model
  @@ dynamic_dispatch value
       ~cases:
         [ ( TextualSil.hack_string_type_name
           , let* string = read_boxed_string_value_dsl value in
             (* namespace\\classname becomes namespace::classname *)
             let string = Str.(global_replace (regexp {|\\\\|}) "::" string) in
             let typ_name = Typ.HackClass (HackClassName.make string) in
             let* class_object = get_static_companion_dsl ~model_desc:"hhbc_class_get_c" typ_name in
             assign_ret class_object ) ]
       ~default:(get_static_class value |> lift_to_monad)


module Dict = struct
  (* We model dict/shape keys as fields. This is a bit unorthodox in Pulse, but we need
     maximum precision on this ubiquitous Hack data structure. *)

  let field_of_string_value value : Fieldname.t DSL.model_monad =
    let open DSL.Syntax in
    let* string = read_boxed_string_value_dsl value in
    TextualSil.wildcard_sil_fieldname Textual.Lang.Hack string |> ret


  let get_bindings values : (Fieldname.t * DSL.aval) list DSL.model_monad =
    let open DSL.Syntax in
    let chunked = List.chunks_of ~length:2 values in
    list_filter_map chunked ~f:(function
      | [string; value] ->
          let* field = field_of_string_value string in
          ret (Some (field, value))
      | _ ->
          ret None )


  (* TODO: handle integers keys *)
  let new_dict args : model =
    let open DSL.Syntax in
    start_model
    @@ let* bindings = payloads_of_args args |> get_bindings in
       let* dict = mk_fresh ~model_desc:"new_dict" in
       let typ = Typ.mk_struct TextualSil.hack_dict_type_name in
       let* () = add_dynamic_type typ dict in
       let* () =
         list_iter bindings ~f:(fun (field, value) -> write_deref_field ~ref:dict field ~obj:value)
       in
       assign_ret dict


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
              let* inner_dict = eval_deref_access Read dict (FieldAccess field) in
              let* copied_inned_dict = deep_copy ~depth_max:1 inner_dict in
              let* () = write_deref_field ~ref:dict field ~obj:copied_inned_dict in
              ret copied_inned_dict )
        in
        let* field = field_of_string_value key in
        let* () = write_deref_field ~ref:inner_dict field ~obj:value in
        assign_ret copy
    | _ when List.length args > 2 ->
        L.d_printfln "multidimensional copy on write not implemented yet" ;
        unreachable
    | _ ->
        L.die InternalError "should not happen"


  let hack_array_get dict keys : unit DSL.model_monad =
    let open DSL.Syntax in
    (* TODO: a key for a non-vec could be also a int *)
    let* value =
      list_fold keys ~init:dict ~f:(fun dict key ->
          let* field = field_of_string_value key in
          eval_deref_access Read dict (FieldAccess field) )
    in
    assign_ret value
end

let hack_array_cow_set this args : model =
  let open DSL.Syntax in
  start_model
  @@
  let this = payload_of_arg this in
  let args = payloads_of_args args in
  let default =
    let* fresh = mk_fresh ~model_desc:"hack_array_cow_set" in
    assign_ret fresh
  in
  dynamic_dispatch this
    ~cases:
      [ (TextualSil.hack_dict_type_name, Dict.hack_array_cow_set_dsl this args)
      ; (TextualSil.hack_vec_type_name, Vec.hack_array_cow_set_dsl this args) ]
    ~default


let hack_array_get this args : model =
  let open DSL.Syntax in
  start_model
  @@
  let this = payload_of_arg this in
  let args = payloads_of_args args in
  let default =
    let* fresh = mk_fresh ~model_desc:"hack_array_get" in
    assign_ret fresh
  in
  dynamic_dispatch this
    ~cases:
      [ (TextualSil.hack_dict_type_name, Dict.hack_array_get this args)
      ; (TextualSil.hack_vec_type_name, Vec.hack_array_get this args) ]
    ~default


let hack_field_get this field : model =
  let open DSL.Syntax in
  start_model
  @@ let* opt_string_field_name = get_const_string field in
     match opt_string_field_name with
     | Some string_field_name -> (
         let* opt_typ = get_dynamic_type ~ask_specialization:true this in
         match opt_typ with
         | Some {Typ.desc= Tstruct type_name} ->
             let field = Fieldname.make type_name string_field_name in
             let* aval = eval_deref_access Read this (FieldAccess field) in
             let* struct_info = tenv_resolve_fieldname type_name field in
             let opt_field_type_name =
               let open IOption.Let_syntax in
               let* {Struct.typ= field_typ} = struct_info in
               if Typ.is_pointer field_typ then Typ.name (Typ.strip_ptr field_typ) else None
             in
             let* () =
               option_iter opt_field_type_name ~f:(fun field_type_name ->
                   add_static_type field_type_name aval )
             in
             assign_ret aval
         | _ ->
             let* fresh = mk_fresh ~model_desc:"hack_field_get" in
             assign_ret fresh )
     | None ->
         L.die InternalError "hack_field_get expect a string constant as 2nd argument"


let make_hack_random_bool : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let* any = mk_fresh ~model_desc:"make_hack_random_bool" in
  let* boxed_bool = PulseModelsCpp.constructor_dsl TextualSil.hack_bool_type_name [("val", any)] in
  ret boxed_bool


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


let int_val_field = Fieldname.make TextualSil.hack_int_type_name "val"

let hhbc_cmp_same x y : model =
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
      [ (let* () = prune_eq x y in
         make_hack_bool true )
      ; (let* () = prune_ne x y in
         disjuncts
           [ ((* either of those is null but not both *)
              let* () = disjuncts [prune_eq_zero x; prune_eq_zero y] in
              make_hack_bool false )
           ; (let* () = prune_ne_zero x in
              let* () = prune_ne_zero y in
              let* x_typ = get_dynamic_type ~ask_specialization:true x in
              let* y_typ = get_dynamic_type ~ask_specialization:true y in
              match (x_typ, y_typ) with
              | None, _ | _, None ->
                  make_hack_random_bool
              | Some {Typ.desc= Tstruct x_typ_name}, Some {Typ.desc= Tstruct y_typ_name}
                when Typ.Name.equal x_typ_name y_typ_name ->
                  if Typ.Name.equal x_typ_name TextualSil.hack_int_type_name then
                    let* x_val = eval_deref_access Read x (FieldAccess int_val_field) in
                    let* y_val = eval_deref_access Read y (FieldAccess int_val_field) in
                    value_equality_test x_val y_val
                  else if Typ.Name.equal x_typ_name TextualSil.hack_bool_type_name then
                    let* x_val = eval_deref_access Read x (FieldAccess bool_val_field) in
                    let* y_val = eval_deref_access Read y (FieldAccess bool_val_field) in
                    value_equality_test x_val y_val
                  else make_hack_random_bool
              | _, _ ->
                  make_hack_bool false ) ] ) ]
  in
  assign_ret res


let hack_is_true b : model =
  let open DSL.Syntax in
  start_model
  @@
  let nullcase =
    let* () = prune_eq_zero b in
    let* zero = eval_read (Const (Cint IntLit.zero)) in
    assign_ret zero
  in
  let nonnullcase =
    let* () = prune_ne_zero b in
    let* b_typ = get_dynamic_type ~ask_specialization:true b in
    match b_typ with
    | None ->
        let _ = L.d_printfln "istrue None" in
        let* ret = make_hack_random_bool in
        assign_ret ret
    | Some {Typ.desc= Tstruct b_typ_name} ->
        if Typ.Name.equal b_typ_name TextualSil.hack_bool_type_name then
          let* b_val = eval_deref_access Read b (FieldAccess bool_val_field) in
          assign_ret b_val
        else
          let _ = L.d_printfln "istrue got typename %a" Typ.Name.pp b_typ_name in
          let* one = eval_read (Const (Cint IntLit.one)) in
          assign_ret one
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
  @@ let* typ_opt = get_dynamic_type ~ask_specialization:true this in
     let* field_v =
       match typ_opt with
       | Some {Typ.desc= Tstruct name} ->
           let* string_field_name = read_boxed_string_value_dsl field in
           let fld = Fieldname.make name string_field_name in
           let* class_object = get_static_companion_dsl ~model_desc name in
           eval_deref_access Read class_object (FieldAccess fld)
       | _ ->
           mk_fresh ~model_desc
     in
     assign_ret field_v


let hack_set_static_prop this prop obj : model =
  let open DSL.Syntax in
  start_model
  @@ let* this = read_boxed_string_value_dsl this in
     let this = String.substr_replace_all ~pattern:"\\" ~with_:":" this in
     let* prop = read_boxed_string_value_dsl prop in
     let name = Typ.HackClass (HackClassName.static_companion (HackClassName.make this)) in
     let* class_object = get_static_companion_dsl ~model_desc:"hack_set_static_prop" name in
     write_deref_field ~ref:class_object ~obj (Fieldname.make name prop)


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
              let* x_typ = get_dynamic_type ~ask_specialization:true x in
              let* y_typ = get_dynamic_type ~ask_specialization:true y in
              match (x_typ, y_typ) with
              | None, _ | _, None ->
                  L.d_printfln "random nones" ;
                  make_hack_random_bool
              | Some {Typ.desc= Tstruct x_typ_name}, Some {Typ.desc= Tstruct y_typ_name}
                when Typ.Name.equal x_typ_name y_typ_name ->
                  if Typ.Name.equal x_typ_name TextualSil.hack_int_type_name then
                    let* x_val = eval_deref_access Read x (FieldAccess int_val_field) in
                    let* y_val = eval_deref_access Read y (FieldAccess int_val_field) in
                    value_lt_test x_val y_val
                  else if Typ.Name.equal x_typ_name TextualSil.hack_bool_type_name then
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
              let* x_typ = get_dynamic_type ~ask_specialization:true x in
              let* y_typ = get_dynamic_type ~ask_specialization:true y in
              match (x_typ, y_typ) with
              | None, _ | _, None ->
                  make_hack_random_bool
              | Some {Typ.desc= Tstruct x_typ_name}, Some {Typ.desc= Tstruct y_typ_name}
                when Typ.Name.equal x_typ_name y_typ_name ->
                  if Typ.Name.equal x_typ_name TextualSil.hack_int_type_name then
                    let* x_val = eval_deref_access Read x (FieldAccess int_val_field) in
                    let* y_val = eval_deref_access Read y (FieldAccess int_val_field) in
                    value_le_test x_val y_val
                  else if Typ.Name.equal x_typ_name TextualSil.hack_bool_type_name then
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
  @@ let* x_typ = get_dynamic_type ~ask_specialization:true x in
     let* y_typ = get_dynamic_type ~ask_specialization:true y in
     match (x_typ, y_typ) with
     | Some {Typ.desc= Tstruct x_typ_name}, Some {Typ.desc= Tstruct y_typ_name}
       when Typ.Name.equal x_typ_name y_typ_name
            && Typ.Name.equal x_typ_name TextualSil.hack_int_type_name ->
         let* x_val = eval_deref_access Read x (FieldAccess int_val_field) in
         let* y_val = eval_deref_access Read y (FieldAccess int_val_field) in
         let* sum = eval_binop (PlusA (Some IInt)) x_val y_val in
         let* res = aval_to_hack_int sum in
         assign_ret res
     | _, _ ->
         let* sum = mk_fresh ~model_desc:"hhbc_add" in
         assign_ret sum (* unconstrained value *)


(* TODO: dynamic dispatch to cover iterators on dicts as well *)
let hhbc_iter_init iteraddr keyaddr eltaddr argv : model =
  let open DSL.Syntax in
  start_model @@ VecIter.iter_init_vec iteraddr keyaddr eltaddr argv


let hhbc_iter_next iteraddr keyaddr eltaddr : model =
  let open DSL.Syntax in
  start_model @@ VecIter.iter_next_vec iteraddr keyaddr eltaddr


module SplatedVec = struct
  let class_name = "HackSplatedVec"

  let field_name = "content"

  let field = mk_hack_field class_name field_name

  let typ_name = Typ.HackClass (HackClassName.make class_name)

  let make arg : model =
    let open DSL.Syntax in
    start_model
    @@ let* boxed = PulseModelsCpp.constructor_dsl typ_name [(field_name, arg)] in
       assign_ret boxed


  let build_vec_for_variadic_callee args : DSL.aval DSL.model_monad =
    let open DSL.Syntax in
    match args with
    | [arg] -> (
        let* arg_typ = get_dynamic_type ~ask_specialization:false arg in
        match arg_typ with
        | Some {Typ.desc= Tstruct name} when Typ.Name.equal name typ_name ->
            eval_deref_access Read arg (FieldAccess field)
        | _ ->
            Vec.new_vec_dsl args )
    | _ ->
        Vec.new_vec_dsl args
end

let build_vec_for_variadic_callee data args astate =
  (SplatedVec.build_vec_for_variadic_callee args |> DSL.unsafe_to_astate_transformer) data astate


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [ -"$builtins" &:: "nondet" <>$$--> Basic.nondet ~desc:"nondet"
  ; +BuiltinDecl.(match_builtin __lazy_class_initialize) <>$ capt_exp $--> lazy_class_initialize
  ; +BuiltinDecl.(match_builtin __get_lazy_class) <>$ capt_exp $--> lazy_class_initialize
  ; -"$builtins" &:: "__sil_splat" <>$ capt_arg_payload $--> SplatedVec.make
  ; -"$builtins" &:: "hhbc_await" <>$ capt_arg_payload $--> hack_await
  ; -"$builtins" &:: "hack_array_get" <>$ capt_arg $++$--> hack_array_get
  ; -"$builtins" &:: "hack_array_cow_set" <>$ capt_arg $++$--> hack_array_cow_set
  ; -"$builtins" &:: "hack_new_dict" &::.*++> Dict.new_dict
  ; -"$builtins" &:: "hhbc_new_vec" &::.*++> Vec.new_vec
  ; -"$builtins" &:: "hhbc_not" <>$ capt_arg_payload $--> hhbc_not
  ; -"$builtins" &:: "hack_get_class" <>$ capt_arg_payload
    $--> Basic.id_first_arg ~desc:"hack_get_class"
    (* not clear why HackC generate this builtin call *)
  ; -"$builtins" &:: "hack_field_get" <>$ capt_arg_payload $+ capt_arg_payload $--> hack_field_get
  ; -"$builtins" &:: "hhbc_class_get_c" <>$ capt_arg_payload $--> hhbc_class_get_c
    (* we should be able to model that directly in Textual once specialization will be stronger *)
  ; -"$builtins" &:: "hhbc_cmp_same" <>$ capt_arg_payload $+ capt_arg_payload $--> hhbc_cmp_same
  ; -"$builtins" &:: "hhbc_cmp_nsame" <>$ capt_arg_payload $+ capt_arg_payload $--> hhbc_cmp_nsame
  ; -"$builtins" &:: "hhbc_cmp_lt" <>$ capt_arg_payload $+ capt_arg_payload $--> hhbc_cmp_lt
  ; -"$builtins" &:: "hhbc_cmp_gt" <>$ capt_arg_payload $+ capt_arg_payload $--> hhbc_cmp_gt
  ; -"$builtins" &:: "hhbc_cmp_ge" <>$ capt_arg_payload $+ capt_arg_payload $--> hhbc_cmp_ge
  ; -"$builtins" &:: "hhbc_cmp_le" <>$ capt_arg_payload $+ capt_arg_payload $--> hhbc_cmp_le
  ; -"$builtins" &:: "hack_is_true" <>$ capt_arg_payload $--> hack_is_true
  ; -"$builtins" &:: "hhbc_add" <>$ capt_arg_payload $+ capt_arg_payload $--> hhbc_add
  ; -"$builtins" &:: "hack_get_static_class" <>$ capt_arg_payload $--> get_static_class
  ; -"$builtins" &:: "hhbc_cls_cns" <>$ capt_arg_payload $+ capt_arg_payload $--> hhbc_cls_cns
  ; -"$builtins" &:: "hack_set_static_prop" <>$ capt_arg_payload $+ capt_arg_payload
    $+ capt_arg_payload $--> hack_set_static_prop
  ; -"$root" &:: "FlibSL::Vec::from_async" <>$ capt_arg_payload $+ capt_arg_payload
    $--> Vec.vec_from_async
  ; -"$builtins" &:: "hhbc_iter_init" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload
    $+ capt_arg_payload $--> hhbc_iter_init
  ; -"$builtins" &:: "hhbc_iter_next" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload
    $--> hhbc_iter_next ]
  |> List.map ~f:(ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist)
