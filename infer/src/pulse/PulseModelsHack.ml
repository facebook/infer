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


  let get_vec argv index : unit DSL.model_monad =
    let open DSL.Syntax in
    let* ret_val = mk_fresh ~model_desc:"vec index" in
    let* new_last_read_val = mk_fresh ~model_desc:"vec index" in
    let* size_val = eval_deref_access Read argv (FieldAccess size_field) in
    let* fst_val = eval_deref_access Read argv (FieldAccess fst_field) in
    let* snd_val = eval_deref_access Read argv (FieldAccess snd_field) in
    let* last_read_val = eval_deref_access Read argv (FieldAccess last_read_field) in
    let* () = write_deref_field ~ref:argv last_read_field ~obj:new_last_read_val in
    let* () = prune_lt index size_val in
    let* () =
      (* Don't return dummy value *)
      prune_ne_int ret_val (IntLit.of_int 9)
    in
    (* TODO: work out how to incorporate type-based, or at least nullability, assertions on ret_val *)
    let case1 : unit DSL.model_monad =
      (* case 1: return is some value equal to neither field
          In this case, we leave the last_read_val field unchanged
          And we also, to avoid false positives, do a fake await of the fst and snd fields
      *)
      let* () = await_hack_value fst_val in
      let* () = await_hack_value snd_val in
      let* () = prune_ne ret_val fst_val in
      let* () = prune_ne ret_val snd_val in
      let* () = prune_eq new_last_read_val last_read_val in
      assign_ret ret_val
    in
    let case2 : unit DSL.model_monad =
      (* case 2: given element is equal to fst_field *)
      let* () = prune_eq_int last_read_val IntLit.two in
      let* () = prune_eq ret_val fst_val in
      let* () = prune_eq_int new_last_read_val IntLit.one in
      assign_ret ret_val
    in
    let case3 : unit DSL.model_monad =
      (* case 3: given element is equal to snd_field *)
      let* () = prune_eq_int last_read_val IntLit.one in
      let* () = prune_eq ret_val snd_val in
      let* () = prune_eq_int new_last_read_val IntLit.two in
      assign_ret ret_val
    in
    disjuncts [case1; case2; case3]


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

let get_static_companion_var type_name =
  Pvar.mk_global (Mangled.mangled (Typ.Name.name type_name) "STATIC")


let get_static_companion ~model_desc path location type_name astate =
  let pvar = get_static_companion_var type_name in
  let var = Var.of_pvar pvar in
  (* we chose on purpose to not abduce [pvar] because we don't want to make a disjunctive case
     if it is already assigned or not. This is problematic when the caller already defines the
     variable because the Pulse summary application will not detect that the variable is set
     both in the callee and the caller. But this is fine as long as both functions perform the
     same initialization of the variable. *)
  match AbductiveDomain.Stack.find_opt var astate with
  | Some addr_hist ->
      (addr_hist, astate)
  | None ->
      let addr = AbstractValue.mk_fresh () in
      let hist = Hist.single_call path location model_desc in
      let astate = AbductiveDomain.Stack.add var (addr, ValueHistory.epoch) astate in
      let static_type_name = Typ.Name.Hack.static_companion type_name in
      let typ = Typ.mk_struct static_type_name in
      let astate = PulseOperations.add_dynamic_type typ addr astate in
      ((addr, hist), astate)


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


let bool_val_field = Fieldname.make TextualSil.hack_bool_type_name "val"

let make_hack_bool bool : DSL.aval DSL.model_monad =
  let open DSL.Syntax in
  let* bool = eval_read (Const (Cint (if bool then IntLit.one else IntLit.zero))) in
  let* boxed_bool = PulseModelsCpp.constructor_dsl TextualSil.hack_bool_type_name [("val", bool)] in
  ret boxed_bool


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
       | Some ({Typ.desc= Tstruct name} as typ) ->
           let* string_field_name = read_boxed_string_value_dsl field in
           let fld = Fieldname.make name string_field_name in
           let class_global_var = get_static_companion_var name in
           (* Ideally, we should not need [eval_read] here since we call [eval_deref_access] below,
              but somehow the latter does NOT update the pre state that we need. *)
           let* _ = eval_read (Lfield (Lvar class_global_var, fld, typ)) in
           let* class_object = get_static_companion_dsl ~model_desc name in
           eval_deref_access Read class_object (FieldAccess fld)
       | _ ->
           mk_fresh ~model_desc
     in
     assign_ret field_v


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [ -"$builtins" &:: "nondet" <>$$--> Basic.nondet ~desc:"nondet"
  ; +BuiltinDecl.(match_builtin __lazy_class_initialize) <>$ capt_exp $--> lazy_class_initialize
  ; +BuiltinDecl.(match_builtin __get_lazy_class) <>$ capt_exp $--> lazy_class_initialize
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
  ; -"$builtins" &:: "hack_get_static_class" <>$ capt_arg_payload $--> get_static_class
  ; -"$builtins" &:: "hhbc_cls_cns" <>$ capt_arg_payload $+ capt_arg_payload $--> hhbc_cls_cns
  ; -"$root" &:: "FlibSL::Vec::from_async" <>$ capt_arg_payload $+ capt_arg_payload
    $--> Vec.vec_from_async ]
  |> List.map ~f:(ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist)
