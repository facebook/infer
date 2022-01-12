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
open PulseOperations.Import
open PulseModelsImport
module Java = PulseModelsJava

let error err astate = [FatalError (ReportableError {astate; diagnostic= ErlangError err}, [])]

let error_badkey : model =
 fun {location} astate -> error (Badkey {calling_context= []; location}) astate


let error_badmap : model =
 fun {location} astate -> error (Badmap {calling_context= []; location}) astate


let error_badmatch : model =
 fun {location} astate -> error (Badmatch {calling_context= []; location}) astate


let error_badrecord : model =
 fun {location} astate -> error (Badrecord {calling_context= []; location}) astate


let error_case_clause : model =
 fun {location} astate -> error (Case_clause {calling_context= []; location}) astate


let error_function_clause : model =
 fun {location} astate -> error (Function_clause {calling_context= []; location}) astate


let error_if_clause : model =
 fun {location} astate -> error (If_clause {calling_context= []; location}) astate


let error_try_clause : model =
 fun {location} astate -> error (Try_clause {calling_context= []; location}) astate


let write_field_and_deref path location ~struct_addr ~field_addr ~field_val field_name astate =
  let* astate =
    PulseOperations.write_field path location ~ref:struct_addr field_name ~obj:field_addr astate
  in
  PulseOperations.write_deref path location ~ref:field_addr ~obj:field_val astate


let write_dynamic_type_and_return (addr_val, hist) typ ret_id astate =
  let typ = Typ.mk_struct (ErlangType typ) in
  let astate = PulseOperations.add_dynamic_type typ addr_val astate in
  PulseOperations.write_id ret_id (addr_val, hist) astate


(** Use for chaining functions of the type ('a->('b,'err) result list). The idea of such functions
    is that they can both fan-out into a (possibly empty) disjunction *and* signal errors. For
    example, consider [f] of type ['a->('b,'err) result list] and [g] of type
    ['b->('c,'err) result list] and [a] is some value of type ['a]. Note that the type of error is
    the same, so they can be propagated forward. To chain the application of these functions, you
    can write [let> x=f a in let> y=g x in \[Ok y\]].

    In several places, we have to compose with functions of the type ['a->('b,'err) result], which
    don't produce a list. One way to handle this is to wrap those functions in a list. For example,
    if [f] and [a] have the same type as before but [g] has type ['b->('c,'err) result], then we can
    write [let> =f a in let> y=\[g x\] in \[Ok y\].] *)
let ( let> ) x f =
  List.concat_map
    ~f:(function
      | FatalError _ as error ->
          [error]
      | Ok ok ->
          f ok
      | Recoverable (ok, errors) ->
          f ok |> List.map ~f:(fun result -> PulseResult.append_errors errors result) )
    x


let prune_type path location (value, hist) typ astate : AbductiveDomain.t AccessResult.t list =
  (* If check_addr_access fails, we stop exploring this path. *)
  let ( let^ ) x f = match x with Recoverable _ | FatalError _ -> [] | Ok astate -> [f astate] in
  let^ astate = PulseOperations.check_addr_access path Read location (value, hist) astate in
  let typ = Typ.mk_struct (ErlangType typ) in
  let instanceof_val = AbstractValue.mk_fresh () in
  let* astate = PulseArithmetic.and_equal_instanceof instanceof_val value typ astate in
  let+ astate = PulseArithmetic.prune_positive instanceof_val astate in
  astate


(** Loads a field from a struct, assuming that it has the correct type (should be checked by
    [prune_type]). *)
let load_field path field location obj astate =
  match Java.load_field path field location obj astate with
  | Recoverable _ | FatalError _ ->
      L.die InternalError "@[<v>@[%s@]@;@[%a@]@;@]"
        "Could not load field. Did you call this function without calling prune_type?"
        AbductiveDomain.pp astate
  | Ok result ->
      result


let atom_value_field = Fieldname.make (ErlangType Atom) ErlangTypeName.atom_value

let atom_hash_field = Fieldname.make (ErlangType Atom) ErlangTypeName.atom_hash

let make_atom value hash : model =
 fun {location; path; ret= ret_id, _} astate ->
  let hist = Hist.single_alloc path location "atom" in
  let addr_atom = (AbstractValue.mk_fresh (), hist) in
  let<*> astate =
    write_field_and_deref path location ~struct_addr:addr_atom
      ~field_addr:(AbstractValue.mk_fresh (), hist)
      ~field_val:value atom_value_field astate
  in
  let<+> astate =
    write_field_and_deref path location ~struct_addr:addr_atom
      ~field_addr:(AbstractValue.mk_fresh (), hist)
      ~field_val:hash atom_hash_field astate
  in
  write_dynamic_type_and_return addr_atom Atom ret_id astate


let convert_bool_to_atom_return path location hist bool_value ret_id astate =
  let addr_atom = (AbstractValue.mk_fresh (), hist) in
  let atom_value = AbstractValue.mk_fresh () in
  let atom_hash = AbstractValue.mk_fresh () in
  let mk_astate atom_str astate =
    let<*> astate =
      PulseArithmetic.and_eq_int atom_hash
        (IntLit.of_int (ErlangTypeName.calculate_hash atom_str))
        astate
    in
    (* TODO: write string to value *)
    let<*> astate =
      write_field_and_deref path location ~struct_addr:addr_atom
        ~field_addr:(AbstractValue.mk_fresh (), hist)
        ~field_val:(atom_value, hist) atom_value_field astate
    in
    let<+> astate =
      write_field_and_deref path location ~struct_addr:addr_atom
        ~field_addr:(AbstractValue.mk_fresh (), hist)
        ~field_val:(atom_hash, hist) atom_hash_field astate
    in
    write_dynamic_type_and_return addr_atom Atom ret_id astate
  in
  let astate_true =
    let<*> astate = PulseArithmetic.prune_positive bool_value astate in
    mk_astate ErlangTypeName.atom_true astate
  in
  let astate_false =
    let<*> astate = PulseArithmetic.prune_eq_zero bool_value astate in
    mk_astate ErlangTypeName.atom_false astate
  in
  astate_true @ astate_false


let erlang_is_atom (atom_val, _atom_hist) : model =
 fun {location; path; ret= ret_id, _} astate ->
  let typ = Typ.mk_struct (ErlangType Atom) in
  let is_atom = AbstractValue.mk_fresh () in
  let hist = Hist.single_call path location "is_atom" in
  let<*> astate = PulseArithmetic.and_equal_instanceof is_atom atom_val typ astate in
  convert_bool_to_atom_return path location hist is_atom ret_id astate


let erlang_is_boolean ((atom_val, _atom_hist) as atom) : model =
 fun {location; path; ret= ret_id, _} astate ->
  let hist = Hist.single_call path location "is_boolean" in
  let astate_not_atom =
    (* Assume not atom: just return false *)
    let typ = Typ.mk_struct (ErlangType Atom) in
    let is_atom = AbstractValue.mk_fresh () in
    let<*> astate = PulseArithmetic.and_equal_instanceof is_atom atom_val typ astate in
    let<*> astate = PulseArithmetic.prune_eq_zero is_atom astate in
    convert_bool_to_atom_return path location hist is_atom ret_id astate
  in
  let astate_is_atom =
    (* Assume atom: return hash==hashof(true) or hash==hashof(false) *)
    let> astate = prune_type path location atom Atom astate in
    let astate, _hash_addr, (hash, _hash_hist) =
      load_field path
        (Fieldname.make (ErlangType Atom) ErlangTypeName.atom_hash)
        location atom astate
    in
    let is_true = AbstractValue.mk_fresh () in
    let is_false = AbstractValue.mk_fresh () in
    let is_bool = AbstractValue.mk_fresh () in
    let<*> astate, is_true =
      PulseArithmetic.eval_binop is_true Binop.Eq (AbstractValueOperand hash)
        (ConstOperand (Cint (IntLit.of_int (ErlangTypeName.calculate_hash ErlangTypeName.atom_true)))
        )
        astate
    in
    let<*> astate, is_false =
      PulseArithmetic.eval_binop is_false Binop.Eq (AbstractValueOperand hash)
        (ConstOperand
           (Cint (IntLit.of_int (ErlangTypeName.calculate_hash ErlangTypeName.atom_false))) )
        astate
    in
    let<*> astate, is_bool =
      PulseArithmetic.eval_binop is_bool Binop.LOr (AbstractValueOperand is_true)
        (AbstractValueOperand is_false) astate
    in
    convert_bool_to_atom_return path location hist is_bool ret_id astate
  in
  astate_not_atom @ astate_is_atom


let cons_head_field = Fieldname.make (ErlangType Cons) ErlangTypeName.cons_head

let cons_tail_field = Fieldname.make (ErlangType Cons) ErlangTypeName.cons_tail

(** Helper function to create a Nil structure without assigning it to return value *)
let make_nil_no_return location path astate =
  let event = Hist.alloc_event path location "[]" in
  let addr_nil_val = AbstractValue.mk_fresh () in
  let addr_nil = (addr_nil_val, Hist.single_event path event) in
  let astate =
    PulseOperations.add_dynamic_type (Typ.mk_struct (ErlangType Nil)) addr_nil_val astate
  in
  (addr_nil, astate)


(** Create a Nil structure and assign it to return value *)
let make_nil : model =
 fun {location; path; ret= ret_id, _} astate ->
  let addr_nil, astate = make_nil_no_return location path astate in
  PulseOperations.write_id ret_id addr_nil astate |> Basic.ok_continue


(** Helper function to create a Cons structure without assigning it to return value *)
let make_cons_no_return astate path location hd tl =
  let hist = Hist.single_alloc path location "[X|Xs]" in
  let addr_cons_val = AbstractValue.mk_fresh () in
  let addr_cons = (addr_cons_val, hist) in
  let* astate =
    write_field_and_deref path location ~struct_addr:addr_cons
      ~field_addr:(AbstractValue.mk_fresh (), hist)
      ~field_val:hd cons_head_field astate
  in
  let+ astate =
    write_field_and_deref path location ~struct_addr:addr_cons
      ~field_addr:(AbstractValue.mk_fresh (), hist)
      ~field_val:tl cons_tail_field astate
  in
  let astate =
    PulseOperations.add_dynamic_type (Typ.mk_struct (ErlangType Cons)) addr_cons_val astate
  in
  (addr_cons, astate)


(** Create a Cons structure and assign it to return value *)
let make_cons head tail : model =
 fun {location; path; ret= ret_id, _} astate ->
  let<+> addr_cons, astate = make_cons_no_return astate path location head tail in
  PulseOperations.write_id ret_id addr_cons astate


(** Assumes that the argument is a Cons and loads the head and tail *)
let load_head_tail cons astate path location =
  let> astate = prune_type path location cons Cons astate in
  let astate, _, head = load_field path cons_head_field location cons astate in
  let astate, _, tail = load_field path cons_tail_field location cons astate in
  [Ok (head, tail, astate)]


(** Assumes that a list is of given length and reads the elements *)
let rec list_assume_and_deconstruct list length astate path location =
  match length with
  | 0 ->
      let> astate = prune_type path location list Nil astate in
      [Ok ([], astate)]
  | _ ->
      let> hd, tl, astate = load_head_tail list astate path location in
      let> elems, astate = list_assume_and_deconstruct tl (length - 1) astate path location in
      [Ok (hd :: elems, astate)]


let lists_append2 ~reverse list1 list2 : model =
 fun {location; path; ret= ret_id, _} astate ->
  (* Makes an abstract state corresponding to appending to a list of given length *)
  let mk_astate_concat length =
    let> elems, astate = list_assume_and_deconstruct list1 length astate path location in
    let elems = if reverse then elems else List.rev elems in
    let> result_list, astate =
      [ PulseResult.list_fold ~init:(list2, astate)
          ~f:(fun (tl, astate) hd -> make_cons_no_return astate path location hd tl)
          elems ]
    in
    [Ok (PulseOperations.write_id ret_id result_list astate)]
  in
  List.concat (List.init Config.erlang_list_unfold_depth ~f:mk_astate_concat)
  |> List.map ~f:Basic.map_continue


let lists_reverse list : model =
 fun ({location; path; _} as data) astate ->
  let nil, astate = make_nil_no_return location path astate in
  lists_append2 ~reverse:true list nil data astate


let erlang_is_list (list_val, _list_hist) : model =
 fun {location; path; ret= ret_id, _} astate ->
  let cons_typ = Typ.mk_struct (ErlangType Cons) in
  let nil_typ = Typ.mk_struct (ErlangType Nil) in
  let is_cons = AbstractValue.mk_fresh () in
  let is_nil = AbstractValue.mk_fresh () in
  let is_list = AbstractValue.mk_fresh () in
  let hist = Hist.single_call path location "erlang:is_list" in
  let<*> astate = PulseArithmetic.and_equal_instanceof is_cons list_val cons_typ astate in
  let<*> astate = PulseArithmetic.and_equal_instanceof is_nil list_val nil_typ astate in
  let<*> astate, is_list =
    PulseArithmetic.eval_binop is_list Binop.LOr (AbstractValueOperand is_cons)
      (AbstractValueOperand is_nil) astate
  in
  convert_bool_to_atom_return path location hist is_list ret_id astate


let make_tuple (args : 'a ProcnameDispatcher.Call.FuncArg.t list) : model =
 fun {location; path; ret= ret_id, _} astate ->
  let tuple_size = List.length args in
  let tuple_typ_name : Typ.name = ErlangType (Tuple tuple_size) in
  let hist = Hist.single_alloc path location "{}" in
  let addr_tuple = (AbstractValue.mk_fresh (), hist) in
  let addr_elems = List.map ~f:(function _ -> (AbstractValue.mk_fresh (), hist)) args in
  let mk_field = Fieldname.make tuple_typ_name in
  let field_names = ErlangTypeName.tuple_field_names tuple_size in
  let get_payload (arg : 'a ProcnameDispatcher.Call.FuncArg.t) = arg.arg_payload in
  let arg_payloads = List.map ~f:get_payload args in
  let addr_elems_fields_payloads =
    List.zip_exn addr_elems (List.zip_exn field_names arg_payloads)
  in
  let write_tuple_field astate (addr_elem, (field_name, payload)) =
    write_field_and_deref path location ~struct_addr:addr_tuple ~field_addr:addr_elem
      ~field_val:payload (mk_field field_name) astate
  in
  let<+> astate =
    PulseResult.list_fold addr_elems_fields_payloads ~init:astate ~f:write_tuple_field
  in
  write_dynamic_type_and_return addr_tuple (Tuple tuple_size) ret_id astate


(** Maps are currently approximated to store only the latest key/value *)
let mk_map_field = Fieldname.make (ErlangType Map)

let map_key_field = mk_map_field "__infer_model_backing_map_key"

let map_value_field = mk_map_field "__infer_model_backing_map_value"

let map_is_empty_field = mk_map_field "__infer_model_backing_map_is_empty"

let make_map (args : 'a ProcnameDispatcher.Call.FuncArg.t list) : model =
 fun {location; path; ret= ret_id, _} astate ->
  let hist = Hist.single_alloc path location "#{}" in
  let addr_map = (AbstractValue.mk_fresh (), hist) in
  let addr_is_empty = (AbstractValue.mk_fresh (), hist) in
  let is_empty_value = AbstractValue.mk_fresh () in
  let fresh_val = (is_empty_value, hist) in
  let is_empty_lit = match args with [] -> IntLit.one | _ -> IntLit.zero in
  (* Reverse the list so we can get last key/value *)
  let<*> astate =
    match List.rev args with
    (* Non-empty map: we just consider the last key/value, rest is ignored (approximation) *)
    | value_arg :: key_arg :: _ ->
        let addr_key = (AbstractValue.mk_fresh (), hist) in
        let addr_value = (AbstractValue.mk_fresh (), hist) in
        write_field_and_deref path location ~struct_addr:addr_map ~field_addr:addr_key
          ~field_val:key_arg.arg_payload map_key_field astate
        >>= write_field_and_deref path location ~struct_addr:addr_map ~field_addr:addr_value
              ~field_val:value_arg.arg_payload map_value_field
    | _ :: _ ->
        L.die InternalError "Map create got one argument (requires even number)"
    (* Empty map *)
    | [] ->
        Ok astate
  in
  let<*> astate =
    write_field_and_deref path location ~struct_addr:addr_map ~field_addr:addr_is_empty
      ~field_val:fresh_val map_is_empty_field astate
  in
  let<+> astate = PulseArithmetic.and_eq_int is_empty_value is_empty_lit astate in
  write_dynamic_type_and_return addr_map Map ret_id astate


let maps_new : model = make_map []

let make_astate_badmap (map_val, _map_hist) data astate =
  let typ = Typ.mk_struct (ErlangType Map) in
  let instanceof_val = AbstractValue.mk_fresh () in
  let<*> astate = PulseArithmetic.and_equal_instanceof instanceof_val map_val typ astate in
  let<*> astate = PulseArithmetic.prune_eq_zero instanceof_val astate in
  error_badmap data astate


let make_astate_goodmap path location map astate = prune_type path location map Map astate

let erlang_is_map (map_val, _map_hist) : model =
 fun {location; path; ret= ret_id, _} astate ->
  let typ = Typ.mk_struct (ErlangType Map) in
  let is_map = AbstractValue.mk_fresh () in
  let hist = Hist.single_call path location "is_map" in
  let<*> astate = PulseArithmetic.and_equal_instanceof is_map map_val typ astate in
  convert_bool_to_atom_return path location hist is_map ret_id astate


let maps_is_key (key, _key_history) map : model =
 fun ({location; path; ret= ret_id, _} as data) astate ->
  let hist = Hist.single_call path location "map_is_key" in
  (* Return 3 cases:
   * - Error & assume not map
   * - Ok & assume map & assume empty & return false
   * - Ok & assume map & assume not empty & assume key is the tracked key & return true
   *)
  let astate_badmap = make_astate_badmap map data astate in
  let astate_empty =
    let ret_val_false = AbstractValue.mk_fresh () in
    let> astate = make_astate_goodmap path location map astate in
    let astate, _isempty_addr, (is_empty, _isempty_hist) =
      load_field path map_is_empty_field location map astate
    in
    let> astate = [PulseArithmetic.prune_positive is_empty astate] in
    let> astate = [PulseArithmetic.and_eq_int ret_val_false IntLit.zero astate] in
    convert_bool_to_atom_return path location hist ret_val_false ret_id astate
  in
  let astate_haskey =
    let ret_val_true = AbstractValue.mk_fresh () in
    let> astate = make_astate_goodmap path location map astate in
    let astate, _isempty_addr, (is_empty, _isempty_hist) =
      load_field path map_is_empty_field location map astate
    in
    let> astate = [PulseArithmetic.prune_eq_zero is_empty astate] in
    let astate, _key_addr, (tracked_key, _hist) =
      load_field path map_key_field location map astate
    in
    let> astate =
      [ PulseArithmetic.prune_binop ~negated:false Binop.Eq (AbstractValueOperand key)
          (AbstractValueOperand tracked_key) astate ]
    in
    let> astate = [PulseArithmetic.and_eq_int ret_val_true IntLit.one astate] in
    convert_bool_to_atom_return path location hist ret_val_true ret_id astate
  in
  astate_empty @ astate_haskey @ astate_badmap


let maps_get (key, _key_history) map : model =
 fun ({location; path; ret= ret_id, _} as data) astate ->
  (* Return 3 cases:
   * - Error & assume not map
   * - Error & assume map & assume empty;
   * - Ok & assume map & assume nonempty & assume key is the tracked key & return tracked value
   *)
  let astate_badmap = make_astate_badmap map data astate in
  let astate_ok =
    let> astate = make_astate_goodmap path location map astate in
    let astate, _isempty_addr, (is_empty, _isempty_hist) =
      load_field path map_is_empty_field location map astate
    in
    let> astate = [PulseArithmetic.prune_eq_zero is_empty astate] in
    let astate, _key_addr, (tracked_key, _hist) =
      load_field path map_key_field location map astate
    in
    let> astate =
      [ PulseArithmetic.prune_binop ~negated:false Binop.Eq (AbstractValueOperand key)
          (AbstractValueOperand tracked_key) astate ]
    in
    let astate, _value_addr, tracked_value = load_field path map_value_field location map astate in
    [Ok (PulseOperations.write_id ret_id tracked_value astate)]
  in
  let astate_badkey =
    let> astate = make_astate_goodmap path location map astate in
    let astate, _isempty_addr, (is_empty, _isempty_hist) =
      load_field path map_is_empty_field location map astate
    in
    let> astate = [PulseArithmetic.prune_positive is_empty astate] in
    error_badkey data astate
  in
  List.map ~f:Basic.map_continue astate_ok @ astate_badkey @ astate_badmap


let maps_put key value map : model =
 fun ({location; path; ret= ret_id, _} as data) astate ->
  (* Ignore old map. We only store one key/value so we can simply create a new map. *)
  (* Return 2 cases:
   * - Error & assume not map
   * - Ok & assume map & return new map
   *)
  let hist = Hist.single_alloc path location "maps_put" in
  let astate_badmap = make_astate_badmap map data astate in
  let astate_ok =
    let addr_map = (AbstractValue.mk_fresh (), hist) in
    let addr_is_empty = (AbstractValue.mk_fresh (), hist) in
    let is_empty_value = AbstractValue.mk_fresh () in
    let fresh_val = (is_empty_value, hist) in
    let addr_key = (AbstractValue.mk_fresh (), hist) in
    let addr_value = (AbstractValue.mk_fresh (), hist) in
    let> astate = make_astate_goodmap path location map astate in
    let> astate =
      [ write_field_and_deref path location ~struct_addr:addr_map ~field_addr:addr_key
          ~field_val:key map_key_field astate ]
    in
    let> astate =
      [ write_field_and_deref path location ~struct_addr:addr_map ~field_addr:addr_value
          ~field_val:value map_value_field astate ]
    in
    let> astate =
      [ write_field_and_deref path location ~struct_addr:addr_map ~field_addr:addr_is_empty
          ~field_val:fresh_val map_is_empty_field astate
        >>= PulseArithmetic.and_eq_int is_empty_value IntLit.zero ]
    in
    [Ok (write_dynamic_type_and_return addr_map Map ret_id astate)]
  in
  List.map ~f:Basic.map_continue astate_ok @ astate_badmap


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  [ +BuiltinDecl.(match_builtin __erlang_make_atom)
    <>$ capt_arg_payload $+ capt_arg_payload $--> make_atom
  ; +BuiltinDecl.(match_builtin __erlang_make_cons)
    <>$ capt_arg_payload $+ capt_arg_payload $--> make_cons
  ; +BuiltinDecl.(match_builtin __erlang_make_tuple) &++> make_tuple
  ; +BuiltinDecl.(match_builtin __erlang_make_nil) <>--> make_nil
  ; +BuiltinDecl.(match_builtin __erlang_make_map) &++> make_map
  ; +BuiltinDecl.(match_builtin __erlang_error_badkey) <>--> error_badkey
  ; +BuiltinDecl.(match_builtin __erlang_error_badmap) <>--> error_badmap
  ; +BuiltinDecl.(match_builtin __erlang_error_badmatch) <>--> error_badmatch
  ; +BuiltinDecl.(match_builtin __erlang_error_badrecord) <>--> error_badrecord
  ; +BuiltinDecl.(match_builtin __erlang_error_case_clause) <>--> error_case_clause
  ; +BuiltinDecl.(match_builtin __erlang_error_function_clause) <>--> error_function_clause
  ; +BuiltinDecl.(match_builtin __erlang_error_if_clause) <>--> error_if_clause
  ; +BuiltinDecl.(match_builtin __erlang_error_try_clause) <>--> error_try_clause
  ; -ErlangTypeName.erlang_namespace &:: "is_map" <>$ capt_arg_payload $--> erlang_is_map
  ; -ErlangTypeName.erlang_namespace &:: "is_list" <>$ capt_arg_payload $--> erlang_is_list
  ; -ErlangTypeName.erlang_namespace &:: "is_atom" <>$ capt_arg_payload $--> erlang_is_atom
  ; -ErlangTypeName.erlang_namespace &:: "is_boolean" <>$ capt_arg_payload $--> erlang_is_boolean
  ; -"lists" &:: "append" <>$ capt_arg_payload $+ capt_arg_payload $--> lists_append2 ~reverse:false
  ; -"lists" &:: "reverse" <>$ capt_arg_payload $--> lists_reverse
  ; -"maps" &:: "is_key" <>$ capt_arg_payload $+ capt_arg_payload $--> maps_is_key
  ; -"maps" &:: "get" <>$ capt_arg_payload $+ capt_arg_payload $--> maps_get
  ; -"maps" &:: "put" <>$ capt_arg_payload $+ capt_arg_payload $+ capt_arg_payload $--> maps_put
  ; -"maps" &:: "new" <>$$--> maps_new ]
