(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface
open PulseOperationResult.Import
open PulseModelsImport
module Collection = PulseModelsGenericArrayBackedCollection
module GenericMapCollection = PulseModelsCpp.GenericMapCollection

(* Models of [std::deque] and of the node-based containers ([std::list], [std::map], [std::set],
   [std::unordered_map], ...), and of their libc++ iterators. On libstdc++, whose iterators are not
   modelled, [begin()], [end()], [find()] and [erase()] are not modelled either; the other models
   apply.

   The elements of a container are the cells of its backing array so that [clear()] can invalidate
   all the elements known to the analysis. An iterator stores the address of its element, or the
   past-the-end value of the container, in [node_field], and the backing array of the container in
   [Collection.field]. Insertions do not invalidate the elements of the node-based containers; the
   invalidation of the iterators of an unordered container by rehashing is not modelled.

   [std::deque] iterators also store the current [token_field] of the container: insertions into a
   deque invalidate the token, that is all the iterators, but no element (insertions in the middle of
   a deque also invalidate references, which is not modelled).

   [begin()] and [find()] return the end iterator only when the container is known to be empty so
   that a lookup is not reported for lack of a comparison with [end()]. *)

let node_field = Fieldname.make PulseOperations.pulse_model_type "__infer_node"

let token_field = Fieldname.make PulseOperations.pulse_model_type "__infer_iterators_token"

let desc class_name name = Printf.sprintf "std::%s::%s()" class_name name

let has_token (container : Invalidation.std_container) =
  match container with Deque -> true | _ -> false


let load_field path location obj field astate =
  PulseOperations.eval_deref_access path Read location obj (FieldAccess field) astate


let store_field path location event obj field (value, hist) astate =
  PulseOperations.write_deref_field path location ~ref:obj field
    ~obj:(value, Hist.add_event event hist)
    astate


let end_function = Formula.Procname (Procname.from_string_c_fun "__infer_end_of_collection")

(* The past-the-end value of a container is a function of its backing array rather than a cell of
   it: comparing an element, abduced in the pre, with a value of the pre would be an assumption on
   the heap of the caller that makes all the paths after a loop over a container parameter latent,
   and a cell added in the post only would hide the writes through the elements from impurity. *)
let eval_end path location container astate =
  let=* astate, arr = Collection.eval path Read location container astate in
  let end_value = AbstractValue.mk_fresh () in
  let++ astate =
    PulseArithmetic.and_equal (AbstractValueOperand end_value)
      (FunctionApplicationOperand {f= end_function; actuals= [fst arr]})
      astate
  in
  (AddressAttributes.mark_as_end_of_collection end_value astate, (end_value, snd arr))


let fresh_element path location container astate =
  Collection.element path location container (AbstractValue.mk_fresh ()) astate


let set_size_positive path location event container astate =
  let size = AbstractValue.mk_fresh () in
  let=* astate =
    PulseOperations.write_deref_field path location ~ref:container Collection.size_field
      ~obj:(size, Hist.single_event event)
      astate
  in
  PulseArithmetic.and_positive size astate


let make_iterator ~has_token path location event container iter elem astate =
  let* astate, arr = Collection.eval path Read location container astate in
  let* astate = store_field path location event iter Collection.field arr astate in
  let* astate = store_field path location event iter node_field elem astate in
  if has_token then
    let* astate, token = load_field path location container token_field astate in
    store_field path location event iter token_field token astate
  else Ok astate


let copy_iterator ~has_token path location event ~src ~dst astate =
  let fields = Collection.field :: node_field :: (if has_token then [token_field] else []) in
  PulseResult.list_fold fields ~init:astate ~f:(fun astate field ->
      let* astate, value = load_field path location src field astate in
      store_field path location event dst field value astate )


(** check that [iter] is valid, and that it is not the end iterator unless [allow_end], then return
    its element and the backing array of its container *)
let check_iterator ~has_token ~allow_end path location iter astate =
  let* astate, elem = load_field path location iter node_field astate in
  let* astate =
    if allow_end then Ok astate else Collection.Iterator.check_not_end location elem astate
  in
  let* astate =
    if has_token then
      let* astate, token = load_field path location iter token_field astate in
      PulseOperations.check_addr_access path NoAccess location token astate
    else Ok astate
  in
  let* astate = PulseOperations.check_addr_access path NoAccess location elem astate in
  (* [clear()] invalidates the backing array, hence the iterators obtained before *)
  let* astate, arr = load_field path location iter Collection.field astate in
  let+ astate = PulseOperations.check_addr_access path NoAccess location arr astate in
  (astate, elem, arr)


(* the fields of the element are invalidated too, e.g. the mapped value of a map entry *)
let invalidate_element path location cause elem astate =
  let rec invalidate_fields (visited, astate) addr =
    Memory.fold_edges (fst addr) astate ~init:(visited, astate)
      ~f:(fun (visited, astate) (access, field_addr) ->
        match (access : Access.t) with
        | FieldAccess _ when not (AbstractValue.Set.mem (fst field_addr) visited) ->
            let astate =
              PulseOperations.invalidate path
                (MemoryAccess {pointer= addr; access; hist_obj_default= snd field_addr})
                location cause field_addr astate
            in
            invalidate_fields (AbstractValue.Set.add (fst field_addr) visited, astate) field_addr
        | _ ->
            (visited, astate) )
  in
  let astate = PulseOperations.invalidate path UntraceableAccess location cause elem astate in
  invalidate_fields (AbstractValue.Set.singleton (fst elem), astate) elem |> snd


module Iterator = struct
  let constructor this other ~has_token ~desc : model_no_non_disj =
   fun {path; location} astate ->
    let event = Hist.call_event path location desc in
    let<+> astate = copy_iterator ~has_token path location event ~src:other ~dst:this astate in
    astate


  let operator_assign this other ~has_token ~desc : model_no_non_disj =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location desc in
    let<+> astate = copy_iterator ~has_token path location event ~src:other ~dst:this astate in
    PulseOperations.write_id ret_id (fst this, Hist.add_event event (snd this)) astate


  let operator_star iter ~has_token ~desc : model_no_non_disj =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location desc in
    let<+> astate, (elem, hist), _ =
      check_iterator ~has_token ~allow_end:false path location iter astate
    in
    PulseOperations.write_id ret_id (elem, Hist.add_event event hist) astate


  let step ~has_token step path location event iter astate =
    let allow_end = match step with `PlusPlus -> false | `MinusMinus -> true in
    let* astate, _, arr = check_iterator ~has_token ~allow_end path location iter astate in
    let* astate, elem =
      Collection.eval_element path location arr (AbstractValue.mk_fresh ()) astate
    in
    store_field path location event iter node_field elem astate


  let operator_step step_ iter ~has_token ~desc : model_no_non_disj =
   fun {path; location; ret= ret_id, _} astate ->
    let event = Hist.call_event path location desc in
    let<+> astate = step ~has_token step_ path location event iter astate in
    PulseOperations.write_id ret_id (fst iter, Hist.add_event event (snd iter)) astate


  (** [it++] and [it--] return a copy of the iterator before the step *)
  let operator_step_postfix step_ iter ret_iter ~has_token ~desc : model_no_non_disj =
   fun {path; location} astate ->
    let event = Hist.call_event path location desc in
    let<+> astate =
      let* astate = copy_iterator ~has_token path location event ~src:iter ~dst:ret_iter astate in
      step ~has_token step_ path location event iter astate
    in
    astate


  let operator_compare comparison ~desc iter_lhs iter_rhs : model_no_non_disj =
   fun ({path; location} as model_data) astate ->
    let<*> astate, (elem_lhs, _) = load_field path location iter_lhs node_field astate in
    let<*> astate, (elem_rhs, _) = load_field path location iter_rhs node_field astate in
    Collection.Iterator.compare_values comparison ~desc elem_lhs elem_rhs model_data astate
end

let begin_or_find this iter container ~desc : model_no_non_disj =
 fun {path; location} astate ->
  let event = Hist.call_event path location desc in
  let<++> astate =
    let=* astate, (size, _) = Collection.to_internal_size_deref path Read location this astate in
    let** astate, elem =
      if PulseArithmetic.is_known_zero astate size then eval_end path location this astate
      else Sat (fresh_element path location this astate)
    in
    Sat (make_iterator ~has_token:(has_token container) path location event this iter elem astate)
  in
  astate


let end_ this iter container ~desc : model_no_non_disj =
 fun {path; location} astate ->
  let event = Hist.call_event path location desc in
  let<++> astate =
    let** astate, end_value = eval_end path location this astate in
    Sat
      (make_iterator ~has_token:(has_token container) path location event this iter end_value astate)
  in
  astate


let erase this pos ret_iter container ~desc : model_no_non_disj =
 fun {path; location} astate ->
  let event = Hist.call_event path location desc in
  let has_token = has_token container in
  let<++> astate =
    let=* astate, elem, _ = check_iterator ~has_token ~allow_end:false path location pos astate in
    let astate = invalidate_element path location (StdContainer (container, Erase)) elem astate in
    let+* astate = Collection.decrease_size path location this ~desc astate in
    let* astate, next = fresh_element path location this astate in
    make_iterator ~has_token path location event this ret_iter next astate
  in
  astate


let clear this container ~desc : model_no_non_disj =
 fun {path; location} astate ->
  let cause = Invalidation.StdContainer (container, Clear) in
  let<++> astate =
    let=* astate, arr = Collection.eval path NoAccess location this astate in
    let astate =
      Memory.fold_edges (fst arr) astate ~init:astate ~f:(fun astate (access, elem) ->
          match (access : Access.t) with
          (* an element equal to [end()] is the past-the-end value, which [end()] keeps returning *)
          | ArrayAccess _ when not (AddressAttributes.is_end_of_collection (fst elem) astate) ->
              invalidate_element path location cause elem astate
          | _ ->
              astate )
      |> PulseOperations.invalidate_deref_access path location cause this Collection.access
    in
    let=* astate =
      PulseOperations.havoc_deref_field path location this Collection.field
        (Hist.single_call path location desc)
        astate
    in
    Collection.assign_size_constant path location this ~constant:IntLit.zero ~desc astate
  in
  astate


(** [count()] and [contains()] return 0 when the container is known to be empty *)
let count ~desc this : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let ret_val = AbstractValue.mk_fresh () in
  let<++> astate =
    let=* astate, (size, _) = Collection.to_internal_size_deref path Read location this astate in
    let astate =
      PulseOperations.write_id ret_id (ret_val, Hist.single_call path location desc) astate
    in
    if PulseArithmetic.is_known_zero astate size then
      PulseArithmetic.and_eq_int ret_val IntLit.zero astate
    else PulseArithmetic.and_nonnegative ret_val astate
  in
  astate


(* [at()], [front()], and [back()] succeed only on non-empty containers; this is assumed rather
   than pruned, since a pruned size coming from the caller would make all the caller's later issues
   latent *)
let assume_non_empty path location this astate =
  let=* astate, (size, _) = Collection.to_internal_size_deref path Read location this astate in
  PulseArithmetic.and_positive size astate


(** [front()] and [back()] *)
let element_reference ~desc this : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let event = Hist.call_event path location desc in
  let<++> astate =
    let** astate = assume_non_empty path location this astate in
    let=+ astate, (elem, hist) = fresh_element path location this astate in
    Ok (PulseOperations.write_id ret_id (elem, Hist.add_event event hist) astate)
  in
  astate


(** [operator[]] and [at()] of maps return a reference to the mapped value of an element;
    [operator[]] inserts an element if needed *)
let mapped_value_reference ~inserts ~desc this : model_no_non_disj =
 fun {path; location; callee_procname; ret= ret_id, _} astate ->
  let event = Hist.call_event path location desc in
  let<++> astate =
    let=* astate, elem = fresh_element path location this astate in
    let=* astate, (value, hist) =
      match Procname.get_class_type_name callee_procname with
      | Some (CppClass {template_spec_info= Template {args= TType key_t :: TType value_t :: _}}) ->
          PulseOperations.eval_access path Read location elem
            (GenericMapCollection.pair_second_access key_t value_t)
            astate
      | _ ->
          Ok (astate, elem)
    in
    let astate =
      PulseOperations.write_id ret_id (value, Hist.add_event event hist) astate
      |> Decompiler.add_call_source value (Call callee_procname) [(this, StdTyp.void)]
    in
    if inserts then set_size_positive path location event this astate
    else assume_non_empty path location this astate
  in
  astate


(* the user-facing iterator classes of the modelled libc++ containers *)
let iterators =
  [ "__deque_iterator"
  ; "__hash_const_iterator"
  ; "__hash_map_const_iterator"
  ; "__hash_map_iterator"
  ; "__list_const_iterator"
  ; "__list_iterator"
  ; "__map_const_iterator"
  ; "__map_iterator"
  ; "__tree_const_iterator" ]


let containers : (string * Invalidation.std_container) list =
  [ ("deque", Deque)
  ; ("list", List)
  ; ("map", Map)
  ; ("multimap", Multimap)
  ; ("multiset", Multiset)
  ; ("set", Set)
  ; ("unordered_map", UnorderedMap)
  ; ("unordered_multimap", UnorderedMultimap)
  ; ("unordered_multiset", UnorderedMultiset)
  ; ("unordered_set", UnorderedSet) ]


(* like the name matchers of [ProcnameDispatcher], ignore the template arguments that the frontend
   appends to some names *)
let strip_template_args name = String.lsplit2 name ~on:'<' |> Option.value_map ~f:fst ~default:name

let is_iterator name = List.mem iterators (strip_template_args name) ~equal:String.equal

let container_of_name name =
  List.Assoc.find containers ~equal:String.equal (strip_template_args name)


let class_name (name : Typ.Name.t) =
  match name with
  | CppClass {name} ->
      QualifiedCppName.extract_last name
      |> Option.map ~f:(fun (last, _) -> strip_template_args last)
  | _ ->
      None


let callee_class_name callee_procname =
  Procname.get_class_type_name callee_procname |> Option.bind ~f:class_name


(** the iterator returned by a call whose last argument is [ret], directly or as the [first] field
    of a [std::pair] *)
let returned_iterator path location event (ret : _ FuncArg.t option) astate =
  let is_iterator_typ (typ : Typ.t) =
    match typ.desc with
    | Tstruct name ->
        Option.exists (class_name name) ~f:is_iterator
    | _ ->
        false
  in
  match ret with
  | Some {arg_payload= ret; typ= {desc= Tptr (typ, _)}} when is_iterator_typ typ ->
      Some (Ok (astate, ret))
  | Some
      { arg_payload= ret
      ; typ=
          { desc=
              Tptr
                ( { desc=
                      Tstruct
                        (CppClass {template_spec_info= Template {args= TType iter_typ :: _}} as pair)
                  }
                , _ ) } }
    when Option.exists (class_name pair) ~f:(String.equal "pair") && is_iterator_typ iter_typ ->
      Some
        (let* astate =
           PulseOperations.write_deref_field path location ~ref:ret (Fieldname.make pair "second")
             ~obj:(AbstractValue.mk_fresh (), Hist.single_event event)
             astate
         in
         PulseOperations.eval_access path Read location ret
           (FieldAccess (Fieldname.make pair "first"))
           astate )
  | _ ->
      None


(** insertions into a [std::deque] invalidate all its iterators; references to its elements are not
    invalidated, which is only right for insertions at either end *)
let deque_insertion deque_f ~ret ~desc this args : model_no_non_disj =
 fun {path; location; ret= ret_id, _} astate ->
  let event = Hist.call_event path location desc in
  let<++> astate =
    let astate =
      PulseOperations.invalidate_deref_access path location
        (StdContainer (Deque, deque_f))
        this (FieldAccess token_field) astate
    in
    let=* astate =
      PulseOperations.havoc_deref_field path location this token_field (Hist.single_event event)
        astate
    in
    let** astate = set_size_positive path location event this astate in
    match ret with
    | `Reference ->
        let=+ astate, (elem, hist) = fresh_element path location this astate in
        Ok (PulseOperations.write_id ret_id (elem, Hist.add_event event hist) astate)
    | `Iterator -> (
      match returned_iterator path location event (List.last args) astate with
      | Some result ->
          let=* astate, ret_iter = result in
          let=* astate, elem = fresh_element path location this astate in
          Sat (make_iterator ~has_token:true path location event this ret_iter elem astate)
      | None ->
          Sat (Ok astate) )
    | `Nothing ->
        Sat (Ok astate)
  in
  astate


(** insertions into a node-based container invalidate nothing; one that returns an iterator inserts
    an element, or finds an equal one *)
let node_insertion ~desc this args : model_no_non_disj =
 fun {path; location} astate ->
  let event = Hist.call_event path location desc in
  let<++> astate =
    match returned_iterator path location event (List.last args) astate with
    | Some result ->
        let=* astate, ret_iter = result in
        let=* astate, elem = fresh_element path location this astate in
        let=* astate =
          make_iterator ~has_token:false path location event this ret_iter elem astate
        in
        set_size_positive path location event this astate
    | None ->
        let=* astate =
          PulseOperations.havoc_deref_field path location this Collection.size_field
            (Hist.single_event event) astate
        in
        Sat (Ok astate)
  in
  astate


let callee_desc callee_procname class_name =
  desc class_name (strip_template_args (Procname.get_method callee_procname))


let on_iterator model : model_no_non_disj =
 fun ({callee_procname} as model_data) astate ->
  let iterator = callee_class_name callee_procname |> Option.value ~default:"" in
  model
    ~has_token:(String.equal iterator "__deque_iterator")
    ~desc:(callee_desc callee_procname iterator)
    model_data astate


let on_container model : model_no_non_disj =
 fun ({callee_procname} as model_data) astate ->
  let class_name = callee_class_name callee_procname |> Option.value ~default:"" in
  match container_of_name class_name with
  | Some container ->
      model container ~desc:(callee_desc callee_procname class_name) model_data astate
  | None ->
      Logging.die InternalError "%a is not a method of a modelled container" Procname.pp
        callee_procname


let matchers : matcher list =
  let open ProcnameDispatcher.Call in
  let iterator_typs () = List.map iterators ~f:(fun iterator -> -"std" &:: iterator) in
  let is_iterator _ name = is_iterator name in
  let is_container ?(f = fun _ -> true) _ name = Option.exists (container_of_name name) ~f in
  let iterator_method name = -"std" &::+ is_iterator &:: name in
  let iterator_matchers =
    [ ( -"std" &::+ is_iterator &::+ is_iterator $ capt_arg_payload
      $+ capt_arg_payload_of_typ_exists (iterator_typs ())
      $--> fun this other -> on_iterator (Iterator.constructor this other) )
    ; ( iterator_method "operator=" <>$ capt_arg_payload $+ capt_arg_payload
      $--> fun this other -> on_iterator (Iterator.operator_assign this other) )
    ; ( iterator_method "operator*" <>$ capt_arg_payload
      $--> fun iter -> on_iterator (Iterator.operator_star iter) )
    ; ( iterator_method "operator->" <>$ capt_arg_payload
      $--> fun iter -> on_iterator (Iterator.operator_star iter) )
    ; ( iterator_method "operator++" <>$ capt_arg_payload
      $--> fun iter -> on_iterator (Iterator.operator_step `PlusPlus iter) )
    ; ( iterator_method "operator++" <>$ capt_arg_payload $+ any_arg $+ capt_arg_payload
      $--> fun iter ret_iter -> on_iterator (Iterator.operator_step_postfix `PlusPlus iter ret_iter)
      )
    ; ( iterator_method "operator--" <>$ capt_arg_payload
      $--> fun iter -> on_iterator (Iterator.operator_step `MinusMinus iter) )
    ; ( iterator_method "operator--" <>$ capt_arg_payload $+ any_arg $+ capt_arg_payload
      $--> fun iter ret_iter ->
      on_iterator (Iterator.operator_step_postfix `MinusMinus iter ret_iter) ) ]
  in
  let comparison_matchers =
    [ -"std" &:: "operator=="
      $ capt_arg_payload_of_typ_exists (iterator_typs ())
      $+ capt_arg_payload_of_typ_exists (iterator_typs ())
      $--> Iterator.operator_compare `Equal ~desc:"iterator operator=="
    ; -"std" &:: "operator!="
      $ capt_arg_payload_of_typ_exists (iterator_typs ())
      $+ capt_arg_payload_of_typ_exists (iterator_typs ())
      $--> Iterator.operator_compare `NotEqual ~desc:"iterator operator!=" ]
  in
  let container_method ?f name = -"std" &::+ is_container ?f &:: name in
  let returns_iterator name model =
    container_method name <>$ capt_arg_payload
    $+ capt_arg_payload_of_typ_exists (iterator_typs ())
    $--> fun this iter -> on_container (model this iter)
  in
  let is_node_based : Invalidation.std_container -> bool = function Deque -> false | _ -> true in
  let container_matchers =
    [ ( -"std" &::+ is_container &::+ is_container <>$ capt_arg_payload
      $--> fun this -> on_container (fun _ ~desc -> Collection.default_constructor ~desc this) )
    ; returns_iterator "begin" begin_or_find
    ; returns_iterator "cbegin" begin_or_find
    ; returns_iterator "end" end_
    ; returns_iterator "cend" end_
    ; ( container_method "erase" <>$ capt_arg_payload
      $+ capt_arg_payload_of_typ_exists (iterator_typs ())
      $+ capt_arg_payload
      $--> fun this pos ret_iter -> on_container (erase this pos ret_iter) )
    ; ( container_method ~f:is_node_based "insert"
      $ capt_arg_payload
      $++$--> fun this args -> on_container (fun _ ~desc -> node_insertion ~desc this args) )
    ; (container_method "clear" <>$ capt_arg_payload $--> fun this -> on_container (clear this))
    ; ( container_method "empty" <>$ capt_arg_payload
      $--> fun this -> on_container (fun _ ~desc -> Collection.empty ~desc this) )
    ; ( container_method "size" <>$ capt_arg_payload
      $--> fun this -> on_container (fun _ ~desc -> Collection.size ~desc this) ) ]
  in
  let is_associative : Invalidation.std_container -> bool = function
    | Deque | List ->
        false
    | Map
    | Multimap
    | Multiset
    | Set
    | UnorderedMap
    | UnorderedMultimap
    | UnorderedMultiset
    | UnorderedSet ->
        true
  in
  let lookup_matchers =
    [ ( container_method ~f:is_associative "find"
      $ capt_arg_payload $+ any_arg
      $+ capt_arg_payload_of_typ_exists (iterator_typs ())
      $--> fun this iter -> on_container (begin_or_find this iter) )
    ; ( container_method ~f:is_associative "count"
      $ capt_arg_payload $+ any_arg
      $--> fun this -> on_container (fun _ ~desc -> count ~desc this) )
    ; ( container_method ~f:is_associative "contains"
      $ capt_arg_payload $+ any_arg
      $--> fun this -> on_container (fun _ ~desc -> count ~desc this) ) ]
  in
  let is_sequence container = not (is_associative container) in
  let front_back_matchers =
    [ ( container_method ~f:is_sequence "front"
      <>$ capt_arg_payload
      $--> fun this -> on_container (fun _ ~desc -> element_reference ~desc this) )
    ; ( container_method ~f:is_sequence "back"
      <>$ capt_arg_payload
      $--> fun this -> on_container (fun _ ~desc -> element_reference ~desc this) ) ]
  in
  let is_map : Invalidation.std_container -> bool = function
    | Map | UnorderedMap ->
        true
    | _ ->
        false
  in
  let map_matchers =
    [ ( container_method ~f:is_map "operator[]"
      <>$ capt_arg_payload $+ any_arg
      $--> fun this -> on_container (fun _ ~desc -> mapped_value_reference ~inserts:true ~desc this)
      )
    ; ( container_method ~f:is_map "at" <>$ capt_arg_payload $+ any_arg
      $--> fun this ->
      on_container (fun _ ~desc -> mapped_value_reference ~inserts:false ~desc this) ) ]
  in
  let deque_desc = desc "deque" in
  let deque_matchers =
    [ -"std" &:: "deque" &:: "operator[]" <>$ capt_arg_payload $+ capt_arg_payload
      $--> PulseModelsCpp.Vector.at ~desc:(deque_desc "operator[]")
    ; -"std" &:: "deque" &:: "at" <>$ capt_arg_payload $+ capt_arg_payload
      $--> PulseModelsCpp.Vector.at ~desc:(deque_desc "at")
    ; -"std" &:: "deque" &:: "push_back" <>$ capt_arg_payload
      $++$--> deque_insertion PushBack ~ret:`Nothing ~desc:(deque_desc "push_back")
    ; -"std" &:: "deque" &:: "push_front" <>$ capt_arg_payload
      $++$--> deque_insertion PushFront ~ret:`Nothing ~desc:(deque_desc "push_front")
    ; -"std" &:: "deque" &:: "emplace_back" $ capt_arg_payload
      $++$--> deque_insertion EmplaceBack ~ret:`Reference ~desc:(deque_desc "emplace_back")
    ; -"std" &:: "deque" &:: "emplace_front" $ capt_arg_payload
      $++$--> deque_insertion EmplaceFront ~ret:`Reference ~desc:(deque_desc "emplace_front")
    ; -"std" &:: "deque" &:: "insert" $ capt_arg_payload
      $++$--> deque_insertion Insert ~ret:`Iterator ~desc:(deque_desc "insert")
    ; -"std" &:: "deque" &:: "emplace" $ capt_arg_payload
      $++$--> deque_insertion Emplace ~ret:`Iterator ~desc:(deque_desc "emplace") ]
  in
  iterator_matchers @ comparison_matchers @ container_matchers @ lookup_matchers
  @ front_back_matchers @ map_matchers @ deque_matchers
  |> List.map ~f:(ProcnameDispatcher.Call.contramap_arg_payload ~f:ValueOrigin.addr_hist)
  |> List.map ~f:(ProcnameDispatcher.Call.map_matcher ~f:lift_model)
