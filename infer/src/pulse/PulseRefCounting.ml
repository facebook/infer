(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

let is_ref_counted v astate =
  AddressAttributes.get_static_type v astate
  |> Option.exists ~f:(fun typ_name -> Typ.Name.is_objc_class typ_name)
  ||
  match PulseArithmetic.get_dynamic_type v astate with
  | Some {typ= {desc= Tstruct typ_name}} ->
      Typ.Name.is_objc_class typ_name
  | _ ->
      false


type access_type = Strong | Weak | Unknown [@@deriving compare, equal]

let pp_access_type fmt access_type =
  let s = match access_type with Strong -> "Strong" | Weak -> "Weak" | Unknown -> "Unknown" in
  String.pp fmt s


let get_access_type _tenv (access : Access.t) : access_type =
  match access with
  | FieldAccess fieldname -> (
    match Fieldname.is_weak fieldname with
    | Some is_weak ->
        if is_weak then Weak else Strong
    | None ->
        Unknown )
  | _ ->
      Strong


let is_strong_access tenv access = equal_access_type (get_access_type tenv access) Strong

(* Starting from each variable in the stack, count all unique strong
   references to each RefCounted object. If a RefCounted object lives in the
   stack but has no strong reference, then it considered as strongly
   referenced by the stack *)
let count_references tenv astate =
  let rec count_references_from addr seen ref_counts =
    if AbstractValue.Set.mem addr seen then (seen, ref_counts)
    else
      let ref_counts =
        if is_ref_counted addr astate then
          AbstractValue.Map.update addr (function None -> Some 0 | some -> some) ref_counts
        else ref_counts
      in
      let seen = AbstractValue.Set.add addr seen in
      Memory.fold_edges addr astate ~init:(seen, ref_counts)
        ~f:(fun (seen, ref_counts) (access, (accessed_addr, _)) ->
          if is_strong_access tenv access then
            let ref_counts =
              if is_ref_counted accessed_addr astate then
                AbstractValue.Map.update accessed_addr
                  (function None -> Some 1 | Some n -> Some (n + 1))
                  ref_counts
              else ref_counts
            in
            count_references_from accessed_addr seen ref_counts
          else (seen, ref_counts) )
  in
  Stack.fold
    (fun _var (addr, _) (seen, ref_counts) -> count_references_from addr seen ref_counts)
    astate
    (AbstractValue.Set.empty, AbstractValue.Map.empty)
  |> snd
  (* a refcount of 0 indicates a value that was not deallocated when seen in an ExitScope
     because e.g. it was part of a retain cycle at that time but was released from that
     cycle later. Changing 0 to 1 will make it available for future deallocation *)
  |> AbstractValue.Map.map (function 0 -> 1 | n -> n)


let is_released tenv astate addr non_retaining_addrs =
  let is_retaining addr = not (List.mem non_retaining_addrs addr ~equal:AbstractValue.equal) in
  let rec is_retained_by src_addr seen =
    if List.mem seen src_addr ~equal:AbstractValue.equal then false
    else
      Memory.exists_edge src_addr astate ~f:(fun (access, (accessed_addr, _)) ->
          is_strong_access tenv access
          && ( AbstractValue.equal accessed_addr addr
             || is_retained_by accessed_addr (src_addr :: seen) ) )
  in
  Stack.exists
    (fun _var (src_addr, _) ->
      (is_retaining src_addr || AbstractValue.equal src_addr addr) && is_retained_by src_addr [] )
    astate
  |> not


(* Variables found in ExitScope are to be removed from the stack by default.
   However there is one case where we do not want to remove them: if they are
   RefCounted and still retained by anyone other than a variable to be removed.
   This is for 2 reasons:
   - avoid calling a wrongful dealloc on a variable locked in a cycle, which
     would havoc the cycle
   - make the variable available for a final dealloc if it is freed from the
   cycle after its ExitScope.

   Never removing a RefCounted by default if it is retained by anyone could lead
   to FP memory leaks as in codetoanalyze/objc/pulse/memory_leaks/DeallocCalls.m,
   hence the non retaining nature of addresses corresponding to variables to be
   removed. *)
let removable_vars tenv astate vars =
  let non_retaining_addrs =
    List.filter_map vars ~f:(fun var -> Stack.find_opt var astate |> Option.map ~f:fst)
  in
  let is_removable addr =
    (not (is_ref_counted addr astate)) || is_released tenv astate addr non_retaining_addrs
  in
  List.filter vars ~f:(fun var ->
      Stack.find_opt var astate |> Option.for_all ~f:(fun (addr, _) -> is_removable addr) )
