(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

exception MaximumSharingLazyValue

module Hashing : sig
  type hash_value = private int

  type state

  val of_int : int -> hash_value

  val of_no_scan_block : 'a -> hash_value

  val alloc_of_block : tag:int -> size:int -> state

  val fold_hash_value : state -> hash_value -> state

  val get_hash_value : state -> hash_value
end = struct
  type hash_value = Hash.hash_value

  type state = Hash.state

  let of_int = Fn.id

  let of_no_scan_block = Caml.Hashtbl.hash

  let alloc_of_block ~tag ~size =
    let state = Hash.alloc () in
    let state = Hash.fold_int state tag in
    let state = Hash.fold_int state size in
    state


  let fold_hash_value = Hash.fold_int

  let get_hash_value = Hash.get_hash_value
end

module Sharer : sig
  type t

  val create : unit -> t

  val normalize_value : t -> 'a -> 'a
end = struct
  module HashedNormalizedObj = struct
    type t = Hashing.hash_value * Obj.t

    let equal ((h1, o1) : t) ((h2, o2) : t) =
      Int.equal (h1 :> int) (h2 :> int)
      &&
      if Obj.tag o1 >= Obj.no_scan_tag then Polymorphic_compare.equal o1 o2
      else PhysEqual.shallow_equal o1 o2


    let hash ((h, _) : t) = (h :> int)
  end

  module H = Caml.Hashtbl.Make (HashedNormalizedObj)

  type t =
    { inplace: bool
          (** Uses [Obj.set_field] on possibly immutable values, hence should be avoided when used with flambda *)
    ; hash_normalized: HashedNormalizedObj.t H.t
    ; fail_on_forward: bool
    ; fail_on_nonstring: bool
    ; fail_on_objects: bool }

  let create () =
    { inplace= false
    ; hash_normalized= H.create 1
    ; (* these are just for safety because the code hasn't been tested on them, it should work fine though *)
      fail_on_forward= true
    ; fail_on_nonstring= true
    ; fail_on_objects= true }


  let normalize_block sharer hash block =
    let hash_block = (hash, block) in
    match H.find_opt sharer.hash_normalized hash_block with
    | Some hash_normalized ->
        hash_normalized
    | None ->
        H.add sharer.hash_normalized hash_block hash_block ;
        hash_block


  let hash_and_normalize_int o = (Hashing.of_int (Obj.obj o : int), o)

  (* 
  TODO: currently the function explores the graph without sharing.  It is possible to build values
  on which this will behave exponentially.  This could be avoided by keeping a hashtbl of visited
  values.  But it would be much more efficient to write it in C to be able to use the GC flags to
  mark visited values.
   *)
  let rec hash_and_normalize_obj sharer o =
    if Obj.is_int o then hash_and_normalize_int o else hash_and_normalize_block sharer o


  and hash_and_normalize_block sharer block =
    let tag = Obj.tag block in
    if tag >= Obj.no_scan_tag then (
      assert ((not sharer.fail_on_nonstring) || Int.equal tag Obj.string_tag) ;
      hash_and_normalize_no_scan_block sharer block )
    else if Int.equal tag Obj.forward_tag then (
      assert (not sharer.fail_on_forward) ;
      hash_and_normalize_obj sharer (Obj.field block 0) )
    else if Int.equal tag Obj.lazy_tag then raise MaximumSharingLazyValue
    else (
      assert ((not sharer.fail_on_objects) || not (Int.equal tag Obj.object_tag)) ;
      let size = Obj.size block in
      hash_and_normalize_block_fields sharer block block size 0 (Hashing.alloc_of_block ~tag ~size) )


  and hash_and_normalize_block_fields sharer original_block new_block size field_i hash_state =
    if field_i >= size then normalize_block sharer (Hashing.get_hash_value hash_state) new_block
    else
      let field_v = Obj.field original_block field_i in
      let field_hash, field_v' = hash_and_normalize_obj sharer field_v in
      let hash_state = Hashing.fold_hash_value hash_state field_hash in
      let new_block =
        if phys_equal field_v field_v' then new_block
        else
          let new_block =
            if phys_equal original_block new_block && not sharer.inplace then
              (* copy-on-write *)
              Obj.dup original_block
            else new_block
          in
          Obj.set_field new_block field_i field_v' ;
          new_block
      in
      (hash_and_normalize_block_fields [@tailcall]) sharer original_block new_block size
        (field_i + 1) hash_state


  and hash_and_normalize_no_scan_block sharer block =
    normalize_block sharer (Hashing.of_no_scan_block block) block


  (**
    Returns a value structurally equal but with potentially more sharing.
    Potentially unsafe if used on mutable values that are modified later.
    Preserves polymorphic compare, hashing, no-sharing marshalling.
    May have an impact on code using [phys_equal] or marshalling with sharing.
  *)
  let normalize_value sharer v = hash_and_normalize_obj sharer (Obj.repr v) |> snd |> Obj.obj
end

module ForHashtbl (H : Caml.Hashtbl.S) = struct
  let normalize h =
    let sharer = Sharer.create () in
    (* If a hash table has been created with [add] and not [replace] only, it is possible to
    have several values for a given key.  We need to collect them all and reinsert them in
    the reverse order. *)
    let rev_bindings = H.fold (fun k v acc -> (k, v) :: acc) h [] in
    (* No need to preserve the initial size of the original hash table *)
    let h' = H.create (H.length h) in
    List.iter rev_bindings ~f:(fun (k, v) ->
        let k' = Sharer.normalize_value sharer k in
        let v' = Sharer.normalize_value sharer v in
        H.add h' k' v' ) ;
    h'
end
