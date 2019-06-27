(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

exception MaximumSharingCyclicValue

exception MaximumSharingLazyValue

module Hashing : sig
  type hash_value = private int

  type state

  val of_int : int -> hash_value

  val shallow : 'a -> hash_value
  (** A deterministic hash function that visits O(1) objects, to ensure termination on cyclic values. *)

  val alloc_of_block : tag:int -> size:int -> state

  val fold_hash_value : state -> hash_value -> state

  val get_hash_value : state -> hash_value
end = struct
  type hash_value = Hash.hash_value

  type state = Hash.state

  let of_int = Fn.id

  let shallow =
    (*
      [hash x] is defined as [seeded_hash_param 10 100 0 x].
      Here we don't care about the specific numbers as long as the function is deterministic.
    *)
    Caml.Hashtbl.hash


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
  let hashed_obj eq =
    ( module struct
      type t = Hashing.hash_value * Obj.t

      let hash ((h, _) : t) = (h :> int)

      let equal ((h1, o1) : t) ((h2, o2) : t) = Int.equal (h1 :> int) (h2 :> int) && eq o1 o2
    end
    : Caml.Hashtbl.HashedType
      with type t = Hashing.hash_value * Obj.t )


  module HashedNoscanBlock = (val hashed_obj Polymorphic_compare.equal)

  module PhysEqualedHashedScannableBlock = (val hashed_obj phys_equal)

  module HashedNormalizedScannableBlock = (val hashed_obj PhysEqual.shallow_equal)

  module HNoscan = Caml.Hashtbl.Make (HashedNoscanBlock)
  module HPhysEq = Caml.Hashtbl.Make (PhysEqualedHashedScannableBlock)
  module HNorm = Caml.Hashtbl.Make (HashedNormalizedScannableBlock)

  type visited = Visiting | Normalized of HashedNormalizedScannableBlock.t

  type t =
    { inplace: bool
          (** Uses [Obj.set_field] on possibly immutable values, hence should be avoided when used with flambda *)
    ; noscan_blocks: HashedNoscanBlock.t HNoscan.t
    ; visited_blocks: visited HPhysEq.t
    ; hash_normalized: HashedNormalizedScannableBlock.t HNorm.t
    ; fail_on_forward: bool
    ; fail_on_nonstring: bool
    ; fail_on_objects: bool }

  let create () =
    { inplace= false
    ; noscan_blocks= HNoscan.create 1
    ; visited_blocks= HPhysEq.create 1
    ; hash_normalized= HNorm.create 1
    ; (* these are just for safety because the code hasn't been tested on them, it should work fine though *)
      fail_on_forward= true
    ; fail_on_nonstring= true
    ; fail_on_objects= true }


  let hash_and_normalize_int o = (Hashing.of_int (Obj.obj o : int), o)

  (* 
    TODO: be much more efficient and write it in C to be able to use the GC flags to
    mark visited values.
   *)
  let rec hash_and_normalize_obj sharer o =
    if Obj.is_int o then hash_and_normalize_int o else hash_and_normalize_block sharer o


  and hash_and_normalize_block sharer block =
    let shallow_hash = Hashing.shallow block in
    let shallow_hash_block = (shallow_hash, block) in
    let tag = Obj.tag block in
    if tag >= Obj.no_scan_tag then (
      (*
        No-scan blocks (strings, int64, closures, weird stuff) are treated separately.
        They are hashed and compared using the Stdlib polymorphic functions.
      *)
      assert ((not sharer.fail_on_nonstring) || Int.equal tag Obj.string_tag) ;
      match HNoscan.find_opt sharer.noscan_blocks shallow_hash_block with
      | Some hash_normalized ->
          hash_normalized
      | None ->
          HNoscan.add sharer.noscan_blocks shallow_hash_block shallow_hash_block ;
          shallow_hash_block )
    else if Int.equal tag Obj.lazy_tag then
      (*
        For now MaximumSharing is used before marshalling.
        It makes little sense to marshal lazy values. 
        Because lazy blocks are normal scannable blocks, this special case could be safely removed.
      *)
      raise MaximumSharingLazyValue
    else
      (*
        This is where we could win by mutating the value directly.
        Instead we need to use a hashtbl using a shallow hash (for termination), which adds a
        multiplicative factor to the running time.
      *)
      match HPhysEq.find_opt sharer.visited_blocks shallow_hash_block with
      | Some (Normalized hash_normalized) ->
          (* The block has already been visited, we can reuse the result. *)
          hash_normalized
      | Some Visiting ->
          (*
            The block is being visited, which means we have a cycle.
          *)
          raise MaximumSharingCyclicValue
      | None ->
          HPhysEq.add sharer.visited_blocks shallow_hash_block Visiting ;
          let hash_normalized =
            if Int.equal tag Obj.forward_tag then (
              assert (not sharer.fail_on_forward) ;
              (*
                Forward_tag is an intermediate block resulting from the evaluating of a lazy.
                As an optimization, let's replace it directly with the normalization of the result
                as if this intermediate block didn't exist.

                This remains untested for now (hence the assertion above).
                Not obvious to test as optimizations or the GC can already do the substitution.
              *)
              hash_and_normalize_obj sharer (Obj.field block 0) )
            else (
              (* For regular blocks, normalize each field then use a shallow comparison. *)
              assert ((not sharer.fail_on_objects) || not (Int.equal tag Obj.object_tag)) ;
              let hash_shallow_normalized =
                let size = Obj.size block in
                hash_and_normalize_block_fields sharer block block size 0
                  (Hashing.alloc_of_block ~tag ~size)
              in
              match HNorm.find_opt sharer.hash_normalized hash_shallow_normalized with
              | Some hash_normalized ->
                  hash_normalized
              | None ->
                  HNorm.add sharer.hash_normalized hash_shallow_normalized hash_shallow_normalized ;
                  hash_shallow_normalized )
          in
          HPhysEq.replace sharer.visited_blocks shallow_hash_block (Normalized hash_normalized) ;
          hash_normalized


  and hash_and_normalize_block_fields sharer original_block new_block size field_i hash_state =
    if field_i >= size then (Hashing.get_hash_value hash_state, new_block)
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
