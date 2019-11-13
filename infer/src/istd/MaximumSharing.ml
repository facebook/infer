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
    end : Caml.Hashtbl.HashedType
      with type t = Hashing.hash_value * Obj.t )


  module HashedNoscanBlock = (val hashed_obj Poly.equal)

  module PhysEqualedHashedScannableBlock = (val hashed_obj phys_equal)

  module HashedNormalizedScannableBlock = (val hashed_obj PhysEqual.shallow_equal)

  module HNoscan = Caml.Hashtbl.Make (HashedNoscanBlock)
  module HPhysEq = Caml.Hashtbl.Make (PhysEqualedHashedScannableBlock)
  module HNorm = Caml.Hashtbl.Make (HashedNormalizedScannableBlock)

  type visited =
    | Visiting of {mutable to_patch: (PhysEqualedHashedScannableBlock.t * int) list}
    | Normalized of HashedNormalizedScannableBlock.t

  type t =
    { noscan_blocks: HashedNoscanBlock.t HNoscan.t
    ; visited_blocks: visited HPhysEq.t
    ; hash_normalized: HashedNormalizedScannableBlock.t HNorm.t
    ; fail_on_forward: bool
    ; fail_on_nonstring: bool
    ; fail_on_objects: bool }

  let create () =
    { noscan_blocks= HNoscan.create 1
    ; visited_blocks= HPhysEq.create 1
    ; hash_normalized= HNorm.create 1
    ; (* these are just for safety because the code hasn't been tested on them, it should work fine though *)
      fail_on_forward= true
    ; fail_on_nonstring= true
    ; fail_on_objects= true }


  let hash_and_normalize_int o = (Hashing.of_int (Obj.obj o : int), o)

  let dummy_should_not_be_hashed_or_used =
    (*
      Must be different than any block found in values.
      Must fail if hashed (there is actually no way to ensure that :( ))
    *)
    Obj.repr (lazy (assert false))


  (*
    TODO: be much more efficient and write it in C to be able to use the GC flags to
    mark visited values.
  *)
  let rec hash_and_normalize_obj sharer o parent_shallow_hash_block parent_field_i =
    if Obj.is_int o then hash_and_normalize_int o
    else hash_and_normalize_block sharer o parent_shallow_hash_block parent_field_i


  and hash_and_normalize_block sharer block parent_shallow_hash_block parent_field_i =
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
      | Some (Visiting visiting) ->
          (*
            The block is being visited, which means we have a cycle.
            We record fields to be patched after we have finished treating the cycle.

            For termination we have to return a shallow hash.
            We also need to return a phys_equally different value so that it will trigger
            copy-on-write on the whole cycle (then patch can safely be applied and in any order),
            even though it may not be necessary if the whole cycle and its dependencies could be
            kept as-is.
            The value that is returned should not be hashed or used.  The current implementation
            respects it.
          *)
          visiting.to_patch <- (parent_shallow_hash_block, parent_field_i) :: visiting.to_patch ;
          (shallow_hash, dummy_should_not_be_hashed_or_used)
      | None ->
          let visited = Visiting {to_patch= []} in
          let[@warning "-8"] (Visiting visiting) = visited in
          HPhysEq.add sharer.visited_blocks shallow_hash_block visited ;
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
              hash_and_normalize_obj sharer (Obj.field block 0) parent_shallow_hash_block
                parent_field_i )
            else (
              (* For regular blocks, normalize each field then use a shallow comparison. *)
              assert ((not sharer.fail_on_objects) || not (Int.equal tag Obj.object_tag)) ;
              let hash_shallow_normalized =
                let size = Obj.size block in
                hash_and_normalize_block_fields sharer shallow_hash_block block block size 0
                  (Hashing.alloc_of_block ~tag ~size)
              in
              match HNorm.find_opt sharer.hash_normalized hash_shallow_normalized with
              | Some hash_normalized ->
                  hash_normalized
              | None ->
                  HNorm.add sharer.hash_normalized hash_shallow_normalized hash_shallow_normalized ;
                  hash_shallow_normalized )
          in
          let hash_normalized =
            match visiting.to_patch with
            | [] (* not the head of a cycle *) ->
                hash_normalized
            | _ :: _ as to_patch ->
                (*
                  The whole cycle has been treated, we now need to patch values that pointed to
                  this block.  We need to look them up in the [visited_blocks] hash table because
                  they have been duplicated since we recorded them.
                *)
                let _, normalized = hash_normalized in
                List.iter to_patch ~f:(fun (hash_block_to_patch, field_i_to_patch) ->
                    let normalized_block_to_patch =
                      if phys_equal hash_block_to_patch shallow_hash_block then
                        (* Self-cycle, e.g. [let rec x = 1 :: x]. No lookup! *)
                        normalized
                      else
                        let[@warning "-8"] (Normalized (_, normalized_block_to_patch)) =
                          HPhysEq.find sharer.visited_blocks hash_block_to_patch
                        in
                        normalized_block_to_patch
                    in
                    Obj.set_field normalized_block_to_patch field_i_to_patch normalized ) ;
                (*
                  For cycle heads, for consistency with the [Visiting] case above we need to
                  use the shallow hash.
                *)
                (shallow_hash, normalized)
          in
          HPhysEq.replace sharer.visited_blocks shallow_hash_block (Normalized hash_normalized) ;
          hash_normalized


  and hash_and_normalize_block_fields sharer original_shallow_hash_block original_block new_block
      size field_i hash_state =
    if field_i >= size then (Hashing.get_hash_value hash_state, new_block)
    else
      let field_v = Obj.field original_block field_i in
      let field_hash, field_v' =
        hash_and_normalize_obj sharer field_v original_shallow_hash_block field_i
      in
      let hash_state = Hashing.fold_hash_value hash_state field_hash in
      let new_block =
        if phys_equal field_v field_v' then new_block
        else
          let new_block =
            if phys_equal original_block new_block then (* copy-on-write *)
              Obj.dup original_block
            else new_block
          in
          Obj.set_field new_block field_i field_v' ;
          new_block
      in
      (hash_and_normalize_block_fields [@tailcall]) sharer original_shallow_hash_block
        original_block new_block size (field_i + 1) hash_state


  let dummy_should_not_be_patched =
    (Hashing.of_int 0, (* Make sure it fails hard if [Obj.set_field] is called on it *) Obj.repr 0)


  (**
    Returns a value structurally equal but with potentially more sharing.
    Potentially unsafe if used on mutable values that are modified later.
    Preserves polymorphic compare, hashing, no-sharing marshalling.
    May have an impact on code using [phys_equal] or marshalling with sharing.
  *)
  let normalize_value sharer v =
    hash_and_normalize_obj sharer (Obj.repr v) dummy_should_not_be_patched (-1) |> snd |> Obj.obj
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
