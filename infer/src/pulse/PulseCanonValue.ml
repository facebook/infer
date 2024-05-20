(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
open PulseBasicInterface

module type S = sig
  type astate

  type t = private AbstractValue.t [@@deriving compare, equal]

  val pp : F.formatter -> t -> unit

  module Set : PrettyPrintable.PPSet with type elt = t

  val downcast_set : Set.t -> AbstractValue.Set.t [@@inline always]

  val unsafe_cast_set : AbstractValue.Set.t -> Set.t [@@deprecated ""] [@@inline always]

  type needs_canon

  val canon : astate -> needs_canon -> t

  val canon' : astate -> AbstractValue.t -> t

  val canon_fst : astate -> needs_canon * 'a -> t * 'a

  val canon_fst' : astate -> AbstractValue.t * 'a -> t * 'a

  val canon_snd_fst : astate -> 'a * (needs_canon * 'b) -> 'a * (t * 'b)

  val canon_opt : astate -> needs_canon option -> t option

  val canon_opt' : astate -> AbstractValue.t option -> t option

  val canon_opt_fst : astate -> (needs_canon * 'a) option -> (t * 'a) option

  val canon_opt_fst4' :
    astate -> (AbstractValue.t * 'a * 'b * 'c) option -> (t * 'a * 'b * 'c) option

  val mk_fresh : unit -> t

  val unsafe_cast : AbstractValue.t -> t [@@deprecated ""] [@@inline always]

  module Stack : sig
    include
      PulseBaseStack.S with type value = needs_canon * ValueHistory.t and type t = PulseBaseStack.t

    val add : Var.t -> AbstractValue.t * ValueHistory.t -> t -> t
  end

  module Memory :
    PulseBaseMemory.S
      with type key := t
       and type in_map_t := AbstractValue.t * ValueHistory.t
       and type out_of_map_t := needs_canon * ValueHistory.t
       and type t = PulseBaseMemory.t
       and type Edges.t = PulseBaseMemory.Edges.t

  val canon_access : astate -> PulseAccess.t -> Memory.Access.t

  module Attributes :
    PulseBaseAddressAttributes.S with type key := t and type t = PulseBaseAddressAttributes.t
end

module Make (AbductiveDomain : sig
  type astate

  val canon : astate -> AbstractValue.t -> AbstractValue.t
end) : S with type astate = AbductiveDomain.astate = struct
  type astate = AbductiveDomain.astate

  type t = AbstractValue.t [@@deriving compare, equal]

  let pp = AbstractValue.pp

  module Set = AbstractValue.Set

  let downcast_set = Fn.id

  let unsafe_cast_set = Fn.id

  type needs_canon = AbstractValue.t

  let mk_fresh = AbstractValue.mk_fresh

  module Stack = PulseBaseStack
  module Memory = PulseBaseMemory
  module Attributes = PulseBaseAddressAttributes

  let canon = AbductiveDomain.canon

  let canon' = canon

  let unsafe_cast = Fn.id

  let canon_fst astate pair =
    let v, snd = pair in
    let v' = canon astate v in
    if AbstractValue.equal v v' then pair else (v', snd)


  let canon_fst' = canon_fst

  let canon_snd_fst astate pair_pair =
    let fst, (v, snd_snd) = pair_pair in
    let v' = canon astate v in
    if AbstractValue.equal v v' then pair_pair else (fst, (v', snd_snd))


  let canon_opt astate opt =
    match opt with
    | None ->
        opt
    | Some v ->
        let v' = canon astate v in
        if AbstractValue.equal v v' then opt else Some v'


  let canon_opt' = canon_opt

  let canon_opt_fst astate pair_opt =
    match pair_opt with
    | None ->
        pair_opt
    | Some (v, snd) ->
        let v' = canon astate v in
        if AbstractValue.equal v v' then pair_opt else Some (v', snd)


  let canon_opt_fst4' astate tuple_opt =
    match tuple_opt with
    | None ->
        tuple_opt
    | Some (v, snd, trd, frt) ->
        let v' = canon' astate v in
        if AbstractValue.equal v v' then tuple_opt else Some (v', snd, trd, frt)


  let canon_access astate (access : PulseAccess.t) : Memory.Access.t =
    match access with
    | ArrayAccess (typ, index) ->
        ArrayAccess (typ, canon astate index)
    (* ocaml insists we copy the other definitions for typing purposes *)
    | FieldAccess f ->
        FieldAccess f
    | Dereference ->
        Dereference
end
