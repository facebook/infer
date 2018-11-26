(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

(** Wrappers for making pretty-printable modules *)

module type PrintableType = sig
  type t

  val pp : F.formatter -> t -> unit
end

module type PrintableEquatableType = sig
  include PrintableType

  val equal : t -> t -> bool
end

module type PrintableOrderedType = sig
  include Caml.Set.OrderedType

  include PrintableType with type t := t
end

module type PPSet = sig
  include Caml.Set.S

  val is_singleton_or_more : t -> elt IContainer.singleton_or_more

  include PrintableType with type t := t

  val pp_element : F.formatter -> elt -> unit
end

module type MonoMap = sig
  type key

  type value

  type t

  val empty : t

  val is_empty : t -> bool

  val mem : key -> t -> bool

  val add : key -> value -> t -> t

  val update : key -> (value option -> value option) -> t -> t

  val singleton : key -> value -> t

  val remove : key -> t -> t

  val merge : (key -> value option -> value option -> value option) -> t -> t -> t

  val union : (key -> value -> value -> value option) -> t -> t -> t

  val compare : (value -> value -> int) -> t -> t -> int

  val equal : (value -> value -> bool) -> t -> t -> bool

  val iter : (key -> value -> unit) -> t -> unit

  val fold : (key -> value -> 'a -> 'a) -> t -> 'a -> 'a

  val for_all : (key -> value -> bool) -> t -> bool

  val exists : (key -> value -> bool) -> t -> bool

  val filter : (key -> value -> bool) -> t -> t

  val partition : (key -> value -> bool) -> t -> t * t

  val cardinal : t -> int

  val bindings : t -> (key * value) list

  val min_binding : t -> key * value

  val min_binding_opt : t -> (key * value) option

  val max_binding : t -> key * value

  val max_binding_opt : t -> (key * value) option

  val choose : t -> key * value

  val choose_opt : t -> (key * value) option

  val split : key -> t -> t * value option * t

  val find : key -> t -> value

  val find_opt : key -> t -> value option

  val find_first : (key -> bool) -> t -> key * value

  val find_first_opt : (key -> bool) -> t -> (key * value) option

  val find_last : (key -> bool) -> t -> key * value

  val find_last_opt : (key -> bool) -> t -> (key * value) option

  val map : (value -> value) -> t -> t

  val mapi : (key -> value -> value) -> t -> t

  val is_singleton_or_more : t -> (key * value) IContainer.singleton_or_more
end

module type PPMap = sig
  include Caml.Map.S

  val is_singleton_or_more : 'a t -> (key * 'a) IContainer.singleton_or_more

  val pp_key : F.formatter -> key -> unit

  val pp : pp_value:(F.formatter -> 'a -> unit) -> F.formatter -> 'a t -> unit
end

let pp_collection ~pp_item fmt c = IContainer.pp_collection ~fold:List.fold ~pp_item fmt c

module MakePPSet (Ord : PrintableOrderedType) = struct
  include Caml.Set.Make (Ord)

  let is_singleton_or_more s =
    if is_empty s then IContainer.Empty
    else
      let mi = min_elt s in
      let ma = max_elt s in
      if phys_equal mi ma then IContainer.Singleton mi else IContainer.More


  let pp_element = Ord.pp

  let pp fmt s = pp_collection ~pp_item:pp_element fmt (elements s)
end

module MakePPMap (Ord : PrintableOrderedType) = struct
  include Caml.Map.Make (Ord)

  let is_singleton_or_more m =
    if is_empty m then IContainer.Empty
    else
      let ((kmi, _) as binding) = min_binding m in
      let kma, _ = max_binding m in
      if phys_equal kmi kma then IContainer.Singleton binding else IContainer.More


  let pp_key = Ord.pp

  let pp ~pp_value fmt m =
    let pp_item fmt (k, v) = F.fprintf fmt "%a -> %a" Ord.pp k pp_value v in
    pp_collection ~pp_item fmt (bindings m)
end

module type PPMonoMap = sig
  include MonoMap

  include PrintableType with type t := t

  val pp_key : F.formatter -> key -> unit
end

module MakePPMonoMap (Ord : PrintableOrderedType) (Val : PrintableType) = struct
  module M = Caml.Map.Make (Ord)

  include (M : module type of M with type 'a t := 'a M.t)

  type t = Val.t M.t

  type value = Val.t

  let pp_key = Ord.pp

  let pp fmt m =
    let pp_item fmt (k, v) = F.fprintf fmt "%a -> %a" Ord.pp k Val.pp v in
    pp_collection ~pp_item fmt (bindings m)


  let is_singleton_or_more m =
    if is_empty m then IContainer.Empty
    else
      let ((kmi, _) as binding) = min_binding m in
      let kma, _ = max_binding m in
      if phys_equal kmi kma then IContainer.Singleton binding else IContainer.More
end
