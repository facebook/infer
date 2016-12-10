(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open! IStd

module F = Format

(** Wrappers for making pretty-printable modules *)

module type SetOrderedType = sig
  include Caml.Set.OrderedType

  val pp_element : F.formatter -> t -> unit
end

module type MapOrderedType = sig
  include Caml.Map.OrderedType

  val pp_key : F.formatter -> t -> unit
end

module type PPSet = sig
  include Caml.Set.S

  val pp_element : F.formatter -> elt -> unit
  val pp : F.formatter -> t -> unit
end

module type PPMap = sig
  include Caml.Map.S

  val pp_key : F.formatter -> key -> unit
  val pp : pp_value:(F.formatter -> 'a -> unit) -> F.formatter -> 'a t -> unit
end

let pp_collection ~pp_item fmt c =
  let pp_collection fmt c =
    let pp_sep fmt () = F.fprintf fmt ", " in
    F.pp_print_list ~pp_sep pp_item fmt c in
  F.fprintf fmt "{ %a }" pp_collection c

module MakePPSet (Ord : SetOrderedType) = struct
  include Caml.Set.Make(Ord)

  let pp_element = Ord.pp_element

  let pp fmt s =
    pp_collection ~pp_item:pp_element fmt (elements s)
end

module MakePPCompareSet
    (Ord : sig include SetOrderedType val compare_pp : t -> t -> int end) = struct
  include Caml.Set.Make(Ord)

  let pp_element = Ord.pp_element

  let pp fmt s =
    let elements_alpha = IList.sort Ord.compare_pp (elements s) in
    pp_collection ~pp_item:pp_element fmt elements_alpha
end

module MakePPMap (Ord : MapOrderedType) = struct
  include Caml.Map.Make(Ord)

  let pp_key = Ord.pp_key

  let pp ~pp_value fmt m =
    let pp_item fmt (k, v) = F.fprintf fmt "%a -> %a" Ord.pp_key k pp_value v in
    pp_collection ~pp_item fmt (bindings m)
end
