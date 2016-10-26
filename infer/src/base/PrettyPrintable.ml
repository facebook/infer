(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module F = Format

(** Wrappers for making pretty-printable modules *)

module type SetOrderedType = sig
  include Set.OrderedType

  val pp_element : F.formatter -> t -> unit
end

module type MapOrderedType = sig
  include Map.OrderedType

  val pp_key : F.formatter -> t -> unit
end

module type PPSet = sig
  include Set.S

  val pp_element : F.formatter -> elt -> unit
  val pp : F.formatter -> t -> unit
end

module type PPMap = sig
  include Map.S

  val pp_key : F.formatter -> key -> unit
  val pp : pp_value:(F.formatter -> 'a -> unit) -> F.formatter -> 'a t -> unit
end

let pp_collection ~pp_item fmt c =
  let pp_collection fmt c =
    let pp_sep fmt () = F.fprintf fmt ", " in
    F.pp_print_list ~pp_sep pp_item fmt c in
  F.fprintf fmt "{ %a }" pp_collection c

module MakePPSet (Ord : SetOrderedType) = struct
  include Set.Make(Ord)

  let pp_element = Ord.pp_element

  let pp fmt s =
    let pp_item fmt e = F.fprintf fmt "%a" Ord.pp_element e in
    pp_collection ~pp_item fmt (elements s)
end

module MakePPMap (Ord : MapOrderedType) = struct
  include Map.Make(Ord)

  let pp_key = Ord.pp_key

  let pp ~pp_value fmt m =
    let pp_item fmt (k, v) = F.fprintf fmt "%a -> %a" Ord.pp_key k pp_value v in
    pp_collection ~pp_item fmt (bindings m)
end
