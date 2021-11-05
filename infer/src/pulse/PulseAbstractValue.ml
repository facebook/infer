(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

type t = int [@@deriving compare, equal]

let initial_next_fresh = 1

let next_fresh = ref initial_next_fresh

let mk_fresh () =
  let l = !next_fresh in
  incr next_fresh ;
  l


let initial_next_fresh_restricted = -1

let next_fresh_restricted = ref initial_next_fresh_restricted

let mk_fresh_restricted () =
  let v = !next_fresh_restricted in
  decr next_fresh_restricted ;
  v


let is_restricted v = v < 0

let is_unrestricted v = v > 0

let pp f v = if is_restricted v then F.fprintf f "a%d" (-v) else F.fprintf f "v%d" v

let yojson_of_t l = `String (F.asprintf "%a" pp l)

let compare_unrestricted_first v1 v2 =
  if is_restricted v1 then
    if is_restricted v2 then (* compare absolute values *) compare v2 v1
    else (* unrestricted [v2] first *) 1
  else if is_restricted v2 then (* unrestricted [v1] first *) -1
  else compare v1 v2


let of_id v = v

module PPKey = struct
  type nonrec t = t [@@deriving compare]

  let pp = pp
end

module Set = PrettyPrintable.MakePPSet (PPKey)

module Map = struct
  include PrettyPrintable.MakePPMap (PPKey)

  let yojson_of_t yojson_of_val m =
    `List (List.map ~f:(fun (k, v) -> `List [yojson_of_t k; yojson_of_val v]) (bindings m))
end

module Constants = struct
  module M = Caml.Map.Make (IntLit)

  type nonrec t = t M.t

  let initial_cache = M.empty

  let cache = ref initial_cache

  let get_int i =
    match M.find_opt i !cache with
    | Some v ->
        v
    | None ->
        let v = mk_fresh () in
        cache := M.add i v !cache ;
        v
end

module State = struct
  type t = int * int * Constants.t

  let get () = (!next_fresh, !next_fresh_restricted, !Constants.cache)

  let set (counter_unrestricted, counter_restricted, cache) =
    next_fresh := counter_unrestricted ;
    next_fresh_restricted := counter_restricted ;
    Constants.cache := cache


  let reset () =
    next_fresh := initial_next_fresh ;
    next_fresh_restricted := initial_next_fresh_restricted ;
    Constants.cache := Constants.initial_cache
end
