(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

type t = int [@@deriving compare]

let equal = [%compare.equal: t]

let initial_next_fresh = 1

let next_fresh = ref initial_next_fresh

let mk_fresh () =
  let l = !next_fresh in
  incr next_fresh ;
  l


let pp f l = F.fprintf f "v%d" l

let yojson_of_t l = `String (F.asprintf "%a" pp l)

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
  type t = int * Constants.t

  let get () = (!next_fresh, !Constants.cache)

  let set (counter, cache) =
    next_fresh := counter ;
    Constants.cache := cache


  let reset () =
    next_fresh := initial_next_fresh ;
    Constants.cache := Constants.initial_cache
end
