(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format

type t = int [@@deriving compare, equal, hash]

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

let mk_fresh_same_kind v = if is_restricted v then mk_fresh_restricted () else mk_fresh ()

let pp f v = if is_restricted v then F.fprintf f "a%d" (-v) else F.fprintf f "v%d" v

let yojson_of_t l = `String (F.asprintf "%a" pp l)

let compare_unrestricted_first v1 v2 =
  if is_restricted v1 then
    if is_restricted v2 then (* compare absolute values *) compare v2 v1
    else (* unrestricted [v2] first *) 1
  else if is_restricted v2 then (* unrestricted [v1] first *) -1
  else compare v1 v2


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

let () =
  AnalysisGlobalState.register_ref next_fresh ~init:(fun () -> initial_next_fresh) ;
  AnalysisGlobalState.register_ref next_fresh_restricted ~init:(fun () ->
      initial_next_fresh_restricted ) ;
  ()
