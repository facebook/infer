(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging

type signedness = Signed | Unsigned [@@deriving compare, sexp, hash, equal, normalize]

let join_signedness signedness1 signedness2 =
  match (signedness1, signedness2) with Signed, Signed -> Signed | _ -> Unsigned


type pointerness = NotPointer | Pointer [@@deriving sexp, hash, equal, normalize]

let join_pointerness pointerness1 pointerness2 =
  match (pointerness1, pointerness2) with NotPointer, NotPointer -> NotPointer | _ -> Pointer


let compare_pointerness _ _ = 0

module Z = struct
  include Z

  let sexp_of_t t = [%sexp_of: string] (Z.to_string t)

  let t_of_sexp sexp = Z.of_string ([%of_sexp: string] sexp)

  let hash_fold_t hash_state t = [%hash_fold: int] hash_state (Z.hash t)

  include HashNormalizer.Make (struct
    type nonrec t = t [@@deriving equal, hash]

    let normalize = Fn.id
  end)
end

(** signed and unsigned integer literals *)
type t = {signedness: signedness; i: Z.t; pointerness: pointerness}
[@@deriving compare, sexp, hash, equal, normalize]

let yojson_of_t {i} = [%yojson_of: string] (Z.to_string i)

exception OversizedShift

let area {signedness; i} =
  match (Z.(lt i zero), signedness) with
  | true, Signed ->
      (* negative signed *) 1
  | false, _ ->
      (* non-negative *) 2
  | true, Unsigned ->
      (* negative unsigned *) 3


let to_signed intlit =
  match intlit with
  | {signedness= Signed} ->
      Some intlit
  | {signedness= Unsigned; i} ->
      if Z.(lt i zero) then None else Some {intlit with signedness= Signed}


let compare_value intlit1 intlit2 =
  [%compare: int * Z.t] (area intlit1, intlit1.i) (area intlit2, intlit2.i)


let eq i1 i2 = Int.equal (compare_value i1 i2) 0

let neq i1 i2 = compare_value i1 i2 <> 0

let leq i1 i2 = compare_value i1 i2 <= 0

let lt i1 i2 = compare_value i1 i2 < 0

let geq i1 i2 = compare_value i1 i2 >= 0

let gt i1 i2 = compare_value i1 i2 > 0

let of_z z_of_int i = {signedness= Signed; i= z_of_int i; pointerness= NotPointer}

let of_int64 = of_z Z.of_int64

let of_int32 = of_z Z.of_int32

let of_int = of_z Z.of_int

let of_string = of_z Z.of_string

let of_big_int = of_z Fn.id

let z_to_int_opt i = try Some (Z.to_int i) with Z.Overflow -> None

let to_int {i} = z_to_int_opt i

let to_int_exn {i} = Z.to_int i

let to_big_int {i} = i

let to_float {i} = Z.to_float i

let null = {signedness= Signed; i= Z.zero; pointerness= Pointer}

let zero = of_int 0

let one = of_int 1

let two = of_int 2

let minus_one = of_int (-1)

let isone {i} = Z.(equal i one)

let iszero {i} = Z.(equal i zero)

let isnull = function {pointerness= Pointer; i} when Z.(equal i zero) -> true | _ -> false

let isminusone = function {signedness= Signed; i} when Z.(equal i minus_one) -> true | _ -> false

let isnegative = function {signedness= Signed; i} when Z.(lt i zero) -> true | _ -> false

let lift2 binop intlit1 intlit2 =
  { signedness= join_signedness intlit1.signedness intlit2.signedness
  ; i= binop intlit1.i intlit2.i
  ; pointerness= join_pointerness intlit1.pointerness intlit2.pointerness }


let lift1 unop intlit = {intlit with i= unop intlit.i}

let neg i = lift1 Z.neg i

let add i1 i2 = lift2 Z.( + ) i1 i2

let mul i1 i2 = lift2 Z.( * ) i1 i2

let div i1 i2 = lift2 Z.( / ) i1 i2

let rem i1 i2 = lift2 Z.rem i1 i2

let logand i1 i2 = lift2 Z.logand i1 i2

let logor i1 i2 = lift2 Z.logor i1 i2

let logxor i1 i2 = lift2 Z.logxor i1 i2

let lognot i = lift1 Z.lognot i

let sub i1 i2 = lift2 Z.( - ) i1 i2

let shift_left intlit1 {i= i2} =
  match z_to_int_opt i2 with
  | None ->
      L.(die InternalError) "Shifting failed with operand %a" Z.pp_print i2
  | Some i2 ->
      if i2 < 0 || i2 >= 64 then raise OversizedShift ;
      lift1 (fun i -> Z.shift_left i i2) intlit1


let shift_right intlit1 {i= i2} =
  match z_to_int_opt i2 with
  | None ->
      L.(die InternalError) "Shifting failed with operand %a" Z.pp_print i2
  | Some i2 ->
      if i2 < 0 || i2 >= 64 then raise OversizedShift ;
      lift1 (fun i -> Z.shift_right i i2) intlit1


let max i1 i2 = if geq i1 i2 then i1 else i2

let min i1 i2 = if leq i1 i2 then i1 else i2

let pp f intlit = if isnull intlit then F.pp_print_string f "null" else Z.pp_print f intlit.i

let to_string i = F.asprintf "%a" pp i

let equal i1 i2 = eq i1 i2
