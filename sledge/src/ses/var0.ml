(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Var_intf

(** Variables, parameterized over their representation *)
module Make (T : REPR) = struct
  include T

  type strength = t -> [`Universal | `Existential | `Anonymous] option

  let ppx strength ppf v =
    let id = id v in
    let name = name v in
    match id with
    | -1 -> Trace.pp_styled `Bold "%@%s" ppf name
    | 0 -> Trace.pp_styled `Bold "%%%s" ppf name
    | _ -> (
      match strength v with
      | None -> Format.fprintf ppf "%%%s_%d" name id
      | Some `Universal -> Trace.pp_styled `Bold "%%%s_%d" ppf name id
      | Some `Existential -> Trace.pp_styled `Cyan "%%%s_%d" ppf name id
      | Some `Anonymous -> Trace.pp_styled `Cyan "_" ppf )

  let pp = ppx (fun _ -> None)

  module Map = struct
    include NS.Map.Make (T)
    include Provide_of_sexp (T)
  end

  module Set = struct
    let pp_t = pp

    include NS.Set.Make (T)
    include Provide_of_sexp (T)

    let ppx strength vs = pp (ppx strength) vs
    let pp vs = pp pp_t vs

    let pp_xs fs xs =
      if not (is_empty xs) then
        Format.fprintf fs "@<2>∃ @[%a@] .@;<1 2>" pp xs
  end

  let fresh name ~wrt =
    let max = match Set.max_elt wrt with None -> 0 | Some max -> id max in
    let x' = make ~id:(max + 1) ~name in
    (x', Set.add wrt x')

  let program ~name ~global = make ~id:(if global then -1 else 0) ~name
  let identified ~name ~id = make ~id ~name
end
