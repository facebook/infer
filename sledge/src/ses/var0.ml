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

  (** Variable renaming substitutions *)
  module Subst = struct
    type t = T.t Map.t [@@deriving compare, equal, sexp_of]
    type x = {sub: t; dom: Set.t; rng: Set.t}

    let t_of_sexp = Map.t_of_sexp t_of_sexp
    let pp = Map.pp pp pp

    let invariant s =
      let@ () = Invariant.invariant [%here] s [%sexp_of: t] in
      let domain, range =
        Map.fold s ~init:(Set.empty, Set.empty)
          ~f:(fun ~key ~data (domain, range) ->
            (* substs are injective *)
            assert (not (Set.mem range data)) ;
            (Set.add domain key, Set.add range data) )
      in
      assert (Set.disjoint domain range)

    let empty = Map.empty
    let is_empty = Map.is_empty

    let freshen vs ~wrt =
      let dom = Set.inter wrt vs in
      ( if Set.is_empty dom then
        ({sub= empty; dom= Set.empty; rng= Set.empty}, wrt)
      else
        let wrt = Set.union wrt vs in
        let sub, rng, wrt =
          Set.fold dom ~init:(empty, Set.empty, wrt)
            ~f:(fun (sub, rng, wrt) x ->
              let x', wrt = fresh (name x) ~wrt in
              let sub = Map.add_exn sub ~key:x ~data:x' in
              let rng = Set.add rng x' in
              (sub, rng, wrt) )
        in
        ({sub; dom; rng}, wrt) )
      |> check (fun ({sub; _}, _) -> invariant sub)

    let fold sub ~init ~f =
      Map.fold sub ~init ~f:(fun ~key ~data s -> f key data s)

    let domain sub =
      Map.fold sub ~init:Set.empty ~f:(fun ~key ~data:_ domain ->
          Set.add domain key )

    let range sub =
      Map.fold sub ~init:Set.empty ~f:(fun ~key:_ ~data range ->
          Set.add range data )

    let invert sub =
      Map.fold sub ~init:empty ~f:(fun ~key ~data sub' ->
          Map.add_exn sub' ~key:data ~data:key )
      |> check invariant

    let restrict sub vs =
      Map.fold sub ~init:{sub; dom= Set.empty; rng= Set.empty}
        ~f:(fun ~key ~data z ->
          if Set.mem vs key then
            {z with dom= Set.add z.dom key; rng= Set.add z.rng data}
          else (
            assert (
              (* all substs are injective, so the current mapping is the
                 only one that can cause [data] to be in [rng] *)
              (not (Set.mem (range (Map.remove sub key)) data))
              || violates invariant sub ) ;
            {z with sub= Map.remove z.sub key} ) )
      |> check (fun {sub; dom; rng} ->
             assert (Set.equal dom (domain sub)) ;
             assert (Set.equal rng (range sub)) )

    let apply sub v = Map.find sub v |> Option.value ~default:v
  end
end
