(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Renaming Substitutions: injective maps from variables to variables *)

include Subst_intf

module Make (Var : VAR) = struct
  module Set = Var.Set
  module Map = Var.Map

  type t = Var.t Map.t [@@deriving compare, equal, sexp_of]
  type x = {sub: t; dom: Set.t; rng: Set.t}

  let t_of_sexp = Map.t_of_sexp Var.t_of_sexp
  let pp = Map.pp Var.pp Var.pp

  let invariant s =
    let@ () = Invariant.invariant [%here] s [%sexp_of: t] in
    let domain, range =
      Map.fold s (Set.empty, Set.empty)
        ~f:(fun ~key ~data (domain, range) ->
          (* substs are injective *)
          assert (not (Set.mem data range)) ;
          (Set.add key domain, Set.add data range) )
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
        Set.fold dom (empty, Set.empty, wrt) ~f:(fun x (sub, rng, wrt) ->
            let x', wrt = Var.freshen x ~wrt in
            let sub = Map.add_exn ~key:x ~data:x' sub in
            let rng = Set.add x' rng in
            (sub, rng, wrt) )
      in
      ({sub; dom; rng}, wrt) )
    |> check (fun ({sub; _}, _) -> invariant sub)

  let fold sub z ~f = Map.fold ~f:(fun ~key ~data -> f key data) sub z
  let domain sub = Set.of_iter (Map.keys sub)
  let range sub = Set.of_iter (Map.values sub)

  let invert sub =
    Map.fold sub empty ~f:(fun ~key ~data sub' ->
        Map.add_exn ~key:data ~data:key sub' )
    |> check invariant

  let restrict_dom sub0 vs =
    Map.fold sub0 {sub= sub0; dom= Set.empty; rng= Set.empty}
      ~f:(fun ~key ~data z ->
        let rng = Set.add data z.rng in
        if Set.mem key vs then {z with dom= Set.add key z.dom; rng}
        else (
          assert (
            (* all substs are injective, so the current mapping is the only
               one that can cause [data] to be in [rng] *)
            (not (Set.mem data (range (Map.remove key sub0))))
            || violates invariant sub0 ) ;
          {z with sub= Map.remove key z.sub; rng} ) )
    |> check (fun {sub; dom; rng} ->
           assert (Set.equal dom (domain sub)) ;
           assert (Set.equal rng (range sub0)) )

  let apply sub v = Map.find v sub |> Option.value ~default:v
end
[@@inline]
