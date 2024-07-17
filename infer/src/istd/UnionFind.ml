(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! Core
module F = Format

module type Element = sig
  type t [@@deriving compare, equal]

  val is_simpler_than : t -> t -> bool
end

module Make
    (X : Element)
    (XSet : Caml.Set.S with type elt = X.t)
    (XMap : Caml.Map.S with type key = X.t) =
struct
  module XSet = Iter.Set.Adapt (XSet)

  (** the union-find backing data structure: maps elements to their representatives *)
  module UF : sig
    (** to get a little bit of type safety *)
    type repr = private X.t

    type t [@@deriving compare, equal]

    val empty : t

    val is_empty : t -> bool

    val find : t -> X.t -> repr

    val merge : t -> repr -> into:repr -> t

    val add_disjoint_class : repr -> XSet.t -> t -> t
    (** [add_disjoint_class repr xs uf] adds a class [{repr} U xs] with representative [repr] to
        [uf], assuming the [repr] is correct and the class does not intersect with any existing
        elements of [uf] *)

    module Map : Caml.Map.S with type key = repr
  end = struct
    type repr = X.t

    type t = X.t XMap.t [@@deriving compare, equal]

    let empty = XMap.empty

    let is_empty = XMap.is_empty

    let find_opt reprs x =
      let rec find_opt_aux candidate_repr =
        (* [x] is in the relation and now we are climbing up to the final representative *)
        match XMap.find_opt candidate_repr reprs with
        | None ->
            (* [candidate_repr] is the representative *)
            candidate_repr
        | Some candidate_repr' ->
            (* keep climbing *)
            find_opt_aux candidate_repr'
      in
      XMap.find_opt x reprs |> Option.map ~f:find_opt_aux


    let find reprs x = find_opt reprs x |> Option.value ~default:x

    let merge reprs x ~into:y = (* TODO: implement path compression *) XMap.add x y reprs

    let add_disjoint_class repr xs reprs = XSet.fold (fun x reprs -> XMap.add x repr reprs) xs reprs

    module Map = XMap
  end

  type repr = UF.repr

  module Classes = struct
    let find classes (x : UF.repr) = UF.Map.find_opt x classes |> Option.value ~default:XSet.empty

    let merge classes (x1 : UF.repr) ~into:(x2 : UF.repr) =
      let class1 = find classes x1 in
      let class2 = find classes x2 in
      let new_class = XSet.union class1 class2 |> XSet.add (x1 :> X.t) in
      UF.Map.remove x1 classes |> UF.Map.add x2 new_class
  end

  type t = {reprs: UF.t; classes: XSet.t UF.Map.t} [@@deriving compare, equal]

  let empty = {reprs= UF.empty; classes= UF.Map.empty}

  let is_empty uf = UF.is_empty uf.reprs && UF.Map.is_empty uf.classes

  let find uf x = UF.find uf.reprs x

  let union uf x1 x2 =
    let repr1 = find uf x1 in
    let repr2 = find uf x2 in
    if X.equal (repr1 :> X.t) (repr2 :> X.t) then
      (* avoid creating loops in the relation *)
      (uf, None)
    else
      let from, into =
        if X.is_simpler_than (repr1 :> X.t) (repr2 :> X.t) then (repr2, repr1) else (repr1, repr2)
      in
      let reprs = UF.merge uf.reprs from ~into in
      let classes = Classes.merge uf.classes from ~into in
      ({reprs; classes}, Some ((from :> X.t), into))


  let fold_congruences {classes} ~init ~f =
    UF.Map.fold (fun repr xs acc -> f acc (repr, xs)) classes init


  let pp pp_item fmt uf =
    let pp_ts_or_repr repr fmt ts =
      if XSet.is_empty ts then pp_item fmt repr
      else
        Pp.collection ~sep:"="
          ~fold:(IContainer.fold_of_pervasives_set_fold XSet.fold)
          pp_item fmt ts
    in
    let pp_aux fmt uf =
      Pp.collection ~sep:" ∧ " ~fold:fold_congruences
        (fun fmt ((repr : repr), ts) ->
          F.fprintf fmt "%a=%a" pp_item (repr :> X.t) (pp_ts_or_repr (repr :> X.t)) ts )
        fmt uf
    in
    F.fprintf fmt "@[<hv>%a@]" pp_aux uf


  let of_classes classes =
    let reprs =
      UF.Map.fold (fun repr xs reprs -> UF.add_disjoint_class repr xs reprs) classes UF.empty
    in
    {reprs; classes}


  let apply_subst subst uf =
    let in_subst x = XMap.mem x subst in
    (* any variable that doesn't have a better representative according to the substitution should
       be kept *)
    let should_keep x = not (in_subst x) in
    let classes_keep =
      fold_congruences uf ~init:UF.Map.empty ~f:(fun classes_keep (repr, clazz) ->
          let repr_in_range_opt =
            if should_keep (repr :> X.t) then Some repr
            else
              (* [repr] is not a good representative for the class as the substitution prefers it
                 another element. Try to find a better representative. *)
              XSet.to_seq clazz |> Iter.find_pred should_keep
              |> (* HACK: trick [Repr] into casting [repr'] to a [repr], bypassing the private type
                    *)
              Option.map ~f:(fun x_repr -> UF.find UF.empty x_repr)
          in
          match repr_in_range_opt with
          | Some repr_in_range ->
              let class_keep =
                XSet.filter
                  (fun x -> (not (X.equal x (repr_in_range :> X.t))) && should_keep x)
                  clazz
              in
              if XSet.is_empty class_keep then classes_keep
              else UF.Map.add repr_in_range class_keep classes_keep
          | None ->
              (* none of the elements in the class should be kept; note that this cannot happen if
                 [subst = reorient ~keep uf] *)
              classes_keep )
    in
    of_classes classes_keep


  let reorient ~should_keep uf =
    fold_congruences uf ~init:XMap.empty ~f:(fun subst (repr, clazz) ->
        (* map every variable in [repr::clazz] to either [repr] if [should_keep repr], or to the
           smallest representative of [clazz] that satisfies [should_keep], if any *)
        if should_keep (repr :> X.t) then
          XSet.fold
            (fun x subst -> if should_keep x then subst else XMap.add x (repr :> X.t) subst)
            clazz subst
        else
          match XSet.to_seq clazz |> Iter.find_pred should_keep with
          | None ->
              (* no good representative: just substitute as in the original [uf] relation so that we
                 can get rid of non-representative variables *)
              XSet.fold (fun x subst -> XMap.add x (repr :> X.t) subst) clazz subst
          | Some repr' ->
              (* map all that should not be kept (including old repr) to [repr'], which
                 should be kept *)
              let subst = XMap.add (repr :> X.t) repr' subst in
              XSet.fold
                (fun x subst -> if should_keep x then subst else XMap.add x repr' subst)
                clazz subst )


  let filter ~f uf =
    let classes =
      UF.Map.filter_map
        (fun repr clazz ->
          let clazz = XSet.filter f clazz in
          if XSet.is_empty clazz && not (f (repr :> X.t)) then None else Some clazz )
        uf.classes
    in
    (* rebuild [reprs] directly from [classes]: does path compression and garbage collection on the
       old [reprs] *)
    of_classes classes


  let fold_elements uf ~init ~f =
    fold_congruences uf ~init ~f:(fun acc (repr, xs) ->
        XSet.fold (Fn.flip f) xs (f acc (repr :> X.t)) )
end
