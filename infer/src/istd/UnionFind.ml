(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module type Element = sig
  type t [@@deriving compare]

  val is_simpler_than : t -> t -> bool
end

module Make (X : Element) (XSet : Caml.Set.S with type elt = X.t) = struct
  module Map = Caml.Map.Make (X)

  let equal_x = [%compare.equal: X.t]

  (** the union-find backing data structure: maps elements to their representatives *)
  module UF : sig
    (** to get a little bit of type safety *)
    type repr = private X.t

    type t

    val empty : t

    val is_empty : t -> bool

    val find_opt : t -> X.t -> repr option

    val find : t -> X.t -> repr

    val merge : t -> repr -> into:repr -> t

    val add_disjoint_class : repr -> XSet.t -> t -> t
    (** [add_disjoint_class repr xs uf] adds a class [{repr} U xs] with representative [repr] to
        [uf], assuming the [repr] is correct and the class does not intersect with any existing
        elements of [uf] *)

    module Map : Caml.Map.S with type key = repr
  end = struct
    type repr = X.t

    type t = X.t Map.t

    let empty = Map.empty

    let is_empty = Map.is_empty

    let find_opt reprs x =
      let rec find_opt_aux candidate_repr =
        (* [x] is in the relation and now we are climbing up to the final representative *)
        match Map.find_opt candidate_repr reprs with
        | None ->
            (* [candidate_repr] is the representative *)
            candidate_repr
        | Some candidate_repr' ->
            (* keep climbing *)
            find_opt_aux candidate_repr'
      in
      Map.find_opt x reprs |> Option.map ~f:find_opt_aux


    let find reprs x = find_opt reprs x |> Option.value ~default:x

    let merge reprs x ~into:y = (* TODO: implement path compression *) Map.add x y reprs

    let add_disjoint_class repr xs reprs = XSet.fold (fun x reprs -> Map.add x repr reprs) xs reprs

    module Map = Map
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

  type t = {reprs: UF.t; classes: XSet.t UF.Map.t}

  let empty = {reprs= UF.empty; classes= UF.Map.empty}

<<<<<<< HEAD
  let is_empty {reprs; classes} = UF.is_empty reprs && UF.Map.is_empty classes

  let find_opt uf x = UF.find_opt uf.reprs x

=======
>>>>>>> upstream/master
  let find uf x = UF.find uf.reprs x

  let union uf x1 x2 =
    let repr1 = find uf x1 in
    let repr2 = find uf x2 in
    if equal_x (repr1 :> X.t) (repr2 :> X.t) then
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


  let pp ~pp_empty pp_item fmt uf =
    let pp_ts_or_repr repr fmt ts =
      if XSet.is_empty ts then pp_item fmt repr
      else
        Pp.collection ~sep:"="
          ~fold:(IContainer.fold_of_pervasives_set_fold XSet.fold)
          ~pp_item fmt ts
    in
    let pp_aux fmt uf =
      let is_empty = ref true in
      Pp.collection ~sep:" ∧ " ~fold:fold_congruences fmt uf
        ~pp_item:(fun fmt ((repr : repr), ts) ->
          is_empty := false ;
          F.fprintf fmt "%a=%a" pp_item (repr :> X.t) (pp_ts_or_repr (repr :> X.t)) ts ) ;
      if !is_empty then pp_empty fmt
    in
    F.fprintf fmt "@[<hv>%a@]" pp_aux uf


  let filter_not_in_closed_set ~keep uf =
    let classes =
      UF.Map.filter
        (fun x _ ->
          (* here we take advantage of the fact [keep] is transitively closed already to drop
             entire classes at once iff their representative is not in [keep]: if the class
             contains *one* item in [keep] then *all* of its items are in [keep] *)
          XSet.mem (x :> X.t) keep )
        uf.classes
    in
    (* rebuild [reprs] directly from [classes]: does path compression and garbage collection on the
       old [reprs] *)
    let reprs =
      UF.Map.fold (fun repr xs reprs -> UF.add_disjoint_class repr xs reprs) classes UF.empty
    in
    {reprs; classes}
end
