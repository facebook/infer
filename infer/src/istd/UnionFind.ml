(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module type Element = sig
  type t [@@deriving compare]

  val is_simpler_than : t -> t -> bool
end

module Make (X : Element) = struct
  module Set = Caml.Set.Make (X)
  module Map = Caml.Map.Make (X)

  let equal_x = [%compare.equal: X.t]

  (** the union-find backing data structure: maps elements to their representatives *)
  module UF : sig
    (** to get a little bit of type safety *)
    type repr = private X.t

    type t

    val empty : t

    val find_opt : t -> X.t -> repr option

    val find : t -> X.t -> repr

    val merge : t -> repr -> into:repr -> t

    module Map : Caml.Map.S with type key = repr
  end = struct
    type repr = X.t

    type t = X.t Map.t

    let empty = Map.empty

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

    module Map = Map
  end

  type repr = UF.repr

  module Classes = struct
    let find classes (x : UF.repr) = UF.Map.find_opt x classes |> Option.value ~default:Set.empty

    let merge classes (x1 : UF.repr) ~into:(x2 : UF.repr) =
      let class1 = find classes x1 in
      let class2 = find classes x2 in
      let new_class = Set.union class1 class2 |> Set.add (x1 :> X.t) in
      UF.Map.remove x1 classes |> UF.Map.add x2 new_class
  end

  type t = {reprs: UF.t; classes: Set.t UF.Map.t}

  let empty = {reprs= UF.empty; classes= UF.Map.empty}

  let find_opt uf x = UF.find_opt uf.reprs x

  let find uf x = UF.find uf.reprs x

  let union uf x1 x2 =
    let repr1 = find uf x1 in
    let repr2 = find uf x2 in
    if equal_x (repr1 :> X.t) (repr2 :> X.t) then (* avoid creating loops in the relation *) uf
    else
      let from, into =
        if X.is_simpler_than (repr1 :> X.t) (repr2 :> X.t) then (repr2, repr1) else (repr1, repr2)
      in
      let reprs = UF.merge uf.reprs from ~into in
      let classes = Classes.merge uf.classes from ~into in
      {reprs; classes}


  let fold_congruences {classes} ~init ~f =
    UF.Map.fold (fun repr xs acc -> f acc (repr, xs)) classes init
end
