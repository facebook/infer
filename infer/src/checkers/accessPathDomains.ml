(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

module Set = struct
  module APSet = PrettyPrintable.MakePPSet (AccessPath.Abs)

  (** TODO (12086310): best-case behavior of some operations can be improved by adding "abstracted"
      bool recording whether an abstracted access path has been introduced *)
  type t = APSet.t

  let pp = APSet.pp

  let empty = APSet.empty

  let is_empty = APSet.is_empty

  let normalize aps =
    APSet.filter
      (fun lhs ->
        not
          (APSet.exists
             (fun rhs -> (not (phys_equal lhs rhs)) && AccessPath.Abs.( <= ) ~lhs ~rhs)
             aps) )
      aps


  let add = APSet.add

  let of_list = APSet.of_list

  let mem ap aps =
    APSet.mem ap aps
    || APSet.exists (fun other_ap -> AccessPath.Abs.( <= ) ~lhs:ap ~rhs:other_ap) aps


  let mem_fuzzy ap aps =
    let has_overlap ap1 ap2 =
      AccessPath.Abs.( <= ) ~lhs:ap1 ~rhs:ap2 || AccessPath.Abs.( <= ) ~lhs:ap2 ~rhs:ap1
    in
    APSet.mem ap aps || APSet.exists (has_overlap ap) aps


  let ( <= ) ~lhs ~rhs =
    if phys_equal lhs rhs then true
    else
      let rhs_contains lhs_ap = mem lhs_ap rhs in
      APSet.subset lhs rhs || APSet.for_all rhs_contains lhs


  let join aps1 aps2 = if phys_equal aps1 aps2 then aps1 else APSet.union aps1 aps2

  let widen ~prev ~next ~num_iters:_ =
    if phys_equal prev next then prev
    else
      let abstract_access_path ap aps =
        match ap with
        | AccessPath.Abs.Exact exact_ap ->
            APSet.add (AccessPath.Abs.Abstracted exact_ap) aps
        | AccessPath.Abs.Abstracted _ ->
            APSet.add ap aps
      in
      let diff_aps = APSet.diff next prev in
      APSet.fold abstract_access_path diff_aps APSet.empty |> join prev
end
