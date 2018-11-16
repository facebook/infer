(*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Generic abstract domains backed by access paths *)

(** add-only set of access paths. To make common operations efficient (namely, add, join, and
    widen), the set is allowed to contain elements whose concretization is redundant (e.g., x* and
    x.f). these redundancies can be eliminated by expressing the set in its canonical form via a
    call to [normalize]. however, [normalize] is quadratic in the size of the set, so it should be
    used sparingly (recommendation: only before computing a summary based on the access path set) *)
module Set : sig
  include AbstractDomain.WithBottom

  val of_list : AccessPath.Abs.t list -> astate

  val mem : AccessPath.Abs.t -> astate -> bool
  (** return true if {%latex:\gamma(\{ap\}) \subseteq \gamma(aps)%}.
      note: this is worst-case linear in the size of the set *)

  val mem_fuzzy : AccessPath.Abs.t -> astate -> bool
  (** more permissive version of [mem]; return true if {%latex:\gamma(\{a\}) \cap \gamma(aps) != \{\}%}.
      note: this is worst-case linear in the size of the set *)

  val add : AccessPath.Abs.t -> astate -> astate

  val normalize : astate -> astate
  (** simplify an access path set to its canonical representation by eliminating redundancies
      between (1) pairs of abstracted access_paths, and (2) exact access paths and abstracted
      access paths. warning: this is quadratic in the size of the set! use sparingly *)
end
