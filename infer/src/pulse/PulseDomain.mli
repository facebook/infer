(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

module Invalidation : sig
  type std_vector_function =
    | Assign
    | Clear
    | Emplace
    | EmplaceBack
    | Insert
    | PushBack
    | Reserve
    | ShrinkToFit
  [@@deriving compare]

  val pp_std_vector_function : Format.formatter -> std_vector_function -> unit

  type t =
    | CFree
    | CppDelete
    | GoneOutOfScope of Pvar.t * Typ.t
    | Nullptr
    | StdVector of std_vector_function
  [@@deriving compare]

  val issue_type_of_cause : t -> IssueType.t

  val describe : Format.formatter -> t -> unit
end

module ValueHistory : sig
  type event =
    | VariableDeclaration of Location.t
    | CppTemporaryCreated of Location.t
    | Assignment of {location: Location.t}
    | Capture of {captured_as: Pvar.t; location: Location.t}
    | Call of
        { f:
            [ `Call of Typ.Procname.t  (** known function with summary *)
            | `UnknownCall of Typ.Procname.t  (** known function without summary *)
            | `IndirectCall of Exp.t  (** couldn't link the expression to a proc name *)
            | `Model of string  (** hardcoded model *) ]
        ; location: Location.t }

  type t = event list [@@deriving compare]

  val add_to_errlog :
    nesting:int -> event list -> Errlog.loc_trace_elem list -> Errlog.loc_trace_elem list
end

module InterprocAction : sig
  type 'a t =
    | Immediate of {imm: 'a; location: Location.t}
    | ViaCall of {action: 'a t; proc_name: Typ.Procname.t; location: Location.t}
  [@@deriving compare]

  val get_immediate : 'a t -> 'a

  val to_outer_location : 'a t -> Location.t
end

module Trace : sig
  type 'a t = {action: 'a InterprocAction.t; history: ValueHistory.t} [@@deriving compare]

  val add_to_errlog :
       header:string
    -> (F.formatter -> 'a -> unit)
    -> 'a t
    -> Errlog.loc_trace_elem list
    -> Errlog.loc_trace_elem list
end

module Attribute : sig
  type t =
    | Invalid of Invalidation.t Trace.t
    | MustBeValid of unit InterprocAction.t
    | AddressOfCppTemporary of Var.t * ValueHistory.t
    | Closure of Typ.Procname.t
    | StdVectorReserve
  [@@deriving compare]
end

module Attributes : sig
  include PrettyPrintable.PPUniqRankSet with type elt = Attribute.t

  val get_must_be_valid : t -> unit InterprocAction.t option
end

module AbstractAddress : sig
  type t = private int [@@deriving compare]

  val equal : t -> t -> bool

  val init : unit -> unit

  val pp : F.formatter -> t -> unit [@@warning "-32"]

  val mk_fresh : unit -> t

  type state

  val get_state : unit -> state

  val set_state : state -> unit
end

module AbstractAddressSet : PrettyPrintable.PPSet with type elt = AbstractAddress.t

module AbstractAddressMap : PrettyPrintable.PPMap with type key = AbstractAddress.t

module AddrTracePair : sig
  type t = AbstractAddress.t * ValueHistory.t [@@deriving compare]
end

module Stack : sig
  include PrettyPrintable.MonoMap with type key = Var.t and type value = AddrTracePair.t

  (* need to shadow the declaration in [MonoMap] even though it is unused since [MapS.compare] has a
     different type *)
  val compare : t -> t -> int [@@warning "-32"]
end

module Memory : sig
  module Access :
    PrettyPrintable.PrintableOrderedType with type t = AbstractAddress.t HilExp.Access.t

  module Edges : PrettyPrintable.PPMap with type key = Access.t

  type edges = AddrTracePair.t Edges.t

  type cell = edges * Attributes.t

  type t

  val filter : (AbstractAddress.t -> bool) -> t -> t

  val find_opt : AbstractAddress.t -> t -> cell option

  val set_cell : AbstractAddress.t -> cell -> t -> t

  val find_attrs_opt : AbstractAddress.t -> t -> Attributes.t option

  val find_edges_opt : AbstractAddress.t -> t -> edges option

  val mem_edges : AbstractAddress.t -> t -> bool

  val register_address : AbstractAddress.t -> t -> t

  val add_edge : AbstractAddress.t -> Access.t -> AddrTracePair.t -> t -> t

  val find_edge_opt : AbstractAddress.t -> Access.t -> t -> AddrTracePair.t option

  val add_attributes : AbstractAddress.t -> Attributes.t -> t -> t

  val invalidate : AbstractAddress.t * ValueHistory.t -> Invalidation.t InterprocAction.t -> t -> t

  val check_valid : AbstractAddress.t -> t -> (unit, Invalidation.t Trace.t) result

  val std_vector_reserve : AbstractAddress.t -> t -> t

  val is_std_vector_reserved : AbstractAddress.t -> t -> bool
end

type t = {heap: Memory.t; stack: Stack.t}

val empty : t

include AbstractDomain.NoJoin with type t := t

val reachable_addresses : t -> AbstractAddressSet.t
(** compute the set of abstract addresses that are "used" in the abstract state, i.e. reachable
    from the stack variables *)

type mapping

val empty_mapping : mapping

type isograph_relation =
  | NotIsomorphic  (** no mapping was found that can make LHS the same as the RHS *)
  | IsomorphicUpTo of mapping  (** [mapping(lhs)] is isomorphic to [rhs] *)

val isograph_map : lhs:t -> rhs:t -> mapping -> isograph_relation

val is_isograph : lhs:t -> rhs:t -> mapping -> bool
