(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

type 'a t = 'a AccessResult.t SatUnsat.t

val sat_ok : 'a AccessResult.t SatUnsat.t -> 'a option

val list_fold : 'a list -> init:'b -> f:('b -> 'a -> 'b t) -> 'b t

(** For [open]ing in other modules. *)
module Import : sig
  (** {2 Monadic operations on the combined monad of [SatUnsat] and [AccessResult].}

      [let] operators produce [_ t] values following the naming convention [letXY] where [X] and [Y]
      depend on the types of the inputs: in [letXY a = b in c], [X] is according to the [SatUnsat.t]
      behaviour and [Y] about the [AccessResult.t] behaviour:

      - [X] is [*] if [b] is a [SatUnsat.t] and [c] a [SatUnsat.t] too (bind)
      - [X] is [+] if [b] is a [SatUnsat.t] and [c] is not (map)
      - [X] is [=] if [b] is *not* a [SatUnsat.t] and [c] is a [SatUnsat.t]

      Similarly [Y] is [*] or [+] depending on whether the operation on the underlying
      [AccessResult.t] is a map or or bind.

      We also define inline operations [>>UV] with [U] being [=], [|], or [>] corresponding to [*],
      [+], or [=] above, and similarly for [V].

      In practice, to figure out which let binder or operator is right for your call site, just have
      a look at the types! To figure out which [letXY] to use for [letXY a = b in c], pick the one
      with the type [type_of_b -> (type_of_a -> type_of_c) -> type_of_letXY_a_equal_b_in_c]. *)

  include module type of PulseResult.Let_syntax

  [@@@warning "-unused-value-declaration"]

  val ( let** ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( >>== ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let++ ) : 'a t -> ('a -> 'b) -> 'b t

  val ( >>|| ) : 'a t -> ('a -> 'b) -> 'b t

  val ( let+* ) : 'a t -> ('a -> 'b AccessResult.t) -> 'b t

  val ( >>|= ) : 'a t -> ('a -> 'b AccessResult.t) -> 'b t

  val ( let=* ) : 'a AccessResult.t -> ('a -> 'b t) -> 'b t

  val ( >>>= ) : 'a AccessResult.t -> ('a -> 'b t) -> 'b t

  val ( let=+ ) : 'a AccessResult.t -> ('a -> 'b AccessResult.t) -> 'b t

  val ( >>>| ) : 'a AccessResult.t -> ('a -> 'b AccessResult.t) -> 'b t

  val ( let<*> ) : 'a AccessResult.t -> ('a -> 'b AccessResult.t list) -> 'b AccessResult.t list
  (** monadic "bind" but not really that turns an [AccessResult.t] into a list of [AccessResult.t]s
      (not really because the first type is not an [AccessResult.t list] but just an
      [AccessResult.t]) *)

  val ( let<**> ) : 'a t -> ('a -> 'b AccessResult.t list) -> 'b AccessResult.t list

  val bind_sat_result :
    'c -> 'a t -> ('a -> 'b AccessResult.t list * 'c) -> 'b AccessResult.t list * 'c

  val ( let<+> ) :
    'a AccessResult.t -> ('a -> AbductiveDomain.t) -> ExecutionDomain.t AccessResult.t list
  (** monadic "map" but even less really that turns a [AccessResult.t] into an analysis result *)

  val ( let<++> ) : 'a t -> ('a -> AbductiveDomain.t) -> ExecutionDomain.t AccessResult.t list

  [@@@warning "+unused-value-declaration"]

  (** {2 Imported types for ease of use and so we can write variants without the corresponding
      module prefix} *)

  type access_mode =
    | Read
    | Write
    | NoAccess
        (** The initialized-ness of the address is not checked when it evaluates a heap address
            without actual memory access, for example, when evaluating [&x.f] we need to check
            initialized-ness of [x], not that of [x.f]. *)

  type 'abductive_domain_t execution_domain_base_t = 'abductive_domain_t ExecutionDomain.base_t =
    | ContinueProgram of 'abductive_domain_t
    | ExceptionRaised of 'abductive_domain_t
    | ExitProgram of AbductiveDomain.Summary.t
    | AbortProgram of AbductiveDomain.Summary.t
    | LatentAbortProgram of {astate: AbductiveDomain.Summary.t; latent_issue: LatentIssue.t}
    | LatentInvalidAccess of
        { astate: AbductiveDomain.Summary.t
        ; address: DecompilerExpr.t
        ; must_be_valid: Trace.t * Invalidation.must_be_valid_reason option
        ; calling_context: (CallEvent.t * Location.t) list }
    | LatentSpecializedTypeIssue of
        {astate: AbductiveDomain.Summary.t; specialized_type: Typ.Name.t; trace: Trace.t}

  type base_error = AccessResult.error =
    | PotentialInvalidAccess of
        { astate: AbductiveDomain.t
        ; address: DecompilerExpr.t
        ; must_be_valid: Trace.t * Invalidation.must_be_valid_reason option }
    | PotentialInvalidSpecializedCall of
        {astate: AbductiveDomain.t; specialized_type: Typ.Name.t; trace: Trace.t}
    | ReportableError of {astate: AbductiveDomain.t; diagnostic: Diagnostic.t}
    | WithSummary of base_error * AbductiveDomain.Summary.t
end
