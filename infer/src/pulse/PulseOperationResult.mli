(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open PulseBasicInterface
open PulseDomainInterface

type ('ok, 'err) t = ('ok, 'err) PulseResult.t SatUnsat.t

type 'a access_t = 'a AccessResult.t SatUnsat.t

val sat_ok : ('ok, _) PulseResult.t SatUnsat.t -> 'ok option

val list_fold : 'a list -> init:'ok -> f:('ok -> 'a -> ('ok, 'err) t) -> ('ok, 'err) t

(** For [open]ing in other modules. *)
module Import : sig
  (** {2 Monadic operations on the combined monad of [SatUnsat] and [PulseResult].}

      [let] operators produce [_ t] values following the naming convention [letXY] where [X] and [Y]
      depend on the types of the inputs: in [letXY a = b in c], [X] is according to the [SatUnsat.t]
      behaviour and [Y] about the [PulseResult.t] behaviour:

      - [X] is [*] if [b] is a [SatUnsat.t] and [c] a [SatUnsat.t] too (bind)
      - [X] is [+] if [b] is a [SatUnsat.t] and [c] is not (map)
      - [X] is [=] if [b] is *not* a [SatUnsat.t] and [c] is a [SatUnsat.t]

      Similarly [Y] is [*] or [+] depending on whether the operation on the underlying
      [PulseResult.t] is a map or or bind.

      We also define inline operations [>>UV] with [U] being [=], [|], or [>] corresponding to [*],
      [+], or [=] above, and similarly for [V].

      In practice, to figure out which let binder or operator is right for your call site, just have
      a look at the types! To figure out which [letXY] to use for [letXY a = b in c], pick the one
      with the type [type_of_b -> (type_of_a -> type_of_c) -> type_of_letXY_a_equal_b_in_c]. *)

  include module type of PulseResult.Let_syntax

  val ( let** ) : ('ok, 'err) t -> ('ok -> ('okk, 'err) t) -> ('okk, 'err) t

  val ( >>== ) : ('ok, 'err) t -> ('ok -> ('okk, 'err) t) -> ('okk, 'err) t

  val ( let++ ) : ('ok, 'err) t -> ('ok -> 'okk) -> ('okk, 'err) t

  val ( >>|| ) : ('ok, 'err) t -> ('ok -> 'okk) -> ('okk, 'err) t

  val ( let+* ) : ('ok, 'err) t -> ('ok -> ('okk, 'err) PulseResult.t) -> ('okk, 'err) t

  val ( >>|= ) : ('ok, 'err) t -> ('ok -> ('okk, 'err) PulseResult.t) -> ('okk, 'err) t

  val ( let=* ) : ('ok, 'err) PulseResult.t -> ('ok -> ('okk, 'err) t) -> ('okk, 'err) t

  val ( >>>= ) : ('ok, 'err) PulseResult.t -> ('ok -> ('okk, 'err) t) -> ('okk, 'err) t

  val ( let<*> ) :
       ('a, 'err) PulseResult.t
    -> ('a -> ('b, 'err) PulseResult.t list)
    -> ('b, 'err) PulseResult.t list
  (** monadic "bind" but not really that turns an [PulseResult.t] into a list of [PulseResult.t]s
      (not really because the first type is not an [PulseResult.t list] but just an [PulseResult.t]) *)

  val ( let<**> ) :
    ('a, 'err) t -> ('a -> ('b, 'err) PulseResult.t list) -> ('b, 'err) PulseResult.t list

  val ( let<+> ) :
       ('a, 'err) PulseResult.t
    -> ('a -> 'abductive_domain_t)
    -> ('abductive_domain_t ExecutionDomain.base_t, 'err) PulseResult.t list
  (** monadic "map" but even less really that turns a [PulseResult.t] into an analysis result *)

  val ( let<++> ) :
       ('a, 'err) t
    -> ('a -> 'abductive_domain_t)
    -> ('abductive_domain_t ExecutionDomain.base_t, 'err) PulseResult.t list

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

  type base_error = AccessResult.error =
    | PotentialInvalidAccess of
        { astate: AbductiveDomain.t
        ; address: DecompilerExpr.t
        ; must_be_valid: Trace.t * Invalidation.must_be_valid_reason option }
    | ReportableError of {astate: AbductiveDomain.t; diagnostic: Diagnostic.t}
    | WithSummary of base_error * AbductiveDomain.Summary.t
end
