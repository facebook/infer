(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

[@@@warning "-unused-value-declaration"]

(** Pulse's base error monad: some errors can be accumulated along paths, others are fatal and
    there's no point continuing the execution. *)
type ('ok, 'err) t = Ok of 'ok | Recoverable of 'ok * 'err list | FatalError of 'err * 'err list

val append_errors : 'err list -> ('ok, 'err) t -> ('ok, 'err) t
(** adds the given error list to the result, possibly changing an [Ok] result into a [Recoverable]
    one in the process *)

val ok : ('ok, _) t -> 'ok option

val ok_exn : ('ok, _) t -> 'ok
(** dies if the argument is not of the form [Ok _] *)

val map : ('ok, 'err) t -> f:('ok -> 'okk) -> ('okk, 'err) t

val map_error : ('ok, 'err) t -> f:('err -> 'err') -> ('ok, 'err') t

val bind : ('ok, 'err) t -> f:('ok -> ('okk, 'err) t) -> ('okk, 'err) t

val join : (('ok, 'err) t, 'err) t -> ('ok, 'err) t

(** {2 Interaction with other datatypes} *)

val of_some : ('ok option, 'err) t -> ('ok, 'err) t option

val recoverable_of_result : ok_of_error:('err -> 'ok) -> ('ok, 'err) result -> ('ok, 'err) t
(** changes [Result.Error] to [Recoverable] using [ok_of_error] *)

val fatal_of_result : ('ok, 'err) result -> ('ok, 'err) t
(** changes [Result.Error] to [FatalError] *)

val to_result : ('ok, 'err) t -> ('ok, 'err list) result
(** [Recoverable] and [FatalError] are both sent to [Error] *)

val list_fold : 'a list -> init:'ok -> f:('ok -> 'a -> ('ok, 'err) t) -> ('ok, 'err) t

val list_fold2 :
     'a list
  -> 'b list
  -> init:'ok
  -> f:('ok -> 'a -> 'b -> ('ok, 'err) t)
  -> ('ok, 'err) t List.Or_unequal_lengths.t

val list_foldi : init:'acc -> f:(int -> 'acc -> 'a -> ('acc, 'err) t) -> 'a list -> ('acc, 'err) t

val container_fold :
     fold:('t, 'a, ('accum, 'err) t) Container.fold
  -> 't
  -> init:'accum
  -> f:('accum -> 'a -> ('accum, 'err) t)
  -> ('accum, 'err) t

(** for opening locally *)
module Type : sig
  type ('ok, 'err) pulse_result = ('ok, 'err) t =
    | Ok of 'ok
    | Recoverable of 'ok * 'err list
    | FatalError of 'err * 'err list
end

module Monad_infix : sig
  include module type of Type

  val ( >>| ) : ('ok, 'err) t -> ('ok -> 'okk) -> ('okk, 'err) t

  val ( >>= ) : ('ok, 'err) t -> ('ok -> ('okk, 'err) t) -> ('okk, 'err) t
end

module Let_syntax : sig
  include module type of Monad_infix

  val ( let+ ) : ('ok, 'err) t -> ('ok -> 'okk) -> ('okk, 'err) t

  val ( let* ) : ('ok, 'err) t -> ('ok -> ('okk, 'err) t) -> ('okk, 'err) t
end
