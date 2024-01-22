(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** buck version *)
type version = V1 | V2

module Target : sig
  type t

  val of_string : string -> t

  val to_string : t -> string

  val add_flavor_v1 : BuckMode.t -> InferCommand.t -> extra_flavors:string list -> t -> t
  (** Add flavors to given targets. NB flavors only exist in buck1, this must not be called for
      buck2 *)
end

val wrap_buck_call :
  ?extend_env:(string * string) list -> version -> label:string -> string list -> string list
(** Wrap a call to buck while (i) logging standard error to our standard error in real time; (ii)
    redirecting standard out to a file, the contents of which are returned; (iii) protect the child
    process from [SIGQUIT].

    In a call [wrap_buck_call ~extend_env ~label cmd], [extend_env] is a list of pairs
    [(variable, value)] that will extend the environment of the subprocess; [label] is appended to
    [buck_] to make the prefix of the temporary file storing the standard output of the command, for
    quick identification; [cmd] is a list of strings making up the shell command to execute; the
    return value is the standard output of the command split on newlines. *)

val config : BuckMode.t -> version -> string list
(** return list of string parameters of the form
    ["--config" :: param_a :: "--config" :: param_b :: ...] describing the buck config flags for the
    given Buck mode. *)

val parse_command_and_targets :
  BuckMode.t -> version -> string list -> string * string list * string list
(** parses given buck command, using the buck configuration returned by [config] above and returns a
    triple [(buck_command, non_target_params, target_params)] *)

val store_args_in_file : identifier:string -> string list -> string list
(** Given a list of arguments, stores them in a file if needed and returns the new command line ;
    [identifier] is the temporary filename prefix *)

val filter_compatible : [> `Targets] -> string list -> string list
(** keep only the options compatible with the given Buck subcommand *)
