(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** Definition and parsing of command line arguments *)

open! IStd

type exe = Analyze | Clang | Interactive | Print | Toplevel

(** Association list of executable (base)names to their [exe]s. *)
val exes : (string * exe) list

val exe_name : exe -> string

val frontend_exes: exe list

(** The [mk_*] functions declare command line options, while [parse] parses then according to the
    declared options.

    The arguments of the declaration functions are largely treated uniformly:
    - [long] declares the option [--long]
    - [short] declares the option [-short] as an alias
    - [deprecated] declares the option [-key] as an alias, for each [key] in [deprecated]
    - [default] specifies the default value
    - [default_to_string] is used to document the default value
    - [f] specifies a transformation to be performed on the parsed value before setting the config
      variable
    - [symbols] is an association list sometimes used in place of [f]
    - [exes] declares that the option should be included in the external documentation (--help) for
      each [exe] in [exes], otherwise it appears only in --help-full
    - [meta] is a meta-variable naming the parsed value for documentation purposes
    - a documentation string
*)
type 'a t =
  ?deprecated:string list -> long:string -> ?short:string ->
  ?exes:exe list -> ?meta:string -> string ->
  'a

(** [mk_set variable value] defines a command line option which sets [variable] to [value]. *)
val mk_set : 'a ref -> 'a -> unit t

val mk_option :
  ?default:'a option -> ?default_to_string:('a option -> string) -> f:(string -> 'a option) ->
  'a option ref t

(** [mk_bool long short doc] defines a [bool ref] set by the command line flag [--long] (and
    [-short]), and cleared by the flag [--no-long] (and [-nshort]).  If [long] already has a "no-",
    or [short] nas an "n", prefix, then the existing prefixes will instead be removed. The default
    value is [false] unless overridden by [~default:true].  The [doc] string will be prefixed with
    either "Activates:" or "Deactivates:", so should be phrased accordingly. *)
val mk_bool : ?deprecated_no:string list ->  ?default:bool -> ?f:(bool -> bool) -> bool ref t

(** [mk_bool_group children not_children] behaves as [mk_bool] with the addition that all the
    [children] are also set and the [no_children] are unset. A child can be unset by including
    "--no-child" later in the arguments. *)
val mk_bool_group :
  ?deprecated_no:string list -> ?default:bool -> (bool ref list -> bool ref list -> bool ref) t

val mk_int : default:int -> int ref t

val mk_int_opt : ?default:int -> int option ref t

val mk_float : default:float -> float ref t

val mk_float_opt : ?default:float -> float option ref t

val mk_string : default:string -> ?f:(string -> string) -> string ref t

val mk_string_opt : ?default:string -> ?f:(string -> string) -> string option ref t

(** [mk_string_list] defines a [string list ref], initialized to [[]] unless overridden by
    [~default].  Each argument of an occurrence of the option will be prepended to the list, so the
    final value will be in the reverse order they appeared on the command line. *)
val mk_string_list :
  ?default:string list -> ?f:(string -> string) -> string list ref t

(** like [mk_string] but will resolve the string into an absolute path so that children processes
    agree on the absolute path that the option represents *)
val mk_path : default:string -> string ref t

(** analogous of [mk_string_opt] with the extra feature of [mk_path] *)
val mk_path_opt : ?default:string -> string option ref t

(** analogous of [mk_string_list] with the extra feature of [mk_path] *)
val mk_path_list : ?default:string list -> string list ref t

(** [mk_symbol long symbols] defines a command line flag [--long <symbol>] where [(<symbol>,_)] is
    an element of [symbols]. *)
val mk_symbol : default:'a -> symbols:(string * 'a) list -> 'a ref t

(** [mk_symbol_opt] is similar to [mk_symbol] but defaults to [None]. *)
val mk_symbol_opt : symbols:(string * 'a) list -> 'a option ref t

(** [mk_symbol_seq long symbols] defines a command line flag [--long <symbol sequence>] where
    [<symbol sequence>] is a comma-separated sequence of [<symbol>]s such that [(<symbol>,_)] is an
    element of [symbols]. *)
val mk_symbol_seq : ?default:'a list -> symbols:(string * 'a) list -> 'a list ref t

val mk_set_from_json : default:'a -> default_to_string:('a -> string)
  -> f:(Yojson.Basic.json -> 'a) -> 'a ref t

val mk_json : Yojson.Basic.json ref t

(** [mk_anon ()] defines a [string list ref] of the anonymous command line arguments, in the reverse
    order they appeared on the command line. *)
val mk_anon :
  unit ->
  string list ref

(** [mk_rest doc] defines a [string list ref] of the command line arguments following ["--"], in the
    reverse order they appeared on the command line.  For example, calling [mk_rest] and parsing
    [exe -opt1 -opt2 -- arg1 arg2] will result in the returned ref containing [arg2; arg1]. *)
val mk_rest :
  ?exes:exe list -> string ->
  string list ref

(** [mk_subcommand doc command_to_speclist] defines a [string list ref] of the command line
    arguments following ["--"], in the reverse order they appeared on the command line.  For
    example, calling [mk_subcommand] and parsing [exe -opt1 -opt2 -- arg1 arg2] will result in the
    returned ref containing [arg2; arg1].  Additionally, the first arg following ["--"] is passed to
    [command_to_speclist] to obtain a list of argument action specifications used when parsing the
    remaining arguments. *)
val mk_subcommand :
  ?exes:exe list -> string ->
  (string -> (Arg.key * Arg.spec * Arg.doc) list) ->
  string list ref

(** environment variable use to pass arguments from parent to child processes *)
val args_env_var : string

(** separator of argv elements when encoded into environment variables *)
val env_var_sep : char

(** [extend_env_args args] appends [args] to those passed via [args_env_var] *)
val extend_env_args : string list -> unit

(** [parse env_var exe_usage exe] parses command line arguments as specified by preceding calls to
    the [mk_*] functions, and returns a function that prints the usage message and help text then
    exits. [exe] is used to construct the help message appropriate for that executable. The decoded
    values of the inferconfig file [config_file], if provided, and of the environment variable
    [env_var] are prepended to [Sys.argv] before parsing.  Therefore arguments passed on the command
    line supersede those specified in the environment variable, which themselves supersede those
    passed via the config file.  WARNING: An argument will be interpreted as many times as it
    appears in all of the config file, the environment variable, and the command line. The [env_var]
    is set to the full set of options parsed.  If [incomplete] is set, unknown options are ignored,
    and [env_var] is not set.  If [accept_unknown] is set, unknown options are treated the same as
    anonymous arguments. *)
val parse : ?incomplete:bool -> ?accept_unknown:bool -> ?config_file:string ->
  exe -> (exe -> Arg.usage_msg) -> (int -> 'a)
