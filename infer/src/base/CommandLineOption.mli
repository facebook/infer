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

(** a section is a part of infer that can be affected by an infer option *)
type section =
    Analysis | BufferOverrun | Checkers | Clang | Crashcontext | Driver | Java | Print | Quandary
[@@deriving compare]

val all_sections : section list

type 'a parse = Differential | Infer of 'a | Javac | NoParse

type parse_mode = section list parse [@@deriving compare]

type parse_action = section parse [@@deriving compare]

val is_originator : bool

val init_work_dir : string

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
    - [parse_mode] declares which parse mode the option is for. In the case of Infer, that includes
      the sections for which the option should be included in the external documentation (--help),
      otherwise it appears only in --help-full
    - [meta] is a meta-variable naming the parsed value for documentation purposes
    - a documentation string
*)
type 'a t =
  ?deprecated:string list -> long:string -> ?short:char ->
  ?parse_mode:parse_mode -> ?meta:string -> string ->
  'a

(** [mk_set variable value] defines a command line option which sets [variable] to [value]. *)
val mk_set : 'a ref -> 'a -> unit t

val mk_option :
  ?default:'a option -> ?default_to_string:('a option -> string) -> f:(string -> 'a option) ->
  'a option ref t

(** [mk_bool long short doc] defines a [bool ref] set by the command line flag [--long] (and
    [-s]), and cleared by the flag [--no-long] (and [-S]).  If [long] already has a "no-" prefix,
    or [s] is capital, then the existing prefixes will instead be removed. The default
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
val mk_symbol : default:'a -> symbols:(string * 'a) list -> eq:('a -> 'a -> bool) -> 'a ref t

(** [mk_symbol_opt] is similar to [mk_symbol] but defaults to [None]. *)
val mk_symbol_opt : symbols:(string * 'a) list -> 'a option ref t

(** [mk_symbol_seq long symbols] defines a command line flag [--long <symbol sequence>] where
    [<symbol sequence>] is a comma-separated sequence of [<symbol>]s such that [(<symbol>,_)] is an
    element of [symbols]. *)
val mk_symbol_seq :
  ?default:'a list -> symbols:(string * 'a) list -> eq:('a -> 'a -> bool) -> 'a list ref t

val mk_set_from_json : default:'a -> default_to_string:('a -> string)
  -> f:(Yojson.Basic.json -> 'a) -> 'a ref t

val mk_json : Yojson.Basic.json ref t

(** [mk_anon ()] defines a [string list ref] of the anonymous command line arguments, in the reverse
    order they appeared on the command line. *)
val mk_anon : unit -> string list ref

(** [mk_rest doc] defines a [string list ref] of the command line arguments following ["--"], in the
    reverse order they appeared on the command line.  For example, calling [mk_rest] and parsing
    [exe -opt1 -opt2 -- arg1 arg2] will result in the returned ref containing [arg2; arg1]. *)
val mk_rest :
  ?parse_mode:parse_mode -> string ->
  string list ref

(** [mk_rest_actions doc ~usage command_to_parse_action] defines a [string list ref] of the command
    line arguments following ["--"], in the reverse order they appeared on the command line. [usage]
    is the usage message in case of parse errors or if --help is passed.  For example, calling
    [mk_action] and parsing [exe -opt1 -opt2 -- arg1 arg2] will result in the returned ref
    containing [arg2; arg1].  Additionally, the first arg following ["--"] is passed to
    [command_to_parse_action] to obtain the parse action that will be used to parse the remaining
    arguments. *)
val mk_rest_actions :
  ?parse_mode:parse_mode -> string ->
  usage:string -> (string -> parse_action)
  -> string list ref


(** when the option is found on the command line, the current parse action is discarded and the
    following arguments are parsed using [parse_action] *)
val mk_switch_parse_action : parse_action -> usage:string -> unit t

(** environment variable use to pass arguments from parent to child processes *)
val args_env_var : string

(** separator of argv elements when encoded into environment variables *)
val env_var_sep : char

(** [extend_env_args args] appends [args] to those passed via [args_env_var] *)
val extend_env_args : string list -> unit

(** [parse ~usage parse_action] parses command line arguments as specified by preceding calls to the
    [mk_*] functions, and returns a function that prints the usage message and help text then exits.

    The decoded values of the inferconfig file [config_file], if provided, are parsed, followed by
    the decoded values of the environment variable [args_env_var], followed by [Sys.argv] if
    [parse_action] is one that should parse command line arguments (this is defined in the
    implementation of this module). Therefore arguments passed on the command line supersede those
    specified in the environment variable, which themselves supersede those passed via the config
    file.

    If [incomplete] is set, unknown options are ignored, and [args_env_var] is not set.

    WARNING: An argument will be interpreted as many times as it appears in all of the config file,
    the environment variable, and the command line. The [args_env_var] is set to the set of options
    parsed in [args_env_var] and on the command line. *)
val parse : ?incomplete:bool -> ?config_file:string ->
  usage:Arg.usage_msg -> parse_action -> parse_action * (int -> 'a)

(** [is_env_var_set var] is true if $[var]=1 *)
val is_env_var_set : string -> bool
