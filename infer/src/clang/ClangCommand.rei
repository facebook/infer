/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
type args;

type t =
  | Assembly args
  /** a normalized clang command that runs the assembler */
  | CC1 args
  /** a -cc1 clang command */
  | ClangError string
  | ClangWarning string
  | NonCCCommand args /** other commands (as, ld, ...) */;


/** [mk qs argv] finds the type of command depending on its arguments [argv]. The quoting style of
    the arguments have to be provided, so that the command may be run later on. */
let mk: ClangQuotes.style => array string => t;


/** change an args object into a string ready to be passed to a shell to be executed */
let command_to_run: args => string;


/** whether the command has this flag set in its arguments */
let has_flag: args => string => bool;


/** the value passed to an option in the arguments of a command */
let value_of_option: args => string => option string;


/** add the arguments needed to attach the facebook-clang-plugins plugin */
let with_plugin_args: args => args;

let prepend_arg: string => args => args;

let prepend_args: list string => args => args;

let append_args: list string => args => args;

let get_orig_argv: args => array string;


/** updates the executable to be run */
let with_exec: string => args => args;
