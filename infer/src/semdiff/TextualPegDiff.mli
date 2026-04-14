(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Textual PEG utilities: conversion helpers and equivalence checking. *)

open! IStd

val convert_and_print : ?debug:bool -> string -> unit
(** Parse a Textual source string, convert each procedure to PEG, print the equations and nested
    term. For expect tests. *)
