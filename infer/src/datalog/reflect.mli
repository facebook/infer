(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Javalib_pack

(*
  Reflection is used in Java programs as follows:
  1) Obtain a Class object (e.g. Class c = Class.forName("C") or obj.getClass())
  2) Get information about the class (e.g. Method[] meths = c.getMethods())
  3) Use reflection API to manipulate the information (e.g. String name = meths[0].toString())

  As a heuristic for detecting usages of reflection, we detect the invocation of methods
  that return a Class object. This is always the first step to use reflection.
  However, if reflection is only used to print some information, then its usage
  does not intefere with most of static analyses.
*)

type refl_call = {refl_ms: string; caller_cl: string; caller_ms: string}

val get_method_refl_calls : JCode.jcode Javalib.concrete_method -> refl_call list
(** Returns the list of reflexive calls inside a concrete method. A call is represented by
    (reflective_method_signature, caller_class, caller_method_signature) *)
