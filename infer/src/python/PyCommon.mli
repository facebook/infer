(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val builtin_name : string -> Textual.qualified_procname

val python_int : Textual.qualified_procname

val python_string : Textual.qualified_procname

val python_tuple : Textual.qualified_procname

(* Pointer to pyObject *)
val pyObject : Textual.Typ.t

val mk_int : int64 -> Textual.Exp.t

val mk_string : string -> Textual.Exp.t

module Builtins : sig
  module Set : Caml.Set.S with type elt = string

  (* Turn a set of builtin we spotted while processing the code into their textual
     representation. This way we only consider builtins that are actually used by the code.
     (note that we always export python_int, python_string and python_tuple *)
  val to_textual : Set.t -> Textual.Module.decl list

  val register : string -> Set.t -> Set.t

  (* TODO(vsiles): to keep things simple, we pass the set of known builtins around. We could also
     keep a set and just refer to it. But I'd like to keep the option to configure this set
     before infer is started, so I don't hard-code it for now. *)
  val is_builtin : string -> Set.t -> bool

  (* Create a set of all the currently known builtins *)
  val mk : unit -> Set.t
end
