(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** Execution environments: basically a cache of where procedures are and what is their type
   environment *)

type file_data

type t = private
  { proc_map: file_data Typ.Procname.Hash.t  (** map from procedure name to file data *)
  ; file_map: file_data SourceFile.Hash.t  (** map from source files to file data *) }

val mk : unit -> t
(** Create a new cache *)

val get_tenv : t -> Typ.Procname.t -> Tenv.t
(** return the type environment associated with the procedure *)

val get_integer_type_widths : t -> Typ.Procname.t -> Typ.IntegerWidths.t
(** return the integer type widths associated with the procedure *)
