open Identifiers
open Types
open Values
include Generated_Expressions

(* FIXME(#287): Avoid derives triggering deprecation warnings *)
[@@@alert "-deprecated"]

type var_id = local_id
[@@deriving show, eq, ord] [@@ocaml.alert deprecated "use [local_id] instead"]

module VarId = LocalId [@@ocaml.alert deprecated "use [LocalId] instead"]
