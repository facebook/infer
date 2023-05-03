(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module T = Textual
module PyBuiltins = PyCommon.Builtins
module Debug = PyDebug

(** In Python, everything is an object, and the interpreter maintains a stack of references to such
    objects. Pushing and popping on the stack are always references to objets that leave in a heap.
    There is no need to model this heap, but the data stack is quite important. *)
module DataStack : sig
  type cell =
    | Const of int  (** index in [co_consts] *)
    | Name of int  (** reference to a global name, stored in [co_names] *)
    | VarName of int  (** reference to a local name, stored in [co_varnames] *)
    | Temp of T.Ident.t  (** SSA variable *)
    | Fun of (string * FFI.Code.t)
        (** [code] Python object with its qualified name. It can be a function, class, closure, ... *)
  [@@deriving show]

  val as_code : FFI.Code.t -> cell -> FFI.Code.t option

  type t = cell list

  val push : t -> cell -> t

  val pop : t -> (t * cell) option
end

module Labels : Caml.Map.S with type key = int

(** Information about global/toplevel declaration *)
type global_info = {is_code: bool}

(** Information about a "yet to reach" label location, with its name, type of ssa parameters, and a
    function to update the environment before processing the code after the label. For example,
    inserting some pruning operations before the label and the code. Since we don't always know the
    location of the label before-hand, the [prelude] is expecting one.

    We also keep track of the label status: if we already [processed] it within the [nodes]
    function. This is to avoid infinite loops. *)
type label_info =
  { label_name: string
  ; ssa_parameters: T.Typ.t list
  ; prelude: T.Location.t -> t -> t
  ; processed: bool }

(** Part of the environment shared by most structures. It gathers information like which builtin has
    been spotted, or what idents and labels have been generated so far. *)
and shared =
  { idents: T.Ident.Set.t
  ; globals: global_info T.VarName.Map.t
        (** Name of the globals spotted while processing the module topevel. The boolean tells us if
            the globals is some code/function or just a variable. *)
  ; builtins: PyBuiltins.t
  ; next_label: int
  ; labels: label_info Labels.t
        (** Map from offset of labels the code might eventually jump to, to the label info necessary
            to create Textual nodes. *) }

(** State of the capture while processing a single node: each node has a dedicated data stack, and
    generates its own set of instructions. *)
and node = {stack: DataStack.t; instructions: T.Instr.t list; last_line: int option}

and t = {shared: shared; node: node}

val empty : shared

val mk_label_info :
  ?ssa_parameters:T.Typ.t list -> ?prelude:(T.Location.t -> t -> t) -> string -> label_info

val stack : t -> DataStack.t

val map : f:(t -> 'a -> t * 'b) -> env:t -> 'a list -> t * 'b list
(** Similar to [List.map] but an [env] is threaded along the way *)

val reset_for_proc : shared -> t
(** Reset the [node] part of an environment, and all of its [idents], to prepare it to process a new
    code unit *)

val reset_for_node : t -> t
(** Reset the [instructions] field of a [node] to prepare the env to deal with a new set of
    instructions. *)

val reset_stack : t -> t
(** Reset the [stack] field of a [node] *)

val update_last_line : t -> int option -> t
(** Update the [last_line] field of an env, if new information is availbe. *)

val loc : t -> T.Location.t
(** Return the last recorded line information from the Python code-unit, if any. *)

val push : t -> DataStack.cell -> t
(** Push a new [DataStack.cell] on the datastack *)

val pop : t -> (t * DataStack.cell) option
(** Pop a [DataStack.cell] from the datastack, if any is available *)

val mk_fresh_ident : t -> t * T.Ident.t
(** Generate a fresh temporary name *)

val push_instr : t -> T.Instr.t -> t
(** Record a new instruction for the current code unit *)

val mk_fresh_label : t -> t * string
(** Generate a fresh label name *)

val register_label : int -> label_info -> t -> t
(** Register the fact that a [label] must be inserted before the instruction at [offset] *)

val label_of_offset : t -> int -> label_info option
(** Check if the instruction is a possible jump location, and return the label information found
    there, if any. *)

val get_instructions : t -> T.Instr.t list
(** Returns the list of all instructions recorded for the current code unit *)

val register_global : t -> T.VarName.t -> global_info -> t
(** Register a global name (function, variable, ...). Since Python allows "toplevel" code, they are
    encoded within a specially named function that behaves as a toplevel scope, and global
    identifiers are scope accordingly. That way, there is no mixing them with locals with the same
    name. *)

val globals : t -> global_info T.VarName.Map.t
(** Return the [globals] map *)

val register_builtin : t -> string -> t
(** Register a known builtin, so they are correctly scoped, and add the relevant Textual
    declarations for them. *)
