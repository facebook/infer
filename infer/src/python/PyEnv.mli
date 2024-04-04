(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module T = Textual
module Ident = PyCommon.Ident

(** Type information about various entities (toplevel declarations, temporaries, ...). *)
module Info : sig
  type kind = Code | Class | Other

  type t = {kind: kind; typ: T.Typ.t}

  val default : T.Typ.t -> t

  val is_code : kind -> bool

  val is_class : kind -> bool
end

module Symbol : sig
  type kind =
    | Name of {is_imported: bool; typ: T.Typ.t}
        (** The identifier is a name, like a variable name, or any name that might have been
            imported *)
    | Builtin  (** The identifier is a known builtin, like [print] or [range] *)
    | Code  (** The identifier is a code object, like a function declaration *)
    | Class  (** The identifier is a class name *)
    | ImportCall
        (** The identifier is an imported name that has been used in a call, so we can suppose it is
            a function *)
    | Import  (** The identifier is the name of an imported module *)

  val pp_kind : Format.formatter -> kind -> unit

  type t = {id: Ident.t; kind: kind; loc: T.Location.t}

  type key = Global of Ident.t | Local of string

  val pp_key : Format.formatter -> key -> unit

  val pp : Format.formatter -> t -> unit
end

module Signature : sig
  type t = {annotations: PyCommon.annotated_name list; is_static: bool; is_abstract: bool}

  val pp : Format.formatter -> t -> unit [@@warning "-unused-value-declaration"]
end

(** In Python, everything is an object, and the interpreter maintains a stack of references to such
    objects. Pushing and popping on the stack are always references to objets that leave in a heap.
    There is no need to model this heap, but the data stack is quite important. *)
module DataStack : sig
  type cell =
    | Const of FFI.Constant.t  (** constant from [co_consts] *)
    | Name of {global: bool; name: string}  (** reference to a name, from [co_names]. *)
    | VarName of string  (** reference to a local name, from[co_varnames] *)
    | Temp of T.Ident.t  (** SSA variable *)
    | Code of {fun_or_class: bool; code_name: string; code: FFI.Code.t}
        (** [code] Python object with its name. It can be a function, class, closure, ... *)
    | List of (PyBuiltin.builder * cell list)  (** Light encoding of raw Python tuples/lists. *)
    | Map of (T.Exp.t * cell) list  (** Light encoding of raw Python maps/dicts. *)
    | BuiltinBuildClass  (** see Python's [LOAD_BUILD_CLASS] *)
    | Import of {import_path: Ident.t; from_list: string list}
        (** imported module path, with optional names of symbols from that module *)
    | ImportFrom of {import_path: Ident.t; imported_name: string}
        (** imported symbol from a module. Must have been loaded via [Import] first *)
    | ImportCall of {id: Ident.t; loc: T.Location.t}  (** Static call to export definition *)
    | MethodCall of {receiver: T.Exp.t; name: T.QualifiedProcName.t}
        (** Virtual call, usually of a method of a class. Could be an access to a closure that is
            called straight away *)
    | StaticCall of {call_name: T.QualifiedProcName.t; receiver: T.Exp.t option}
        (** call to static method in class. Because we turn some method calls into static ones, we
            have to keep the receiver around, just in case. *)
    | Super  (** special name to refer to the parent class, like in [super().__init__()] *)
    | Path of Ident.t  (** Qualified path for sequence of imports, attribute accesses, ... *)
    | WithContext of T.Ident.t
        (** value to be used for calling __enter__ and __exit__ with the `with context` statement *)
    | NoException
        (** Special NONE symbol pushed by the exception infra to express that no exception has been
            raised *)

  val pp_cell : Format.formatter -> cell -> unit

  val as_code : cell -> FFI.Code.t option

  val as_name : cell -> string option

  val as_id : cell -> Ident.t option

  val is_path : cell -> bool

  val is_no_exception : cell -> bool

  type t = cell list
end

(** Global environment used during bytecode processing. Stores common global information like the
    toplevel symbols processed so far, or more local ones like the set of labels or variable ids
    currently used by a declaration. *)
type t

module Label : sig
  (** Information about a "yet to reach" label location, with its name, type of ssa parameters, and
      a function to update the environment before processing the code after the label. For example,
      inserting some pruning operations before the label and the code. Since we don't always know
      the location of the label before-hand, the [prelude] is expecting one.

      We also keep track of the label status: if we already [processed] it within the [nodes]
      function. This is to avoid infinite loops. *)
  type info

  val mk : ?ssa_parameters:T.Typ.t list -> ?prelude:(T.Location.t -> t -> t) -> string -> info
  (** Create a [label_info] with the provided arguments. Mostly used with the defaults *)

  val update_ssa_parameters : info -> T.Typ.t list -> info
  (** Update the [ssa_parameters] of a label *)

  val is_processed : info -> bool
  (** Returns true iff the label was already encountered during processing *)

  val name : info -> string
  (** Returns the [name] of a label *)

  val to_textual : t -> T.Location.t -> info -> t * string * (T.Ident.t * T.Typ.t) list
  (** Process a label [info] and turn it into Textual information *)
end

(** Class level info. For now, only the parent info (if present) is tracked, supporting multiple
    inheritance and its full qualified name (for nested classes). We may track more information in
    the future, like being an abstract class, a dataclass, ... *)
type class_info = {qualified_name: Ident.t; parents: Ident.t list}

(** Method level info. We store a method/function signature, and its default arugments *)
type method_info = {signature: Signature.t; default_arguments: T.Exp.t list}

val empty : Ident.t -> t

val loc : t -> T.Location.t
(** Return the last recorded line information from the Python code-unit, if any. *)

val stack : t -> DataStack.t
(** Returns the [DataStack.t] for the current declaration *)

val globals : t -> Symbol.t Ident.Map.t
(** Return the [globals] map *)

val get_used_builtins : t -> PyBuiltin.Set.t
(** Return a set of [Builtin] the we spotted in the code *)

val instructions : t -> T.Instr.t list
(** Returns the list of all instructions recorded for the current code unit *)

val label_of_offset : t -> int -> Label.info option
(** Check if the instruction is a possible jump location, and return the label information found
    there, if any. *)

val mk_fresh_ident : t -> Info.t -> t * T.Ident.t
(** Generate a fresh temporary name *)

val get_ident_info : t -> T.Ident.t -> Info.t option
(** Get back the information of a temporary *)

val mk_fresh_label : t -> t * string
(** Generate a fresh label name *)

val map : f:(t -> 'a -> t * 'b) -> env:t -> 'a list -> t * 'b list
(** Similar to [List.map] but an [env] is threaded along the way *)

val map_result : f:(t -> 'a -> (t * 'b, 'c) result) -> env:t -> 'a list -> (t * 'b list, 'c) result
(** Similar to [map] but supports functions that return [Result.t] *)

val enter_proc :
  is_toplevel:bool -> is_static:bool -> module_name:Ident.t -> params:string list -> t -> t
(** Set the environment when entering a new code unit (like reset the instruction buffer, or
    id/label generators. *)

val set_annotations : t -> t
(** Configure the environment to keep track of variable annotations in the dedicated
    [__annotations__] dictionary. Right now, we register its existence but don't store anything in
    it *)

val has_annotations : t -> bool
(** Returns the fact that annotations are tracked or not *)

val enter_node : t -> t
(** Set the environment when entering a new node. Reset the [instructions] buffer. *)

val reset_stack : t -> t
(** Reset the [stack] field of a [node] *)

val update_last_line : t -> int option -> t
(** Update the [last_line] field of an env, if new information is availbe. *)

val push : t -> DataStack.cell -> t
(** Push a new [DataStack.cell] on the datastack *)

val pop : t -> (t * DataStack.cell) option
(** Pop a [DataStack.cell] from the datastack, if any is available *)

val peek : t -> DataStack.cell option
(** Peek a [DataStack.cell] from the datastack, if any is available *)

val push_instr : t -> T.Instr.t -> t
(** Record a new instruction for the current code unit *)

val register_label : offset:int -> Label.info -> t -> t
(** Register the fact that a new label [info] must be inserted before the instruction at [offset] *)

val register_with_target : offset:int -> t -> t
(** Register the location of a [with] statement clean up code *)

val is_with_target : offset:int -> t -> bool
(** Check whether a code offset is the target of a [with] statement *)

val process_label : offset:int -> Label.info -> t -> t
(** Mark the label [info] at [offset] as processed *)

val register_symbol : t -> Symbol.key -> Symbol.t -> t
(** Register a name (function, variable, ...). It might be a [global] symbol at the module level or
    in a local object. *)

val lookup_symbol : t -> Symbol.key -> Symbol.t option
(** Lookup information about a global/local symbol previously registered via [register_symbol] *)

val register_call : t -> Ident.t -> t
(** Register a function call. It enables us to deal correctly with builtin declaration. *)

val mk_builtin_call : t -> PyBuiltin.textual -> T.Exp.t list -> t * T.Ident.t * T.Typ.t
(** Wrapper to compute the Textual version of a call to a "textual" builtin * function (a builtin we
    introduced for modeling purpose) *)

val register_function : t -> string -> T.Location.t -> PyCommon.signature -> T.Exp.t list -> t
(** Register a function declaration. We keep track of them since they might shadow Python builtins
    or previous definitions *)

val register_method :
  t -> enclosing_class:Ident.t -> method_name:string -> Signature.t -> T.Exp.t list -> t
(** Register a method declaration. We mostly keep track of their signatures *)

val register_fields : t -> T.TypeName.t -> PyCommon.signature -> t
(** Extended the set of fields of class [class_name] with [class_fields]. We might have multiple
    calls to this function with the same class name in a best effort attempt: Python is dynamic, and
    any [self.foo] access could give rise to such a registration *)

val lookup_method : t -> enclosing_class:Ident.t -> string -> method_info option
(** Lookup the information stored for a function/method in the relevant [enclosing_class] *)

val lookup_fields : t -> T.TypeName.t -> PyCommon.signature option

(** Lookup the signature of a function / method *)

val register_class : t -> string -> Ident.t -> Ident.t list -> t
(** Register a class declaration (based on [LOAD_BUILD_CLASS]) *)

val get_declared_classes : t -> class_info PyCommon.SMap.t
(** Return information of classes defined in the current module *)

val register_imported_value : t -> Ident.t -> t
(** register global names imported from other modules *)

val get_textual_imports : t -> T.Module.decl list
(** Get back the list of registered imports *)

val is_toplevel : t -> bool
(** Are we processing top level instructions, or something in a function/class ? *)

val is_static : t -> bool
[@@warning "-unused-value-declaration"]
(** Are we processing instructions from a static method ? *)

val get_params : t -> string list
(** Return the name of the method/function parameters, if any *)

val module_name : t -> Ident.t
(** Returns the name of the current module *)
