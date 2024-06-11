(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module Hashtbl = Caml.Hashtbl

module Lang : sig
  type t = Java | Hack | Python [@@deriving equal]

  val of_string : string -> t option [@@warning "-unused-value-declaration"]

  val to_string : t -> string [@@warning "-unused-value-declaration"]
end

module Location : sig
  type t = Known of {line: int; col: int} | Unknown [@@deriving compare]

  val known : line:int -> col:int -> t

  val pp : F.formatter -> t -> unit

  val pp_line : F.formatter -> t -> unit
end

module type NAME = sig
  type t = {value: string; loc: Location.t} [@@deriving compare, equal, hash]

  val of_java_name : string -> t

  val pp : F.formatter -> t -> unit

  val is_hack_init : t -> bool

  module Hashtbl : Hashtbl.S with type key = t

  module HashSet : HashSet.S with type elt = t

  module Map : Caml.Map.S with type key = t

  module Set : Caml.Set.S with type elt = t
end

module ProcName : NAME (* procedure names, without their attachement type *)

module VarName : sig
  (* variables names *)
  include NAME

  val is_hack_reified_generics_param : t -> bool
end

module FieldName : NAME (* field names, without their enclosing types *)

val builtin_allocate : string

module NodeName : NAME (* node names, also called labels *)

module TypeName : sig
  (* structured value type name *)
  include NAME

  val hack_generics : t

  val wildcard : t
end

module QualifiedProcName : sig
  type enclosing_class = TopLevel | Enclosing of TypeName.t

  type t = {enclosing_class: enclosing_class; name: ProcName.t} [@@deriving compare, equal, hash]
  (* procedure name [name] is attached to the name space [enclosing_class] *)

  val pp : F.formatter -> t -> unit

  val name : t -> ProcName.t

  val contains_wildcard : t -> bool

  module Hashtbl : Hashtbl.S with type key = t
end

type qualified_fieldname = {enclosing_class: TypeName.t; name: FieldName.t}
(* field name [name] must be declared in type [enclosing_class] *)

val pp_qualified_fieldname : F.formatter -> qualified_fieldname -> unit

module Attr : sig
  type t = {name: string; values: string list; loc: Location.t}

  val name : t -> string [@@warning "-unused-value-declaration"]

  val values : t -> string list [@@warning "-unused-value-declaration"]

  val mk_source_language : Lang.t -> t

  val mk_static : t

  val mk_final : t

  val is_async : t -> bool

  val is_abstract : t -> bool

  val is_alias : t -> bool

  val is_hack_wrapper : t -> bool

  val is_final : t -> bool

  val is_notnull : t -> bool

  val is_static : t -> bool

  val is_interface : t -> bool

  val is_trait : t -> bool

  val is_variadic : t -> bool

  val is_const : t -> bool

  val mk_trait : t

  val pp : F.formatter -> t -> unit [@@warning "-unused-value-declaration"]

  val pp_with_loc : F.formatter -> t -> unit [@@warning "-unused-value-declaration"]
end

module Typ : sig
  type t =
    | Int  (** integer type *)
    | Float  (** float type *)
    | Null
    | Void  (** void type *)
    | Ptr of t  (** pointer type *)
    | Struct of TypeName.t  (** structured value type name *)
    | Array of t  (** array type *)
  [@@deriving equal]

  val pp : F.formatter -> t -> unit

  type annotated = {typ: t; attributes: Attr.t list}

  val is_annotated : f:(Attr.t -> bool) -> annotated -> bool

  val mk_without_attributes : t -> annotated
end

module Ident : sig
  type t [@@deriving equal]

  module Set : Caml.Set.S with type elt = t

  module Map : Caml.Map.S with type key = t

  val of_int : int -> t

  val to_int : t -> int

  val next : t -> t

  val fresh : Set.t -> t

  val to_ssa_var : t -> VarName.t

  val pp : F.formatter -> t -> unit
end

module Const : sig
  type t =
    | Int of Z.t  (** integer constants *)
    | Null
    | Str of string  (** string constants *)
    | Float of float  (** float constants *)
end

module ProcSig : sig
  (** Signature uniquely identifies a called procedure in a target language. *)
  type t =
    | Hack of {qualified_name: QualifiedProcName.t; arity: int option}
        (** Hack doesn't support function overloading but it does support functions with default
            arguments. This means that a procedure is uniquely identified by its name and the number
            of arguments. *)
    | Python of {qualified_name: QualifiedProcName.t; arity: int option}
        (** Python supports function overloading and default arguments. This means that a procedure
            is uniquely identified by its name and the number of arguments. *)
    | Other of {qualified_name: QualifiedProcName.t}
        (** Catch-all case for languages that currently lack support for function overloading at the
            Textual level.

            For instance, in C++ and Java the signature consists of a procedure name and types of
            formals. However, the syntax of procedure calls in Textual doesn't give us the types of
            formals and inferring them from the types of arguments would be brittle. We should
            extend the syntax of Textual to allow unambiguous call target resolution when we need to
            add support for other languages. *)
  [@@deriving equal, hash, show]

  val to_qualified_procname : t -> QualifiedProcName.t

  val arity : t -> int option

  val incr_arity : t -> t

  val decr_arity : t -> int -> t

  val is_hack_init : t -> bool

  module Hashtbl : Hashtbl.S with type key = t
end

module ProcDecl : sig
  type t =
    { qualified_name: QualifiedProcName.t
    ; formals_types: Typ.annotated list option
          (** The list of formal argument types may be unknown. Currently, it is possible only for
              external function declarations when translating from Hack and is denoted with a
              special [...] syntax. Functions defined within a textual module always have a fully
              declared list of formal parameters. *)
    ; result_type: Typ.annotated
    ; attributes: Attr.t list }

  val formals_or_die : ?context:string -> t -> Typ.annotated list

  val to_sig : t -> Lang.t option -> ProcSig.t

  val pp : F.formatter -> t -> unit

  val of_unop : Unop.t -> QualifiedProcName.t

  val to_unop : QualifiedProcName.t -> Unop.t option

  val of_binop : Binop.t -> QualifiedProcName.t

  val to_binop : QualifiedProcName.t -> Binop.t option

  val is_cast_builtin : QualifiedProcName.t -> bool

  val is_generics_constructor_builtin : QualifiedProcName.t -> bool

  val is_instanceof_builtin : QualifiedProcName.t -> bool

  val allocate_object_name : QualifiedProcName.t

  val is_allocate_object_builtin : QualifiedProcName.t -> bool

  val allocate_array_name : QualifiedProcName.t

  val is_allocate_array_builtin : QualifiedProcName.t -> bool

  val is_get_lazy_class_builtin : QualifiedProcName.t -> bool

  val is_lazy_class_initialize_builtin : QualifiedProcName.t -> bool

  val is_side_effect_free_sil_expr : QualifiedProcName.t -> bool

  val is_not_regular_proc : QualifiedProcName.t -> bool

  val is_curry_invoke : t -> bool

  val is_variadic : t -> bool
end

module Global : sig
  type t = {name: VarName.t; typ: Typ.t; attributes: Attr.t list}
end

module FieldDecl : sig
  type t = {qualified_name: qualified_fieldname; typ: Typ.t; attributes: Attr.t list}
end

module Exp : sig
  type call_kind = Virtual | NonVirtual [@@deriving equal]

  type t =
    | Var of Ident.t  (** pure variable: it is not an lvalue *)
    | Load of {exp: t; typ: Typ.t option}
    | Lvar of VarName.t  (** the address of a program variable *)
    | Field of {exp: t; field: qualified_fieldname}  (** field offset *)
    | Index of t * t  (** an array index offset: [exp1[exp2]] *)
    | Const of Const.t
    | Call of {proc: QualifiedProcName.t; args: t list; kind: call_kind}
    | Closure of {proc: QualifiedProcName.t; captured: t list; params: VarName.t list}
    | Apply of {closure: t; args: t list}
    | Typ of Typ.t

  val call_non_virtual : QualifiedProcName.t -> t list -> t

  val call_virtual : QualifiedProcName.t -> t -> t list -> t

  val call_sig : QualifiedProcName.t -> int -> Lang.t option -> ProcSig.t

  val allocate_object : TypeName.t -> t

  (* logical not ! *)
  val not : t -> t

  val cast : Typ.t -> t -> t

  val vars : t -> Ident.Set.t

  val pp : F.formatter -> t -> unit
end

module BoolExp : sig
  type t = Exp of Exp.t | Not of t | And of t * t | Or of t * t

  val pp : F.formatter -> t -> unit [@@warning "-unused-value-declaration"]
end

module Instr : sig
  type t =
    | Load of {id: Ident.t; exp: Exp.t; typ: Typ.t option; loc: Location.t}
        (** id <- *exp with *exp:typ *)
    | Store of {exp1: Exp.t; typ: Typ.t option; exp2: Exp.t; loc: Location.t}
        (** *exp1 <- exp2 with exp2:typ *)
    | Prune of {exp: Exp.t; loc: Location.t}  (** assume exp *)
    | Let of {id: Ident.t; exp: Exp.t; loc: Location.t}  (** id = exp *)
  (* Remark that because Sil operations (add, mult...) are calls, we let the Textual programmer put
     expression in local variables, while SIL forbid that. The to_sil transformation will have to
     inline these definitions. *)

  val loc : t -> Location.t

  val pp : F.formatter -> t -> unit
end

module Terminator : sig
  type node_call = {label: NodeName.t; ssa_args: Exp.t list}

  type t =
    | If of {bexp: BoolExp.t; then_: t; else_: t}
    | Ret of Exp.t
    | Jump of node_call list  (** non empty list *)
    | Throw of Exp.t
    | Unreachable
end

module Node : sig
  type t =
    { label: NodeName.t
    ; ssa_parameters: (Ident.t * Typ.t) list
    ; exn_succs: NodeName.t list  (** successor exception nodes *)
    ; last: Terminator.t
    ; instrs: Instr.t list
    ; last_loc: Location.t  (** location of last instruction in file *)
    ; label_loc: Location.t  (** location of label in file *) }
  [@@deriving equal]
end

module ProcDesc : sig
  type t =
    { procdecl: ProcDecl.t
    ; nodes: Node.t list
    ; start: NodeName.t
    ; params: VarName.t list
    ; locals: (VarName.t * Typ.annotated) list
    ; exit_loc: Location.t }

  val pp : F.formatter -> t -> unit [@@warning "-unused-value-declaration"]

  val formals : t -> Typ.annotated list

  val is_ready_for_to_sil_conversion : t -> bool
end

module Body : sig
  type t = {nodes: Node.t list; locals: (VarName.t * Typ.annotated) list}

  val dummy : Location.t -> t
end

module Struct : sig
  type t =
    {name: TypeName.t; supers: TypeName.t list; fields: FieldDecl.t list; attributes: Attr.t list}
end

module SsaVerification : sig
  val run : ProcDesc.t -> unit
end

module SourceFile : sig
  type t

  val create : ?line_map:LineMap.t -> string -> t

  val line_map : t -> LineMap.t option

  val file : t -> SourceFile.t

  val pp : F.formatter -> t -> unit
end

module Module : sig
  type decl =
    | Global of Global.t
    | Struct of Struct.t
    | Procdecl of ProcDecl.t
    | Proc of ProcDesc.t

  type t = {attrs: Attr.t list; decls: decl list; sourcefile: SourceFile.t}

  val lang : t -> Lang.t option

  val pp : F.formatter -> t -> unit [@@warning "-unused-value-declaration"]
end

type transform_error = {loc: Location.t; msg: string Lazy.t}

val pp_transform_error : SourceFile.t -> F.formatter -> transform_error -> unit

exception TextualTransformError of transform_error list

exception SpecialSyntaxError of Location.t * string
