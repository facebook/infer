(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Programs / translation units

    LLAIR (Low-Level Analysis Internal Representation) is an IR optimized
    for static analysis using a low-level model of memory. Compared to a
    compiler IR such as LLVM, an analyzer does not need to perform register
    allocation, instruction selection, code generation, etc. or even much
    code transformation, so the constraints on the IR are very different.

    LLAIR is a "Functional SSA" form where control transfers pass arguments
    instead of using ϕ-nodes. An analyzer will need good support for
    parameter passing anyhow, and ϕ-nodes make it hard to express program
    properties as predicates on states, since some execution history is
    needed to evaluate ϕ instructions. SSA form is beneficial for analysis
    as it means that all "modified variables" side-conditions on program
    logic rules trivially hold. An alternative view is that the scope of
    variables [reg] assigned in instructions such as [Load] is the successor
    block as well as all blocks the instruction dominates in the
    control-flow graph. This language is first-order, and a term structure
    for the code constituting the scope of variables is not needed, so SSA
    and not CPS suffices.

    Additionally, the focus on memory analysis leads to a design where the
    arithmetic and logic operations are not "instructions" but instead are
    complex expressions (see [Exp]) that refer to registers (see [Var]). *)

(** Instructions for memory manipulation or other non-control effects. *)
type inst = private
  | Load of {reg: Var.t; ptr: Exp.t; loc: Loc.t}
      (** Read the contents of memory at address [ptr] into [reg]. *)
  | Store of {ptr: Exp.t; exp: Exp.t; loc: Loc.t}
      (** Write [exp] into memory at address [ptr]. *)
  | Memcpy of {dst: Exp.t; src: Exp.t; len: Exp.t; loc: Loc.t}
      (** Copy [len] bytes starting from address [src] to [dst], undefined
          if ranges overlap. *)
  | Memmov of {dst: Exp.t; src: Exp.t; len: Exp.t; loc: Loc.t}
      (** Copy [len] bytes starting from address [src] to [dst]. *)
  | Memset of {dst: Exp.t; byt: Exp.t; len: Exp.t; loc: Loc.t}
      (** Store byte [byt] into [len] memory addresses starting from [dst]. *)
  | Alloc of {reg: Var.t; num: Exp.t; loc: Loc.t}
      (** Allocate a block of memory large enough to store [num] elements of
          type [t] where [reg : t*] and bind [reg] to the first address. *)
  | Free of {ptr: Exp.t; loc: Loc.t}
      (** Deallocate the previously allocated block at address [ptr]. *)
  | Nondet of {reg: Var.t option; msg: string; loc: Loc.t}
      (** Bind [reg] to an arbitrary value of its type, representing
          non-deterministic approximation of behavior described by [msg]. *)

(** A (straight-line) command is a sequence of instructions. *)
type cmnd = inst vector

(** A label is a name of a block. *)
type label = string

(** A jump with arguments. *)
type 'a control_transfer =
  { mutable dst: 'a
  ; args: Exp.t vector
  ; mutable retreating: bool
        (** Holds if [dst] is an ancestor in a depth-first traversal. *) }

(** A jump with arguments to a block. *)
type jump = block control_transfer

(** Block terminators for function call/return or other control transfers. *)
and term = private
  | Switch of {key: Exp.t; tbl: (Z.t * jump) vector; els: jump; loc: Loc.t}
      (** Invoke the [jump] in [tbl] associated with the integer [z] which
          is equal to [key], if any, otherwise invoke [els]. *)
  | ISwitch of {ptr: Exp.t; tbl: jump vector; loc: Loc.t}
      (** Invoke the [jump] in [tbl] whose [dst] is equal to [ptr]. *)
  | Call of
      { call: Exp.t control_transfer  (** [Global] for non-virtual call. *)
      ; return: jump  (** Return destination or trampoline. *)
      ; throw: jump option  (** Handler destination or trampoline. *)
      ; ignore_result: bool  (** Drop return value when invoking return. *)
      ; loc: Loc.t }
      (** Call function [call.dst] with arguments [call.args]. *)
  | Return of {exp: Exp.t option; loc: Loc.t}
      (** Invoke [return] of the dynamically most recent [Call]. *)
  | Throw of {exc: Exp.t; loc: Loc.t}
      (** Invoke [throw] of the dynamically most recent [Call] with [throw]
          not [None]. *)
  | Unreachable
      (** Halt as control is assumed to never reach [Unreachable]. *)

(** A block is a destination of a jump with arguments, contains code. *)
and block = private
  { lbl: label
  ; params: Var.t vector
  ; cmnd: cmnd
  ; term: term
  ; mutable parent: func
  ; mutable sort_index: int
        (** Position in a topological order, ignoring [retreating] edges. *)
  }

and cfg

(** A function is a control-flow graph with distinguished entry block, whose
    arguments are the function arguments. *)
and func = private {name: Global.t; entry: block; cfg: cfg}

type t = private
  { typ_defns: Typ.t list  (** Type definitions. *)
  ; globals: Global.t vector  (** Global variable definitions. *)
  ; functions: func vector  (** (Global) function definitions. *) }

val mk :
  typ_defns:Typ.t list -> globals:Global.t list -> functions:func list -> t

val fmt : t fmt

module Inst : sig
  type t = inst

  val mkLoad : reg:Var.t -> ptr:Exp.t -> loc:Loc.t -> inst

  val mkStore : ptr:Exp.t -> exp:Exp.t -> loc:Loc.t -> inst

  val mkMemcpy : dst:Exp.t -> src:Exp.t -> len:Exp.t -> loc:Loc.t -> inst

  val mkMemmov : dst:Exp.t -> src:Exp.t -> len:Exp.t -> loc:Loc.t -> inst

  val mkMemset : dst:Exp.t -> byt:Exp.t -> len:Exp.t -> loc:Loc.t -> inst

  val mkAlloc : reg:Var.t -> num:Exp.t -> loc:Loc.t -> inst

  val mkFree : ptr:Exp.t -> loc:Loc.t -> inst

  val mkNondet : reg:Var.t option -> msg:string -> loc:Loc.t -> inst

  val fmt : t fmt
end

module Jump : sig
  type t = jump

  val mk : string -> Exp.t vector -> jump

  val fmt : jump fmt
end

module Term : sig
  type t = term

  val mkSwitch :
    key:Exp.t -> tbl:(Z.t * jump) vector -> els:jump -> loc:Loc.t -> term

  val mkISwitch : ptr:Exp.t -> tbl:jump vector -> loc:Loc.t -> term

  val mkCall :
    func:Exp.t -> args:Exp.t vector -> return:jump -> throw:jump option
    -> ignore_result:bool -> loc:Loc.t -> term

  val mkReturn : exp:Exp.t option -> loc:Loc.t -> term

  val mkThrow : exc:Exp.t -> loc:Loc.t -> term

  val mkUnreachable : term

  val fmt : t fmt
end

module Block : sig
  type t = block

  val mk :
    lbl:label -> params:Var.t vector -> cmnd:cmnd -> term:term -> block

  val fmt : t fmt

  val compare : t -> t -> int

  val hash : t -> int

  val sexp_of_t : t -> Sexp.t

  val t_of_sexp : Sexp.t -> t
end

module Func : sig
  type t = func

  val mk : name:Global.t -> entry:block -> cfg:block vector -> func

  val mk_undefined : name:Global.t -> params:Var.t vector -> t

  val find : func vector -> Global.t -> func option
  (** Look up a function of the given name in the given functions. *)

  val is_undefined : func -> bool
  (** Holds of functions that are declared but not defined. *)

  val fmt : t fmt
end
