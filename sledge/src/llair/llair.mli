(*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Translation units

    LLAIR (Low-Level Analysis Internal Representation) is an IR tailored for
    static analysis using a low-level model of memory. Compared to a
    compiler IR such as LLVM, an analyzer does not need to perform register
    allocation, instruction selection, code generation, etc. or even much
    code transformation, so the constraints on the IR are very different.

    LLAIR is a "Functional SSA" form where control transfers pass arguments
    instead of using ϕ-nodes. An analyzer will need good support for
    parameter passing anyhow, and ϕ-nodes make it hard to express program
    properties as predicates on states, since some execution history is
    needed to evaluate ϕ instructions. An alternative view is that the scope
    of variables [reg] assigned in instructions such as [Load] is the
    successor block as well as all blocks the instruction dominates in the
    control-flow graph. This language is first-order, and a term structure
    for the code constituting the scope of variables is not needed, so SSA
    rather than full CPS suffices.

    Additionally, the focus on memory analysis leads to a design where the
    arithmetic and logic operations are not "instructions" but instead are
    complex expressions (see [Exp]) that refer to registers (see [Var]). *)

(** Instructions for memory manipulation or other non-control effects. *)
type inst = private
  | Load of {reg: Var.t; ptr: Exp.t; len: Exp.t; loc: Loc.t}
      (** Read a [len]-byte value from the contents of memory at address
          [ptr] into [reg]. *)
  | Store of {ptr: Exp.t; exp: Exp.t; len: Exp.t; loc: Loc.t}
      (** Write [len]-byte value [exp] into memory at address [ptr]. *)
  | Memset of {dst: Exp.t; byt: Exp.t; len: Exp.t; loc: Loc.t}
      (** Store byte [byt] into [len] memory addresses starting from [dst]. *)
  | Memcpy of {dst: Exp.t; src: Exp.t; len: Exp.t; loc: Loc.t}
      (** Copy [len] bytes starting from address [src] to [dst], undefined
          if ranges overlap. *)
  | Memmov of {dst: Exp.t; src: Exp.t; len: Exp.t; loc: Loc.t}
      (** Copy [len] bytes starting from address [src] to [dst]. *)
  | Alloc of {reg: Var.t; num: Exp.t; len: Exp.t; loc: Loc.t}
      (** Allocate a block of memory large enough to store [num] elements of
          [len] bytes each and bind [reg] to the first address. *)
  | Free of {ptr: Exp.t; loc: Loc.t}
      (** Deallocate the previously allocated block at address [ptr]. *)
  | Nondet of {reg: Var.t option; msg: string; loc: Loc.t}
      (** Bind [reg] to an arbitrary value, representing non-deterministic
          approximation of behavior described by [msg]. *)
  | Abort of {loc: Loc.t}  (** Trigger abnormal program termination *)

(** A (straight-line) command is a sequence of instructions. *)
type cmnd = inst vector

(** A label is a name of a block. *)
type label = string

(** A jump with arguments. *)
type 'a control_transfer =
  { mutable dst: 'a
  ; args: Exp.t list  (** Stack of arguments, first-arg-last *)
  ; mutable retreating: bool
        (** Holds if [dst] is an ancestor in a depth-first traversal. *) }

(** A jump with arguments to a block. *)
type jump = block control_transfer

(** Block terminators for function call/return or other control transfers. *)
and term = private
  | Switch of {key: Exp.t; tbl: (Exp.t * jump) vector; els: jump; loc: Loc.t}
      (** Invoke the [jump] in [tbl] associated with the integer expression
          [case] which is equal to [key], if any, otherwise invoke [els]. *)
  | Iswitch of {ptr: Exp.t; tbl: jump vector; loc: Loc.t}
      (** Invoke the [jump] in [tbl] whose [dst] is equal to [ptr]. *)
  | Call of
      { call: Exp.t control_transfer  (** A [global] for non-virtual call. *)
      ; typ: Typ.t  (** Type of the callee. *)
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
  ; params: Var.t list  (** Formal parameters, first-param-last stack *)
  ; locals: Var.Set.t  (** Local variables, including [params]. *)
  ; cmnd: cmnd
  ; term: term
  ; mutable parent: func
  ; mutable sort_index: int
        (** Position in a topological order, ignoring [retreating] edges. *)
  }

and cfg

(** A function is a control-flow graph with distinguished entry block, whose
    parameters are the function parameters. *)
and func = private {name: Global.t; entry: block; cfg: cfg}

type functions

type t = private
  { globals: Global.t vector  (** Global variable definitions. *)
  ; functions: functions  (** (Global) function definitions. *) }

val pp : t pp

include Invariant.S with type t := t

val mk : globals:Global.t list -> functions:func list -> t

module Inst : sig
  type t = inst

  val pp : t pp
  val load : reg:Var.t -> ptr:Exp.t -> len:Exp.t -> loc:Loc.t -> inst
  val store : ptr:Exp.t -> exp:Exp.t -> len:Exp.t -> loc:Loc.t -> inst
  val memset : dst:Exp.t -> byt:Exp.t -> len:Exp.t -> loc:Loc.t -> inst
  val memcpy : dst:Exp.t -> src:Exp.t -> len:Exp.t -> loc:Loc.t -> inst
  val memmov : dst:Exp.t -> src:Exp.t -> len:Exp.t -> loc:Loc.t -> inst
  val alloc : reg:Var.t -> num:Exp.t -> len:Exp.t -> loc:Loc.t -> inst
  val free : ptr:Exp.t -> loc:Loc.t -> inst
  val nondet : reg:Var.t option -> msg:string -> loc:Loc.t -> inst
  val abort : loc:Loc.t -> inst
  val loc : inst -> Loc.t
  val locals : inst -> Var.Set.t
end

module Jump : sig
  type t = jump [@@deriving compare, equal, sexp_of]

  val pp : jump pp
  val mk : string -> Exp.t list -> jump
end

module Term : sig
  type t = term

  val pp : t pp

  val goto : dst:jump -> loc:Loc.t -> term
  (** Construct a [Switch] representing an unconditional branch. *)

  val branch : key:Exp.t -> nzero:jump -> zero:jump -> loc:Loc.t -> term
  (** Construct a [Switch] representing a conditional branch. *)

  val switch :
    key:Exp.t -> tbl:(Exp.t * jump) vector -> els:jump -> loc:Loc.t -> term

  val iswitch : ptr:Exp.t -> tbl:jump vector -> loc:Loc.t -> term

  val call :
       func:Exp.t
    -> typ:Typ.t
    -> args:Exp.t list
    -> return:jump
    -> throw:jump option
    -> ignore_result:bool
    -> loc:Loc.t
    -> term

  val return : exp:Exp.t option -> loc:Loc.t -> term
  val throw : exc:Exp.t -> loc:Loc.t -> term
  val unreachable : term
  val loc : term -> Loc.t
end

module Block : sig
  type t = block [@@deriving compare, equal, sexp_of]

  include Comparator.S with type t := t

  val pp : t pp

  include Invariant.S with type t := t

  val mk : lbl:label -> params:Var.t list -> cmnd:cmnd -> term:term -> block
end

module Func : sig
  type t = func

  val pp : t pp

  include Invariant.S with type t := t

  val mk : name:Global.t -> entry:block -> cfg:block vector -> func
  val mk_undefined : name:Global.t -> params:Var.t list -> t

  val find : functions -> Var.t -> func option
  (** Look up a function of the given name in the given functions. *)

  val is_undefined : func -> bool
  (** Holds of functions that are declared but not defined. *)
end
