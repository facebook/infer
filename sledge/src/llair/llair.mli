(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** LLAIR (Low-Level Analysis Internal Representation) is an IR tailored for
    static analysis using a low-level model of memory. *)

module Loc = Loc
module Typ = Typ
module Reg = Reg
module Exp = Exp
module FuncName = FuncName
module Global = Global
module GlobalDefn = GlobalDefn

val cct_schedule_points : bool ref

module Builtin : sig
  include module type of Builtins

  val of_name : string -> t option
  val pp : t pp
end

module Intrinsic : sig
  include module type of Intrinsics

  val of_name : string -> t option
  val pp : t pp
end

(** Instructions for memory manipulation or other non-control effects. *)
type inst = private
  | Move of {reg_exps: (Reg.t * Exp.t) iarray; loc: Loc.t}
      (** Move each value [exp] into corresponding register [reg]. All of
          the moves take effect simultaneously. *)
  | Load of {reg: Reg.t; ptr: Exp.t; len: Exp.t; loc: Loc.t}
      (** Read a [len]-byte value from the contents of memory at address
          [ptr] into [reg]. *)
  | Store of {ptr: Exp.t; exp: Exp.t; len: Exp.t; loc: Loc.t}
      (** Write [len]-byte value [exp] into memory at address [ptr]. *)
  | AtomicRMW of {reg: Reg.t; ptr: Exp.t; exp: Exp.t; len: Exp.t; loc: Loc.t}
      (** Atomically load a [len]-byte value [val] from the contents of
          memory at address [ptr], set [reg] to [val], and store the
          [len]-byte value of [(λreg. exp) val] into memory at address
          [ptr]. Note that, unlike other instructions and arguments,
          occurrences of [reg] in [exp] refer to the new, not old, value. *)
  | AtomicCmpXchg of
      { reg: Reg.t
      ; ptr: Exp.t
      ; cmp: Exp.t
      ; exp: Exp.t
      ; len: Exp.t
      ; len1: Exp.t
      ; loc: Loc.t }
      (** Atomically load a [len]-byte value from the contents of memory at
          address [ptr], compare the value to [cmp], and if equal store
          [len]-byte value [exp] into memory at address [ptr]. Sets [reg] to
          the loaded value concatenated to a [len1]-byte value [1] if the
          store was performed, otherwise [0]. *)
  | Alloc of {reg: Reg.t; num: Exp.t; len: int; loc: Loc.t}
      (** Allocate a block of memory large enough to store [num] elements of
          [len] bytes each and bind [reg] to the first address. *)
  | Free of {ptr: Exp.t; loc: Loc.t}
      (** Deallocate the previously allocated block at address [ptr]. *)
  | Nondet of {reg: Reg.t option; msg: string; loc: Loc.t}
      (** Bind [reg] to an arbitrary value, representing non-deterministic
          approximation of behavior described by [msg]. *)
  | Builtin of
      {reg: Reg.t option; name: Builtin.t; args: Exp.t iarray; loc: Loc.t}
      (** Bind [reg] to the value of applying builtin [name] to [args]. *)

(** A (straight-line) command is a sequence of instructions. *)
type cmnd = inst iarray

(** A label is a name of a block. *)
type label = string

(** A jump to a block. *)
type jump = private {mutable dst: block; mutable retreating: bool}

and call_target =
  { mutable func: func
  ; mutable recursive: bool
        (** Holds unless this call target is definitely not recursive *) }

and icall_target =
  { ptr: Exp.t  (** Dynamically resolved function pointer. *)
  ; mutable candidates: call_target iarray
        (** Statically computed over-approximation of possible call targets. *)
  }

and callee =
  | Direct of call_target  (** Statically resolved function *)
  | Indirect of icall_target  (** Dynamically resolved function *)
  | Intrinsic of Intrinsic.t
      (** Intrinsic implemented in analyzer rather than source code *)

(** A call to a function. *)
and 'a call =
  { callee: 'a
  ; typ: Typ.t  (** Type of the callee. *)
  ; actuals: Exp.t iarray  (** Actual arguments. *)
  ; areturn: Reg.t option  (** Register to receive return value. *)
  ; return: jump  (** Return destination. *)
  ; throw: jump option  (** Handler destination. *)
  ; loc: Loc.t }

(** Block terminators for function call/return or other control transfers. *)
and term = private
  | Switch of {key: Exp.t; tbl: (Exp.t * jump) iarray; els: jump; loc: Loc.t}
      (** Invoke the [jump] in [tbl] associated with the integer expression
          [case] which is equal to [key], if any, otherwise invoke [els]. *)
  | Iswitch of {ptr: Exp.t; tbl: jump iarray; loc: Loc.t}
      (** Invoke the [jump] in [tbl] whose [dst] is equal to [ptr]. *)
  | Call of callee call  (** Call function with arguments. *)
  | Return of {exp: Exp.t option; loc: Loc.t}
      (** Invoke [return] of the dynamically most recent [Call]. *)
  | Throw of {exc: Exp.t; loc: Loc.t}
      (** Invoke [throw] of the dynamically most recent [Call] with [throw]
          not [None]. *)
  | Abort of {loc: Loc.t}  (** Trigger abnormal program termination *)
  | Unreachable
      (** Halt as control is assumed to never reach [Unreachable]. *)

(** A block is a destination of a jump with arguments, contains code. *)
and block = private
  { lbl: label
  ; cmnd: cmnd
  ; term: term
  ; mutable parent: func
  ; mutable sort_index: int
        (** Position in a topological order, ignoring [retreating] edges. *)
  ; mutable goal_distance: int
        (** An upper bound on the distance from this block to the end of the
            current goal trace, measured in blocks. *) }

(** A function is a control-flow graph with distinguished entry block, whose
    parameters are the function parameters. *)
and func = private
  { name: FuncName.t
  ; formals: Reg.t iarray  (** Formal parameters *)
  ; freturn: Reg.t option
  ; fthrow: Reg.t
  ; locals: Reg.Set.t  (** Local registers *)
  ; entry: block
  ; loc: Loc.t }

type ip
type functions = func FuncName.Map.t

type program = private
  { globals: GlobalDefn.t iarray  (** Global definitions. *)
  ; functions: functions  (** (Global) function definitions. *) }

module Inst : sig
  type t = inst [@@deriving compare, equal]

  val pp : t pp
  val move : reg_exps:(Reg.t * Exp.t) iarray -> loc:Loc.t -> inst
  val load : reg:Reg.t -> ptr:Exp.t -> len:Exp.t -> loc:Loc.t -> inst
  val store : ptr:Exp.t -> exp:Exp.t -> len:Exp.t -> loc:Loc.t -> inst

  val atomic_rmw :
    reg:Reg.t -> ptr:Exp.t -> exp:Exp.t -> len:Exp.t -> loc:Loc.t -> inst

  val atomic_cmpxchg :
       reg:Reg.t
    -> ptr:Exp.t
    -> cmp:Exp.t
    -> exp:Exp.t
    -> len:Exp.t
    -> len1:Exp.t
    -> loc:Loc.t
    -> inst

  val alloc : reg:Reg.t -> num:Exp.t -> len:int -> loc:Loc.t -> inst
  val free : ptr:Exp.t -> loc:Loc.t -> inst
  val nondet : reg:Reg.t option -> msg:string -> loc:Loc.t -> inst

  val builtin :
       reg:Reg.t option
    -> name:Builtin.t
    -> args:Exp.t iarray
    -> loc:Loc.t
    -> t

  val loc : inst -> Loc.t
  val locals : inst -> Reg.Set.t
  val fold_exps : inst -> 's -> f:(Exp.t -> 's -> 's) -> 's
end

module Jump : sig
  type t = jump [@@deriving compare, equal, sexp_of]

  val pp : jump pp
  val mk : string -> jump
end

module Term : sig
  type t = term [@@deriving compare, equal]

  val pp : t pp
  val pp_callee : callee pp
  val invariant : ?parent:func -> t -> unit

  val goto : dst:jump -> loc:Loc.t -> term
  (** Construct a [Switch] representing an unconditional branch. *)

  val branch : key:Exp.t -> nzero:jump -> zero:jump -> loc:Loc.t -> term
  (** Construct a [Switch] representing a conditional branch. *)

  val switch :
    key:Exp.t -> tbl:(Exp.t * jump) iarray -> els:jump -> loc:Loc.t -> term

  val iswitch : ptr:Exp.t -> tbl:jump iarray -> loc:Loc.t -> term

  val call :
       name:string
    -> typ:Typ.t
    -> actuals:Exp.t iarray
    -> areturn:Reg.t option
    -> return:jump
    -> throw:jump option
    -> loc:Loc.t
    -> term * (callee:func -> unit)

  val icall :
       callee:Exp.t
    -> typ:Typ.t
    -> actuals:Exp.t iarray
    -> areturn:Reg.t option
    -> return:jump
    -> throw:jump option
    -> loc:Loc.t
    -> term * (candidates:func iarray -> unit)

  val intrinsic :
       callee:Intrinsic.t
    -> typ:Typ.t
    -> actuals:Exp.t iarray
    -> areturn:Reg.t option
    -> return:jump
    -> throw:jump option
    -> loc:Loc.t
    -> term

  val return : exp:Exp.t option -> loc:Loc.t -> term
  val throw : exc:Exp.t -> loc:Loc.t -> term
  val unreachable : term
  val abort : loc:Loc.t -> term
  val loc : term -> Loc.t
end

module Block : sig
  type t = block [@@deriving compare, equal, sexp_of]

  val pp : t pp
  val pp_ident : t pp
  val mk : lbl:label -> cmnd:cmnd -> term:term -> block
  val set_goal_distance : int -> t -> unit
  val iter : t -> ip iter

  module Map : Map.S with type key := t
  module Tbl : HashTable.S with type key := t
end

module IP : sig
  type t = ip [@@deriving compare, equal, sexp_of]

  val pp : t pp
  val mk : block -> t
  val block : t -> block
  val index : t -> int
  val inst : t -> inst option
  val loc : t -> Loc.t
  val succ : t -> t
  val is_schedule_point : t -> bool

  module Tbl : HashTable.S with type key := t
end

module Func : sig
  type t = func [@@deriving compare, equal]

  val pp : t pp
  val pp_call : t call pp

  include Invariant.S with type t := t

  val mk :
       name:FuncName.t
    -> formals:Reg.t iarray
    -> freturn:Reg.t option
    -> fthrow:Reg.t
    -> entry:block
    -> cfg:block iarray
    -> loc:Loc.t
    -> t

  val mk_undefined :
       name:FuncName.t
    -> formals:Reg.t iarray
    -> freturn:Reg.t option
    -> fthrow:Reg.t
    -> loc:Loc.t
    -> t

  val find : string -> functions -> t option
  (** Look up a function of the given name in the given functions. *)

  val fold_cfg : func -> 'a -> f:(block -> 'a -> 'a) -> 'a
  (** Fold over the blocks of the control-flow graph of a function. *)

  val is_undefined : t -> bool
  (** Holds of functions that are declared but not defined. *)
end

module Program : sig
  type t = program

  val pp : t pp

  include Invariant.S with type t := t

  val mk : globals:GlobalDefn.t list -> functions:func list -> t
end
