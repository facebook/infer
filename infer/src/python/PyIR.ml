(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
module Debug = PyDebug
module Builtin = PyBuiltin
module SMap = PyCommon.SMap
module IMap = PyCommon.IMap
module Const = FFI.Constant

(* will probably disappear in a next diff *)

module ConstMap = Caml.Map.Make (Const)

module Ident : sig
  type t

  val mk : string -> t

  val pp : F.formatter -> t -> unit
end = struct
  type t = string

  let pp fmt ident = F.pp_print_string fmt ident

  let mk ident = ident
end

module ScopedIdent = struct
  type scope = Global | Fast | Name | Deref

  type t = {scope: scope; ident: Ident.t}

  let pp fmt {scope; ident} =
    match scope with
    | Global ->
        F.fprintf fmt "GLOBAL[%a]" Ident.pp ident
    | Fast ->
        F.fprintf fmt "LOCAL[%a]" Ident.pp ident
    | Name ->
        F.fprintf fmt "TOPLEVEL[%a]" Ident.pp ident
    | Deref ->
        F.fprintf fmt "DEREF[%a]" Ident.pp ident
end

module QualName : sig
  type t

  val from_qualified_string : sep:char -> string -> t

  val extend : t -> string -> t

  val pp : F.formatter -> t -> unit

  module Map : Caml.Map.S with type key = t
end = struct
  type t = {root: string; path: string list} [@@deriving compare]

  let pp fmt {root; path} =
    if List.is_empty path then F.pp_print_string fmt root
    else F.fprintf fmt "%s.%a" root (Pp.seq ~sep:"." F.pp_print_string) (List.rev path)


  let from_qualified_string ~sep s =
    let l = String.split ~on:sep s in
    match l with
    | [] ->
        L.die ExternalError "QualName.from_qualified_string with an empty string"
    | hd :: tl ->
        {root= hd; path= List.rev tl}


  let extend {root; path} attr = {root; path= attr :: path}

  module Map = Caml.Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

module NodeName : sig
  type t

  val mk : string -> t

  val pp : F.formatter -> t -> unit

  module Map : Caml.Map.S with type key = t
end = struct
  type t = string

  let pp fmt ident = F.pp_print_string fmt ident

  let mk ident = ident

  module Map = SMap
end

module SSA = struct
  type t = int

  let pp fmt i = F.fprintf fmt "n%d" i

  let next n = 1 + n
end

module BuiltinCaller = struct
  type format_function = Str | Repr | Ascii

  let show_format_function = function Str -> "str" | Repr -> "repr" | Ascii -> "ascii"

  let show_binary op =
    match (op : Builtin.binary_op) with
    | Add ->
        "Add"
    | And ->
        "And"
    | FloorDivide ->
        "FloorDivide"
    | LShift ->
        "LShift"
    | MatrixMultiply ->
        "MatrixMultiply"
    | Modulo ->
        "Modulo"
    | Multiply ->
        "Multiply"
    | Or ->
        "Or"
    | Power ->
        "Power"
    | RShift ->
        "RShift"
    | Subtract ->
        "Subtract"
    | TrueDivide ->
        "TrueDivide"
    | Xor ->
        "Xor"


  let show_unary op =
    match (op : Builtin.unary_op) with
    | Positive ->
        "Positive"
    | Negative ->
        "Negative"
    | Not ->
        "Not"
    | Invert ->
        "Invert"


  type t =
    | BuildClass  (** [LOAD_BUILD_CLASS] *)
    | Format
    | FormatFn of format_function
    | Inplace of Builtin.binary_op
    | ImportName of string
    | ImportFrom of string
    | Binary of Builtin.binary_op
    | Unary of Builtin.unary_op
    | Compare of Builtin.Compare.t
    | GetIter  (** [GET_ITER] *)
    | NextIter  (** [FOR_ITER] *)
    | HasNextIter  (** [FOR_ITER] *)
    | IterData  (** [FOR_ITER] *)
    | GetYieldFromIter  (** [GET_YIELD_FROM_ITER] *)
    | ListAppend  (** [LIST_APPEND] *)
    | SetAdd  (** [SET_ADD] *)
    | DictSetItem  (** [MAP_ADD] *)
    | Delete of ScopedIdent.t  (** [DELETE_FAST] & cie *)
    | DeleteAttr of string
    | DeleteSubscr
    | YieldFrom  (** [YIELD_FROM] *)
    | GetAwaitable  (** [GET_AWAITABLE] *)
    | UnpackEx  (** [UNPACK_EX] *)
    | GetPreviousException  (** [RAISE_VARARGS] *)

  let show = function
    | BuildClass ->
        "$BuildClass"
    | Format ->
        "$Format"
    | FormatFn fn ->
        sprintf "$FormatFn.%s" (show_format_function fn)
    | ImportName name ->
        sprintf "$ImportName(%s)" name
    | ImportFrom name ->
        sprintf "$ImportFrom(%s)" name
    | Binary op ->
        let op = show_binary op in
        sprintf "$Binary.%s" op
    | Inplace op ->
        let op = show_binary op in
        sprintf "$Inplace.%s" op
    | Unary op ->
        let op = show_unary op in
        sprintf "$Unary.%s" op
    | Compare op ->
        sprintf "$Compare.%s" (Builtin.Compare.to_string op)
    | GetIter ->
        "$GetIter"
    | NextIter ->
        "$NextIter"
    | HasNextIter ->
        "$HasNextIter"
    | IterData ->
        "$IterData"
    | GetYieldFromIter ->
        "$GetYieldFromIter"
    | ListAppend ->
        "$ListAppend"
    | SetAdd ->
        "$SetAdd"
    | DictSetItem ->
        "$DictSetItem"
    | Delete ident ->
        F.asprintf "$Delete(%a)" ScopedIdent.pp ident
    | DeleteAttr name ->
        sprintf "$DeleteAttr(%s)" name
    | DeleteSubscr ->
        "$DeleteSubscr"
    | YieldFrom ->
        "$YieldFrom"
    | GetAwaitable ->
        "$GetAwaitable"
    | UnpackEx ->
        "$UnpackEx"
    | GetPreviousException ->
        "GetPreviousException"
end

module Exp = struct
  type collection = List | Set | Tuple | Slice | Map | String

  let show_collection = function
    | List ->
        "list"
    | Set ->
        "set"
    | Tuple ->
        "tuple"
    | Slice ->
        "slice"
    | Map ->
        "map"
    | String ->
        "string"
  [@@warning "-unused-value-declaration"]


  (** An expression is an abstraction of the state of Python bytecode interpreter, waiting to be
      turned into Textual. During the translation from bytecode to expressions, we introduce SSA
      identifiers for "yet to be named" expressions.

      In this IR, name resolution is done so naming is not ambiguous. Also, we have reconstructed
      the CFG of the program, lost during Python compilation *)
  type t =
    | Const of Const.t
    | Var of ScopedIdent.t
    | Temp of SSA.t
    | Subscript of {exp: t; index: t}  (** foo[bar] *)
    | Collection of {kind: collection; values: t list; packed: bool}
        (** Helper for [BUILD_LIST] and other builder opcodes *)
    (* [packed] is tracking the [BUILD_*_UNPACK] that should be flatten in Pulse *)
    | ConstMap of t ConstMap.t
    | Function of
        { qual_name: QualName.t
        ; short_name: Ident.t
        ; code: FFI.Code.t
        ; default_values: t SMap.t
        ; annotations: t ConstMap.t }  (** Result of the [MAKE_FUNCTION] opcode *)
    | GetAttr of (t * string) (* foo.bar *)
    | LoadMethod of (t * string)  (** [LOAD_METHOD] *)
    | Ref of string  (** [LOAD_CLOSURE] *)
    | Not of t
    | BuiltinCaller of BuiltinCaller.t
    | ContextManagerExit of t
    (* TODO: maybe this one is not useful, but at least it is not harmful :)
       Maybe consider removing it *)
    | Packed of {exp: t; is_map: bool}
    | Yield of t

  let show = function
    | Const _ ->
        "Const"
    | Var _ ->
        "Var"
    | Temp _ ->
        "Temp"
    | Subscript _ ->
        "Subscript"
    | Collection _ ->
        "Collection"
    | ConstMap _ ->
        "ConstMap"
    | Function _ ->
        "Function"
    | GetAttr _ ->
        "GetAttr"
    | LoadMethod _ ->
        "LoadMethod"
    | Ref _ ->
        "Ref"
    | Not _ ->
        "Not"
    | BuiltinCaller _ ->
        "BuiltinCaller"
    | ContextManagerExit _ ->
        "ContextManagerExit"
    | Packed _ ->
        "Packed"
    | Yield _ ->
        "Yield"
  [@@warning "-unused-value-declaration"]


  let rec pp fmt = function
    | Const c ->
        Const.pp fmt c
    | Var scope_ident ->
        ScopedIdent.pp fmt scope_ident
    | Temp i ->
        SSA.pp fmt i
    | Subscript {exp; index} ->
        F.fprintf fmt "%a[@[%a@]]" pp exp pp index
    | Collection {kind; values; packed} -> (
        let packed = if packed then "(packed)" else "" in
        match kind with
        | List ->
            F.fprintf fmt "%s[@[%a@]]" packed (Pp.seq ~sep:", " pp) values
        | Set ->
            F.fprintf fmt "%s{@[%a@]}" packed (Pp.seq ~sep:", " pp) values
        | Map ->
            F.fprintf fmt "%s{|@[%a@]|}" packed (Pp.seq ~sep:", " pp) values
        | Tuple ->
            F.fprintf fmt "%s(@[%a@])" packed (Pp.seq ~sep:", " pp) values
        | String ->
            F.fprintf fmt "$Concat%s(%a)" packed (Pp.seq ~sep:", " pp) values
        | Slice ->
            F.fprintf fmt "%s[@[%a@]]" packed (Pp.seq ~sep:":" pp) values )
    | ConstMap map ->
        F.fprintf fmt "{@[" ;
        ConstMap.iter (fun key exp -> F.fprintf fmt "%a: %a, " Const.pp key pp exp) map ;
        F.fprintf fmt "@]}"
    | Function {qual_name; short_name; default_values} ->
        F.fprintf fmt "$FuncObj(%a, %a, {" Ident.pp short_name QualName.pp qual_name ;
        SMap.iter (fun key value -> F.fprintf fmt "(%s, %a); " key pp value) default_values ;
        F.pp_print_string fmt "})"
    | GetAttr (t, name) ->
        F.fprintf fmt "%a.%s" pp t name
    | LoadMethod (self, meth) ->
        F.fprintf fmt "$LoadMethod(%a, %s)" pp self meth
    | Ref s ->
        F.fprintf fmt "$Ref(%s)" s
    | Not exp ->
        F.fprintf fmt "$Not(%a)" pp exp
    | BuiltinCaller bc ->
        F.pp_print_string fmt (BuiltinCaller.show bc)
    | ContextManagerExit exp ->
        F.fprintf fmt "CM(%a).__exit__" pp exp
    | Packed {exp; is_map} ->
        F.fprintf fmt "$Packed%s(%a)" (if is_map then "Map" else "") pp exp
    | Yield exp ->
        F.fprintf fmt "$Yield(%a)" pp exp
end

module Location = struct
  type t = int option

  let pp fmt loc =
    let loc = Option.value ~default:(-1) loc in
    F.pp_print_int fmt loc


  let of_instruction {FFI.Instruction.starts_line} = starts_line

  let of_instructions = function instr :: _ -> of_instruction instr | [] -> None

  let of_code {FFI.Code.instructions} = of_instructions instructions
end

module Error = struct
  type kind =
    | EmptyStack of string
    | UnsupportedOpcode of string
    | MakeFunction of string * Exp.t
    | BuildConstKeyMapLength of int * int
    | BuildConstKeyMapKeys of Exp.t
    | CompareOp of int
    | CodeWithoutQualifiedName of FFI.Code.t
    | UnpackSequence of int
    | FormatValueSpec of Exp.t
    | NextOffsetMissing
    | MissingBackEdge of int * int
    | InvalidBackEdgeArity of (string * int * int)
    | CallKeywordNotString0 of Const.t
    | CallKeywordNotString1 of Exp.t
    | MakeFunctionInvalidDefaults of Exp.t
    | WithCleanupFinish of Exp.t
    | RaiseException of int
    | RaiseExceptionInvalid of int

  type t = L.error * Location.t * kind

  let pp_kind fmt = function
    | EmptyStack op ->
        F.fprintf fmt "Cannot %s, stack is empty" op
    | UnsupportedOpcode s ->
        F.fprintf fmt "Unsupported opcode: %s" s
    | MakeFunction (kind, exp) ->
        F.fprintf fmt "MAKE_FUNCTION: expected %s but got %a" kind Exp.pp exp
    | BuildConstKeyMapLength (m, n) ->
        F.fprintf fmt "BUILD_CONST_KEY_MAP: expected %d keys but got %d" m n
    | BuildConstKeyMapKeys exp ->
        F.fprintf fmt "BUILD_CONST_KEY_MAP: expect constant set of keys but got %a" Exp.pp exp
    | CompareOp n ->
        F.fprintf fmt "COMPARE_OP(%d): invalid operation" n
    | CodeWithoutQualifiedName {FFI.Code.co_name; co_firstlineno; co_filename} ->
        F.fprintf fmt "Unknown code object: %s, at line %d, in %s" co_name co_firstlineno
          co_filename
    | UnpackSequence n ->
        F.fprintf fmt "UNPACK_SEQUENCE: invalid count %d" n
    | FormatValueSpec exp ->
        F.fprintf fmt "FORMAT_VALUE: expected string literal or temporary, got %a" Exp.pp exp
    | NextOffsetMissing ->
        F.fprintf fmt "Jump to next instruction detected, but next instruction is missing"
    | MissingBackEdge (from, to_) ->
        F.fprintf fmt "Invalid absolute jump: missing target of back-edge from %d to %d" from to_
    | InvalidBackEdgeArity (name, expect, actual) ->
        F.fprintf fmt "Invalid backedge to #%s with arity mismatch (expecting %d but got %d)" name
          expect actual
    | CallKeywordNotString0 cst ->
        F.fprintf fmt "CALL_FUNCTION_KW: keyword is not a string: %a" Const.pp cst
    | CallKeywordNotString1 exp ->
        F.fprintf fmt "CALL_FUNCTION_KW: keyword is not a tuple of strings: %a" Exp.pp exp
    | MakeFunctionInvalidDefaults exp ->
        F.fprintf fmt "MAKE_FUNCTION: expecting tuple of default values but got %a" Exp.pp exp
    | WithCleanupFinish exp ->
        F.fprintf fmt "WITH_CLEANUP_FINISH/TODO: unsupported scenario with %a" Exp.pp exp
    | RaiseException n ->
        F.fprintf fmt "RAISE_VARARGS/TODO: Unsupported argc = %d" n
    | RaiseExceptionInvalid n ->
        F.fprintf fmt "RAISE_VARARGS: Invalid mode %d" n
end

module Stmt = struct
  (** Statements are an extension of Textual's statements: we still keep some specific Python
      behavior around, like the difference between [CALL_FUNCTION] and [CALL_METHOD] *)
  type call_arg = {name: string option; value: Exp.t}

  let pp_call_arg fmt {name; value} =
    match name with
    | None ->
        Exp.pp fmt value
    | Some name ->
        F.fprintf fmt "%s= %a" name Exp.pp value


  let unnamed_call_arg value = {name= None; value}

  let unnamed_call_args args = List.map ~f:unnamed_call_arg args

  type t =
    | Assign of {lhs: Exp.t; rhs: Exp.t}
    | Call of {lhs: SSA.t; exp: Exp.t; args: call_arg list; packed: bool}
    | CallMethod of {lhs: SSA.t; call: Exp.t; args: Exp.t list}
    | BuiltinCall of {lhs: SSA.t; call: BuiltinCaller.t; args: Exp.t list}
    | SetupAnnotations

  let pp fmt = function
    | Assign {lhs; rhs} ->
        F.fprintf fmt "%a <- %a" Exp.pp lhs Exp.pp rhs
    | Call {lhs; exp; args; packed} ->
        F.fprintf fmt "%a <- %a(@[%a@])%s" SSA.pp lhs Exp.pp exp (Pp.seq ~sep:", " pp_call_arg) args
          (if packed then " !packed" else "")
    | CallMethod {lhs; call; args} ->
        F.fprintf fmt "%a <- $CallMethod(%a, @[%a@])" SSA.pp lhs Exp.pp call
          (Pp.seq ~sep:", " Exp.pp) args
    | BuiltinCall {lhs; call; args} ->
        F.fprintf fmt "%a <- %s(@[%a@])" SSA.pp lhs (BuiltinCaller.show call)
          (Pp.seq ~sep:", " Exp.pp) args
    | SetupAnnotations ->
        F.pp_print_string fmt "$SETUP_ANNOTATIONS"
end

module Offset = struct
  type t = int

  let pp fmt n = F.pp_print_int fmt n

  let of_code instructions =
    match instructions with FFI.Instruction.{offset} :: _ -> Some offset | _ -> None


  let get ~loc opt = Result.of_option ~error:(L.InternalError, loc, Error.NextOffsetMissing) opt
end

module Block = struct
  (** The Python interpreter keeps a stack of "blocks" to store information about finally and except
      handlers *)
  type block_type = Normal | Except | Finally

  let show_block_type = function Normal -> "N" | Except -> "E" | Finally -> "F"

  type t = {block_type: block_type}

  let show {block_type} = show_block_type block_type

  let mk block_type = {block_type}

  let is_except {block_type} = match block_type with Except -> true | _ -> false
end

module Stack = struct
  type 'a t = 'a list

  let empty = []

  let size stack = List.length stack

  let push stack exp = exp :: stack

  let pop = function [] -> None | elt :: stack -> Some (elt, stack)

  let rec peek n = function
    | [] ->
        None
    | elt :: rest ->
        if Int.equal 0 n then Some elt else peek (n - 1) rest
end

module Label = struct
  type name = string

  let pp_name fmt name = F.pp_print_string fmt name

  type handler_type = Label | Finally | Except

  let pp_handler_type fmt = function
    | Label ->
        F.pp_print_string fmt ".label"
    | Finally ->
        F.pp_print_string fmt ".finally"
    | Except ->
        F.pp_print_string fmt ".except"


  (** A label is a position in the bytecode where the control-flow can jump. Think "if/then/else",
      loops, ... Some Python opcode are quite involved in how the stack might be modified during
      jumps. For example, [FOR_ITER] might leave a value on the stack at all time, popping it only
      on exit.

      To account for such complex scenario, each label has a [prelude] to modify the stack and the
      state in non-trivial ways. To avoid a full-recursive type definition between [State] and
      [Label], we generalize [prelude] to be ['a -> 'a] and we'll instantiate it with [State.t]
      later on *)
  type 'a t =
    { name: name
    ; ssa_parameters: SSA.t list
    ; processed: bool
    ; handler_type: handler_type
    ; block_stack: Block.t Stack.t
    ; prelude: ('a t -> 'a -> 'a) option }

  let pp fmt {name; ssa_parameters; handler_type} =
    F.fprintf fmt "#%s" name ;
    if not (List.is_empty ssa_parameters) then
      F.fprintf fmt "(%a)" (Pp.seq ~sep:", " SSA.pp) ssa_parameters ;
    F.fprintf fmt " %a" pp_handler_type handler_type


  let mk ?(handler_type = Label) ?prelude name ssa_parameters block_stack =
    {name; ssa_parameters; processed= false; handler_type; prelude; block_stack}


  let is_processed {processed} = processed

  let is_except {handler_type} = match handler_type with Except -> true | _ -> false

  let process label ssa_parameters = {label with processed= true; ssa_parameters}

  let set_handler_type label handler_type = {label with handler_type}

  let set_last_block_type ({block_stack} as label) handler_type =
    let block_stack =
      match Stack.pop block_stack with
      | None ->
          block_stack
      | Some (_, block_stack) ->
          Stack.push block_stack (Block.mk handler_type)
    in
    {label with block_stack}
end

module Jump = struct
  type 'a info = {label: 'a Label.t; offset: Offset.t; ssa_args: Exp.t list}

  let pp_info fmt {label; offset; ssa_args} =
    F.fprintf fmt "JumpInfo(%a, %a, %d)" Label.pp label Offset.pp offset (List.length ssa_args)


  type 'a t =
    | Absolute of 'a info
    | Label of 'a info
    | Return of Exp.t
    | TwoWay of {condition: Exp.t; next_info: 'a info; other_info: 'a info}
    | Throw of Exp.t

  let pp fmt = function
    | Absolute info ->
        F.fprintf fmt "Absolute(%a)" pp_info info
    | Label info ->
        F.fprintf fmt "Label(%a)" pp_info info
    | Return ret ->
        F.fprintf fmt "Return(%a)" Exp.pp ret
    | TwoWay {condition; next_info; other_info} ->
        F.fprintf fmt "TwoWay(%a, %a, %a)" Exp.pp condition pp_info next_info pp_info other_info
    | Throw exp ->
        F.fprintf fmt "Throw(%a)" Exp.pp exp
  [@@warning "-unused-value-declaration"]
end

module Terminator = struct
  type node_call = {label: Label.name; ssa_args: Exp.t list}

  let pp_node_call fmt {label; ssa_args} =
    Label.pp_name fmt label ;
    if not (List.is_empty ssa_args) then F.fprintf fmt "(@[%a@])" (Pp.seq ~sep:", " Exp.pp) ssa_args


  type t =
    | Return of Exp.t
    | Jump of node_call list  (** non empty list *)
    | If of {exp: Exp.t; then_: t; else_: t}
    | Throw of Exp.t

  let rec pp fmt = function
    | Return exp ->
        F.fprintf fmt "return %a" Exp.pp exp
    | Jump lst ->
        F.fprintf fmt "jmp %a" (Pp.seq ~sep:", " pp_node_call) lst
    | If {exp; then_; else_} ->
        F.fprintf fmt "if %a then @[%a@] else @[%a@]" Exp.pp exp pp then_ pp else_
    | Throw exp ->
        F.fprintf fmt "throw %a" Exp.pp exp


  let of_jump jump_info =
    let node_info {Jump.label; ssa_args} =
      let {Label.name} = label in
      {label= name; ssa_args}
    in
    let jmp node = Jump [node_info node] in
    match jump_info with
    | `OneTarget info ->
        jmp info
    | `TwoTargets (exp, then_, else_) ->
        let then_ = jmp then_ in
        let else_ = jmp else_ in
        If {exp; then_; else_}
end

module CFGBuilder = struct
  type 'a t = {labels: 'a Label.t IMap.t; fresh_label: int}

  let fresh_start = 0

  let empty = {labels= IMap.empty; fresh_label= fresh_start}

  let label_of_int i = sprintf "b%d" i

  let fresh_label ({fresh_label} as cfg) =
    let fresh = label_of_int fresh_label in
    let cfg = {cfg with fresh_label= fresh_label + 1} in
    (fresh, cfg)


  let lookup_label {labels} offset = IMap.find_opt offset labels

  let process_label ({labels} as cfg) offset ssa_parameters =
    match IMap.find_opt offset labels with
    | None ->
        cfg
    | Some label ->
        let label = Label.process label ssa_parameters in
        let labels = IMap.add offset label labels in
        {cfg with labels}


  let set_handler_type ({labels} as cfg) offset handler_type =
    match IMap.find_opt offset labels with
    | None ->
        cfg
    | Some label ->
        let label = Label.set_handler_type label handler_type in
        let labels = IMap.add offset label labels in
        {cfg with labels}


  let set_last_block_type ({labels} as cfg) offset handler_type =
    match IMap.find_opt offset labels with
    | None ->
        (None, cfg)
    | Some label ->
        let label = Label.set_last_block_type label handler_type in
        let labels = IMap.add offset label labels in
        (Some label, {cfg with labels})


  (** Helper to fetch label info if there's already one registered at the specified [offset], or
      create a fresh one. *)
  let get_label cfg ?prelude ?handler_type offset ~ssa_parameters block_stack =
    match lookup_label cfg offset with
    | Some label ->
        (false, label, cfg)
    | None ->
        let name, cfg = fresh_label cfg in
        let label = Label.mk name ?prelude ?handler_type ssa_parameters block_stack in
        (true, label, cfg)


  let register_label ({labels} as cfg) ~offset label =
    let labels = IMap.add offset label labels in
    {cfg with labels}
end

module NodeBuilder = struct
  (** Linear block of code, without any jump. Starts with a label definition and ends with a
      terminator (jump, return, ...) *)
  type 'a t =
    { label: 'a Label.t
    ; label_loc: Location.t
    ; last_loc: Location.t
    ; stmts: (Location.t * Stmt.t) list
    ; last: Terminator.t }

  let pp fmt {label; stmts; last} =
    F.fprintf fmt "@[<hv2>%a:@\n" Label.pp label ;
    List.iter stmts ~f:(fun (_, stmt) -> F.fprintf fmt "@[<hv2>%a@]@\n" Stmt.pp stmt) ;
    F.fprintf fmt "%a@\n" Terminator.pp last ;
    F.fprintf fmt "@]@\n"
  [@@warning "-unused-value-declaration"]
end

module CFG = struct
  module Node = struct
    type t =
      { name: NodeName.t
      ; first_loc: Location.t
      ; last_loc: Location.t
      ; stmts: (Location.t * Stmt.t) list
      ; last: Terminator.t }

    let pp fmt {name; stmts; last} =
      F.fprintf fmt "@[<hv2>%a:@\n" NodeName.pp name ;
      List.iter stmts ~f:(fun (_, stmt) -> F.fprintf fmt "@[<hv2>%a@]@\n" Stmt.pp stmt) ;
      F.fprintf fmt "%a@\n" Terminator.pp last ;
      F.fprintf fmt "@]@\n"


    let of_node_builder {NodeBuilder.label; label_loc; last_loc; stmts; last} =
      {name= NodeName.mk label.Label.name; first_loc= label_loc; last_loc; stmts; last}
  end

  type t = {entry: NodeName.t; nodes: Node.t NodeName.Map.t}

  let pp fmt {nodes} = NodeName.Map.iter (fun _ node -> Node.pp fmt node) nodes

  let of_node_builder_list nodes =
    let entry = CFGBuilder.label_of_int CFGBuilder.fresh_start |> NodeName.mk in
    let nodes =
      List.fold nodes ~init:NodeName.Map.empty ~f:(fun map node_builder ->
          let ({Node.name} as node) = Node.of_node_builder node_builder in
          NodeName.Map.add name node map )
    in
    {nodes; entry}
end

module State = struct
  (*
  (** Bytecode offset of except/finally handlers *)
  type exn_handler = {offset: int}
     *)

  (** Internal state of the Bytecode -> IR compiler *)
  type t =
    { debug: bool
    ; code_qual_name: FFI.Code.t -> QualName.t option
    ; loc: Location.t
    ; cfg: t CFGBuilder.t
          (* TODO:
             ; exn_handlers: exn_handler list
                   (** Stack of except/finally handlers info. The data we store here will be used (TODO) to
                       inform Textual about statements that can raise exceptions. *)
          *)
    ; stack: Exp.t Stack.t
    ; block_stack: Block.t Stack.t
    ; stmts: (Location.t * Stmt.t) list
    ; fresh_id: SSA.t }

  let empty ~debug ~code_qual_name ~loc =
    let p = if debug then Debug.todo else Debug.p in
    let block_stack = Stack.push Stack.empty (Block.mk Normal) in
    p "State.empty@\n" ;
    { debug
    ; code_qual_name
    ; loc
    ; cfg= CFGBuilder.empty (* ; exn_handlers= [] *)
    ; stack= Stack.empty
    ; block_stack
    ; stmts= []
    ; fresh_id= 0 }


  let debug {debug} = if debug then Debug.todo else Debug.p

  (** Each time a new object is discovered (they can be heavily nested), we need to clear the state
      of temporary data like the SSA counter, but keep other parts of it, like global and local
      names *)
  let enter ~debug ~code_qual_name ~loc = empty ~debug ~code_qual_name ~loc

  let fresh_id ({fresh_id} as st) =
    let st = {st with fresh_id= SSA.next fresh_id} in
    (fresh_id, st)


  let push ({stack} as st) exp =
    let stack = Stack.push stack exp in
    {st with stack}


  let push_block ({block_stack} as st) block =
    debug st "push-block: %s@\n" (Block.show block) ;
    let block_stack = Stack.push block_stack block in
    {st with block_stack}


  let pop ({stack; loc} as st) =
    match Stack.pop stack with
    | None ->
        Error (L.InternalError, loc, Error.EmptyStack "pop")
    | Some (exp, stack) ->
        let st = {st with stack} in
        Ok (exp, st)


  let pop_block ({block_stack; loc} as st) =
    match Stack.pop block_stack with
    | None ->
        Error (L.InternalError, loc, Error.EmptyStack "pop (block)")
    | Some (block, block_stack) ->
        debug st "pop-block: %s@\n" (Block.show block) ;
        let st = {st with block_stack} in
        Ok (block, st)


  let peek ?(depth = 0) {stack; loc} =
    match Stack.peek depth stack with
    | None ->
        Error (L.InternalError, loc, Error.EmptyStack "peek (block)")
    | Some exp ->
        Ok exp


  let pop_n st n =
    let rec aux acc st n =
      let open IResult.Let_syntax in
      if n > 0 then
        let* hd, st = pop st in
        aux (hd :: acc) st (n - 1)
      else Ok (acc, st)
    in
    aux [] st n


  (* TODO: use the [exn_handlers] info to mark statement that can possibly raise * something *)
  let push_stmt ({stmts; loc} as st) stmt = {st with stmts= (loc, stmt) :: stmts}

  let size {stack} = Stack.size stack

  (** When reaching a jump instruction, turn the stack into ssa variables. We only turn the last
      block into variables, as the current code shouldn't jump outside of its "block" *)
  let to_ssa ({stack} as st) =
    let st = {st with stack= Stack.empty} in
    Ok (stack, st)


  let mk_ssa_parameters st arity =
    let rec mk st acc n =
      if n > 0 then
        let i, st = fresh_id st in
        mk st (i :: acc) (n - 1)
      else (acc, st)
    in
    mk st [] arity


  let register_label ({cfg} as st) ~offset label =
    debug st "register_label %d %a@\n" offset Label.pp label ;
    let cfg = CFGBuilder.register_label cfg ~offset label in
    {st with cfg}


  let lookup_label {cfg} offset = CFGBuilder.lookup_label cfg offset

  let get_label ({cfg; block_stack} as st) ?prelude ?fix_type ?handler_type offset ~ssa_parameters =
    debug st "get_label at offset %d BLOCK_LAYOUT %s@\n" offset
      (String.concat ~sep:"" @@ List.map ~f:Block.show block_stack) ;
    let block_stack =
      match (handler_type : Label.handler_type option) with
      | None | Some Label ->
          block_stack
      | Some Except ->
          Stack.push block_stack (Block.mk Except)
      | Some Finally ->
          Stack.push block_stack (Block.mk Finally)
    in
    let new_label, label, cfg =
      CFGBuilder.get_label cfg ?prelude ?handler_type offset ~ssa_parameters block_stack
    in
    let label, cfg =
      match fix_type with
      | None ->
          (label, cfg)
      | Some fix_type ->
          let opt_label, cfg = CFGBuilder.set_last_block_type cfg offset fix_type in
          let label = Option.value ~default:label opt_label in
          (label, cfg)
    in
    let st = {st with cfg} in
    let st = if new_label then register_label st ~offset label else st in
    (label, st)


  let process_label ({cfg} as st) offset ssa_parameters =
    let cfg = CFGBuilder.process_label cfg offset ssa_parameters in
    {st with cfg}


  let set_handler_type ({cfg} as st) offset handler_type =
    let cfg = CFGBuilder.set_handler_type cfg offset handler_type in
    {st with cfg}


  let fresh_label ({cfg} as st) =
    let fresh_label, cfg = CFGBuilder.fresh_label cfg in
    let st = {st with cfg} in
    (fresh_label, st)


  let instr_is_jump_target ({cfg; block_stack} as st) {FFI.Instruction.offset; is_jump_target} =
    match CFGBuilder.lookup_label cfg offset with
    | Some label ->
        let info = if Label.is_processed label then None else Some (offset, label) in
        (info, st)
    | None ->
        if is_jump_target then
          (* Probably the target of a back edge. Let's register a target with the current stack
             information. When we'll detect the jump to here, we'll make sure things are
             compatible *)
          let arity = size st in
          let ssa_parameters, st = mk_ssa_parameters st arity in
          let name, st = fresh_label st in
          let label = Label.mk name ssa_parameters block_stack in
          let st = register_label st ~offset label in
          (Some (offset, label), st)
        else (None, st)


  let starts_with_jump_target st = function
    | [] ->
        (None, st)
    | instr :: _ ->
        instr_is_jump_target st instr


  let enter_node st ({Label.ssa_parameters; prelude; block_stack} as label) =
    debug st "enter-node: Label %a@\n" Label.pp label ;
    debug st "stack size upon entry: %d\n" (size st) ;
    (* Drain statements, stack and the current block *)
    let st = {st with stmts= []; block_stack} in
    let st =
      if Label.is_except label then
        let () = debug st "> is .except hander@\n" in
        let block = Block.mk Except in
        push_block st block
      else
        let () = debug st "> is not .except hander@\n" in
        let st = {st with stack= []} in
        st
    in
    (* If we have ssa_parameters, we need to push them on the stack to restore its right shape.
       Doing a fold_right is important to keep the correct order. *)
    let st = List.fold_right ssa_parameters ~init:st ~f:(fun ssa st -> push st (Exp.Temp ssa)) in
    (* Install the prelude before processing the instructions *)
    let st = match prelude with None -> st | Some f -> f label st in
    debug st "> current stack size: %d\n" (size st) ;
    st


  let drain_stmts ({stmts} as st) =
    let st = {st with stmts= []} in
    (List.rev stmts, st)
end

let error kind {State.loc} err = Error (kind, loc, err)

let external_error st err = error L.ExternalError st err

let internal_error st err = error L.InternalError st err

let read_code_qual_name st c =
  match st.State.code_qual_name c with
  | Some qual_name ->
      Ok qual_name
  | None ->
      internal_error st (Error.CodeWithoutQualifiedName c)


let call_function_with_unnamed_args st ?(packed = false) exp args =
  let args = Stmt.unnamed_call_args args in
  let lhs, st = State.fresh_id st in
  let stmt = Stmt.Call {lhs; exp; args; packed} in
  let st = State.push_stmt st stmt in
  (lhs, st)


let call_builtin_function st call args =
  let lhs, st = State.fresh_id st in
  let stmt = Stmt.BuiltinCall {lhs; call; args} in
  let st = State.push_stmt st stmt in
  (lhs, st)


(** Helper to compile the binary/unary/... ops into IR *)
let parse_op st call n =
  let open IResult.Let_syntax in
  let* args, st = State.pop_n st n in
  let lhs, st = call_builtin_function st call args in
  let st = State.push st (Exp.Temp lhs) in
  Ok (st, None)


(** Helper to compile the BUILD_ opcodes into IR *)
let build_collection st kind count =
  let open IResult.Let_syntax in
  let* values, st = State.pop_n st count in
  let exp = Exp.Collection {kind; values; packed= false} in
  let st = State.push st exp in
  Ok (st, None)


(** In Python, named arguments are always "at the end":

    - when a function call has named arguments, any positional argument must come first
    - when a function definition has a default value for any argument, then all arguments on its
      right must have a default value too. For this purpose, we implement a special "zip" function
      that takes data and tags, and will correctly zip them depending on their "positional" vs
      "named" status.

    [argc] is the _total_ number of arguments (both positional and named). [f] is a function to
    build the resulting type from the positional or named data. *)
let named_argument_zip opname argc ~f ~init data tags =
  let nr_positional = argc - List.length tags in
  let rec zip pos data tags =
    match (data, tags) with
    | [], [] ->
        init
    | [], _ :: _ ->
        L.die InternalError "%s: less data than tags" opname
    | _ :: _, [] ->
        L.die InternalError "%s: not enough tags" opname
    | data :: remaining_data, tag :: remaining_tags ->
        if pos < nr_positional then
          let tl = zip (1 + pos) remaining_data tags in
          f (`Positional data) tl
        else
          let tl = zip (1 + pos) remaining_data remaining_tags in
          f (`Named (data, tag)) tl
  in
  zip 0 data tags


let make_function st flags =
  let open IResult.Let_syntax in
  let* _qual_name, st = State.pop st in
  (* we use our own notion of qualified name *)
  let* codeobj, st = State.pop st in
  let* code =
    match (codeobj : Exp.t) with
    | Const (PYCCode c) ->
        Ok c
    | _ ->
        internal_error st (Error.MakeFunction ("a code object", codeobj))
  in
  let* qual_name = read_code_qual_name st code in
  let short_name = Ident.mk code.FFI.Code.co_name in
  let* st =
    if flags land 0x08 <> 0 then
      (* TODO: closure *)
      let* _cells_for_closure, st = State.pop st in
      Ok st
    else Ok st
  in
  let* annotations, st =
    if flags land 0x04 <> 0 then
      let* annotations, st = State.pop st in
      let* annotations =
        match (annotations : Exp.t) with
        | ConstMap map ->
            Ok map
        | _ ->
            internal_error st (Error.MakeFunction ("some type annotations", annotations))
      in
      Ok (annotations, st)
    else Ok (ConstMap.empty, st)
  in
  let* st =
    if flags land 0x02 <> 0 then
      (* TODO: kw defaults *)
      let* _kw_defaults, st = State.pop st in
      Ok st
    else Ok st
  in
  let* default_values, st =
    if flags land 0x01 <> 0 then
      let* defaults, st = State.pop st in
      match (defaults : Exp.t) with
      | Const (PYCTuple defaults) ->
          let fn_args = FFI.Code.get_arguments code |> Array.to_list in
          let argc = List.length fn_args in
          let defaults = Array.to_list defaults in
          let defaults =
            named_argument_zip "MAKE_FUNCTION" argc ~init:SMap.empty
              ~f:(fun hd tl ->
                match hd with
                | `Positional _ ->
                    tl
                | `Named (argname, const) ->
                    SMap.add argname (Exp.Const const) tl )
              fn_args defaults
          in
          Ok (defaults, st)
      | Collection {kind= Tuple; values} ->
          let fn_args = FFI.Code.get_arguments code |> Array.to_list in
          let argc = List.length fn_args in
          let defaults =
            named_argument_zip "MAKE_FUNCTION" argc ~init:SMap.empty
              ~f:(fun hd tl ->
                match hd with
                | `Positional _ ->
                    tl
                | `Named (argname, const) ->
                    SMap.add argname const tl )
              fn_args values
          in
          Ok (defaults, st)
      | _ ->
          external_error st (Error.MakeFunctionInvalidDefaults defaults)
    else Ok (SMap.empty, st)
  in
  let exp = Exp.Function {annotations; short_name; default_values; qual_name; code} in
  let st = State.push st exp in
  Ok (st, None)


let call_function st arg =
  let open IResult.Let_syntax in
  let* args, st = State.pop_n st arg in
  let* fun_exp, st = State.pop st in
  let id, st = call_function_with_unnamed_args st fun_exp args in
  let st = State.push st (Exp.Temp id) in
  Ok (st, None)


let call_function_kw st argc =
  let open IResult.Let_syntax in
  let extract_kw_names st arg_names =
    match (arg_names : Exp.t) with
    | Const (PYCTuple tuple) ->
        (* kw names should be constant tuple of strings, so we directly access them *)
        Array.fold_right tuple ~init:(Ok []) ~f:(fun const acc ->
            let* acc in
            match Const.as_name const with
            | Some name ->
                Ok (name :: acc)
            | None ->
                external_error st (Error.CallKeywordNotString0 const) )
    | _ ->
        external_error st (Error.CallKeywordNotString1 arg_names)
  in
  let partial_zip args kwnames =
    named_argument_zip "CALL_FUNCTION_KW" argc ~init:[]
      ~f:(fun hd tl ->
        match hd with
        | `Positional value ->
            let hd = Stmt.unnamed_call_arg value in
            hd :: tl
        | `Named (value, name) ->
            let hd = {Stmt.name= Some name; value} in
            hd :: tl )
      args kwnames
  in
  let* arg_names, st = State.pop st in
  let* arg_names = extract_kw_names st arg_names in
  let* args, st = State.pop_n st argc in
  let args = partial_zip args arg_names in
  let* exp, st = State.pop st in
  let lhs, st = State.fresh_id st in
  let stmt = Stmt.Call {lhs; exp; args; packed= false} in
  let st = State.push_stmt st stmt in
  let st = State.push st (Exp.Temp lhs) in
  Ok (st, None)


let call_function_ex st flags =
  let open IResult.Let_syntax in
  let* opt_args, st =
    if flags land 1 <> 0 then
      let* exp, st = State.pop st in
      Ok ([Exp.Packed {exp; is_map= true}], st)
    else Ok ([], st)
  in
  let* packed_arg, st = State.pop st in
  let packed_arg =
    match (packed_arg : Exp.t) with Packed _ -> packed_arg | exp -> Exp.Packed {is_map= false; exp}
  in
  let* call, st = State.pop st in
  let id, st = call_function_with_unnamed_args st call ~packed:true (packed_arg :: opt_args) in
  let st = State.push st (Exp.Temp id) in
  Ok (st, None)


let unpack_sequence st count =
  let open IResult.Let_syntax in
  let* tos, st = State.pop st in
  let rec unpack st n =
    if n < 0 then Ok st
    else
      let index = Exp.Const (Const.of_int n) in
      let exp = Exp.Subscript {exp= tos; index} in
      let st = State.push st exp in
      unpack st (n - 1)
  in
  let* () = if count < 0 then external_error st (Error.UnpackSequence count) else Ok () in
  let* st = unpack st (count - 1) in
  Ok (st, None)


let format_value st flags =
  let open IResult.Let_syntax in
  let has_fmt_spec flags = Int.equal (flags land 0x04) 0x04 in
  let mk_conv flags =
    match flags land 0x03 with
    | 0x00 ->
        None
    | 0x01 ->
        Some BuiltinCaller.Str
    | 0x02 ->
        Some BuiltinCaller.Repr
    | 0x03 ->
        Some BuiltinCaller.Ascii
    | _ ->
        L.die InternalError "FORMAT_VALUE: unreachable"
  in
  let* fmt_spec, st =
    (* fmt_spec must be a concatenation of string literals *)
    if has_fmt_spec flags then
      let* fmt_spec, st = State.pop st in
      match (fmt_spec : Exp.t) with
      | Const (PYCString _) ->
          Ok (fmt_spec, st)
      | Collection {kind= String; values= _} ->
          Ok (fmt_spec, st)
      | Temp _ ->
          (* Usually it will be string literals, but it can also be the
             return value of a format function, so we have to allow
             temporaries too *)
          Ok (fmt_spec, st)
      | _ ->
          external_error st (Error.FormatValueSpec fmt_spec)
    else Ok (Exp.Const Const.none, st)
  in
  let* exp, st = State.pop st in
  let conv_fn = mk_conv flags in
  let exp, st =
    match conv_fn with
    | None ->
        (exp, st)
    | Some conv_fn ->
        let id, st = call_builtin_function st (FormatFn conv_fn) [exp] in
        (Exp.Temp id, st)
  in
  let id, st = call_builtin_function st Format [exp; fmt_spec] in
  let st = State.push st (Exp.Temp id) in
  Ok (st, None)


let pop_jump_if ~next_is st target next_offset_opt =
  let open IResult.Let_syntax in
  let {State.loc} = st in
  let* condition, st = State.pop st in
  (* Turn the stack into SSA parameters *)
  let arity = State.size st in
  let* ssa_args, st = State.to_ssa st in
  let next_ssa, st = State.mk_ssa_parameters st arity in
  let other_ssa, st = State.mk_ssa_parameters st arity in
  let* next_offset = Offset.get ~loc next_offset_opt in
  let next_label, st = State.get_label st next_offset ~ssa_parameters:next_ssa in
  let other_label, st = State.get_label st target ~ssa_parameters:other_ssa in
  (* Compute the relevant pruning expressions *)
  let condition = if next_is then condition else Exp.Not condition in
  Ok
    ( st
    , Some
        (Jump.TwoWay
           { condition
           ; next_info= {label= next_label; offset= next_offset; ssa_args}
           ; other_info= {label= other_label; offset= target; ssa_args} } ) )


let jump_if_or_pop st ~jump_if target next_offset_opt =
  let open IResult.Let_syntax in
  let {State.loc} = st in
  (* We only peek the top of stack so the logic of [to_ssa] correctly captures it for
     the branch where "it stays". It will be restored as part of the stack restore logic when
     we process the "other" node. *)
  let* condition = State.peek st in
  let condition = if jump_if then Exp.Not condition else condition in
  let arity = State.size st in
  let* ssa_args, st = State.to_ssa st in
  let next_ssa, st = State.mk_ssa_parameters st arity in
  let other_ssa, st = State.mk_ssa_parameters st arity in
  let* next_offset = Offset.get ~loc next_offset_opt in
  (* In the next branch, we make sure the top-of-stack is no longer in the ssa parameters
     so it is not restored, effectively dropped as per the opcode description. *)
  let next_label, st =
    let ssa_parameters = List.tl next_ssa |> Option.value ~default:[] in
    State.get_label st next_offset ~ssa_parameters
  in
  let next_ssa_args = List.tl ssa_args |> Option.value ~default:[] in
  let other_label, st = State.get_label st target ~ssa_parameters:other_ssa in
  Ok
    ( st
    , Some
        (Jump.TwoWay
           { condition
           ; next_info= {label= next_label; offset= next_offset; ssa_args= next_ssa_args}
           ; other_info= {label= other_label; offset= target; ssa_args} } ) )


(* TODO: maybe add pruning here ? *)
let for_iter st delta next_offset_opt =
  let open IResult.Let_syntax in
  let {State.loc} = st in
  let* iter, st = State.pop st in
  let id, st = call_builtin_function st NextIter [iter] in
  let has_item, st = call_builtin_function st HasNextIter [Exp.Temp id] in
  let condition = Exp.Temp has_item in
  let arity = State.size st in
  let* ssa_args, st = State.to_ssa st in
  let next_ssa, st = State.mk_ssa_parameters st arity in
  let other_ssa, st = State.mk_ssa_parameters st arity in
  (* In the next branch, we know the iterator has an item available. Let's fetch it and
     push it on the stack. *)
  let next_prelude _ st =
    (* The iterator object stays on the stack while in the for loop, let's push it back *)
    let st = State.push st iter in
    (* The result of calling [__next__] is also pushed on the stack *)
    let next, st = call_builtin_function st IterData [Exp.Temp id] in
    State.push st (Exp.Temp next)
  in
  let* next_offset = Offset.get ~loc next_offset_opt in
  let next_label, st =
    State.get_label st next_offset ~ssa_parameters:next_ssa ~prelude:next_prelude
  in
  let other_offset = delta + next_offset in
  let other_label, st = State.get_label st other_offset ~ssa_parameters:other_ssa in
  Ok
    ( st
    , Some
        (Jump.TwoWay
           { condition
           ; next_info= {label= next_label; offset= next_offset; ssa_args}
           ; other_info= {label= other_label; offset= other_offset; ssa_args} } ) )


(** Extract from Python3.8 specification:

    Starts cleaning up the stack when a with statement block exits. At the top of the stack are
    either [NULL] (pushed by [BEGIN_FINALLY]) or 6 values pushed if an exception has been raised in
    the with block. Below is the context manager’s [__exit__()] or [__aexit__()] bound method.

    If top-of-stack is [NULL], calls [SECOND(None, None, None)], removes the function from the
    stack, leaving top-of-stack, and pushes [NULL] to the stack. Otherwise calls
    [SEVENTH(TOP, SECOND, THIRD)], shifts the bottom 3 values of the stack down, replaces the empty
    spot with [NULL] and pushes top-of-stack. Finally pushes the result of the call.

    vsiles notes:

    - the doc is quite confusing. Here is my source of truth
      https://github.com/python/cpython/blob/3.8/Python/ceval.c#L3300
    - we only support the first case with [NULL]
    - we only support [__exit__].
    - TODO: learn how [__aexit__] works
    - TODO: duplicate the handler to deal with the except case *)
let with_cleanup_start st =
  let open IResult.Let_syntax in
  let* context_manager_exit, st = State.pop st in
  let lhs, st =
    call_function_with_unnamed_args st context_manager_exit
      [Const Const.none; Const Const.none; Const Const.none]
  in
  let st = State.push st (Const Const.none) in
  let st = State.push st (Exp.Temp lhs) in
  Ok (st, None)


(** Extract from Python3.8 specification:

    Finishes cleaning up the stack when a with statement block exits. top-of-stack is result of
    [__exit__()] or [__aexit__()] function call pushed by [WITH_CLEANUP_START]. [SECOND] is [None]
    or an exception type (pushed when an exception has been raised).

    Pops two values from the stack. If [SECOND] is not [NULL] and top-of-stack is true unwinds the
    [EXCEPT_HANDLER] block which was created when the exception was caught and pushes [NULL] to the
    stack.

    Note: we only support the [None] case for the moment. See
    https://github.com/python/cpython/blob/3.8/Python/ceval.c#L3373.

    TODO: duplicate the node and deal with the EXCEPT_HANDLER case *)
let with_cleanup_finish st =
  let open IResult.Let_syntax in
  let* _exit_res, st = State.pop st in
  let* tos, st = State.pop st in
  match (tos : Exp.t) with
  | Const PYCNone ->
      Ok (st, None)
  | _ ->
      internal_error st (Error.WithCleanupFinish tos)


let raise_varargs st argc =
  let open IResult.Let_syntax in
  match argc with
  | 0 ->
      (* There should be something like 3 values left on the stack at this
         point, leftovers from the 6 values pushed on the exception entry
         point. We should use them to compute the right "previous" exception,
         but this logic is left TODO *)
      let id, st = call_builtin_function st GetPreviousException [] in
      let throw = Jump.Throw (Exp.Temp id) in
      Ok (st, Some throw)
  | 1 ->
      let* tos, st = State.pop st in
      let throw = Jump.Throw tos in
      Ok (st, Some throw)
  | 2 ->
      internal_error st (Error.RaiseException argc)
  | _ ->
      external_error st (Error.RaiseExceptionInvalid argc)


let get_cell_name {FFI.Code.co_cellvars; co_freevars} arg =
  let sz = Array.length co_cellvars in
  if arg < sz then co_cellvars.(arg) else co_freevars.(arg - sz)


let build_collection_unpack st count collection =
  let open IResult.Let_syntax in
  let* values, st = State.pop_n st count in
  let values = List.map ~f:(fun exp -> Exp.Packed {exp; is_map= false}) values in
  let exp = Exp.Collection {kind= collection; values; packed= true} in
  let st = State.push st exp in
  Ok (st, None)


let collection_add st opname arg ?(map = false) builtin =
  (* LIST_APPEND(i) | SET_ADD(i) | MAP_ADD(i)

     Calls list.append(TOS1[-i], TOS).
     Calls set.add(TOS1[-i], TOS).
     Calls dict.__setitem__(TOS1[-i], TOS1, TOS).

     Used to implement list/set/dict comprehensions.

     For all of the [SET_ADD], [LIST_APPEND] and [MAP_ADD] instructions, while the added value or
     key/value pair is popped off, the container object remains on the stack so that it is
     available for further iterations of the loop. *)
  let open IResult.Let_syntax in
  if arg < 1 then L.die ExternalError "%s with %d" opname arg ;
  let* args, st =
    if map then
      let* value, st = State.pop st in
      let* key, st = State.pop st in
      Ok ([key; value], st)
    else
      let* elt, st = State.pop st in
      Ok ([elt], st)
  in
  let* tos1 = State.peek st ~depth:(arg - 1) in
  let _id, st = call_builtin_function st builtin (tos1 :: args) in
  Ok (st, None)


let parse_bytecode st ({FFI.Code.co_consts; co_names; co_varnames} as code)
    {FFI.Instruction.opname; starts_line; arg; offset= instr_offset} next_offset_opt =
  let open IResult.Let_syntax in
  let st =
    if Option.is_some starts_line then (
      State.debug st "@\n" ;
      State.debug st ".line %d@\n" (Option.value_exn starts_line) ;
      {st with State.loc= starts_line} )
    else st
  in
  State.debug st "%a: %s %d (0x%x) - STACK_SIZE %d - BLOCK_LAYOUT %s@\n" Offset.pp instr_offset
    opname arg arg (State.size st)
    (String.concat ~sep:"" @@ List.map ~f:Block.show st.State.block_stack) ;
  match opname with
  | "LOAD_CONST" ->
      let c = co_consts.(arg) in
      let exp = Exp.Const c in
      let st = State.push st exp in
      Ok (st, None)
  | "LOAD_NAME" ->
      let exp = Exp.Var {scope= Name; ident= Ident.mk co_names.(arg)} in
      let st = State.push st exp in
      Ok (st, None)
  | "LOAD_GLOBAL" ->
      let exp = Exp.Var {scope= Global; ident= Ident.mk co_names.(arg)} in
      let st = State.push st exp in
      Ok (st, None)
  | "LOAD_FAST" ->
      let exp = Exp.Var {scope= Fast; ident= Ident.mk co_varnames.(arg)} in
      let st = State.push st exp in
      Ok (st, None)
  | "LOAD_ATTR" ->
      let name = co_names.(arg) in
      let* tos, st = State.pop st in
      let exp = Exp.GetAttr (tos, name) in
      let st = State.push st exp in
      Ok (st, None)
  | "LOAD_CLASSDEREF" | "LOAD_DEREF" ->
      let name = get_cell_name code arg in
      let exp = Exp.Var {scope= Deref; ident= Ident.mk name} in
      let st = State.push st exp in
      Ok (st, None)
  | "STORE_NAME" ->
      let lhs = Exp.Var {scope= Name; ident= Ident.mk co_names.(arg)} in
      let* rhs, st = State.pop st in
      let stmt = Stmt.Assign {lhs; rhs} in
      let st = State.push_stmt st stmt in
      Ok (st, None)
  | "STORE_GLOBAL" ->
      let lhs = Exp.Var {scope= Global; ident= Ident.mk co_names.(arg)} in
      let* rhs, st = State.pop st in
      let stmt = Stmt.Assign {lhs; rhs} in
      let st = State.push_stmt st stmt in
      Ok (st, None)
  | "STORE_FAST" ->
      let lhs = Exp.Var {scope= Fast; ident= Ident.mk co_varnames.(arg)} in
      let* rhs, st = State.pop st in
      let stmt = Stmt.Assign {lhs; rhs} in
      let st = State.push_stmt st stmt in
      Ok (st, None)
  | "STORE_ATTR" ->
      let name = co_names.(arg) in
      let* root, st = State.pop st in
      let* rhs, st = State.pop st in
      let lhs = Exp.GetAttr (root, name) in
      let stmt = Stmt.Assign {lhs; rhs} in
      let st = State.push_stmt st stmt in
      Ok (st, None)
  | "STORE_SUBSCR" ->
      (* Implements TOS1[TOS] = TOS2.  *)
      let* index, st = State.pop st in
      let* exp, st = State.pop st in
      let* rhs, st = State.pop st in
      let lhs = Exp.Subscript {exp; index} in
      let stmt = Stmt.Assign {lhs; rhs} in
      let st = State.push_stmt st stmt in
      Ok (st, None)
  | "STORE_DEREF" ->
      let name = get_cell_name code arg in
      let lhs = Exp.Var {scope= Deref; ident= Ident.mk name} in
      let* rhs, st = State.pop st in
      let stmt = Stmt.Assign {lhs; rhs} in
      let st = State.push_stmt st stmt in
      Ok (st, None)
  | "RETURN_VALUE" ->
      let* ret, st = State.pop st in
      Ok (st, Some (Jump.Return ret))
  | "CALL_FUNCTION" ->
      call_function st arg
  | "CALL_FUNCTION_KW" ->
      call_function_kw st arg
  | "CALL_FUNCTION_EX" ->
      call_function_ex st arg
  | "POP_TOP" -> (
      (* TODO: rethink this in the future.
         Keep the popped values around in case their construction involves side effects *)
      let* rhs, st = State.pop st in
      State.debug st "popping %a@\n" Exp.pp rhs ;
      match (rhs : Exp.t) with
      | Temp _ ->
          Ok (st, None)
      | _ ->
          let id, st = State.fresh_id st in
          let stmt = Stmt.Assign {lhs= Exp.Temp id; rhs} in
          let st = State.push_stmt st stmt in
          Ok (st, None) )
  | "BINARY_ADD" ->
      parse_op st (Binary Add) 2
  | "BINARY_SUBTRACT" ->
      parse_op st (Binary Subtract) 2
  | "BINARY_AND" ->
      parse_op st (Binary And) 2
  | "BINARY_FLOOR_DIVIDE" ->
      parse_op st (Binary FloorDivide) 2
  | "BINARY_LSHIFT" ->
      parse_op st (Binary LShift) 2
  | "BINARY_MATRIX_MULTIPLY" ->
      parse_op st (Binary MatrixMultiply) 2
  | "BINARY_MODULO" ->
      parse_op st (Binary Modulo) 2
  | "BINARY_MULTIPLY" ->
      parse_op st (Binary Multiply) 2
  | "BINARY_OR" ->
      parse_op st (Binary Or) 2
  | "BINARY_POWER" ->
      parse_op st (Binary Power) 2
  | "BINARY_RSHIFT" ->
      parse_op st (Binary RShift) 2
  | "BINARY_TRUE_DIVIDE" ->
      parse_op st (Binary TrueDivide) 2
  | "BINARY_XOR" ->
      parse_op st (Binary Xor) 2
  | "INPLACE_ADD" ->
      parse_op st (Inplace Add) 2
  | "INPLACE_SUBTRACT" ->
      parse_op st (Inplace Subtract) 2
  | "INPLACE_AND" ->
      parse_op st (Inplace And) 2
  | "INPLACE_FLOOR_DIVIDE" ->
      parse_op st (Inplace FloorDivide) 2
  | "INPLACE_LSHIFT" ->
      parse_op st (Inplace LShift) 2
  | "INPLACE_MATRIX_MULTIPLY" ->
      parse_op st (Inplace MatrixMultiply) 2
  | "INPLACE_MODULO" ->
      parse_op st (Inplace Modulo) 2
  | "INPLACE_MULTIPLY" ->
      parse_op st (Inplace Multiply) 2
  | "INPLACE_OR" ->
      parse_op st (Inplace Or) 2
  | "INPLACE_POWER" ->
      parse_op st (Inplace Power) 2
  | "INPLACE_RSHIFT" ->
      parse_op st (Inplace RShift) 2
  | "INPLACE_TRUE_DIVIDE" ->
      parse_op st (Inplace TrueDivide) 2
  | "INPLACE_XOR" ->
      parse_op st (Inplace Xor) 2
  | "UNARY_POSITIVE" ->
      parse_op st (Unary Positive) 1
  | "UNARY_NEGATIVE" ->
      parse_op st (Unary Negative) 1
  | "UNARY_NOT" ->
      parse_op st (Unary Not) 1
  | "UNARY_INVERT" ->
      parse_op st (Unary Invert) 1
  | "MAKE_FUNCTION" ->
      make_function st arg
  | "BUILD_CONST_KEY_MAP" ->
      let* keys, st = State.pop st in
      let* keys =
        match (keys : Exp.t) with
        | Const (PYCTuple keys) ->
            Ok keys
        | _ ->
            internal_error st (Error.BuildConstKeyMapKeys keys)
      in
      let nr_keys = Array.length keys in
      let* tys, st = State.pop_n st arg in
      let* () =
        if Int.equal nr_keys arg then Ok ()
        else internal_error st (Error.BuildConstKeyMapLength (arg, nr_keys))
      in
      let keys = Array.to_list keys in
      let map =
        List.fold2_exn ~init:ConstMap.empty ~f:(fun map key ty -> ConstMap.add key ty map) keys tys
      in
      let st = State.push st (Exp.ConstMap map) in
      Ok (st, None)
  | "BUILD_LIST" ->
      build_collection st List arg
  | "BUILD_SET" ->
      build_collection st Set arg
  | "BUILD_TUPLE" ->
      build_collection st Tuple arg
  | "BUILD_SLICE" ->
      build_collection st Slice arg
  | "BUILD_STRING" ->
      build_collection st String arg
  | "BUILD_MAP" ->
      build_collection st Map (2 * arg)
  | "BINARY_SUBSCR" ->
      let* index, st = State.pop st in
      let* exp, st = State.pop st in
      let exp = Exp.Subscript {exp; index} in
      let st = State.push st exp in
      Ok (st, None)
  | "LOAD_BUILD_CLASS" ->
      let st = State.push st (BuiltinCaller BuildClass) in
      Ok (st, None)
  | "LOAD_METHOD" ->
      let name = co_names.(arg) in
      let* tos, st = State.pop st in
      let exp = Exp.LoadMethod (tos, name) in
      let st = State.push st exp in
      Ok (st, None)
  | "CALL_METHOD" ->
      let* args, st = State.pop_n st arg in
      let* call, st = State.pop st in
      let lhs, st = State.fresh_id st in
      let stmt = Stmt.CallMethod {lhs; call; args} in
      let st = State.push_stmt st stmt in
      let st = State.push st (Exp.Temp lhs) in
      Ok (st, None)
  | "SETUP_ANNOTATIONS" ->
      let st = State.push_stmt st SetupAnnotations in
      Ok (st, None)
  | "IMPORT_NAME" ->
      let name = co_names.(arg) in
      let* fromlist, st = State.pop st in
      let* level, st = State.pop st in
      let id, st = call_builtin_function st (ImportName name) [fromlist; level] in
      let st = State.push st (Exp.Temp id) in
      Ok (st, None)
  | "IMPORT_FROM" ->
      let name = co_names.(arg) in
      let* module_obj = State.peek st in
      let id, st = call_builtin_function st (ImportFrom name) [module_obj] in
      let st = State.push st (Exp.Temp id) in
      Ok (st, None)
  | "COMPARE_OP" ->
      let* cmp_op =
        match List.nth Builtin.Compare.all arg with
        | Some op ->
            Ok op
        | None ->
            external_error st (Error.CompareOp arg)
      in
      let* rhs, st = State.pop st in
      let* lhs, st = State.pop st in
      let id, st = call_builtin_function st (Compare cmp_op) [lhs; rhs] in
      let st = State.push st (Exp.Temp id) in
      Ok (st, None)
  | "LOAD_CLOSURE" ->
      let cell = get_cell_name code arg in
      (* We currently do nothing. It will be up to the IR -> Textual step to deal with these *)
      let exp = Exp.Ref cell in
      let st = State.push st exp in
      Ok (st, None)
  | "DUP_TOP" ->
      let* tos = State.peek st in
      let st = State.push st tos in
      Ok (st, None)
  | "UNPACK_SEQUENCE" ->
      unpack_sequence st arg
  | "FORMAT_VALUE" ->
      format_value st arg
  | "POP_JUMP_IF_TRUE" ->
      pop_jump_if ~next_is:false st arg next_offset_opt
  | "POP_JUMP_IF_FALSE" ->
      pop_jump_if ~next_is:true st arg next_offset_opt
  | "JUMP_FORWARD" ->
      let {State.loc} = st in
      (* This instruction gives us a relative delta w.r.t the next offset, so we turn it into an
         absolute offset right away *)
      let* next_offset = Offset.get ~loc next_offset_opt in
      let offset = next_offset + arg in
      let arity = State.size st in
      let* ssa_args, st = State.to_ssa st in
      let ssa_parameters, st = State.mk_ssa_parameters st arity in
      let label, st = State.get_label st offset ~ssa_parameters in
      let jump = Jump.Absolute {ssa_args; label; offset} in
      Ok (st, Some jump)
  | "JUMP_ABSOLUTE" ->
      let arity = State.size st in
      let* ssa_args, st = State.to_ssa st in
      let ssa_parameters, st = State.mk_ssa_parameters st arity in
      (* sanity check: we should already have allocated a label for this jump, if it is a backward
         edge. *)
      let* () =
        if arg < instr_offset then
          match State.lookup_label st arg with
          | None ->
              external_error st (Error.MissingBackEdge (instr_offset, arg))
          | Some {Label.name; ssa_parameters} ->
              let sz = List.length ssa_parameters in
              if sz <> arity then internal_error st (Error.InvalidBackEdgeArity (name, arity, sz))
              else Ok ()
        else Ok ()
      in
      let label, st = State.get_label st arg ~ssa_parameters in
      let jump = Jump.Absolute {ssa_args; label; offset= arg} in
      Ok (st, Some jump)
  | "GET_ITER" ->
      let* tos, st = State.pop st in
      let id, st = call_builtin_function st GetIter [tos] in
      let exp = Exp.Temp id in
      let st = State.push st exp in
      Ok (st, None)
  | "FOR_ITER" ->
      for_iter st arg next_offset_opt
  | "JUMP_IF_TRUE_OR_POP" ->
      jump_if_or_pop ~jump_if:true st arg next_offset_opt
  | "JUMP_IF_FALSE_OR_POP" ->
      jump_if_or_pop ~jump_if:false st arg next_offset_opt
  | "DUP_TOP_TWO" ->
      let* tos0, st = State.pop st in
      let* tos1, st = State.pop st in
      let st = State.push st tos1 in
      let st = State.push st tos0 in
      let st = State.push st tos1 in
      let st = State.push st tos0 in
      Ok (st, None)
  | "EXTENDED_ARG" ->
      (* The FFI.Instruction framework already did the magic and this opcode can be ignored. *)
      Ok (st, None)
  | "POP_BLOCK" ->
      let* _block, st = State.pop_block st in
      Ok (st, None)
  | "ROT_TWO" ->
      let* tos0, st = State.pop st in
      let* tos1, st = State.pop st in
      let st = State.push st tos0 in
      let st = State.push st tos1 in
      Ok (st, None)
  | "ROT_THREE" ->
      let* tos0, st = State.pop st in
      let* tos1, st = State.pop st in
      let* tos2, st = State.pop st in
      let st = State.push st tos0 in
      let st = State.push st tos2 in
      let st = State.push st tos1 in
      Ok (st, None)
  | "ROT_FOUR" ->
      let* tos0, st = State.pop st in
      let* tos1, st = State.pop st in
      let* tos2, st = State.pop st in
      let* tos3, st = State.pop st in
      let st = State.push st tos0 in
      let st = State.push st tos3 in
      let st = State.push st tos2 in
      let st = State.push st tos1 in
      Ok (st, None)
  | "SETUP_WITH" ->
      (* TODO: share code with SETUP_FINALLY ? *)
      let {State.loc} = st in
      (* This instruction gives us a relative delta w.r.t the next offset, so we turn it into an
         absolute offset right away *)
      let* next_offset = Offset.get ~loc next_offset_opt in
      let offset = next_offset + arg in
      let* context_manager, st = State.pop st in
      let st = State.push st (ContextManagerExit context_manager) in
      let exp = Exp.LoadMethod (context_manager, PyCommon.enter) in
      let lhs, st = call_function_with_unnamed_args st exp [] in
      State.debug st "setup-with: current block size: %d@\n" (State.size st) ;
      let arity = State.size st in
      let ssa_parameters, st = State.mk_ssa_parameters st arity in
      (* The "finally" block will have WITH_CLEANUP_START/FINISH that expect the Context Manager
         to be on the stack *)
      let _label, st = State.get_label st ~handler_type:Finally offset ~ssa_parameters in
      let block = Block.mk Normal in
      let st = State.push_block st block in
      let st = State.push st (Exp.Temp lhs) in
      Ok (st, None)
  | "BEGIN_FINALLY" ->
      (* TODO: this is not fully correct, as BEGIN_FINALLY can be inserted by
         the compiler in exception handlers with code like

         ```
         def f():
             ...

         try:
             f()
         except C as c:
             print(c)
         ```

         To solve this, we need to think of a way to encode the push/pop of
         the NULL this instruction inserts in the stack in a more precise way.
         Using only the kind of block is not enough *)
      let {State.loc} = st in
      (* BEGIN_FINALLY is used as a terminator to detect the beginning of "finally" blocks, as
         they are not always the target of a SETUP_WITH opcode.
         We create a jump to the next instruction.
         We don't push anything on the stack as this path is not any expecting information *)
      let* offset = Offset.get ~loc next_offset_opt in
      let arity = State.size st in
      let ssa_parameters, st = State.mk_ssa_parameters st arity in
      (* Because we can't decide in SETUP_FINALLY if the block will be a finally vs except block,
         we overwrite its type here. We also change the type of the current block to reflect this
         decision *)
      let label, st =
        State.get_label st offset ~handler_type:Finally ~fix_type:Finally ~ssa_parameters
      in
      let st = State.set_handler_type st offset Finally in
      let* ssa_args, st = State.to_ssa st in
      let info = {Jump.label; offset; ssa_args} in
      Ok (st, Some (Jump.Label info))
  | "SETUP_FINALLY" ->
      let {State.loc} = st in
      (* This instruction gives us a relative delta w.r.t the next offset, so we turn it into an
         absolute offset right away *)
      let* next_offset = Offset.get ~loc next_offset_opt in
      let offset = next_offset + arg in
      let arity = State.size st in
      (* TODO:
         this won't work correctly with Textual at the moment, because it will
         create an except node with more than the (6) exception items the
         runtime pust on the stack. We currently preserves the stack shape as
         ssa parameters, which enable us to correctly track everything.

         Once we decide to actively deal with exception in the frontend, we
         should save the stack (internally, or as specially named variables)
         and only leave the exception items as input argument of the except
         node *)
      let ssa_parameters, st = State.mk_ssa_parameters st arity in
      State.debug st "setup-finally: current block size: %d@\n" (State.size st) ;
      (* There's two possible targets during this block:
         - the [finally] block which expects a [NULL] on the stack
         - the [except] block which expects 3 values on the stack

         Note that we don't know yet if the handler is a finally or an except.
         So we do two things:
         - we use [Except] as an handler type, and replace it with [Finally] in BEGIN_FINALLY if necessary
         - we set the stack like a [Finally] handler, and we'll throw it away if enter an [Except] block
         We'll make this decision if we have a BEGIN_FINALLY on the path *)
      let _label, st = State.get_label st ~handler_type:Except offset ~ssa_parameters in
      (* Fall-through to the rest of the "normal" code *)
      let block = Block.mk Normal in
      let st = State.push_block st block in
      Ok (st, None)
  | "END_FINALLY" ->
      (* See doc for [END_FINALLY]. *)
      let* block, st = State.pop_block st in
      let is_except = Block.is_except block in
      let* st =
        if is_except then
          (* TODO: handle the re-raise case *)
          (* pop the block and 6 values from the stack *)
          let* _, st = State.pop_block st in
          let* st =
            let l = List.init 6 ~f:(fun x -> x) in
            List.fold_result l ~init:st ~f:(fun st _ ->
                let* _, st = State.pop st in
                Ok st )
          in
          Ok st
        else Ok st
      in
      Ok (st, None)
  | "POP_FINALLY" ->
      (* TODO: like [END_FINALLY], see official doc. We only support the "no exception" case for
         now. Supporting exceptions will require duplicating some nodes.
         We could just do nothing, but I wanted to have the 'return' logic in place for the
         future *)
      let* return_value, st =
        if arg <> 0 then
          let* ret, st = State.pop st in
          Ok (Some ret, st)
        else Ok (None, st)
      in
      let* _, st = State.pop_block st in
      (* MUST BE FINALLY BLOCK *)
      (* TODO: insert popping of tos when CALL_FINALLY is supported. Right now
         we didn't push anything on the stack so we don't pop anything *)
      let st = match return_value with Some ret -> State.push st ret | None -> st in
      Ok (st, None)
  | "WITH_CLEANUP_START" ->
      with_cleanup_start st
  | "WITH_CLEANUP_FINISH" ->
      with_cleanup_finish st
  | "RAISE_VARARGS" ->
      raise_varargs st arg
  | "POP_EXCEPT" ->
      let {State.loc} = st in
      (* From the doc: "The popped block must be an exception handler block, as implicitly created
         when entering an except handler".
         TODO: We do not check this for the momemt *)
      let* _block, st = State.pop_block st in
      (* POP the 3 remaining exceptions inputs *)
      let* _, st = State.pop st in
      let* _, st = State.pop st in
      let* _, st = State.pop st in
      (* Introduce a custom jump to the next instruction to exit the "except" node *)
      let* offset = Offset.get ~loc next_offset_opt in
      let arity = State.size st in
      let* ssa_args, st = State.to_ssa st in
      let ssa_parameters, st = State.mk_ssa_parameters st arity in
      let label, st = State.get_label st offset ~ssa_parameters in
      let jump = Jump.Absolute {ssa_args; label; offset} in
      Ok (st, Some jump)
  | "BUILD_TUPLE_UNPACK_WITH_CALL"
    (* No real difference betwen the two but in case of an error, which shouldn't happen since
       the code is known to compile *)
  | "BUILD_TUPLE_UNPACK" ->
      build_collection_unpack st arg Tuple
  | "BUILD_LIST_UNPACK" ->
      build_collection_unpack st arg List
  | "BUILD_SET_UNPACK" ->
      build_collection_unpack st arg Set
  | "BUILD_MAP_UNPACK_WITH_CALL" | "BUILD_MAP_UNPACK" ->
      (* No real difference betwen the two but in case of an error, which shouldn't happen since
         the code is known to compile *)
      build_collection_unpack st arg Map
  | "YIELD_VALUE" ->
      (* TODO: it is quite uncertain how we'll deal with these in textual.
         At the moment this seems to correctly model the stack life-cycle, so
         I'm happy. We might change/improve things in the future *)
      let* tos, st = State.pop st in
      let st = State.push st (Exp.Yield tos) in
      Ok (st, None)
  | "YIELD_FROM" ->
      (* TODO: it is quite uncertain how we'll deal with these in textual.
         At the moment this seems to correctly model the stack life-cycle, so
         I'm happy. We might change/improve things in the future *)
      let* exp, st = State.pop st in
      (* TODO: learn more about this construct. My understanding is that the
               receiver stays on the stack if there's a value to yield.
               we probably should do some encoding like GET_ITER/FOR_ITER.

               For now, we leave it as is until it becomes a problem :D *)
      let* receiver = State.peek st in
      (* TODO: it seems that sometimes the TOS is changed from receiver to exp
         at this point. Check C code and try to understand it better *)
      let _, st = call_builtin_function st YieldFrom [receiver; exp] in
      Ok (st, None)
  | "GET_YIELD_FROM_ITER" ->
      (* TODO: it is quite uncertain how we'll deal with these in textual.
         At the moment this seems to correctly model the stack life-cycle, so
         I'm happy. We might change/improve things in the future *)
      let* tos, st = State.pop st in
      let id, st = call_builtin_function st GetYieldFromIter [tos] in
      let st = State.push st (Exp.Temp id) in
      Ok (st, None)
  | "LIST_APPEND" ->
      collection_add st opname arg ListAppend
  | "SET_ADD" ->
      collection_add st opname arg SetAdd
  | "MAP_ADD" ->
      collection_add st opname arg ~map:true DictSetItem
  | "DELETE_NAME" ->
      let ident = Ident.mk co_names.(arg) in
      let _id, st = call_builtin_function st (Delete {scope= Name; ident}) [] in
      Ok (st, None)
  | "DELETE_GLOBAL" ->
      let ident = Ident.mk co_names.(arg) in
      let _id, st = call_builtin_function st (Delete {scope= Global; ident}) [] in
      Ok (st, None)
  | "DELETE_FAST" ->
      let ident = Ident.mk co_varnames.(arg) in
      let _id, st = call_builtin_function st (Delete {scope= Fast; ident}) [] in
      Ok (st, None)
  | "DELETE_ATTR" ->
      let name = co_names.(arg) in
      let* tos, st = State.pop st in
      let _id, st = call_builtin_function st (DeleteAttr name) [tos] in
      Ok (st, None)
  | "DELETE_DEREF" ->
      let ident = get_cell_name code arg |> Ident.mk in
      let _id, st = call_builtin_function st (Delete {scope= Deref; ident}) [] in
      Ok (st, None)
  | "DELETE_SUBSCR" ->
      let* index, st = State.pop st in
      let* exp, st = State.pop st in
      let _id, st = call_builtin_function st DeleteSubscr [exp; index] in
      Ok (st, None)
  | "GET_AWAITABLE" ->
      let* tos, st = State.pop st in
      let id, st = call_builtin_function st GetAwaitable [tos] in
      let st = State.push st (Exp.Temp id) in
      Ok (st, None)
  | "UNPACK_EX" ->
      (* The low byte of counts is the number of values before the list value, the high byte of
         counts the number of values after it. *)
      let to_int i = Exp.Const (Const.of_int i) in
      let nr_before = arg land 0xff in
      let nr_after = (arg lsr 8) land 0xff in
      let* tos, st = State.pop st in
      (* [UnpackEx m n exp] should unpack the [exp] collection into
         - [m] single values which are the first items in [exp]
         - [n] single values which are the latest items in [exp]
         - the rest stays into an iterable collection.
         We'll consider it returns a tuple of the right size that we can index
         to populate the stack *)
      let res_id, st = call_builtin_function st UnpackEx [to_int nr_before; to_int nr_after; tos] in
      let res = Exp.Temp res_id in
      let rec push st nr =
        if nr > 0 then
          let exp = Exp.Subscript {exp= res; index= to_int (nr - 1)} in
          let st = State.push st exp in
          push st (nr - 1)
        else st
      in
      let st = push st (nr_before + nr_after + 1) in
      Ok (st, None)
  | _ ->
      internal_error st (Error.UnsupportedOpcode opname)


let rec parse_bytecode_until_terminator ({State.loc} as st) code instructions =
  let open IResult.Let_syntax in
  let label_info, st = State.starts_with_jump_target st instructions in
  match label_info with
  | Some (offset, label) ->
      State.debug st "At offset %d, label spotted: %a@\n" offset Label.pp label ;
      (* If the analysis reached a known jump target, stop processing the node. *)
      let* ssa_args, st = State.to_ssa st in
      let* offset = Offset.of_code instructions |> Offset.get ~loc in
      let info = {Jump.label; offset; ssa_args} in
      Ok (st, Some (Jump.Label info), instructions)
  | None -> (
    (* Otherwise, continue the analysis of the bytecode *)
    match instructions with
    | [] ->
        Ok (st, None, [])
    | instr :: remaining ->
        let next_offset_opt = Offset.of_code remaining in
        let* st, terminator = parse_bytecode st code instr next_offset_opt in
        if Option.is_some terminator then Ok (st, terminator, remaining)
        else parse_bytecode_until_terminator st code remaining )


let mk_node st label code instructions =
  let open IResult.Let_syntax in
  let label_loc = Location.of_instructions instructions in
  let st = State.enter_node st label in
  let* st, jump_op, instructions = parse_bytecode_until_terminator st code instructions in
  (* Collect all the statements to be added to the node *)
  let stmts, st = State.drain_stmts st in
  let {State.loc= last_loc} = st in
  let jump_op =
    match jump_op with
    | None ->
        (* Unreachable: Python inserts [return None] at the end of any block
           without an explicit returns *)
        L.die ExternalError "mk_node: reached the EOF without a terminator"
    | Some jump_op ->
        jump_op
  in
  match (jump_op : _ Jump.t) with
  | Absolute info ->
      let last = Terminator.of_jump (`OneTarget info) in
      let node = {NodeBuilder.label; last; stmts; last_loc; label_loc} in
      Ok (st, instructions, node)
  | Return ret ->
      let last = Terminator.Return ret in
      let node = {NodeBuilder.label; last; stmts; last_loc; label_loc} in
      Ok (st, instructions, node)
  | Label ({offset} as info) ->
      (* Current invariant, might/will change with the introduction of * back-edges *)
      let opt = State.lookup_label st offset in
      if Option.is_none opt then L.die InternalError "stumbled upon unregistered label" ;
      (* A label was spotted after a non Terminator instruction. Insert a jump to this label to
         create a proper node, and resume the processing. *)
      let last = Terminator.of_jump (`OneTarget info) in
      let node = {NodeBuilder.label; last; stmts; last_loc; label_loc} in
      Ok (st, instructions, node)
  | TwoWay {condition; next_info; other_info} ->
      (* The current node ended up with a two-way jump. Either continue to the "next"
         (fall-through) part of the code, or jump to the "other" section of the code. For this
         purpose, register a fresh label for the jump. *)
      (* Register the jump target *)
      let last = Terminator.of_jump (`TwoTargets (condition, next_info, other_info)) in
      let node = {NodeBuilder.label; last; stmts; last_loc; label_loc} in
      Ok (st, instructions, node)
  | Throw exp ->
      (* TODO: Track exceptions *)
      let node = {NodeBuilder.label; last= Throw exp; stmts; last_loc; label_loc} in
      Ok (st, instructions, node)


let rec mk_nodes st label code instructions =
  let open IResult.Let_syntax in
  let* st, instructions, node = mk_node st label code instructions in
  (* Python bytecode has deadcode. From what we can see for now:
     - the compiler inserts [return None] at the end of every function, even if all execution path
       have returned. For example in
       ```
       if foo:
         return X
       else:
         return Y
       ```
       the compiler will insert an addition [return None] after everything.
       This one is not a real problem.

     - return/throw in loops are followed by a "jump back to the start of the loop".
       This is more problematic because this jump doesn't have a stack compatible with the one at
       the start of the loop: it is often empty (especially after a return) when the start of the
       loop usually have at least the iterator stacked.

     For these reason, we are also draining the stack here, and skipping all opcodes until we
     reach a label where to resume processing. *)
  let rec find_next_label st = function
    | [] ->
        Ok (None, st)
    | instr :: remaining_instructions as instructions -> (
        (* If the next instruction has a label, use it, otherwise pick a fresh one. *)
        let label_info, st = State.instr_is_jump_target st instr in
        match label_info with
        | Some (offset, label) ->
            State.debug st "mk_nodes: spotted label %a at offset %a@\n" Label.pp label Offset.pp
              offset ;
            let {Label.ssa_parameters} = label in
            (* When an except label is created, we can't decide yet if it will
               be an except or finally. Here, we know, so we can fix the arity
               of the "except" entry block to match the 3 exception inputs pushed by
               the Python runtime and pushed them on a newly added exception block *)
            let ssa_parameters, st =
              if Label.is_except label then
                let l = List.init 6 ~f:(fun x -> x) in
                let st, ssa_ids =
                  List.fold_left ~init:(st, ssa_parameters) l ~f:(fun (st, ssa_ids) _ ->
                      let id, st = State.fresh_id st in
                      (st, id :: ssa_ids) )
                in
                (ssa_ids, st)
              else (ssa_parameters, st)
            in
            let st = State.process_label st offset ssa_parameters in
            (* TODO: Fix this API *)
            let label = {label with Label.ssa_parameters} in
            (* Don't forget to keep [instr] here *)
            Ok (Some (label, instructions), st)
        | None ->
            State.debug st "mk_nodes: skipping dead instruction %a@\n" FFI.Instruction.pp instr ;
            find_next_label st remaining_instructions )
  in
  let* label_info, st = find_next_label st instructions in
  match label_info with
  | None ->
      Ok (st, [node])
  | Some (label, instructions) ->
      let* st, nodes = mk_nodes st label code instructions in
      Ok (st, node :: nodes)


module Module = struct
  type t = {name: Ident.t; toplevel: CFG.t; functions: CFG.t QualName.Map.t}

  let pp fmt {name; toplevel; functions} =
    F.fprintf fmt "@[<hv2>module %a:@\n@\n" Ident.pp name ;
    F.fprintf fmt "@[<hv2>toplevel:@\n%a@]@\n" CFG.pp toplevel ;
    QualName.Map.iter
      (fun name cfg -> F.fprintf fmt "@[<hv2>%a:@\n%a@]@\n" QualName.pp name CFG.pp cfg)
      functions ;
    F.fprintf fmt "@]@\n"
end

module CodeMap : Caml.Map.S with type key = FFI.Code.t = Caml.Map.Make (FFI.Code)

let build_code_object_unique_name file_path code =
  let rec visit map outer_name ({FFI.Code.co_consts} as code) =
    if CodeMap.mem code map then map
    else
      let map = CodeMap.add code outer_name map in
      Array.fold co_consts ~init:map ~f:(fun map constant ->
          match constant with
          | FFI.Constant.PYCCode code ->
              let outer_name = QualName.extend outer_name code.FFI.Code.co_name in
              visit map outer_name code
          | _ ->
              map )
  in
  let map = visit CodeMap.empty (QualName.from_qualified_string ~sep:'/' file_path) code in
  fun code -> CodeMap.find_opt code map


let build_cfg ~debug ~code_qual_name ({FFI.Code.instructions} as code) =
  let open IResult.Let_syntax in
  let loc = Location.of_code code in
  let st = State.enter ~debug ~code_qual_name ~loc in
  let label, st = State.get_label st 0 ~ssa_parameters:[] in
  let st = State.process_label st 0 [] in
  let* _, nodes = mk_nodes st label code instructions in
  let cfg = CFG.of_node_builder_list nodes in
  let* qual_name = read_code_qual_name st code in
  Ok (qual_name, cfg)


let build_cfgs ~debug ~code_qual_name co_consts =
  let rec visit cfg_map = function
    | FFI.Constant.PYCCode ({FFI.Code.co_consts} as code) ->
        let open IResult.Let_syntax in
        let* qual_name, cfg = build_cfg ~debug ~code_qual_name code in
        let cfg_map = QualName.Map.add qual_name cfg cfg_map in
        Array.fold_result co_consts ~init:cfg_map ~f:visit
    | _ ->
        Ok cfg_map
  in
  Array.fold_result co_consts ~init:QualName.Map.empty ~f:visit


let mk ~debug ({FFI.Code.co_filename} as code) =
  let open IResult.Let_syntax in
  let file_path =
    let sz = String.length co_filename in
    if sz >= 2 && String.equal "./" (String.sub co_filename ~pos:0 ~len:2) then
      String.sub co_filename ~pos:2 ~len:(sz - 2)
    else co_filename
  in
  let file_path = Stdlib.Filename.remove_extension file_path in
  let code_qual_name = build_code_object_unique_name file_path code in
  let name = Ident.mk file_path in
  let* _, toplevel = build_cfg ~debug ~code_qual_name code in
  let* functions = build_cfgs ~debug ~code_qual_name code.FFI.Code.co_consts in
  Ok {Module.name; toplevel; functions}


let test ?(filename = "dummy.py") ?(debug = false) source =
  if not (Py.is_initialized ()) then Py.initialize ~interpreter:Version.python_exe () ;
  let code =
    match FFI.from_string ~source ~filename with
    | Error (kind, err) ->
        L.die kind "FFI error: %a@\n" FFI.Error.pp_kind err
    | Ok code ->
        code
  in
  Py.finalize () ;
  match mk ~debug code with
  | Error (kind, _loc, err) -> (
    match kind with
    | L.InternalError ->
        L.internal_error "IR error: %a@\n" Error.pp_kind err
    | L.UserError ->
        L.user_error "IR error: %a@\n" Error.pp_kind err
    | L.ExternalError ->
        L.external_error "IR error: %a@\n" Error.pp_kind err )
  | Ok module_ ->
      F.printf "%a" Module.pp module_
  | exception (Py.E _ as e) ->
      L.die ExternalError "Pyml exception: %s@\n" (Exn.to_string e)
