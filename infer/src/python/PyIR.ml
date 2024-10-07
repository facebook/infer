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
  type t [@@deriving equal]

  val mk : string -> t

  val pp : F.formatter -> t -> unit
end = struct
  type t = string [@@deriving equal]

  let pp fmt ident = F.pp_print_string fmt ident

  let mk ident = ident
end

module ScopedIdent = struct
  type scope = Global | Fast | Name | Deref [@@deriving equal]

  type t = {scope: scope; ident: Ident.t} [@@deriving equal]

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
  type t [@@deriving equal]

  val from_qualified_string : sep:char -> string -> t

  val extend : t -> string -> t

  val pp : F.formatter -> t -> unit

  module Map : Caml.Map.S with type key = t
end = struct
  type t = {root: string; path: string list} [@@deriving compare, equal]

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
  type t [@@deriving equal]

  val mk : string -> t

  val pp : F.formatter -> t -> unit

  module Map : Caml.Map.S with type key = t
end = struct
  type t = string [@@deriving equal]

  let pp fmt ident = F.pp_print_string fmt ident

  let mk ident = ident

  module Map = SMap
end

module SSA = struct
  type t = int [@@deriving equal]

  let pp fmt i = F.fprintf fmt "n%d" i

  let next n = 1 + n
end

module BuiltinCaller = struct
  type format_function = Str | Repr | Ascii [@@deriving equal]

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
    | ImportStar
    | Binary of Builtin.binary_op
    | Unary of Builtin.unary_op
    | Compare of Builtin.Compare.t
    | GetAIter  (** [GET_AITER] *)
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
  [@@deriving equal]

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
    | ImportStar ->
        sprintf "$ImportStar"
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
    | GetAIter ->
        "$GetAIter"
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
  type collection = List | Set | Tuple | Slice | Map | String [@@deriving equal]

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
  [@@deriving equal]

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
    | LoadMethodExpected of Exp.t
    | CompareOp of int
    | CodeWithoutQualifiedName of FFI.Code.t
    | UnpackSequence of int
    | FormatValueSpec of Exp.t
    | NextOffsetMissing
    | MissingNodeInformation of int
    | CallKeywordNotString0 of Const.t
    | CallKeywordNotString1 of Exp.t
    | MakeFunctionInvalidDefaults of Exp.t
    | WithCleanupFinish of Exp.t
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
    | LoadMethodExpected exp ->
        F.fprintf fmt "LOAD_METHOD_EXPECTED: expected a LOAD_METHOD result but got %a" Exp.pp exp
    | NextOffsetMissing ->
        F.fprintf fmt "Jump to next instruction detected, but next instruction is missing"
    | MissingNodeInformation offset ->
        F.fprintf fmt "No information about offset %d" offset
    | CallKeywordNotString0 cst ->
        F.fprintf fmt "CALL_FUNCTION_KW: keyword is not a string: %a" Const.pp cst
    | CallKeywordNotString1 exp ->
        F.fprintf fmt "CALL_FUNCTION_KW: keyword is not a tuple of strings: %a" Exp.pp exp
    | MakeFunctionInvalidDefaults exp ->
        F.fprintf fmt "MAKE_FUNCTION: expecting tuple of default values but got %a" Exp.pp exp
    | WithCleanupFinish exp ->
        F.fprintf fmt "WITH_CLEANUP_FINISH/TODO: unsupported scenario with %a" Exp.pp exp
    | RaiseExceptionInvalid n ->
        F.fprintf fmt "RAISE_VARARGS: Invalid mode %d" n
end

module Stmt = struct
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
    | Let of {lhs: SSA.t; rhs: Exp.t}
    | SetAttr of {lhs: Exp.t; attr: string; rhs: Exp.t}
    | Store of {lhs: ScopedIdent.t; rhs: Exp.t}
    | StoreSubscript of {lhs: Exp.t; index: Exp.t; rhs: Exp.t}
    | Call of {lhs: SSA.t; exp: Exp.t; args: call_arg list; packed: bool}
    | CallMethod of {lhs: SSA.t; name: string; self_if_needed: Exp.t; args: Exp.t list}
    | BuiltinCall of {lhs: SSA.t; call: BuiltinCaller.t; args: Exp.t list}
    | SetupAnnotations

  let pp fmt = function
    | Let {lhs; rhs} ->
        F.fprintf fmt "%a <- %a" SSA.pp lhs Exp.pp rhs
    | SetAttr {lhs; attr; rhs} ->
        F.fprintf fmt "%a.%s <- %a" Exp.pp lhs attr Exp.pp rhs
    | Store {lhs; rhs} ->
        F.fprintf fmt "%a <- %a" ScopedIdent.pp lhs Exp.pp rhs
    | StoreSubscript {lhs; index; rhs} ->
        F.fprintf fmt "%a[%a] <- %a" Exp.pp lhs Exp.pp index Exp.pp rhs
    | Call {lhs; exp; args; packed} ->
        F.fprintf fmt "%a <- %a(@[%a@])%s" SSA.pp lhs Exp.pp exp (Pp.seq ~sep:", " pp_call_arg) args
          (if packed then " !packed" else "")
    | CallMethod {lhs; name; self_if_needed; args} ->
        F.fprintf fmt "%a <- %a.%s(@[%a@])" SSA.pp lhs Exp.pp self_if_needed name
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

  let get ~loc opt = Result.of_option ~error:(L.InternalError, loc, Error.NextOffsetMissing) opt
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

module Terminator = struct
  type node_call = {label: NodeName.t; ssa_args: Exp.t list}

  let pp_node_call fmt {label; ssa_args} =
    NodeName.pp fmt label ;
    if not (List.is_empty ssa_args) then F.fprintf fmt "(@[%a@])" (Pp.seq ~sep:", " Exp.pp) ssa_args


  type t =
    | Return of Exp.t
    | Jump of node_call list  (** non empty list *)
    | If of {exp: Exp.t; then_: t; else_: t}
    | Throw of Exp.t

  let mk_jump label exps = Jump [{label; ssa_args= List.rev exps}]

  let rec pp fmt = function
    | Return exp ->
        F.fprintf fmt "return %a" Exp.pp exp
    | Jump lst ->
        F.fprintf fmt "jmp %a" (Pp.seq ~sep:", " pp_node_call) lst
    | If {exp; then_; else_} ->
        F.fprintf fmt "if %a then @[%a@] else @[%a@]" Exp.pp exp pp then_ pp else_
    | Throw exp ->
        F.fprintf fmt "throw %a" Exp.pp exp
end

module Node = struct
  type t =
    { name: NodeName.t
    ; first_loc: Location.t
    ; last_loc: Location.t
    ; ssa_parameters: SSA.t list
    ; stmts: (Location.t * Stmt.t) list
    ; last: Terminator.t }

  let pp fmt {name; ssa_parameters; stmts; last} =
    F.fprintf fmt "@[<hv2>%a%t:@\n" NodeName.pp name (fun fmt ->
        if List.is_empty ssa_parameters then F.pp_print_string fmt ""
        else F.fprintf fmt "(%a)" (Pp.seq ~sep:", " SSA.pp) ssa_parameters ) ;
    List.iter stmts ~f:(fun (_, stmt) -> F.fprintf fmt "@[<hv2>%a@]@\n" Stmt.pp stmt) ;
    F.fprintf fmt "%a@\n" Terminator.pp last ;
    F.fprintf fmt "@]@\n"
end

module CFGBuilder = struct
  type t = {nodes: Node.t NodeName.Map.t; fresh_label: int}

  let fresh_start = 0

  let empty = {nodes= NodeName.Map.empty; fresh_label= fresh_start}

  let label_of_int i = sprintf "b%d" i

  let fresh_node_name ({fresh_label} as cfg) =
    let fresh = label_of_int fresh_label in
    let cfg = {cfg with fresh_label= fresh_label + 1} in
    (NodeName.mk fresh, cfg)


  let add name ~first_loc ~last_loc ssa_parameters stmts last {nodes; fresh_label} =
    let node = {Node.name; first_loc; last_loc; ssa_parameters; stmts; last} in
    {nodes= NodeName.Map.add name node nodes; fresh_label}
end

module CFG = struct
  type t = {entry: NodeName.t; nodes: Node.t NodeName.Map.t}

  let pp fmt {nodes} = NodeName.Map.iter (fun _ node -> Node.pp fmt node) nodes

  let of_builder {CFGBuilder.nodes} =
    let entry = CFGBuilder.label_of_int CFGBuilder.fresh_start |> NodeName.mk in
    {nodes; entry}
end

module State = struct
  (** Internal state of the Bytecode -> IR compiler *)
  type t =
    { debug: bool
    ; code_qual_name: FFI.Code.t -> QualName.t option
    ; get_node_name: Offset.t -> (NodeName.t, Error.t) result
    ; loc: Location.t
    ; cfg: CFGBuilder.t
    ; stack: Exp.t Stack.t
    ; stmts: (Location.t * Stmt.t) list
    ; ssa_parameters: SSA.t list
    ; fresh_id: SSA.t }

  let build_get_node_name loc cfg_skeleton =
    let map, cfg =
      IMap.fold
        (fun offset _ (map, cfg) ->
          let name, cfg = CFGBuilder.fresh_node_name cfg in
          (IMap.add offset name map, cfg) )
        cfg_skeleton (IMap.empty, CFGBuilder.empty)
    in
    let get_node_name offset =
      match IMap.find_opt offset map with
      | Some name ->
          Ok name
      | None ->
          Error (L.InternalError, loc, Error.MissingNodeInformation offset)
    in
    (cfg, get_node_name)


  let empty ~debug ~code_qual_name ~loc ~cfg_skeleton =
    let p = if debug then Debug.todo else Debug.p in
    p "State.empty@\n" ;
    let cfg, get_node_name = build_get_node_name loc cfg_skeleton in
    { debug
    ; code_qual_name
    ; get_node_name
    ; loc
    ; cfg
    ; stack= Stack.empty
    ; stmts= []
    ; ssa_parameters= []
    ; fresh_id= 0 }


  let debug {debug} = if debug then Debug.todo else Debug.p

  (** Each time a new object is discovered (they can be heavily nested), we need to clear the state
      of temporary data like the SSA counter, but keep other parts of it, like global and local
      names *)
  let enter ~debug ~code_qual_name ~loc ~cfg_skeleton =
    empty ~debug ~code_qual_name ~loc ~cfg_skeleton


  let get_node_name {get_node_name} offset = get_node_name offset

  let fresh_id ({fresh_id} as st) =
    let st = {st with fresh_id= SSA.next fresh_id} in
    (fresh_id, st)


  let push ({stack} as st) exp =
    let stack = Stack.push stack exp in
    {st with stack}


  let pop ({stack; loc} as st) =
    match Stack.pop stack with
    | None ->
        Error (L.InternalError, loc, Error.EmptyStack "pop")
    | Some (exp, stack) ->
        let st = {st with stack} in
        Ok (exp, st)


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

  let add_new_node ({cfg} as st) name ~first_loc ~last_loc ssa_parameters stmts last =
    {st with cfg= CFGBuilder.add name ~first_loc ~last_loc ssa_parameters stmts last cfg}


  let get_terminal_node ({cfg= {CFGBuilder.nodes}} as st) offset succ_name =
    let open IResult.Let_syntax in
    let+ name = get_node_name st offset in
    match NodeName.Map.find_opt name nodes with
    | Some {last} -> (
      match last with
      | Jump [{ssa_args}] ->
          `AlreadyThere (name, ssa_args)
      | If {then_= Jump [{label; ssa_args}]} when NodeName.equal label succ_name ->
          `AlreadyThere (name, ssa_args)
      | If {else_= Jump [{label; ssa_args}]} when NodeName.equal label succ_name ->
          `AlreadyThere (name, ssa_args)
      | _ ->
          `NotADirectJump name )
    | None ->
        `NotYetThere name


  let drop_first_args_terminal_node ({cfg} as st) ~pred_name ~succ_name k =
    (* 0 <= k <= size *)
    (* drop elements k, ..., size-1 *)
    let nodes = cfg.CFGBuilder.nodes in
    match NodeName.Map.find_opt pred_name nodes with
    | Some ({last} as node) ->
        let opt_new_last =
          match last with
          | Jump [{label; ssa_args}] ->
              let ssa_args = List.drop ssa_args k in
              let last = Terminator.Jump [{label; ssa_args}] in
              Some last
          | If {exp; then_= Jump [{label; ssa_args}]; else_} when NodeName.equal label succ_name ->
              let ssa_args = List.drop ssa_args k in
              let then_ = Terminator.Jump [{label; ssa_args}] in
              Some (Terminator.If {exp; then_; else_})
          | If {exp; then_; else_= Jump [{label; ssa_args}]} when NodeName.equal label succ_name ->
              let ssa_args = List.drop ssa_args k in
              let else_ = Terminator.Jump [{label; ssa_args}] in
              Some (Terminator.If {exp; then_; else_})
          | _ ->
              None
        in
        Option.value_map opt_new_last ~default:st ~f:(fun last ->
            let node = {node with last} in
            let nodes = NodeName.Map.add pred_name node nodes in
            let cfg = {cfg with nodes} in
            {st with cfg} )
    | None ->
        st


  let mk_ssa_parameters st arity =
    let rec mk st acc n =
      if n > 0 then
        let i, st = fresh_id st in
        mk st (i :: acc) (n - 1)
      else (acc, st)
    in
    mk st [] arity


  let enter_node st ~offset ~arity bottom_stack =
    debug st "enter-node at offset %d@\n" offset ;
    let st = {st with stmts= []; stack= bottom_stack} in
    let ssa_parameters, st = mk_ssa_parameters st (arity - List.length bottom_stack) in
    let st = List.fold_right ssa_parameters ~init:st ~f:(fun ssa st -> push st (Exp.Temp ssa)) in
    let st = {st with ssa_parameters} in
    debug st "> current stack size: %d\n" (size st) ;
    st


  let get_ssa_parameters {ssa_parameters} = List.rev ssa_parameters

  let get_stmts {stmts} = List.rev stmts
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
  let stmt =
    match exp with
    | Exp.BuiltinCaller call ->
        let args = List.map args ~f:(fun {Stmt.value} -> value) in
        Stmt.BuiltinCall {lhs; call; args}
    | exp ->
        Stmt.Call {lhs; exp; args; packed}
  in
  let st = State.push_stmt st stmt in
  (lhs, st)


let call_builtin_function st call args =
  let lhs, st = State.fresh_id st in
  let stmt = Stmt.BuiltinCall {lhs; call; args} in
  let st = State.push_stmt st stmt in
  (lhs, st)


let call_method st name self_if_needed args =
  let lhs, st = State.fresh_id st in
  let stmt = Stmt.CallMethod {lhs; name; self_if_needed; args} in
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
  let* next_offset = Offset.get ~loc next_offset_opt in
  let* next_label = State.get_node_name st next_offset in
  let* other_label = State.get_node_name st target in
  let condition = if next_is then condition else Exp.Not condition in
  let {State.stack} = st in
  Ok
    ( st
    , Some
        (Terminator.If
           { exp= condition
           ; then_= Terminator.mk_jump next_label stack
           ; else_= Terminator.mk_jump other_label stack } ) )


let jump_if_or_pop st ~jump_if target next_offset_opt =
  let open IResult.Let_syntax in
  let {State.loc} = st in
  (* We only peek the top of stack so the logic of [to_ssa] correctly captures it for
     the branch where "it stays". It will be restored as part of the stack restore logic when
     we process the "other" node. *)
  let* condition = State.peek st in
  let condition = if jump_if then Exp.Not condition else condition in
  let {State.stack} = st in
  let* next_offset = Offset.get ~loc next_offset_opt in
  (* In the next branch, we make sure the top-of-stack is no longer there. *)
  let* next_label = State.get_node_name st next_offset in
  let next_stack = List.tl stack |> Option.value ~default:[] in
  let* other_label = State.get_node_name st target in
  Ok
    ( st
    , Some
        (Terminator.If
           { exp= condition
           ; then_= Terminator.mk_jump next_label next_stack
           ; else_= Terminator.mk_jump other_label stack } ) )


let for_iter st delta next_offset_opt =
  let open IResult.Let_syntax in
  let {State.loc} = st in
  let* iter, st = State.pop st in
  let {State.stack= other_stack} = st in
  let id, st = call_builtin_function st NextIter [iter] in
  let has_item, st = call_builtin_function st HasNextIter [iter] in
  let condition = Exp.Temp has_item in
  (* In the next branch, we know the iterator has an item available. Let's fetch it and
     push it on the stack. *)
  let* next_offset = Offset.get ~loc next_offset_opt in
  (* The iterator object stays on the stack while in the for loop, let's push it back *)
  let st = State.push st iter in
  let st = State.push st (Exp.Temp id) in
  let* next_label = State.get_node_name st next_offset in
  let {State.stack= next_stack} = st in
  let other_offset = delta + next_offset in
  let* other_label = State.get_node_name st other_offset in
  Ok
    ( st
    , Some
        (Terminator.If
           { exp= condition
           ; then_= Terminator.mk_jump next_label next_stack
           ; else_= Terminator.mk_jump other_label other_stack } ) )


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
      let throw = Terminator.Throw (Exp.Temp id) in
      Ok (st, Some throw)
  | 1 ->
      let* tos, st = State.pop st in
      let throw = Terminator.Throw tos in
      Ok (st, Some throw)
  | 2 ->
      let* rhs, st = State.pop st in
      let* lhs, st = State.pop st in
      let st = State.push_stmt st (Stmt.SetAttr {lhs; attr= "__cause__"; rhs}) in
      let throw = Terminator.Throw lhs in
      Ok (st, Some throw)
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


let assign_to_temp_and_push st rhs =
  let id, st = State.fresh_id st in
  let exp = Exp.Temp id in
  let stmt = Stmt.Let {lhs= id; rhs} in
  let st = State.push_stmt st stmt in
  let st = State.push st exp in
  Ok (st, None)


let load st scope name =
  let rhs = Exp.Var {scope; ident= Ident.mk name} in
  assign_to_temp_and_push st rhs


let store st scope name =
  let open IResult.Let_syntax in
  let lhs = {ScopedIdent.scope; ident= Ident.mk name} in
  let* rhs, st = State.pop st in
  let stmt = Stmt.Store {lhs; rhs} in
  let st = State.push_stmt st stmt in
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
  State.debug st "%a: %s %d (0x%x) - STACK_SIZE %d @\n" Offset.pp instr_offset opname arg arg
    (State.size st) ;
  match opname with
  | "LOAD_CONST" ->
      let c = co_consts.(arg) in
      let exp = Exp.Const c in
      let st = State.push st exp in
      Ok (st, None)
  | "LOAD_NAME" ->
      load st Name co_names.(arg)
  | "LOAD_GLOBAL" ->
      load st Global co_names.(arg)
  | "LOAD_FAST" ->
      load st Fast co_varnames.(arg)
  | "LOAD_ATTR" ->
      let name = co_names.(arg) in
      let* tos, st = State.pop st in
      let exp = Exp.GetAttr (tos, name) in
      assign_to_temp_and_push st exp
  | "LOAD_CLASSDEREF" | "LOAD_DEREF" ->
      let name = get_cell_name code arg in
      load st Deref name
  | "STORE_NAME" ->
      store st Name co_names.(arg)
  | "STORE_GLOBAL" ->
      store st Global co_names.(arg)
  | "STORE_FAST" ->
      store st Fast co_varnames.(arg)
  | "STORE_ATTR" ->
      let attr = co_names.(arg) in
      let* lhs, st = State.pop st in
      let* rhs, st = State.pop st in
      let stmt = Stmt.SetAttr {lhs; attr; rhs} in
      let st = State.push_stmt st stmt in
      Ok (st, None)
  | "STORE_SUBSCR" ->
      (* Implements TOS1[TOS] = TOS2.  *)
      let* index, st = State.pop st in
      let* lhs, st = State.pop st in
      let* rhs, st = State.pop st in
      let stmt = Stmt.StoreSubscript {lhs; index; rhs} in
      let st = State.push_stmt st stmt in
      Ok (st, None)
  | "STORE_DEREF" ->
      let name = get_cell_name code arg in
      store st Deref name
  | "RETURN_VALUE" ->
      let* ret, st = State.pop st in
      Ok (st, Some (Terminator.Return ret))
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
          let stmt = Stmt.Let {lhs= id; rhs} in
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
      assign_to_temp_and_push st exp
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
      let* self_if_needed, name =
        match call with
        | Exp.LoadMethod (self_if_needed, name) ->
            Ok (self_if_needed, name)
        | _ ->
            internal_error st (Error.LoadMethodExpected call)
      in
      let id, st = call_method st name self_if_needed args in
      let st = State.push st (Exp.Temp id) in
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
  | "IMPORT_STAR" ->
      let* module_object, st = State.pop st in
      let id, st = call_builtin_function st ImportStar [module_object] in
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
      let* label = State.get_node_name st offset in
      let {State.stack} = st in
      let jump = Terminator.mk_jump label stack in
      Ok (st, Some jump)
  | "JUMP_ABSOLUTE" ->
      let* label = State.get_node_name st arg in
      let {State.stack} = st in
      let jump = Terminator.mk_jump label stack in
      Ok (st, Some jump)
  | "GET_ITER" ->
      let* tos, st = State.pop st in
      let id, st = call_builtin_function st GetIter [tos] in
      let exp = Exp.Temp id in
      let st = State.push st exp in
      Ok (st, None)
  | "GET_AITER" ->
      let* tos, st = State.pop st in
      let id, st = call_method st PyCommon.aiter tos [] in
      let st = State.push st (Exp.Temp id) in
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
      let* context_manager, st = State.pop st in
      let st = State.push st (ContextManagerExit context_manager) in
      let id, st = call_method st PyCommon.enter context_manager [] in
      let st = State.push st (Exp.Temp id) in
      State.debug st "setup-with: current block size: %d@\n" (State.size st) ;
      (* The "finally" block will have WITH_CLEANUP_START/FINISH that expect the Context Manager
         to be on the stack *)
      Ok (st, None)
  | "BEGIN_FINALLY" ->
      let st = State.push st (Exp.Const Const.none) in
      Ok (st, None)
  | "SETUP_FINALLY" ->
      Ok (st, None)
  | "END_FINALLY" ->
      let* _, st = State.pop st in
      Ok (st, None)
  | "CALL_FINALLY" ->
      let {State.loc} = st in
      let* next_offset = Offset.get ~loc next_offset_opt in
      let jump_offset = next_offset + arg in
      let st = State.push st (Exp.Const (Const.of_int jump_offset)) in
      let* label = State.get_node_name st jump_offset in
      let {State.stack} = st in
      Ok (st, Some (Terminator.mk_jump label stack))
  | "POP_FINALLY" ->
      let* return_value, st =
        if arg <> 0 then
          let* ret, st = State.pop st in
          Ok (Some ret, st)
        else Ok (None, st)
      in
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
      internal_error st (Error.UnsupportedOpcode opname)
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
  | "GET_ANEXT" ->
      let* tos = State.peek st in
      let id, st = call_method st PyCommon.anext tos [] in
      let id, st = call_builtin_function st GetAwaitable [Exp.Temp id] in
      let st = State.push st (Exp.Temp id) in
      Ok (st, None)
  | "BEFORE_ASYNC_WITH" ->
      let* tos = State.peek st in
      let st = State.push st (ContextManagerExit tos) in
      let id, st = call_method st PyCommon.enter tos [] in
      let st = State.push st (Exp.Temp id) in
      Ok (st, None)
  | "SETUP_ASYNC_WITH" ->
      (* This is nope operation until we translate exceptino throwing *)
      Ok (st, None)
  | "END_ASYNC_FOR" ->
      (* This instructions designates the end of an async for loop. Such a loop
         always ends with an exception.
         https://quentin.pradet.me/blog/using-asynchronous-for-loops-in-python.html
         https://superfastpython.com/asyncio-async-for/
         We model it like a throwing exception for now. This offset will not be reached
         by our DFS anyway since we don't model exceptionnal edges yet.
      *)
      Ok (st, Some (Terminator.Throw (Exp.Const Const.none)))
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


let get_successors_offset {FFI.Instruction.opname; arg} =
  match opname with
  | "LOAD_CONST"
  | "LOAD_NAME"
  | "LOAD_GLOBAL"
  | "LOAD_FAST"
  | "LOAD_ATTR"
  | "LOAD_CLASSDEREF"
  | "LOAD_DEREF"
  | "STORE_NAME"
  | "STORE_GLOBAL"
  | "STORE_FAST"
  | "STORE_ATTR"
  | "STORE_SUBSCR"
  | "STORE_DEREF"
  | "CALL_FUNCTION"
  | "CALL_FUNCTION_KW"
  | "CALL_FUNCTION_EX"
  | "POP_TOP"
  | "BINARY_ADD"
  | "BINARY_SUBTRACT"
  | "BINARY_AND"
  | "BINARY_FLOOR_DIVIDE"
  | "BINARY_LSHIFT"
  | "BINARY_MATRIX_MULTIPLY"
  | "BINARY_MODULO"
  | "BINARY_MULTIPLY"
  | "BINARY_OR"
  | "BINARY_POWER"
  | "BINARY_RSHIFT"
  | "BINARY_TRUE_DIVIDE"
  | "BINARY_XOR"
  | "INPLACE_ADD"
  | "INPLACE_SUBTRACT"
  | "INPLACE_AND"
  | "INPLACE_FLOOR_DIVIDE"
  | "INPLACE_LSHIFT"
  | "INPLACE_MATRIX_MULTIPLY"
  | "INPLACE_MODULO"
  | "INPLACE_MULTIPLY"
  | "INPLACE_OR"
  | "INPLACE_POWER"
  | "INPLACE_RSHIFT"
  | "INPLACE_TRUE_DIVIDE"
  | "INPLACE_XOR"
  | "UNARY_POSITIVE"
  | "UNARY_NEGATIVE"
  | "UNARY_NOT"
  | "UNARY_INVERT"
  | "MAKE_FUNCTION"
  | "BUILD_CONST_KEY_MAP"
  | "BUILD_LIST"
  | "BUILD_SET"
  | "BUILD_TUPLE"
  | "BUILD_SLICE"
  | "BUILD_STRING"
  | "BUILD_MAP"
  | "BINARY_SUBSCR"
  | "LOAD_BUILD_CLASS"
  | "LOAD_METHOD"
  | "CALL_METHOD"
  | "SETUP_ANNOTATIONS"
  | "IMPORT_NAME"
  | "IMPORT_FROM"
  | "IMPORT_STAR"
  | "COMPARE_OP"
  | "LOAD_CLOSURE"
  | "DUP_TOP"
  | "UNPACK_SEQUENCE"
  | "FORMAT_VALUE"
  | "GET_AITER"
  | "GET_ANEXT"
  | "GET_ITER"
  | "DUP_TOP_TWO"
  | "EXTENDED_ARG"
  | "POP_BLOCK"
  | "ROT_TWO"
  | "ROT_THREE"
  | "ROT_FOUR"
  | "SETUP_WITH"
  | "BEGIN_FINALLY"
  | "SETUP_FINALLY"
  | "END_FINALLY"
  | "POP_FINALLY"
  | "WITH_CLEANUP_START"
  | "WITH_CLEANUP_FINISH"
  | "POP_EXCEPT"
  | "BUILD_TUPLE_UNPACK_WITH_CALL"
  | "BUILD_TUPLE_UNPACK"
  | "BUILD_LIST_UNPACK"
  | "BUILD_SET_UNPACK"
  | "BUILD_MAP_UNPACK_WITH_CALL"
  | "BUILD_MAP_UNPACK"
  | "YIELD_VALUE"
  | "YIELD_FROM"
  | "GET_YIELD_FROM_ITER"
  | "LIST_APPEND"
  | "SET_ADD"
  | "MAP_ADD"
  | "DELETE_NAME"
  | "DELETE_GLOBAL"
  | "DELETE_FAST"
  | "DELETE_ATTR"
  | "DELETE_DEREF"
  | "DELETE_SUBSCR"
  | "GET_AWAITABLE"
  | "BEFORE_ASYNC_WITH"
  | "SETUP_ASYNC_WITH"
  | "UNPACK_EX" ->
      `NextInstrOnly
  | "RETURN_VALUE" ->
      `Return
  | "POP_JUMP_IF_TRUE" | "POP_JUMP_IF_FALSE" ->
      `NextInstrOrAbsolute arg
  | "JUMP_IF_TRUE_OR_POP" | "JUMP_IF_FALSE_OR_POP" ->
      `NextInstrWithPopOrAbsolute arg
  | "FOR_ITER" ->
      `NextInstrOrRelativeWith2Pop arg
  | "CALL_FINALLY" | "JUMP_FORWARD" ->
      `Relative arg
  | "JUMP_ABSOLUTE" ->
      `Absolute arg
  | "END_ASYNC_FOR" | "RAISE_VARARGS" ->
      `Throw
  | _ ->
      `UnsupportedOpcode


let lookup_remaining = function
  | [] ->
      (None, false)
  | {FFI.Instruction.offset; is_jump_target} :: _ ->
      (Some offset, is_jump_target)


type cfg_info =
  { successors: (Offset.t * int) list
  ; predecessors: Offset.t list
  ; instructions: FFI.Instruction.t list }

let dummy_cfg_info = {successors= []; predecessors= []; instructions= []}

let build_cfg_skeleton_without_predecessors {FFI.Code.instructions} :
    (cfg_info IMap.t, Error.t) result =
  let open IResult.Let_syntax in
  let process_instr map action next_offset_opt next_is_jump_target
      ({FFI.Instruction.opname; starts_line; offset} as instr) instructions =
    let get_next_offset () =
      Result.of_option
        ~error:(L.InternalError, starts_line, Error.NextOffsetMissing)
        next_offset_opt
    in
    let current_node_offset, current_instructions, action =
      match action with
      | `StartNewOne ->
          (offset, instructions, `InNodeStartingAt (offset, instructions))
      | `InNodeStartingAt (current_node_offset, current_instructions) ->
          (current_node_offset, current_instructions, action)
    in
    let register_current_node successors =
      let predecessors = [] in
      (* we will update this field lated after a first pass *)
      Ok
        ( IMap.add current_node_offset
            {successors; predecessors; instructions= current_instructions}
            map
        , `StartNewOne )
    in
    match get_successors_offset instr with
    | `NextInstrOnly when not next_is_jump_target ->
        Ok (map, action)
    | `NextInstrOnly ->
        let* next_offset = get_next_offset () in
        register_current_node [(next_offset, 0)]
    | `Throw | `Return ->
        register_current_node []
    | `NextInstrWithPopOrAbsolute other_offset ->
        let* next_offset = get_next_offset () in
        register_current_node [(next_offset, -1); (other_offset, 0)]
    | `NextInstrOrAbsolute other_offset ->
        let* next_offset = get_next_offset () in
        register_current_node [(next_offset, 0); (other_offset, 0)]
    | `NextInstrOrRelativeWith2Pop delta ->
        let* next_offset = get_next_offset () in
        register_current_node [(next_offset, 0); (next_offset + delta, -2)]
    | `Relative delta ->
        let* next_offset = get_next_offset () in
        register_current_node [(next_offset + delta, 0)]
    | `Absolute offset ->
        register_current_node [(offset, 0)]
    | `UnsupportedOpcode ->
        Error (L.InternalError, starts_line, Error.UnsupportedOpcode opname)
  in
  let rec loop map action instructions =
    match instructions with
    | [] ->
        Ok map
    | instr :: remaining ->
        let next_offset_opt, next_is_jump_target = lookup_remaining remaining in
        let* map, action =
          process_instr map action next_offset_opt next_is_jump_target instr instructions
        in
        loop map action remaining
  in
  loop IMap.empty `StartNewOne instructions


module ISet = IInt.Set

let set_predecessors offset successors map =
  List.fold successors ~init:map ~f:(fun map (succ_offset, _) ->
      let ({predecessors} as info) =
        IMap.find_opt succ_offset map |> Option.value ~default:dummy_cfg_info
      in
      IMap.add succ_offset {info with predecessors= offset :: predecessors} map )


let build_topological_order cfg_skeleton_without_predecessors =
  (* weakly topological order *)
  let rec visit ((seen, map, post_visited) as acc) (offset, _) =
    if ISet.mem offset seen then acc
    else
      let {successors} =
        IMap.find_opt offset cfg_skeleton_without_predecessors
        |> Option.value ~default:dummy_cfg_info
      in
      let seen = ISet.add offset seen in
      let seen, map, post_visited = List.fold successors ~f:visit ~init:(seen, map, post_visited) in
      let map = set_predecessors offset successors map in
      let post_visited = offset :: post_visited in
      (seen, map, post_visited)
  in
  let _, cfg, post_visited = visit (ISet.empty, cfg_skeleton_without_predecessors, []) (0, 0) in
  (post_visited, cfg)


let constant_folding_ssa_params st succ_name {predecessors} =
  let open IResult.Let_syntax in
  let* predecessors_stacks =
    List.fold_result predecessors
      ~init:(`AllAvailable ([], []))
      ~f:(fun acc predecessor ->
        match acc with
        | `AllAvailable (predecessors, stacks) -> (
            let* last_of_predecessor = State.get_terminal_node st predecessor succ_name in
            match last_of_predecessor with
            | `NotYetThere predecessor_name ->
                Ok (`AtLeastOneNotAvailable predecessor_name)
            | `NotADirectJump predecessor_name ->
                Ok (`AtLeastOneNotADirectJump predecessor_name)
            | `AlreadyThere (predecessor_name, args) ->
                Ok (`AllAvailable (predecessor_name :: predecessors, args :: stacks)) )
        | _ ->
            Ok acc )
  in
  let st, bottom_stack =
    match predecessors_stacks with
    | `AtLeastOneNotAvailable _pred_name ->
        (st, [])
    | `AtLeastOneNotADirectJump _pred_name ->
        (st, [])
    | `AllAvailable (predecessors, _) when List.length predecessors <= 0 ->
        (st, [])
    | `AllAvailable (predecessors, stacks) ->
        let stack0 =
          match stacks with
          | [] ->
              L.die InternalError "there should be at least one predecessor"
          | stack :: _ ->
              stack
        in
        let k = IList.k_first_columns_same_cell ~equal:Exp.equal stacks in
        (* for all 0 <= i < k-1, forall 0 < j < n_preds, stacks[j][i]=stacks[0][i]
           with n_preds = List.length stacks *)
        let st =
          List.fold predecessors ~init:st ~f:(fun st pred_name ->
              State.drop_first_args_terminal_node st ~pred_name ~succ_name k )
        in
        let bottom_stack = List.take stack0 k |> List.rev in
        (st, bottom_stack)
  in
  Ok (st, bottom_stack)


let process_node st code ~offset ~arity ({instructions} as info) =
  let open IResult.Let_syntax in
  let* name = State.get_node_name st offset in
  let* st, bottom_stack = constant_folding_ssa_params st name info in
  let ({State.loc= first_loc} as st) = State.enter_node st ~offset ~arity bottom_stack in
  let ssa_parameters = State.get_ssa_parameters st in
  let rec loop st = function
    | [] ->
        let {State.loc} = st in
        Error (L.InternalError, loc, Error.NextOffsetMissing)
    | instr :: remaining -> (
        let next_offset_opt, next_is_jump_target = lookup_remaining remaining in
        let* st, opt_terminator = parse_bytecode st code instr next_offset_opt in
        match opt_terminator with
        | Some last ->
            let {State.loc= last_loc} = st in
            let stmts = State.get_stmts st in
            let st = State.add_new_node st name ~first_loc ~last_loc ssa_parameters stmts last in
            Ok st
        | None when next_is_jump_target ->
            let {State.loc= last_loc; stack} = st in
            let stmts = State.get_stmts st in
            let* next_offset = Offset.get ~loc:last_loc next_offset_opt in
            let* next_name = State.get_node_name st next_offset in
            let last = Terminator.mk_jump next_name stack in
            let st = State.add_new_node st name ~first_loc ~last_loc ssa_parameters stmts last in
            Ok st
        | None ->
            loop st remaining )
  in
  loop st instructions


let build_cfg ~debug ~code_qual_name code =
  let open IResult.Let_syntax in
  let loc = Location.of_code code in
  let* cfg_skeleton_without_predecessors = build_cfg_skeleton_without_predecessors code in
  let topological_order, cfg_skeleton = build_topological_order cfg_skeleton_without_predecessors in
  let get_info offset =
    match IMap.find_opt offset cfg_skeleton with
    | None ->
        Error (L.InternalError, loc, Error.MissingNodeInformation offset)
    | Some l ->
        Ok l
  in
  let visit (st, arity_map) offset =
    let* ({successors} as info) = get_info offset in
    let* arity =
      match IMap.find_opt offset arity_map with
      | Some arity ->
          Ok arity
      | None ->
          internal_error st (Error.MissingNodeInformation offset)
    in
    let* st = process_node st code ~offset ~arity info in
    let arity = State.size st in
    let arity_map =
      List.fold successors ~init:arity_map ~f:(fun arity_map (succ, delta) ->
          if IMap.mem succ arity_map then arity_map else IMap.add succ (arity + delta) arity_map )
    in
    Ok (st, arity_map)
  in
  let st = State.enter ~debug ~code_qual_name ~loc ~cfg_skeleton in
  let* st, _ = List.fold_result topological_order ~init:(st, IMap.add 0 0 IMap.empty) ~f:visit in
  let cfg = CFG.of_builder st.State.cfg in
  let* qual_name = read_code_qual_name st code in
  Ok (qual_name, cfg)


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


let test_cfg_skeleton code =
  match build_cfg_skeleton_without_predecessors code with
  | Error (_, _, err) ->
      L.internal_error "IR error: %a@\n" Error.pp_kind err
  | Ok map ->
      let topological_order, map = build_topological_order map in
      F.printf "%a@\n" FFI.Code.pp_instructions code ;
      let pp_succ_and_delta fmt (succ, delta) =
        if Int.equal delta 0 then F.pp_print_int fmt succ else F.fprintf fmt "%d(%d)" succ delta
      in
      F.printf "CFG successors:@\n" ;
      IMap.iter
        (fun offset {successors} ->
          F.printf "%4d: %a@\n" offset (Pp.seq ~sep:" " pp_succ_and_delta) successors )
        map ;
      F.printf "CFG predecessors:@\n" ;
      IMap.iter
        (fun offset {predecessors} ->
          F.printf "%4d: %a@\n" offset (Pp.seq ~sep:" " F.pp_print_int) predecessors )
        map ;
      F.printf "topological order: %a@\n" (Pp.seq ~sep:" " F.pp_print_int) topological_order


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


let test_cfg_skeleton ?(filename = "dummy.py") source =
  if not (Py.is_initialized ()) then Py.initialize ~interpreter:Version.python_exe () ;
  let code =
    match FFI.from_string ~source ~filename with
    | Error (kind, err) ->
        L.die kind "FFI error: %a@\n" FFI.Error.pp_kind err
    | Ok code ->
        code
  in
  Py.finalize () ;
  test_cfg_skeleton code
