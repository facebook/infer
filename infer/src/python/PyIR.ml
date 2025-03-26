(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
module IMap = IInt.Map

let remove_angles str =
  if String.is_prefix str ~prefix:"<" && String.is_suffix str ~suffix:">" then
    let str = String.sub str ~pos:1 ~len:(String.length str - 2) in
    "_$" ^ str
  else str


module rec Ident : sig
  type t [@@deriving equal, compare]

  val mk : string -> t

  val pp : F.formatter -> t -> unit

  val to_textual_base_type_name : t -> Textual.BaseTypeName.t

  module Special : sig
    val aiter : t

    val anext : t

    val cause : t

    val enter : t

    val exit : t

    val name : t

    val print : t
  end

  module Hashtbl : Stdlib.Hashtbl.S with type key = t
end = struct
  type t = string [@@deriving equal, compare, hash]

  let pp fmt ident = F.pp_print_string fmt ident

  let mk ident = remove_angles ident

  let to_textual_base_type_name value = Textual.{BaseTypeName.value; loc= Location.Unknown}

  module Special = struct
    let aiter = "__aiter__"

    let anext = "__anext__"

    let cause = "__cause__"

    let enter = "__enter__"

    let exit = "__exit__"

    let name = "__name__"

    let print = "print"
  end

  module Hashtbl = Stdlib.Hashtbl.Make (struct
    type nonrec t = t

    let equal = equal

    let hash = hash
  end)
end

and QualName : sig
  type t = {module_name: Ident.t; function_name: Ident.t} [@@deriving equal]

  val init : module_name:string -> t

  val extend : t -> string -> int -> t

  val pp : F.formatter -> t -> unit

  module Map : Stdlib.Map.S with type key = t
end = struct
  type t = {module_name: string; function_name: string} [@@deriving compare, equal]

  let pp fmt {module_name; function_name} =
    if String.is_empty module_name then F.pp_print_string fmt function_name
    else if String.is_empty function_name then F.pp_print_string fmt module_name
    else F.fprintf fmt "%s.%s" module_name function_name


  let init ~module_name = {function_name= ""; module_name}

  let extend {module_name; function_name} attr counter =
    let attr = remove_angles attr in
    let str_counter = match counter with 0 -> "" | _ -> string_of_int counter in
    let function_name =
      if String.is_empty function_name then F.asprintf "%s%s" attr str_counter
      else F.asprintf "%s.%s%s" function_name attr str_counter
    in
    {function_name; module_name}


  module Map = Stdlib.Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)
end

module ScopedIdent = struct
  type scope = Global | Fast | Name [@@deriving equal]

  type t = {scope: scope; ident: Ident.t} [@@deriving equal]

  let pp fmt {scope; ident} =
    match scope with
    | Global ->
        F.fprintf fmt "GLOBAL[%a]" Ident.pp ident
    | Fast ->
        F.fprintf fmt "LOCAL[%a]" Ident.pp ident
    | Name ->
        F.fprintf fmt "TOPLEVEL[%a]" Ident.pp ident
end

module NodeName : sig
  type t [@@deriving equal]

  val mk : offset:int -> string -> t

  val get_offset : t -> int

  val pp : F.formatter -> t -> unit

  module Map : sig
    include Stdlib.Map.S with type key = t

    val map_result : f:(key -> 'a -> ('b, 'c) result) -> 'a t -> ('b t, 'c) result
  end
end = struct
  type t = {name: string; offset: int [@compare.ignore] [@equal.ignore]} [@@deriving equal, compare]

  let pp ~debug fmt {name; offset} =
    if debug then F.fprintf fmt "%s@%d" name offset else F.pp_print_string fmt name


  let pp = pp ~debug:false

  let mk ~offset name = {name; offset}

  let get_offset {offset} = offset

  module Map = struct
    include Stdlib.Map.Make (struct
      type nonrec t = t

      let compare = compare
    end)

    let map_result ~f map =
      let open IResult.Let_syntax in
      fold
        (fun key elt acc ->
          let* map = acc in
          let+ elt = f key elt in
          add key elt map )
        map (Ok empty)
  end
end

module SSA = struct
  type t = int [@@deriving equal, hash]

  let id x = x

  let pp fmt i = F.fprintf fmt "n%d" i

  let next n = 1 + n

  module Hashtbl = Stdlib.Hashtbl.Make (struct
    type nonrec t = t

    let equal = equal

    let hash = hash
  end)
end

module CompareOp = struct
  (* Some interesting source of information: https://docs.python.org/3/library/operator.html *)
  type t = Lt | Le | Eq | Neq | Gt | Ge | In | NotIn | Is | IsNot | Exception | BAD
  [@@deriving compare, enumerate, equal]

  let to_string = function
    | Lt ->
        "lt"
    | Le ->
        "le"
    | Eq ->
        "eq"
    | Neq ->
        "neq"
    | Gt ->
        "gt"
    | Ge ->
        "ge"
    | In ->
        "in"
    | NotIn ->
        "not_in"
    | Is ->
        "is"
    | IsNot ->
        "is_not"
    | Exception ->
        "exception"
    | BAD ->
        "bad"
end

module UnaryOp = struct
  type t = Positive | Negative | Not | Invert [@@deriving compare, equal]

  let to_string op =
    match op with
    | Positive ->
        "Positive"
    | Negative ->
        "Negative"
    | Not ->
        "Not"
    | Invert ->
        "Invert"
end

module BinaryOp = struct
  type t =
    | Add
    | And
    | FloorDivide
    | LShift
    | MatrixMultiply
    | Modulo
    | Multiply
    | Or
    | Power
    | RShift
    | Subtract
    | TrueDivide
    | Xor
  [@@deriving compare, equal]

  let to_string op =
    match op with
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
end

module FormatFunction = struct
  type t = Str | Repr | Ascii [@@deriving equal]

  let to_string = function Str -> "str" | Repr -> "repr" | Ascii -> "ascii"
end

module BuiltinCaller = struct
  type unary_intrinsics =
    | PrintExpr
    | ImportStar
    | StopiterationError
    | AsyncGenValueWrapperNew
    | UnaryPos
    | ListToTuple
    | MakeTypevar
    | MakeParamspec
    | MakeTypevartuple
    | SubscriptGeneric
    | MakeTypealias
  [@@deriving equal, show, enumerate]

  let unary_intrinsics = Array.of_list all_of_unary_intrinsics

  type binary_intrinsics =
    | PrepReraiseStar
    | TypevarWithBound
    | TypevarWithConstraints
    | SetFunctionTypeParams
  [@@deriving equal, show, enumerate]

  let binary_intrinsics = Array.of_list all_of_binary_intrinsics

  type t =
    | BuildClass  (** [LOAD_BUILD_CLASS] *)
    | BuildConstKeyMap  (** [BUILD_CONST_KEY_MAP] *)
    | Format
    | FormatFn of FormatFunction.t
    | Inplace of BinaryOp.t
    | Binary of BinaryOp.t
    | BinarySlice
    | Unary of UnaryOp.t
    | Compare of CompareOp.t
    | GetAIter  (** [GET_AITER] *)
    | GetIter  (** [GET_ITER] *)
    | NextIter  (** [FOR_ITER] *)
    | HasNextIter  (** [FOR_ITER] *)
    | IterData  (** [FOR_ITER] *)
    | GetYieldFromIter  (** [GET_YIELD_FROM_ITER] *)
    | ListAppend  (** [LIST_APPEND] *)
    | ListExtend  (** [LIST_EXTEND] *)
    | ListToTuple  (** [LIST_TO_TUPLE] *)
    | SetAdd  (** [SET_ADD] *)
    | SetUpdate  (** [SET_UPDATE] *)
    | DictSetItem  (** [MAP_ADD] *)
    | DictUpdate  (** [DICT_UPDATE] *)
    | DictMerge  (** [DICT_MERGE] *)
    | DeleteSubscr
    | YieldFrom  (** [YIELD_FROM] *)
    | GetAwaitable  (** [GET_AWAITABLE] *)
    | UnpackEx  (** [UNPACK_EX] *)
    | GetPreviousException  (** [RAISE_VARARGS] *)
    | UnaryIntrinsic of unary_intrinsics
    | BinaryIntrinsic of binary_intrinsics
  [@@deriving equal]

  let show = function
    | BuildClass ->
        "$BuildClass"
    | BuildConstKeyMap ->
        "$BuildConstKeyMap"
    | Format ->
        "$Format"
    | FormatFn fn ->
        sprintf "$FormatFn.%s" (FormatFunction.to_string fn)
    | Binary op ->
        let op = BinaryOp.to_string op in
        sprintf "$Binary.%s" op
    | BinarySlice ->
        "$BinarySlice"
    | Inplace op ->
        let op = BinaryOp.to_string op in
        sprintf "$Inplace.%s" op
    | Unary op ->
        let op = UnaryOp.to_string op in
        sprintf "$Unary.%s" op
    | Compare op ->
        sprintf "$Compare.%s" (CompareOp.to_string op)
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
    | ListExtend ->
        "$ListExtend"
    | ListToTuple ->
        "$ListToTuple"
    | SetAdd ->
        "$SetAdd"
    | SetUpdate ->
        "$SetUpdate"
    | DictSetItem ->
        "$DictSetItem"
    | DictUpdate ->
        "$DictUpdate"
    | DictMerge ->
        "$DictMerge"
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
    | UnaryIntrinsic unop ->
        show_unary_intrinsics unop
    | BinaryIntrinsic binop ->
        show_binary_intrinsics binop
end

module Const = struct
  type t =
    | Bool of bool
    | Int of Z.t
    | Float of float
    | Complex of {real: float; imag: float}
    | String of string
    | InvalidUnicode
    | Bytes of bytes
    | None
  [@@deriving equal]

  let pp fmt = function
    | Bool b ->
        F.pp_print_bool fmt b
    | Int i ->
        F.pp_print_string fmt (Z.to_string i)
    | Float f ->
        F.pp_print_float fmt f
    | Complex {real; imag} ->
        F.fprintf fmt "Complex[real:%f; imag:%f ]" real imag
    | String s ->
        F.fprintf fmt "\"%s\"" s
    | InvalidUnicode ->
        F.pp_print_string fmt "InvalidUnicode"
    | Bytes bytes ->
        Bytes.pp fmt bytes
    | None ->
        F.pp_print_string fmt "None"
end

module Exp = struct
  type collection = List | Set | Tuple | Map [@@deriving equal]

  let show_collection = function List -> "List" | Set -> "Set" | Tuple -> "Tuple" | Map -> "Map"
  [@@warning "-unused-value-declaration"]


  (** An expression is an abstraction of the state of Python bytecode interpreter, waiting to be
      turned into Textual. During the translation from bytecode to expressions, we introduce SSA
      identifiers for "yet to be named" expressions.

      In this IR, name resolution is done so naming is not ambiguous. Also, we have reconstructed
      the CFG of the program, lost during Python compilation *)
  type t =
    | AssertionError
    | BuildFrozenSet of t list
    | BuildSlice of t list (* 2 < length <= 3 *)
    | BuildString of t list
    | Collection of {kind: collection; values: t list; unpack: bool}
        (** Helper for [BUILD_LIST/SET/TUPLE/MAP] opcodes *)
    (* [unpack=true] if the arguments in [values] are collections that must be
       unpack and flatten all together to create a unique collection *)
    | Const of Const.t
    | Function of
        { qual_name: QualName.t
        ; default_values: t
        ; default_values_kw: t
        ; annotations: t
        ; cells_for_closure: t }  (** [MAKE_FUNCTION] *)
    | GetAttr of {exp: t; attr: Ident.t} (* foo.bar *)
    | ImportFrom of {name: Ident.t; exp: t}
    | ImportName of {name: Ident.t; fromlist: t; level: t}
    | LoadClassDeref of {name: Ident.t; slot: int}  (** [LOAD_CLASSDEREF] *)
    | LoadClosure of {name: Ident.t; slot: int}  (** [LOAD_CLOSURE] *)
    | LoadDeref of {name: Ident.t; slot: int}  (** [LOAD_DEREF] *)
    | LoadFastCheck of {name: Ident.t}  (** [LOAD_FAST_CHECK] *)
    | LoadFastAndClear of {name: Ident.t}  (** [LOAD_FAST_AND_CLEAR] *)
    | LoadLocals  (** [LOAD_LOCALS] *)
    | LoadFromDictOrDeref of {slot: int; mapping: t}  (** [LOAD_FROM_DICT_OR_DEREF] *)
    | LoadSuperAttr of {attr: Ident.t; super: t; class_: t; self: t}  (** [LOAD_SUPER_ATTR] *)
    | MatchClass of {subject: t; type_: t; count: int; names: t}
    | BoolOfMatchClass of t
    | AttributesOfMatchClass of t
    | MatchSequence of t
    | GetLen of t
    | Subscript of {exp: t; index: t}  (** foo[bar] *)
    | Temp of SSA.t
    | Var of ScopedIdent.t
  [@@deriving equal]

  type opstack_symbol =
    | Exp of t
    | Null
    | Code of FFI.Code.t
    | Keywords of t
    | LoadMethod of (t * Ident.t)  (** [LOAD_METHOD] *)
    | BuiltinCaller of BuiltinCaller.t
    | CallFinallyReturn of {offset: int}
    | ContextManagerExit of t
  [@@deriving equal]

  let none = Temp 0

  let of_int i = Const (Int (Z.of_int i))

  let rec pp fmt = function
    | AssertionError ->
        F.fprintf fmt "$AssertionError"
    | Const c ->
        Const.pp fmt c
    | Var scope_ident ->
        ScopedIdent.pp fmt scope_ident
    | Temp i ->
        SSA.pp fmt i
    | ImportFrom {name; exp} ->
        F.fprintf fmt "$ImportFrom(%a, %a)" Ident.pp name pp exp
    | ImportName {name; fromlist; level} ->
        F.fprintf fmt "$ImportName(%a, %a, %a)" Ident.pp name pp fromlist pp level
    | Subscript {exp; index} ->
        F.fprintf fmt "%a[@[%a@]]" pp exp pp index
    | MatchClass {subject; type_; count; names} ->
        F.fprintf fmt "$MatchClass(%a, %a, %d, %a)" pp subject pp type_ count pp names
    | BoolOfMatchClass exp ->
        F.fprintf fmt "$BoolOfMatchClass(%a)" pp exp
    | AttributesOfMatchClass exp ->
        F.fprintf fmt "$AttributesOfMatchClass(%a)" pp exp
    | MatchSequence exp ->
        F.fprintf fmt "$MatchSequence(%a)" pp exp
    | GetLen exp ->
        F.fprintf fmt "$GetLen(%a)" pp exp
    | BuildSlice values ->
        F.fprintf fmt "$BuildSlice(%a)" (Pp.seq ~sep:", " pp) values
    | BuildString values ->
        F.fprintf fmt "$BuildString(%a)" (Pp.seq ~sep:", " pp) values
    | BuildFrozenSet values ->
        F.fprintf fmt "$BuildFrozenSet(%a)" (Pp.seq ~sep:", " pp) values
    | Collection {kind; values; unpack} ->
        F.fprintf fmt "$Build%s%s(%a)" (show_collection kind)
          (if unpack then "Unpack" else "")
          (Pp.seq ~sep:", " pp) values
    | GetAttr {exp; attr} ->
        F.fprintf fmt "%a.%a" pp exp Ident.pp attr
    | LoadClosure {name; slot} ->
        F.fprintf fmt "$LoadClosure(%d,\"%a\")" slot Ident.pp name
    | LoadDeref {name; slot} ->
        F.fprintf fmt "$LoadDeref(%d,\"%a\")" slot Ident.pp name
    | LoadLocals ->
        F.pp_print_string fmt "$Loadlocals()"
    | LoadFromDictOrDeref {slot; mapping} ->
        F.fprintf fmt "$LoadFromDictOrDeref(%d, %a)" slot pp mapping
    | LoadSuperAttr {attr; self; class_; super} ->
        F.fprintf fmt "$LoadSuperAttr(\"%a\", %a, %a, %a)" Ident.pp attr pp self pp class_ pp super
    | LoadClassDeref {name; slot} ->
        F.fprintf fmt "$LoadClassDeref(%d,\"%a\")" slot Ident.pp name
    | LoadFastCheck {name} ->
        F.fprintf fmt "$LoadFastCheck(\"%a\")" Ident.pp name
    | LoadFastAndClear {name} ->
        F.fprintf fmt "$LoadFastAndClear(\"%a\")" Ident.pp name
    | Function {qual_name; default_values; default_values_kw; annotations; cells_for_closure} ->
        F.fprintf fmt "$MakeFunction[\"%a\", %a, %a, %a, %a]" QualName.pp qual_name pp
          default_values pp default_values_kw pp annotations pp cells_for_closure


  let pp_opstack_symbol fmt = function
    | Exp exp ->
        pp fmt exp
    | Null ->
        F.pp_print_string fmt "NULL"
    | Code {FFI.Code.co_name} ->
        F.fprintf fmt "<%s>" co_name
    | Keywords tuple ->
        F.fprintf fmt "KW %a" pp tuple
    | LoadMethod (self, meth) ->
        F.fprintf fmt "$LoadMethod(%a, %a)" pp self Ident.pp meth
    | BuiltinCaller bc ->
        F.fprintf fmt "BUILTIN_CALLER(%s)" (BuiltinCaller.show bc)
    | ContextManagerExit exp ->
        F.fprintf fmt "CM(%a).__exit__" pp exp
    | CallFinallyReturn {offset} ->
        F.fprintf fmt "CFR(%d)" offset
end

module Location = struct
  type t = int option

  let pp fmt = function None -> F.pp_print_string fmt "?" | Some line -> F.pp_print_int fmt line

  let of_instruction {FFI.Instruction.starts_line} = starts_line

  let of_instructions = function instr :: _ -> of_instruction instr | [] -> None

  let of_code {FFI.Code.instructions} = of_instructions instructions

  let line x = x
end

module Error = struct
  type kind =
    | EmptyStack of string
    | IndexOutOfBound of string
    | ArgValStringExpected of FFI.Constant.t
    | UnsupportedOpcode of string
    | MakeFunction of string * Exp.opstack_symbol
    | LoadMethodExpected of Exp.opstack_symbol
    | CompareOp of int
    | CodeWithoutQualifiedName of FFI.Code.t
    | IllFormedOpstack of int
    | UnpackSequence of int
    | UnexpectedExpression of Exp.opstack_symbol
    | FixpointComputationHeaderReachedTwice of int
    | FixpointComputationNotReached of {src: int; dest: int}
    | NextOffsetMissing
    | MissingNodeInformation of int
    | RaiseExceptionInvalid of int
    | SubroutineEmptyStack of int

  type t = L.error * Location.t * kind

  let pp_kind fmt = function
    | EmptyStack op ->
        F.fprintf fmt "Cannot %s, stack is empty" op
    | IndexOutOfBound opname ->
        F.fprintf fmt "opcode %s raised in IndexOutOfBound error" opname
    | ArgValStringExpected cst ->
        F.fprintf fmt "argval %a should be a string" FFI.Constant.pp cst
    | UnsupportedOpcode s ->
        F.fprintf fmt "Unsupported opcode: %s" s
    | MakeFunction (kind, exp) ->
        F.fprintf fmt "MAKE_FUNCTION: expected %s but got %a" kind Exp.pp_opstack_symbol exp
    | CompareOp n ->
        F.fprintf fmt "COMPARE_OP(%d): invalid operation" n
    | CodeWithoutQualifiedName {FFI.Code.co_name; co_firstlineno; co_filename} ->
        F.fprintf fmt "Unknown code object: %s, at line %d, in %s" co_name co_firstlineno
          co_filename
    | IllFormedOpstack offset ->
        F.fprintf fmt "bad operand stack: offset %d is reachable with two stacks of different sizes"
          offset
    | UnpackSequence n ->
        F.fprintf fmt "UNPACK_SEQUENCE: invalid count %d" n
    | UnexpectedExpression exp ->
        F.fprintf fmt "UNEXPECTED_EXPRESSION: %a" Exp.pp_opstack_symbol exp
    | FixpointComputationHeaderReachedTwice offset ->
        F.fprintf fmt "FIXPOINT_COMPUTATION: loop header %d is already assigned a symbolic stack"
          offset
    | FixpointComputationNotReached {src; dest} ->
        F.fprintf fmt
          "FIXPOINT_COMPUTATION: loop header invariant at %d is not compatible with stack obtained \
           at predecessor %d"
          dest src
    | LoadMethodExpected exp ->
        F.fprintf fmt "LOAD_METHOD_EXPECTED: expected a LOAD_METHOD result but got %a"
          Exp.pp_opstack_symbol exp
    | NextOffsetMissing ->
        F.fprintf fmt "Jump to next instruction detected, but next instruction is missing"
    | MissingNodeInformation offset ->
        F.fprintf fmt "No information about offset %d" offset
    | RaiseExceptionInvalid n ->
        F.fprintf fmt "RAISE_VARARGS: Invalid mode %d" n
    | SubroutineEmptyStack offset ->
        F.fprintf fmt "SUBROUTINE_EMPTY_STACK: Invalid subroutine call stack at offset %d" offset
end

type 'a pyresult = ('a, Error.t) result

module Stmt = struct
  let pp_call_arg fmt value = Exp.pp fmt value

  type gen_kind = Generator | Coroutine | AsyncGenerator

  type t =
    | Let of {lhs: SSA.t; rhs: Exp.t}
    | SetAttr of {lhs: Exp.t; attr: Ident.t; rhs: Exp.t}
    | Store of {lhs: ScopedIdent.t; rhs: Exp.t}
    | StoreSlice of {container: Exp.t; start: Exp.t; end_: Exp.t; rhs: Exp.t}
    | StoreSubscript of {lhs: Exp.t; index: Exp.t; rhs: Exp.t}
    | Call of {lhs: SSA.t; exp: Exp.t; args: Exp.t list; arg_names: Exp.t}
    | CallEx of {lhs: SSA.t; exp: Exp.t; kargs: Exp.t; arg_names: Exp.t}
    | CallMethod of
        {lhs: SSA.t; name: Ident.t; self_if_needed: Exp.t; args: Exp.t list; arg_names: Exp.t}
    | BuiltinCall of {lhs: SSA.t; call: BuiltinCaller.t; args: Exp.t list; arg_names: Exp.t}
    | StoreDeref of {name: Ident.t; slot: int; rhs: Exp.t}  (** [STORE_DEREF] *)
    | Delete of ScopedIdent.t  (** [DELETE_FAST] & cie *)
    | DeleteDeref of {name: Ident.t; slot: int}  (** [DELETE_DEREF] *)
    | DeleteAttr of {exp: Exp.t; attr: Ident.t}
    | MakeCell of int  (** [MAKE_CELL] *)
    | CopyFreeVars of int  (** [COPY_FREE_VARS] *)
    | ImportStar of Exp.t
    | GenStart of {kind: gen_kind}
    | SetupAnnotations
    | Yield of {lhs: SSA.t; rhs: Exp.t}

  let pp fmt = function
    | Let {lhs; rhs} ->
        F.fprintf fmt "%a <- %a" SSA.pp lhs Exp.pp rhs
    | SetAttr {lhs; attr; rhs} ->
        F.fprintf fmt "%a.%a <- %a" Exp.pp lhs Ident.pp attr Exp.pp rhs
    | Store {lhs; rhs} ->
        F.fprintf fmt "%a <- %a" ScopedIdent.pp lhs Exp.pp rhs
    | StoreSlice {container; start; end_; rhs} ->
        F.fprintf fmt "%a[%a:%a] <- %a" Exp.pp container Exp.pp start Exp.pp end_ Exp.pp rhs
    | StoreSubscript {lhs; index; rhs} ->
        F.fprintf fmt "%a[%a] <- %a" Exp.pp lhs Exp.pp index Exp.pp rhs
    | Call {lhs; exp; args; arg_names} ->
        F.fprintf fmt "%a <- $Call(@[%a@])" SSA.pp lhs (Pp.seq ~sep:", " pp_call_arg)
          ((exp :: args) @ [arg_names])
    | CallEx {lhs; exp; kargs; arg_names} ->
        F.fprintf fmt "%a <- $Call(@[%a@])" SSA.pp lhs (Pp.seq ~sep:", " pp_call_arg)
          [exp; kargs; arg_names]
    | CallMethod {lhs; name; self_if_needed; args; arg_names} ->
        F.fprintf fmt "%a <- $CallMethod[%a](@[%a@])" SSA.pp lhs Ident.pp name
          (Pp.seq ~sep:", " Exp.pp)
          ((self_if_needed :: args) @ [arg_names])
    | BuiltinCall {lhs; call; args; arg_names} ->
        F.fprintf fmt "%a <- %s(@[%a@])" SSA.pp lhs (BuiltinCaller.show call)
          (Pp.seq ~sep:", " pp_call_arg) (args @ [arg_names])
    | StoreDeref {name; slot; rhs} ->
        F.fprintf fmt "$StoreDeref(%d,\"%a\", %a)" slot Ident.pp name Exp.pp rhs
    | Delete ident ->
        F.fprintf fmt "$Delete(%a)" ScopedIdent.pp ident
    | DeleteDeref {name; slot} ->
        F.fprintf fmt "$DeleteDeref[%d,\"%a\")" slot Ident.pp name
    | DeleteAttr {exp; attr} ->
        F.fprintf fmt "$DeleteAttr(%a, %a)" Exp.pp exp Ident.pp attr
    | ImportStar exp ->
        F.fprintf fmt "$ImportStart(%a)" Exp.pp exp
    | GenStart {kind} ->
        let kind =
          match kind with
          | Generator ->
              "Generator"
          | Coroutine ->
              "Coroutine"
          | AsyncGenerator ->
              "AsyncGenerator"
        in
        F.fprintf fmt "$GenStart%s()" kind
    | MakeCell i ->
        F.fprintf fmt "$MakeCell(%d)" i
    | CopyFreeVars i ->
        F.fprintf fmt "$CopyFreeVars(%d)" i
    | SetupAnnotations ->
        F.pp_print_string fmt "$SETUP_ANNOTATIONS"
    | Yield {lhs; rhs} ->
        F.fprintf fmt "%a <- $Yield(%a)" SSA.pp lhs Exp.pp rhs
end

let cast_exp ~loc = function
  | Exp.Exp exp ->
      Ok exp
  | exp ->
      Error (L.InternalError, loc, Error.UnexpectedExpression exp)


let cast_exps ~loc exps =
  let open IResult.Let_syntax in
  List.fold_result (List.rev exps) ~init:[] ~f:(fun l exp ->
      let+ exp = cast_exp ~loc exp in
      exp :: l )


module Offset = struct
  type t = int

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


  let pp ~pp fmt list = F.fprintf fmt "[%a]" (Pp.semicolon_seq pp) (List.rev list)
  [@@warning "-unused-value-declaration"]
end

module TerminatorBuilder = struct
  (** This is a terminator in a temporary state. We will cast the [ssa_args] when finalizing the
      consrtruction *)
  type node_call = {label: NodeName.t; ssa_args: Exp.opstack_symbol list}

  let pp_node_call fmt {label; ssa_args} =
    NodeName.pp fmt label ;
    if not (List.is_empty ssa_args) then
      F.fprintf fmt "(@[%a@])" (Pp.seq ~sep:", " Exp.pp_opstack_symbol) ssa_args


  type t =
    | Return of Exp.t
    | Jump of node_call
    | If of {exp: Exp.t; then_: node_call; else_: node_call}
    | Throw of Exp.t

  let mk_node_call label exps = {label; ssa_args= List.rev exps}

  let mk_jump label exps = Jump (mk_node_call label exps)

  let pp fmt = function
    | Return exp ->
        F.fprintf fmt "return %a" Exp.pp exp
    | Jump node_call ->
        F.fprintf fmt "jmp %a" pp_node_call node_call
    | If {exp; then_; else_} ->
        F.fprintf fmt "if %a then jmp %a else jmp %a" Exp.pp exp pp_node_call then_ pp_node_call
          else_
    | Throw exp ->
        F.fprintf fmt "throw %a" Exp.pp exp
  [@@warning "-unused-value-declaration"]
end

module Terminator = struct
  type node_call = {label: NodeName.t; ssa_args: Exp.t list}

  let pp_node_call fmt {label; ssa_args} =
    NodeName.pp fmt label ;
    if not (List.is_empty ssa_args) then F.fprintf fmt "(@[%a@])" (Pp.seq ~sep:", " Exp.pp) ssa_args


  type t =
    | Return of Exp.t
    | Jump of node_call
    | If of {exp: Exp.t; then_: node_call; else_: node_call}
    | Throw of Exp.t

  let pp fmt = function
    | Return exp ->
        F.fprintf fmt "return %a" Exp.pp exp
    | Jump node_call ->
        F.fprintf fmt "jmp %a" pp_node_call node_call
    | If {exp; then_; else_} ->
        F.fprintf fmt "if %a then jmp %a else jmp %a" Exp.pp exp pp_node_call then_ pp_node_call
          else_
    | Throw exp ->
        F.fprintf fmt "throw %a" Exp.pp exp


  let cast_node_call ~loc {TerminatorBuilder.label; ssa_args} =
    let open IResult.Let_syntax in
    let+ ssa_args = cast_exps ~loc ssa_args in
    {label; ssa_args}


  let of_builder ~loc terminator =
    let open IResult.Let_syntax in
    match (terminator : TerminatorBuilder.t) with
    | Return exp ->
        Ok (Return exp)
    | Jump node_call ->
        let+ node_call = cast_node_call ~loc node_call in
        Jump node_call
    | If {exp; then_; else_} ->
        let* then_ = cast_node_call ~loc then_ in
        let+ else_ = cast_node_call ~loc else_ in
        If {exp; then_; else_}
    | Throw exp ->
        Ok (Throw exp)
end

module Node = struct
  type t =
    { name: NodeName.t
    ; first_loc: Location.t
    ; last_loc: Location.t
    ; ssa_parameters: SSA.t list
    ; stmts: (Location.t * Stmt.t) list
    ; last: Terminator.t }

  let pp fmt {name; first_loc; ssa_parameters; stmts; last; last_loc} =
    F.fprintf fmt "@[<hv2>%a%t: @%a@\n" NodeName.pp name
      (fun fmt ->
        if List.is_empty ssa_parameters then F.pp_print_string fmt ""
        else F.fprintf fmt "(%a)" (Pp.seq ~sep:", " SSA.pp) ssa_parameters )
      Location.pp first_loc ;
    List.iter stmts ~f:(fun (loc, stmt) ->
        F.fprintf fmt "@[<hv2>%a @%a@]@\n" Stmt.pp stmt Location.pp loc ) ;
    F.fprintf fmt "%a@\n" Terminator.pp last ;
    F.fprintf fmt "@] @%a@\n" Location.pp last_loc
end

module CFGBuilder = struct
  (** For each (node, terminator) pair, the terminator node.last is not finalized yet, terminator is
      the ground truth instead *)
  type t = {nodes: (Node.t * TerminatorBuilder.t) NodeName.Map.t; fresh_label: int}

  let fresh_start = 0

  let empty = {nodes= NodeName.Map.empty; fresh_label= fresh_start}

  let label_of_int i = sprintf "b%d" i

  let fresh_node_name ({fresh_label} as cfg) offset =
    let fresh = label_of_int fresh_label in
    let cfg = {cfg with fresh_label= fresh_label + 1} in
    (NodeName.mk fresh ~offset, cfg)


  let add name ~first_loc ~last_loc ssa_parameters stmts last {nodes; fresh_label} =
    let node = {Node.name; first_loc; last_loc; ssa_parameters; stmts; last= Return Exp.none} in
    {nodes= NodeName.Map.add name (node, last) nodes; fresh_label}
end

module CodeInfo = struct
  type t =
    { (* see https://docs.python.org/3.8/reference/datamodel.html#index-55 *)
      co_name: Ident.t
    ; co_firstlineno: int
    ; co_nlocals: int
    ; co_argcount: int
    ; co_posonlyargcount: int
    ; co_kwonlyargcount: int
    ; co_cellvars: Ident.t array
    ; co_freevars: Ident.t array
    ; co_names: Ident.t array
    ; co_varnames: Ident.t array
    ; has_star_arguments: bool
    ; has_star_keywords: bool
    ; is_async: bool
    ; is_generator: bool }

  let is_async = function
    | {FFI.Instruction.opname= "GEN_START"; arg= 1} :: _
    | {FFI.Instruction.opname= "RETURN_GENERATOR"} :: _ ->
        true
    | _ ->
        false


  let of_code
      { FFI.Code.co_name
      ; co_firstlineno
      ; co_flags
      ; co_nlocals
      ; co_argcount
      ; co_posonlyargcount
      ; co_kwonlyargcount
      ; co_cellvars
      ; co_freevars
      ; co_names
      ; co_varnames
      ; instructions } =
    { co_name= Ident.mk co_name
    ; co_firstlineno
    ; has_star_arguments= co_flags land 0x04 <> 0
    ; has_star_keywords= co_flags land 0x08 <> 0
    ; is_generator= co_flags land 0x20 <> 0
    ; is_async= is_async instructions
    ; co_nlocals
    ; co_argcount
    ; co_posonlyargcount
    ; co_kwonlyargcount
    ; co_cellvars= Array.map co_cellvars ~f:Ident.mk
    ; co_freevars= Array.map co_freevars ~f:Ident.mk
    ; co_names= Array.map co_names ~f:Ident.mk
    ; co_varnames= Array.map co_varnames ~f:Ident.mk }
end

module CFG = struct
  type t = {entry: NodeName.t; nodes: Node.t NodeName.Map.t; code_info: CodeInfo.t}

  let pp ~name fmt {nodes; code_info= {co_varnames; co_argcount; is_async}} =
    F.fprintf fmt "%sfunction %s(%a):@\n"
      (if is_async then "async " else "")
      name (Pp.seq ~sep:", " Ident.pp)
      (Array.slice co_varnames 0 co_argcount |> Array.to_list) ;
    NodeName.Map.iter (fun _ node -> Node.pp fmt node) nodes


  let of_builder {CFGBuilder.nodes} code =
    let open IResult.Let_syntax in
    let offset = 0 in
    let entry = CFGBuilder.label_of_int CFGBuilder.fresh_start |> NodeName.mk ~offset in
    let+ nodes =
      NodeName.Map.map_result nodes ~f:(fun _name (node, last) ->
          let loc = node.Node.last_loc in
          let+ last = Terminator.of_builder ~loc last in
          {node with Node.last} )
    in
    let nodes =
      NodeName.Map.find_opt entry nodes
      |> Option.value_map ~default:nodes ~f:(fun ({first_loc; stmts} as entry_node : Node.t) ->
             let entry_node =
               {entry_node with stmts= (first_loc, Let {lhs= 0; rhs= Exp.Const Const.None}) :: stmts}
             in
             NodeName.Map.add entry entry_node nodes )
    in
    {nodes; entry; code_info= CodeInfo.of_code code}
end

module State = struct
  (** Internal state of the Bytecode -> IR compiler *)
  type t =
    { debug: bool
    ; version: FFI.version
    ; code_qual_name: FFI.Code.t -> QualName.t option
    ; get_node_name: Offset.t -> NodeName.t pyresult
    ; first_loc: Location.t (* the first source location we have seen during this node session *)
    ; loc: Location.t (* the last source location we have seen during this node sesssion *)
    ; no_loc_seen_during_this_session: bool
    ; cfg: CFGBuilder.t
    ; stack: Exp.opstack_symbol Stack.t
    ; stmts: (Location.t * Stmt.t) list
    ; ssa_parameters: SSA.t list
    ; stack_at_loop_headers: Exp.opstack_symbol Stack.t IMap.t
    ; fresh_id: SSA.t }

  let try_set_current_source_location ({no_loc_seen_during_this_session} as st) loc =
    if Option.is_none loc then st
    else if no_loc_seen_during_this_session then
      (* this is first instruction we found with a loc during this node-construction session *)
      {st with first_loc= loc; loc; no_loc_seen_during_this_session= false}
    else {st with loc}


  let build_get_node_name loc cfg_skeleton =
    let map, cfg =
      IMap.fold
        (fun offset _ (map, cfg) ->
          let name, cfg = CFGBuilder.fresh_node_name cfg offset in
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


  let empty ~debug ~code_qual_name ~loc ~cfg_skeleton version =
    let cfg, get_node_name = build_get_node_name loc cfg_skeleton in
    { debug
    ; version
    ; code_qual_name
    ; get_node_name
    ; first_loc= None
    ; loc= None
    ; no_loc_seen_during_this_session= true
    ; cfg
    ; stack= Stack.empty
    ; stmts= []
    ; ssa_parameters= []
    ; stack_at_loop_headers= IMap.empty
    ; fresh_id= 3 (* we reserve 0 for Const.None, 1 for *&locals and 2 for *&globals *) }


  let dummy_formatter = F.make_formatter (fun _ _ _ -> ()) (fun () -> ())

  let debug {debug} =
    if debug then F.kasprintf (fun s -> F.printf "%s" s) else F.ifprintf dummy_formatter


  (** Each time a new object is discovered (they can be heavily nested), we need to clear the state
      of temporary data like the SSA counter, but keep other parts of it, like global and local
      names *)
  let enter ~debug:d ~code_qual_name ~loc ~cfg_skeleton version qual_name =
    let st = empty ~debug:d ~code_qual_name ~loc ~cfg_skeleton version in
    Option.iter qual_name ~f:(debug st "Translating %a...@\n" QualName.pp) ;
    st


  let get_node_name {get_node_name} offset = get_node_name offset

  let fresh_id ({fresh_id} as st) =
    let st = {st with fresh_id= SSA.next fresh_id} in
    (fresh_id, st)


  let cast_exp st = cast_exp ~loc:st.loc

  let cast_exps st = cast_exps ~loc:st.loc

  let push ({stack} as st) exp =
    let stack = Stack.push stack (Exp.Exp exp) in
    {st with stack}


  let push_symbol ({stack} as st) exp =
    let stack = Stack.push stack exp in
    {st with stack}


  let pop ({stack; loc} as st) =
    match Stack.pop stack with
    | None ->
        Error (L.InternalError, loc, Error.EmptyStack "pop")
    | Some (exp, stack) ->
        let st = {st with stack} in
        Ok (exp, st)


  let pop_and_cast st =
    let open IResult.Let_syntax in
    let* exp, st = pop st in
    let+ exp = cast_exp st exp in
    (exp, st)


  let peek ?(depth = 0) {stack; loc} =
    match Stack.peek depth stack with
    | None ->
        Error (L.InternalError, loc, Error.EmptyStack "peek (block)")
    | Some exp ->
        Ok exp


  let pop_n_and_cast st n =
    let open IResult.Let_syntax in
    let rec aux acc st n =
      let open IResult.Let_syntax in
      if n > 0 then
        let* hd, st = pop st in
        aux (hd :: acc) st (n - 1)
      else Ok (acc, st)
    in
    let* exps, st = aux [] st n in
    let+ exps = cast_exps st exps in
    (exps, st)


  let rot_n st n =
    let {loc; stack} = st in
    if n <= 0 then L.die InternalError "rot_n need a positive argument"
    else if List.length stack < n then
      let msg = F.asprintf "rot_n with n = %d is impossible" n in
      Error (L.InternalError, loc, Error.EmptyStack msg)
    else
      let top_n, rest = List.split_n stack n in
      let top = List.hd_exn top_n in
      let top_1_n = List.tl_exn top_n in
      let stack = top_1_n @ [top] @ rest in
      Ok {st with stack}


  (* TODO: use the [exn_handlers] info to mark statement that can possibly raise * something *)
  let push_stmt ({stmts; loc} as st) stmt = {st with stmts= (loc, stmt) :: stmts}

  let size {stack} = Stack.size stack

  let add_new_node ({cfg} as st) name ~first_loc ~last_loc ssa_parameters stmts last =
    {st with cfg= CFGBuilder.add name ~first_loc ~last_loc ssa_parameters stmts last cfg}


  let record_stack_at_loop_header ({loc; stack; stack_at_loop_headers} as st) header_name =
    let offset = NodeName.get_offset header_name in
    if IMap.mem offset stack_at_loop_headers then
      Error (L.InternalError, loc, Error.FixpointComputationHeaderReachedTwice offset)
    else Ok {st with stack_at_loop_headers= IMap.add offset stack stack_at_loop_headers}


  let check_stack_if_at_back_edge {loc; stack; stack_at_loop_headers} src
      ({TerminatorBuilder.label} as dest_call_node) =
    let dest = NodeName.get_offset label in
    if IMap.mem dest stack_at_loop_headers then
      (* [dest] is a loop header *)
      if List.equal Exp.equal_opstack_symbol stack (IMap.find dest stack_at_loop_headers) then
        Ok {TerminatorBuilder.label; ssa_args= []}
      else Error (L.InternalError, loc, Error.FixpointComputationNotReached {src; dest})
    else Ok dest_call_node


  let check_terminator_if_back_edge st offset terminator =
    let open IResult.Let_syntax in
    let src = offset in
    match (terminator : TerminatorBuilder.t) with
    | Jump call_node ->
        let+ new_call_node = check_stack_if_at_back_edge st src call_node in
        if phys_equal call_node new_call_node then None
        else Some (TerminatorBuilder.Jump new_call_node)
    | If {exp; then_; else_} ->
        let* new_then = check_stack_if_at_back_edge st src then_ in
        let+ new_else = check_stack_if_at_back_edge st src else_ in
        if (not (phys_equal then_ new_then)) || not (phys_equal else_ new_else) then
          Some (TerminatorBuilder.If {exp; then_= new_then; else_= new_else})
        else None
    | _ ->
        Ok None


  let get_terminal_node ({cfg= {CFGBuilder.nodes}} as st) offset succ_name =
    let open IResult.Let_syntax in
    let+ name = get_node_name st offset in
    match NodeName.Map.find_opt name nodes with
    | Some (_, last) -> (
      match last with
      | Jump {ssa_args} ->
          `AlreadyThere (name, ssa_args)
      | If {then_= {label; ssa_args}} when NodeName.equal label succ_name ->
          `AlreadyThere (name, ssa_args)
      | If {else_= {label; ssa_args}} when NodeName.equal label succ_name ->
          `AlreadyThere (name, ssa_args)
      | Return _ | Throw _ | If _ ->
          (* we should only call [get_terminal_node] from a successor, but we
             can not find it there *)
          L.die InternalError "invalid predecessor" )
    | None ->
        `NotYetThere name


  let drop_first_args_terminal_node ({cfg} as st) ~pred_name ~succ_name k =
    (* 0 <= k <= size *)
    (* drop elements k, ..., size-1 *)
    let nodes = cfg.CFGBuilder.nodes in
    match NodeName.Map.find_opt pred_name nodes with
    | Some (node, last) ->
        let opt_new_last =
          match last with
          | Jump {label; ssa_args} ->
              let ssa_args = List.drop ssa_args k in
              let last = TerminatorBuilder.Jump {label; ssa_args} in
              Some last
          | If {exp; then_; else_} ->
              let try_drop ({TerminatorBuilder.label; ssa_args} as node_call) =
                if NodeName.equal label succ_name then
                  {node_call with ssa_args= List.drop ssa_args k}
                else node_call
              in
              Some (TerminatorBuilder.If {exp; then_= try_drop then_; else_= try_drop else_})
          | _ ->
              None
        in
        Option.value_map opt_new_last ~default:st ~f:(fun last ->
            let nodes = NodeName.Map.add pred_name (node, last) nodes in
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


  let enter_node st ~offset ~arity previous_last_loc bottom_stack =
    let st =
      { st with
        stmts= []
      ; stack= bottom_stack
      ; first_loc= previous_last_loc
      ; loc= previous_last_loc
      ; no_loc_seen_during_this_session= true }
    in
    let ssa_parameters, st = mk_ssa_parameters st (arity - List.length bottom_stack) in
    let st = List.fold_right ssa_parameters ~init:st ~f:(fun ssa st -> push st (Exp.Temp ssa)) in
    let st = {st with ssa_parameters} in
    let pp_ssa_parameters fmt =
      if List.is_empty ssa_parameters then F.fprintf fmt ""
      else
        F.fprintf fmt " with params (%a)" (Pp.comma_seq F.pp_print_string)
          (List.map ssa_parameters ~f:(fun i -> sprintf "n%d" i))
    in
    debug st "Building a new node, starting from offset %d%t@\n" offset pp_ssa_parameters ;
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


let call_method st name ?arg_names self_if_needed args =
  let lhs, st = State.fresh_id st in
  let arg_names = Option.value arg_names ~default:Exp.none in
  let stmt = Stmt.CallMethod {lhs; name; self_if_needed; args; arg_names} in
  let st = State.push_stmt st stmt in
  (lhs, st)


type calling_mode = Kargs | Args of int

let call_function st version mode =
  let open IResult.Let_syntax in
  let* tos = State.peek st in
  let* arg_names, st =
    match tos with
    | Exp.Keywords exp ->
        let* _, st = State.pop st in
        Ok (exp, st)
    | _ ->
        Ok (Exp.none, st)
  in
  let arg = match mode with Kargs -> 1 | Args arg -> arg in
  let* args, st = State.pop_n_and_cast st arg in
  let* prepare_args =
    match (version : FFI.version) with
    | Python_3_10 ->
        let+ fun_exp, st = State.pop st in
        `RegularCall (fun_exp, args, st)
    | Python_3_12 -> (
        let* tos1, st = State.pop st in
        let* tos2, st = State.pop st in
        match tos2 with
        | Exp.LoadMethod (self, name) ->
            let id, st = call_method st name self args in
            let st = State.push st (Exp.Temp id) in
            Ok (`MethodCall st)
        | Exp.Null ->
            Ok (`RegularCall (tos1, args, st))
        | _ ->
            let* self = State.cast_exp st tos1 in
            Ok (`RegularCall (tos2, self :: args, st)) )
  in
  let* st =
    match prepare_args with
    | `RegularCall (fun_exp, args, st) ->
        let lhs, st = State.fresh_id st in
        let+ stmt =
          match fun_exp with
          | Exp.BuiltinCaller call ->
              Ok (Stmt.BuiltinCall {lhs; call; args; arg_names})
          | Exp.ContextManagerExit self_if_needed ->
              let name = Ident.Special.exit in
              Ok (Stmt.CallMethod {lhs; name; self_if_needed; args= []; arg_names})
          | exp -> (
              let+ exp = State.cast_exp st exp in
              match (mode, args) with
              | Args _, _ ->
                  Stmt.Call {lhs; exp; args; arg_names}
              | Kargs, [kargs] ->
                  Stmt.CallEx {lhs; exp; kargs; arg_names}
              | Kargs, _ ->
                  L.die InternalError "unexpected number of arguments" )
        in
        let st = State.push_stmt st stmt in
        State.push st (Exp.Temp lhs)
    | `MethodCall st ->
        Ok st
  in
  Ok (st, None)


let call_builtin_function st ?arg_names call args =
  let lhs, st = State.fresh_id st in
  let arg_names = Option.value arg_names ~default:Exp.none in
  let stmt = Stmt.BuiltinCall {lhs; call; args; arg_names} in
  let st = State.push_stmt st stmt in
  Ok (lhs, st)


(** Helper to compile the binary/unary/... ops into IR *)
let parse_op st call n =
  let open IResult.Let_syntax in
  let* args, st = State.pop_n_and_cast st n in
  let* lhs, st = call_builtin_function st call args in
  let st = State.push st (Exp.Temp lhs) in
  Ok (st, None)


let make_function st version flags =
  let open IResult.Let_syntax in
  let* st =
    match (version : FFI.version) with
    | Python_3_10 ->
        let+ _qual_name, st = State.pop st in
        st
    | Python_3_12 ->
        Ok st
  in
  (* we use our own notion of qualified name *)
  let* codeobj, st = State.pop st in
  let* code =
    match (codeobj : Exp.opstack_symbol) with
    | Code c ->
        Ok c
    | _ ->
        internal_error st (Error.MakeFunction ("a code object", codeobj))
  in
  let* qual_name = read_code_qual_name st code in
  let* cells_for_closure, st =
    if flags land 0x08 <> 0 then State.pop_and_cast st else Ok (Exp.none, st)
  in
  let* annotations, st =
    if flags land 0x04 <> 0 then State.pop_and_cast st else Ok (Exp.none, st)
  in
  let* default_values_kw, st =
    if flags land 0x02 <> 0 then State.pop_and_cast st else Ok (Exp.none, st)
  in
  let* default_values, st =
    if flags land 0x01 <> 0 then State.pop_and_cast st else Ok (Exp.none, st)
  in
  let lhs, st = State.fresh_id st in
  let rhs =
    Exp.Function {qual_name; default_values; default_values_kw; annotations; cells_for_closure}
  in
  let stmt = Stmt.Let {lhs; rhs} in
  let st = State.push_stmt st stmt in
  let st = State.push st (Exp.Temp lhs) in
  Ok (st, None)


let call_function_kw st version argc =
  let open IResult.Let_syntax in
  let* kw_exp, st = State.pop_and_cast st in
  let st = State.push_symbol st (Keywords kw_exp) in
  call_function st version (Args argc)


let call_function_ex st version flags =
  let open IResult.Let_syntax in
  let with_keyword_args = flags land 1 <> 0 in
  let* st =
    if with_keyword_args then
      let* kw_exp, st = State.pop_and_cast st in
      let st = State.push_symbol st (Keywords kw_exp) in
      Ok st
    else Ok st
  in
  call_function st version Kargs


let unpack_sequence st count =
  let open IResult.Let_syntax in
  let* tos, st = State.pop_and_cast st in
  let rec unpack st n =
    if n < 0 then Ok st
    else
      let index = Exp.of_int n in
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
        Some FormatFunction.Str
    | 0x02 ->
        Some FormatFunction.Repr
    | 0x03 ->
        Some FormatFunction.Ascii
    | _ ->
        L.die InternalError "FORMAT_VALUE: unreachable"
  in
  let* fmt_spec, st =
    (* fmt_spec must be a concatenation of string literals *)
    if has_fmt_spec flags then State.pop_and_cast st else Ok (Exp.none, st)
  in
  let* exp, st = State.pop_and_cast st in
  let conv_fn = mk_conv flags in
  let* exp, st =
    match conv_fn with
    | None ->
        Ok (exp, st)
    | Some conv_fn ->
        let+ id, st = call_builtin_function st (FormatFn conv_fn) [exp] in
        (Exp.Temp id, st)
  in
  let* id, st = call_builtin_function st Format [exp; fmt_spec] in
  let st = State.push st (Exp.Temp id) in
  Ok (st, None)


let mk_if b exp then_ else_ =
  if b then TerminatorBuilder.If {exp; then_; else_}
  else TerminatorBuilder.If {exp; then_= else_; else_= then_}


let pop_jump_if ~next_is st arg next_offset_opt =
  let open IResult.Let_syntax in
  let {State.loc} = st in
  let* condition, st = State.pop_and_cast st in
  (* Turn the stack into SSA parameters *)
  let* next_offset = Offset.get ~loc next_offset_opt in
  let* next_label = State.get_node_name st next_offset in
  let other_target =
    match st.State.version with Python_3_10 -> 2 * arg | Python_3_12 -> next_offset + (2 * arg)
  in
  let* other_label = State.get_node_name st other_target in
  let mk_if = mk_if next_is condition in
  let {State.stack} = st in
  Ok
    ( st
    , Some
        (mk_if
           (TerminatorBuilder.mk_node_call next_label stack)
           (TerminatorBuilder.mk_node_call other_label stack) ) )


let jump_if_or_pop st ~jump_if target next_offset_opt =
  let open IResult.Let_syntax in
  let {State.loc} = st in
  (* We only peek the top of stack so the logic of [to_ssa] correctly captures it for
     the branch where "it stays". It will be restored as part of the stack restore logic when
     we process the "other" node. *)
  let* condition = State.peek st in
  let* condition = State.cast_exp st condition in
  let mk_if = mk_if (not jump_if) condition in
  let {State.stack} = st in
  let* next_offset = Offset.get ~loc next_offset_opt in
  (* In the next branch, we make sure the top-of-stack is no longer there. *)
  let* next_label = State.get_node_name st next_offset in
  let next_stack = List.tl stack |> Option.value ~default:[] in
  let* other_label = State.get_node_name st target in
  Ok
    ( st
    , Some
        (mk_if
           (TerminatorBuilder.mk_node_call next_label next_stack)
           (TerminatorBuilder.mk_node_call other_label stack) ) )


let for_iter st delta next_offset_opt =
  let open IResult.Let_syntax in
  let {State.loc} = st in
  let* iter, st =
    match st.State.version with
    | Python_3_10 ->
        State.pop_and_cast st
    | Python_3_12 ->
        let* iter = State.peek st in
        let+ iter = State.cast_exp st iter in
        (iter, st)
  in
  let {State.stack= other_stack} = st in
  let* id, st = call_builtin_function st NextIter [iter] in
  let* has_item, st = call_builtin_function st HasNextIter [iter] in
  let condition = Exp.Temp has_item in
  (* In the next branch, we know the iterator has an item available. Let's fetch it and
     push it on the stack. *)
  let* next_offset = Offset.get ~loc next_offset_opt in
  let st =
    match st.State.version with
    | Python_3_10 ->
        (* The iterator object stays on the stack while in the for loop, let's push it back *)
        State.push st iter
    | Python_3_12 ->
        st
  in
  let st = State.push st (Exp.Temp id) in
  let* next_label = State.get_node_name st next_offset in
  let next_stack, other_stack =
    match st.State.version with
    | Python_3_10 ->
        (st.State.stack, other_stack)
    | Python_3_12 ->
        (st.State.stack, st.State.stack)
  in
  let other_offset = delta + next_offset in
  let* other_label = State.get_node_name st other_offset in
  Ok
    ( st
    , Some
        (TerminatorBuilder.If
           { exp= condition
           ; then_= TerminatorBuilder.mk_node_call next_label next_stack
           ; else_= TerminatorBuilder.mk_node_call other_label other_stack } ) )


let raise_varargs st argc =
  let open IResult.Let_syntax in
  match argc with
  | 0 ->
      (* There should be something like 3 values left on the stack at this
         point, leftovers from the 6 values pushed on the exception entry
         point. We should use them to compute the right "previous" exception,
         but this logic is left TODO *)
      let* id, st = call_builtin_function st GetPreviousException [] in
      let throw = TerminatorBuilder.Throw (Exp.Temp id) in
      Ok (st, Some throw)
  | 1 ->
      let* tos, st = State.pop_and_cast st in
      let throw = TerminatorBuilder.Throw tos in
      Ok (st, Some throw)
  | 2 ->
      let* rhs, st = State.pop_and_cast st in
      let* lhs, st = State.pop_and_cast st in
      let st = State.push_stmt st (Stmt.SetAttr {lhs; attr= Ident.Special.cause; rhs}) in
      let throw = TerminatorBuilder.Throw lhs in
      Ok (st, Some throw)
  | _ ->
      external_error st (Error.RaiseExceptionInvalid argc)


let get_cell_name {FFI.Code.co_cellvars; co_freevars; version} st arg argval =
  match (version : FFI.version) with
  | Python_3_10 ->
      let sz = Array.length co_cellvars in
      let name = if arg < sz then co_cellvars.(arg) else co_freevars.(arg - sz) in
      Ok (Ident.mk name)
  | Python_3_12 -> (
    match argval with
    | FFI.Constant.PYCString str ->
        Ok (Ident.mk str)
    | _ ->
        external_error st (Error.ArgValStringExpected argval) )


let build_collection st count ~f =
  let open IResult.Let_syntax in
  let* values, st = State.pop_n_and_cast st count in
  let rhs = f values in
  let id, st = State.fresh_id st in
  let exp = Exp.Temp id in
  let stmt = Stmt.Let {lhs= id; rhs} in
  let st = State.push_stmt st stmt in
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
      let* value, st = State.pop_and_cast st in
      let* key, st = State.pop_and_cast st in
      Ok ([key; value], st)
    else
      let* elt, st = State.pop_and_cast st in
      Ok ([elt], st)
  in
  let* tos1 = State.peek st ~depth:(arg - 1) in
  let* tos1 = State.cast_exp st tos1 in
  let* _id, st = call_builtin_function st builtin (tos1 :: args) in
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
  let* rhs, st = State.pop_and_cast st in
  let stmt = Stmt.Store {lhs; rhs} in
  let st = State.push_stmt st stmt in
  Ok (st, None)


let rec convert_ffi_const st (const : FFI.Constant.t) : Exp.opstack_symbol pyresult =
  let open IResult.Let_syntax in
  match const with
  | PYCBool b ->
      Ok (Exp (Const (Bool b)))
  | PYCInt i ->
      Ok (Exp (Const (Int i)))
  | PYCFloat f ->
      Ok (Exp (Const (Float f)))
  | PYCComplex {real; imag} ->
      Ok (Exp (Const (Complex {real; imag})))
  | PYCString s ->
      Ok (Exp (Const (String s)))
  | PYCInvalidUnicode ->
      Ok (Exp (Const InvalidUnicode))
  | PYCBytes bytes ->
      Ok (Exp (Const (Bytes bytes)))
  | PYCTuple array ->
      let+ values = Array.to_list array |> map_convert_ffi_const st in
      Exp.Exp (Collection {kind= Tuple; values; unpack= false})
  | PYCFrozenSet list ->
      let+ values = map_convert_ffi_const st list in
      Exp.Exp (BuildFrozenSet values)
  | PYCCode c ->
      Ok (Code c)
  | PYCNone ->
      Ok (Exp Exp.none)


and map_convert_ffi_const st values =
  let open IResult.Let_syntax in
  List.fold_result (List.rev values) ~init:[] ~f:(fun values cst ->
      let* exp = convert_ffi_const st cst in
      let+ exp = State.cast_exp st exp in
      exp :: values )


let only_supported_in_python_3_10 opname version =
  match version with
  | FFI.Python_3_10 ->
      ()
  | FFI.Python_3_12 ->
      L.die UserError "opcode %s should not appear with Python3.12" opname


let only_supported_from_python_3_12 opname version =
  match version with
  | FFI.Python_3_12 ->
      ()
  | FFI.Python_3_10 ->
      L.die UserError "opcode %s should not appear with Python3.10" opname


let binary_ops : BinaryOp.t array =
  [| Add
   ; And
   ; FloorDivide
   ; LShift
   ; MatrixMultiply
   ; Multiply
   ; Modulo (* named Remainder in Python 3.12 *)
   ; Or
   ; Power
   ; RShift
   ; Subtract
   ; TrueDivide
   ; Xor |]


let nb_binary_ops = Array.length binary_ops

let jump_absolute st arg next_offset =
  let open IResult.Let_syntax in
  let offset = next_offset + (2 * arg) in
  let* label = State.get_node_name st offset in
  let {State.stack} = st in
  let jump = TerminatorBuilder.mk_jump label stack in
  Ok (st, Some jump)


let parse_bytecode st ({FFI.Code.co_consts; co_names; co_varnames; version} as code)
    ({FFI.Instruction.opname; starts_line; arg; argval} as instr) next_offset_opt =
  let open IResult.Let_syntax in
  let st = State.try_set_current_source_location st starts_line in
  State.debug st "%a@\n" FFI.Instruction.pp instr ;
  try
    match opname with
    | "LOAD_ASSERTION_ERROR" ->
        assign_to_temp_and_push st AssertionError
    | "LOAD_CONST" ->
        let* exp = convert_ffi_const st co_consts.(arg) in
        let st = State.push_symbol st exp in
        Ok (st, None)
    | "LOAD_NAME" ->
        load st Name co_names.(arg)
    | "LOAD_GLOBAL" ->
        let idx = match version with Python_3_10 -> arg | Python_3_12 -> arg lsr 1 in
        let st =
          match version with
          | Python_3_12 when arg land 1 <> 0 ->
              State.push_symbol st Null
          | _ ->
              st
        in
        load st Global co_names.(idx)
    | "LOAD_FAST" ->
        load st Fast co_varnames.(arg)
    | "LOAD_ATTR" -> (
        let idx = match version with Python_3_10 -> arg | Python_3_12 -> arg lsr 1 in
        let attr = co_names.(idx) |> Ident.mk in
        let* exp, st = State.pop_and_cast st in
        match version with
        | Python_3_12 when arg land 1 <> 0 ->
            let st = State.push_symbol st (LoadMethod (exp, attr)) in
            let st = State.push st exp in
            Ok (st, None)
        | _ ->
            let exp = Exp.GetAttr {exp; attr} in
            assign_to_temp_and_push st exp )
    | "LOAD_DEREF" ->
        let* name = get_cell_name code st arg argval in
        let rhs = Exp.LoadDeref {slot= arg; name} in
        let lhs, st = State.fresh_id st in
        let stmt = Stmt.Let {lhs; rhs} in
        let st = State.push_stmt st stmt in
        let st = State.push st (Exp.Temp lhs) in
        Ok (st, None)
    | "LOAD_CLASSDEREF" ->
        let* name = get_cell_name code st arg argval in
        let rhs = Exp.LoadClassDeref {slot= arg; name} in
        let lhs, st = State.fresh_id st in
        let stmt = Stmt.Let {lhs; rhs} in
        let st = State.push_stmt st stmt in
        let st = State.push st (Exp.Temp lhs) in
        Ok (st, None)
    | "LOAD_FAST_CHECK" ->
        only_supported_from_python_3_12 opname version ;
        let name = co_varnames.(arg) |> Ident.mk in
        let rhs = Exp.LoadFastCheck {name} in
        let lhs, st = State.fresh_id st in
        let stmt = Stmt.Let {lhs; rhs} in
        let st = State.push_stmt st stmt in
        let st = State.push st (Exp.Temp lhs) in
        Ok (st, None)
    | "LOAD_FAST_AND_CLEAR" ->
        only_supported_from_python_3_12 opname version ;
        let name = co_varnames.(arg) |> Ident.mk in
        let rhs = Exp.LoadFastAndClear {name} in
        let lhs, st = State.fresh_id st in
        let stmt = Stmt.Let {lhs; rhs} in
        let st = State.push_stmt st stmt in
        let st = State.push st (Exp.Temp lhs) in
        Ok (st, None)
    | "STORE_NAME" ->
        store st Name co_names.(arg)
    | "STORE_GLOBAL" ->
        store st Global co_names.(arg)
    | "STORE_FAST" ->
        store st Fast co_varnames.(arg)
    | "STORE_ATTR" ->
        let attr = co_names.(arg) |> Ident.mk in
        let* lhs, st = State.pop_and_cast st in
        let* rhs, st = State.pop_and_cast st in
        let stmt = Stmt.SetAttr {lhs; attr; rhs} in
        let st = State.push_stmt st stmt in
        Ok (st, None)
    | "STORE_SUBSCR" ->
        (* Implements TOS1[TOS] = TOS2.  *)
        let* index, st = State.pop_and_cast st in
        let* lhs, st = State.pop_and_cast st in
        let* rhs, st = State.pop_and_cast st in
        let stmt = Stmt.StoreSubscript {lhs; index; rhs} in
        let st = State.push_stmt st stmt in
        Ok (st, None)
    | "STORE_DEREF" ->
        let* name = get_cell_name code st arg argval in
        let* rhs, st = State.pop_and_cast st in
        let stmt = Stmt.StoreDeref {name; slot= arg; rhs} in
        let st = State.push_stmt st stmt in
        Ok (st, None)
    | "STORE_SLICE" ->
        only_supported_from_python_3_12 opname version ;
        let* end_, st = State.pop_and_cast st in
        let* start, st = State.pop_and_cast st in
        let* container, st = State.pop_and_cast st in
        let* rhs, st = State.pop_and_cast st in
        let st = State.push_stmt st (StoreSlice {container; start; end_; rhs}) in
        Ok (st, None)
    | "COPY_FREE_VARS" ->
        only_supported_from_python_3_12 opname version ;
        let st = State.push_stmt st (CopyFreeVars arg) in
        Ok (st, None)
    | "MAKE_CELL" ->
        only_supported_from_python_3_12 opname version ;
        let st = State.push_stmt st (MakeCell arg) in
        Ok (st, None)
    | "CHECK_EXC_MATCH" ->
        only_supported_from_python_3_12 opname version ;
        let* _, st = State.pop st in
        let* _, st = State.pop st in
        let st = State.push_symbol st Exp.Null in
        Ok (st, None)
    | "CLEANUP_THROW" ->
        only_supported_from_python_3_12 opname version ;
        Ok (st, None)
    | "PUSH_EXC_INFO" ->
        only_supported_from_python_3_12 opname version ;
        let* tos, st = State.pop st in
        let st = State.push_symbol st Null in
        let st = State.push_symbol st tos in
        Ok (st, None)
    | "RETURN_GENERATOR" ->
        let st = State.push_symbol st Exp.Null in
        (* NOTE: unclear what we should do with generated value *)
        Ok (st, None)
    | "RETURN_VALUE" ->
        let* ret, st = State.pop_and_cast st in
        Ok (st, Some (TerminatorBuilder.Return ret))
    | "KW_NAMES" ->
        only_supported_from_python_3_12 opname version ;
        let* exp = convert_ffi_const st co_consts.(arg) in
        let* names_tuple = State.cast_exp st exp in
        let st = State.push_symbol st (Keywords names_tuple) in
        Ok (st, None)
    | "CALL" ->
        only_supported_from_python_3_12 opname version ;
        call_function st version (Args arg)
    | "CALL_FUNCTION" ->
        only_supported_in_python_3_10 opname version ;
        call_function st version (Args arg)
    | "CALL_FUNCTION_KW" ->
        only_supported_in_python_3_10 opname version ;
        call_function_kw st version arg
    | "CALL_FUNCTION_EX" ->
        call_function_ex st version arg
    | "CALL_INTRINSIC_1" ->
        let* tos, st = State.pop_and_cast st in
        let* id, st =
          call_builtin_function st (UnaryIntrinsic BuiltinCaller.unary_intrinsics.(arg - 1)) [tos]
        in
        let st = State.push st (Exp.Temp id) in
        Ok (st, None)
    | "CALL_INTRINSIC_2" ->
        let* arg2, st = State.pop_and_cast st in
        let* arg1, st = State.pop_and_cast st in
        let* id, st =
          call_builtin_function st
            (BinaryIntrinsic BuiltinCaller.binary_intrinsics.(arg - 1))
            [arg1; arg2]
        in
        let st = State.push st (Exp.Temp id) in
        Ok (st, None)
    | "POP_TOP" ->
        let* _, st = State.pop st in
        Ok (st, None)
    | "BINARY_OP" ->
        only_supported_from_python_3_12 opname version ;
        let op : BuiltinCaller.t =
          if arg < nb_binary_ops then Binary binary_ops.(arg)
          else if arg < 2 * nb_binary_ops then Inplace binary_ops.(arg - nb_binary_ops)
          else assert false
        in
        parse_op st op 2
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
    | "BINARY_SLICE" ->
        only_supported_from_python_3_12 opname version ;
        let* end_, st = State.pop_and_cast st in
        let* start, st = State.pop_and_cast st in
        let* container, st = State.pop_and_cast st in
        let* id, st = call_builtin_function st BinarySlice [container; start; end_] in
        let st = State.push st (Exp.Temp id) in
        Ok (st, None)
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
        make_function st version arg
    | "BUILD_CONST_KEY_MAP" -> (
        let* keys, st = State.pop_and_cast st in
        match keys with
        | Exp.Collection {kind= Tuple; values= keys_list} ->
            build_collection st arg ~f:(fun values ->
                let bindings =
                  List.concat (List.map2_exn ~f:(fun x y -> [x; y]) keys_list values)
                in
                Exp.Collection {kind= Map; values= bindings; unpack= false} )
        | _ ->
            L.die InternalError "BUILD_CONST_KEY_MAP: keys must be a tuple" )
    | "BUILD_LIST" ->
        build_collection st arg ~f:(fun values ->
            Exp.Collection {kind= List; values; unpack= false} )
    | "BUILD_SET" ->
        build_collection st arg ~f:(fun values -> Exp.Collection {kind= Set; values; unpack= false})
    | "BUILD_TUPLE" ->
        build_collection st arg ~f:(fun values ->
            Exp.Collection {kind= Tuple; values; unpack= false} )
    | "BUILD_SLICE" ->
        build_collection st arg ~f:(fun values -> Exp.BuildSlice values)
    | "BUILD_STRING" ->
        build_collection st arg ~f:(fun values -> Exp.BuildString values)
    | "BUILD_MAP" ->
        build_collection st (2 * arg) ~f:(fun values ->
            Exp.Collection {kind= Map; values; unpack= false} )
    | "BINARY_SUBSCR" ->
        let* index, st = State.pop_and_cast st in
        let* exp, st = State.pop_and_cast st in
        let exp = Exp.Subscript {exp; index} in
        assign_to_temp_and_push st exp
    | "LOAD_BUILD_CLASS" ->
        let st = State.push_symbol st (BuiltinCaller BuildClass) in
        Ok (st, None)
    | "LOAD_METHOD" ->
        let name = co_names.(arg) |> Ident.mk in
        let* tos, st = State.pop_and_cast st in
        let exp = Exp.LoadMethod (tos, name) in
        let st = State.push_symbol st exp in
        Ok (st, None)
    | "CALL_METHOD" ->
        let* args, st = State.pop_n_and_cast st arg in
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
    | "GEN_START" ->
        (* Note: the spec says we need to pop the stack, but in practice there is nothing to pop.
           I think there is implicit initialization of the operand stack going on for coroutines.
           We will investigate later when coroutines will be managed more seriously *)
        let kind : Stmt.gen_kind =
          match arg with
          | 0 ->
              Generator
          | 1 ->
              Coroutine
          | 2 ->
              AsyncGenerator
          | _ ->
              (* should never happen according to
                 https://github.com/python/cpython/blob/v3.10.9/Python/ceval.c#L2653 *)
              L.die InternalError "GEN_START wrong arg"
        in
        let stmt = Stmt.GenStart {kind} in
        let st = State.push_stmt st stmt in
        Ok (st, None)
    | "IMPORT_NAME" | "EAGER_IMPORT_NAME" (* note: I did not find spec for this opcode *) ->
        let name = co_names.(arg) |> Ident.mk in
        let* fromlist, st = State.pop_and_cast st in
        let* level, st = State.pop_and_cast st in
        let lhs, st = State.fresh_id st in
        let rhs = Exp.ImportName {name; fromlist; level} in
        let stmt = Stmt.Let {lhs; rhs} in
        let st = State.push_stmt st stmt in
        let st = State.push st (Exp.Temp lhs) in
        Ok (st, None)
    | "IMPORT_STAR" ->
        let* module_object, st = State.pop_and_cast st in
        let st = State.push_stmt st (ImportStar module_object) in
        Ok (st, None)
    | "IMPORT_FROM" ->
        let name = co_names.(arg) |> Ident.mk in
        let* exp = State.peek st in
        let* exp = State.cast_exp st exp in
        let lhs, st = State.fresh_id st in
        let rhs = Exp.ImportFrom {name; exp} in
        let stmt = Stmt.Let {lhs; rhs} in
        let st = State.push_stmt st stmt in
        let st = State.push st (Exp.Temp lhs) in
        Ok (st, None)
    | "COMPARE_OP" ->
        let idx = match version with Python_3_10 -> arg | Python_3_12 -> arg lsr 4 in
        let* cmp_op =
          match List.nth CompareOp.all idx with
          | Some op ->
              Ok op
          | None ->
              external_error st (Error.CompareOp idx)
        in
        let* rhs, st = State.pop_and_cast st in
        let* lhs, st = State.pop_and_cast st in
        let* id, st = call_builtin_function st (Compare cmp_op) [lhs; rhs] in
        let st = State.push st (Exp.Temp id) in
        Ok (st, None)
    | "LOAD_LOCALS" ->
        let st = State.push st LoadLocals in
        Ok (st, None)
    | "LOAD_FROM_DICT_OR_DEREF" ->
        let* mapping, st = State.pop_and_cast st in
        let st = State.push st (LoadFromDictOrDeref {slot= arg; mapping}) in
        Ok (st, None)
    | "LOAD_SUPER_ATTR" ->
        let* self, st = State.pop_and_cast st in
        let* class_, st = State.pop_and_cast st in
        let* super, st = State.pop_and_cast st in
        let attr = co_names.(arg lsr 2) |> Ident.mk in
        let lhs, st = State.fresh_id st in
        let rhs = Exp.LoadSuperAttr {attr; self; class_; super} in
        let st = State.push_stmt st (Let {lhs; rhs}) in
        (* TODO: use [arg land 2] *)
        let st = if arg land 1 <> 0 then State.push_symbol st Null else st in
        let st = State.push st (Exp.Temp lhs) in
        Ok (st, None)
    | "LOAD_CLOSURE" ->
        let* name = get_cell_name code st arg argval in
        let rhs = Exp.LoadClosure {slot= arg; name} in
        let lhs, st = State.fresh_id st in
        let stmt = Stmt.Let {lhs; rhs} in
        let st = State.push_stmt st stmt in
        let st = State.push st (Exp.Temp lhs) in
        Ok (st, None)
    | "RESUME" ->
        only_supported_from_python_3_12 opname version ;
        Ok (st, None)
    | "NOP" ->
        Ok (st, None)
    | "IS_OP" ->
        let* rhs, st = State.pop_and_cast st in
        let* lhs, st = State.pop_and_cast st in
        let cmp = if Int.equal arg 1 then CompareOp.IsNot else CompareOp.Is in
        let* id, st = call_builtin_function st (Compare cmp) [lhs; rhs] in
        let st = State.push st (Exp.Temp id) in
        Ok (st, None)
    | "CONTAINS_OP" ->
        let* rhs, st = State.pop_and_cast st in
        let* lhs, st = State.pop_and_cast st in
        let cmp = if Int.equal arg 1 then CompareOp.NotIn else CompareOp.In in
        let* id, st = call_builtin_function st (Compare cmp) [lhs; rhs] in
        let st = State.push st (Exp.Temp id) in
        Ok (st, None)
    | "COPY" ->
        only_supported_from_python_3_12 opname version ;
        let* elt = State.peek ~depth:(arg - 1) st in
        let st = State.push_symbol st elt in
        Ok (st, None)
    | "DUP_TOP" ->
        let* tos = State.peek st in
        let st = State.push_symbol st tos in
        Ok (st, None)
    | "SWAP" ->
        only_supported_from_python_3_12 opname version ;
        let* tos1, st = State.pop st in
        let* tos2, st = State.pop st in
        let st = State.push_symbol st tos1 in
        let st = State.push_symbol st tos2 in
        Ok (st, None)
    | "PUSH_NULL" ->
        only_supported_from_python_3_12 opname version ;
        let st = State.push_symbol st Null in
        Ok (st, None)
    | "RETURN_CONST" ->
        only_supported_from_python_3_12 opname version ;
        let* exp = convert_ffi_const st co_consts.(arg) in
        let* ret = State.cast_exp st exp in
        Ok (st, Some (TerminatorBuilder.Return ret))
    | "UNPACK_SEQUENCE" ->
        unpack_sequence st arg
    | "FORMAT_VALUE" ->
        format_value st arg
    | "POP_JUMP_IF_TRUE" ->
        pop_jump_if ~next_is:false st arg next_offset_opt
    | "POP_JUMP_IF_FALSE" ->
        pop_jump_if ~next_is:true st arg next_offset_opt
    | "POP_JUMP_IF_NONE" ->
        only_supported_from_python_3_12 opname version ;
        pop_jump_if ~next_is:true st arg next_offset_opt
    | "POP_JUMP_IF_NOT_NONE" ->
        only_supported_from_python_3_12 opname version ;
        pop_jump_if ~next_is:false st arg next_offset_opt
    | "JUMP_FORWARD" ->
        let {State.loc} = st in
        let* next_offset = Offset.get ~loc next_offset_opt in
        jump_absolute st arg next_offset
    | "JUMP_BACKWARD" | "JUMP_BACKWARD_NO_INTERRUPT" ->
        only_supported_from_python_3_12 opname version ;
        let next_offset = instr.FFI.Instruction.offset + 2 in
        jump_absolute st (-arg) next_offset
    | "JUMP_ABSOLUTE" ->
        let* label = State.get_node_name st (2 * arg) in
        let {State.stack} = st in
        let jump = TerminatorBuilder.mk_jump label stack in
        Ok (st, Some jump)
    | "GET_ITER" ->
        let* tos, st = State.pop_and_cast st in
        let* id, st = call_builtin_function st GetIter [tos] in
        let exp = Exp.Temp id in
        let st = State.push st exp in
        Ok (st, None)
    | "GET_AITER" ->
        let* tos, st = State.pop_and_cast st in
        let id, st = call_method st Ident.Special.aiter tos [] in
        let st = State.push st (Exp.Temp id) in
        Ok (st, None)
    | "FOR_ITER" ->
        for_iter st (2 * arg) next_offset_opt
    | "END_FOR" ->
        let* _, st = State.pop st in
        let* _, st = State.pop st in
        Ok (st, None)
    | "JUMP_IF_TRUE_OR_POP" ->
        jump_if_or_pop ~jump_if:true st (2 * arg) next_offset_opt
    | "JUMP_IF_FALSE_OR_POP" ->
        jump_if_or_pop ~jump_if:false st (2 * arg) next_offset_opt
    | "DUP_TOP_TWO" ->
        let* tos0, st = State.pop st in
        let* tos1, st = State.pop st in
        let st = State.push_symbol st tos1 in
        let st = State.push_symbol st tos0 in
        let st = State.push_symbol st tos1 in
        let st = State.push_symbol st tos0 in
        Ok (st, None)
    | "EXTENDED_ARG" ->
        (* The FFI.Instruction framework already did the magic and this opcode can be ignored. *)
        Ok (st, None)
    | "POP_BLOCK" ->
        Ok (st, None)
    | "ROT_TWO" ->
        let* st = State.rot_n st 2 in
        Ok (st, None)
    | "ROT_THREE" ->
        let* st = State.rot_n st 3 in
        Ok (st, None)
    | "ROT_FOUR" ->
        let* st = State.rot_n st 4 in
        Ok (st, None)
    | "ROT_N" ->
        let* st = State.rot_n st arg in
        Ok (st, None)
    | "SETUP_WITH" | "BEFORE_WITH" ->
        (* SETUP_WITH is used in Python3.10 while BEFORE_WITH is used in Python3.12 *)
        let* context_manager, st = State.pop_and_cast st in
        let st = State.push_symbol st (ContextManagerExit context_manager) in
        let id, st = call_method st Ident.Special.enter context_manager [] in
        let st = State.push st (Exp.Temp id) in
        Ok (st, None)
    | "BEGIN_FINALLY" ->
        let {State.loc} = st in
        let* next_offset = Offset.get ~loc next_offset_opt in
        let* label = State.get_node_name st next_offset in
        let st = State.push st Exp.none in
        let {State.stack} = st in
        let jump = TerminatorBuilder.mk_jump label stack in
        Ok (st, Some jump)
    | "SETUP_FINALLY" ->
        Ok (st, None)
    | "END_FINALLY" ->
        let {State.loc} = st in
        let* next_offset = Offset.get ~loc next_offset_opt in
        let* ret, st = State.pop st in
        let {State.stack} = st in
        let label_offset =
          match ret with Exp.CallFinallyReturn {offset} -> offset | _ -> next_offset
        in
        let* label = State.get_node_name st label_offset in
        let jump = TerminatorBuilder.mk_jump label stack in
        Ok (st, Some jump)
    | "CALL_FINALLY" ->
        let {State.loc} = st in
        let* next_offset = Offset.get ~loc next_offset_opt in
        let jump_offset = next_offset + arg in
        let st = State.push_symbol st (Exp.CallFinallyReturn {offset= next_offset}) in
        let* label = State.get_node_name st jump_offset in
        let {State.stack} = st in
        Ok (st, Some (TerminatorBuilder.mk_jump label stack))
    | "POP_FINALLY" ->
        (* see https://github.com/python/cpython/blob/3.8/Python/ceval.c#L2129 *)
        let* st =
          if arg <> 0 then
            let* ret, st = State.pop st in
            let* _, st = State.pop st in
            let st = State.push_symbol st ret in
            Ok st
          else
            let* _, st = State.pop st in
            Ok st
        in
        Ok (st, None)
    | "RAISE_VARARGS" ->
        raise_varargs st arg
    | "POP_EXCEPT" ->
        internal_error st (Error.UnsupportedOpcode opname)
    | "BUILD_TUPLE_UNPACK_WITH_CALL"
      (* No real difference between the two but in case of an error, which shouldn't happen since
         the code is known to compile *)
    | "BUILD_TUPLE_UNPACK" ->
        build_collection st arg ~f:(fun values ->
            Exp.Collection {kind= Tuple; values; unpack= true} )
    | "BUILD_LIST_UNPACK" ->
        build_collection st arg ~f:(fun values -> Exp.Collection {kind= List; values; unpack= true})
    | "BUILD_SET_UNPACK" ->
        build_collection st arg ~f:(fun values -> Exp.Collection {kind= Set; values; unpack= true})
    | "BUILD_MAP_UNPACK_WITH_CALL" | "BUILD_MAP_UNPACK" ->
        (* No real difference between the two but in case of an error, which shouldn't happen since
           the code is known to compile *)
        build_collection st arg ~f:(fun values -> Exp.Collection {kind= Map; values; unpack= true})
    | "YIELD_VALUE" ->
        let lhs, st = State.fresh_id st in
        let* rhs, st = State.pop_and_cast st in
        let st = State.push_stmt st (Yield {lhs; rhs}) in
        let st = State.push st (Temp lhs) in
        Ok (st, None)
    | "YIELD_FROM" ->
        (* TODO: it is quite uncertain how we'll deal with these in textual.
           At the moment this seems to correctly model the stack life-cycle, so
           I'm happy. We might change/improve things in the future *)
        let* exp, st = State.pop_and_cast st in
        (* TODO: learn more about this construct. My understanding is that the
                 receiver stays on the stack if there's a value to yield.
                 we probably should do some encoding like GET_ITER/FOR_ITER.

                 For now, we leave it as is until it becomes a problem :D *)
        let* receiver = State.peek st in
        let* receiver = State.cast_exp st receiver in
        (* TODO: it seems that sometimes the TOS is changed from receiver to exp
           at this point. Check C code and try to understand it better *)
        let* _, st = call_builtin_function st YieldFrom [receiver; exp] in
        Ok (st, None)
    | "GET_YIELD_FROM_ITER" ->
        (* TODO: it is quite uncertain how we'll deal with these in textual.
           At the moment this seems to correctly model the stack life-cycle, so
           I'm happy. We might change/improve things in the future *)
        let* tos, st = State.pop_and_cast st in
        let* id, st = call_builtin_function st GetYieldFromIter [tos] in
        let st = State.push st (Exp.Temp id) in
        Ok (st, None)
    | "LIST_APPEND" ->
        collection_add st opname arg ListAppend
    | "LIST_EXTEND" ->
        collection_add st opname arg ListExtend
    | "LIST_TO_TUPLE" ->
        let* tos, st = State.pop_and_cast st in
        let* id, st = call_builtin_function st ListToTuple [tos] in
        let st = State.push st (Exp.Temp id) in
        Ok (st, None)
    | "SET_ADD" ->
        collection_add st opname arg SetAdd
    | "SET_UPDATE" ->
        collection_add st opname arg SetUpdate
    | "MAP_ADD" ->
        collection_add st opname arg ~map:true DictSetItem
    | "DICT_UPDATE" ->
        collection_add st opname arg DictUpdate
    | "DICT_MERGE" ->
        collection_add st opname arg DictMerge
    | "DELETE_NAME" ->
        let ident = Ident.mk co_names.(arg) in
        let stmt = Stmt.Delete {scope= Name; ident} in
        let st = State.push_stmt st stmt in
        Ok (st, None)
    | "DELETE_GLOBAL" ->
        let ident = Ident.mk co_names.(arg) in
        let stmt = Stmt.Delete {scope= Global; ident} in
        let st = State.push_stmt st stmt in
        Ok (st, None)
    | "DELETE_FAST" ->
        let ident = Ident.mk co_varnames.(arg) in
        let stmt = Stmt.Delete {scope= Fast; ident} in
        let st = State.push_stmt st stmt in
        Ok (st, None)
    | "DELETE_ATTR" ->
        let attr = co_names.(arg) |> Ident.mk in
        let* exp, st = State.pop_and_cast st in
        let stmt = Stmt.DeleteAttr {exp; attr} in
        let st = State.push_stmt st stmt in
        Ok (st, None)
    | "DELETE_DEREF" ->
        let* name = get_cell_name code st arg argval in
        let stmt = Stmt.DeleteDeref {name; slot= arg} in
        let st = State.push_stmt st stmt in
        Ok (st, None)
    | "DELETE_SUBSCR" ->
        let* index, st = State.pop_and_cast st in
        let* exp, st = State.pop_and_cast st in
        let* _id, st = call_builtin_function st DeleteSubscr [exp; index] in
        Ok (st, None)
    | "GET_AWAITABLE" ->
        let* tos, st = State.pop_and_cast st in
        let* id, st = call_builtin_function st GetAwaitable [tos] in
        let st = State.push st (Exp.Temp id) in
        Ok (st, None)
    | "GET_ANEXT" ->
        let* tos = State.peek st in
        let* tos = State.cast_exp st tos in
        let id, st = call_method st Ident.Special.anext tos [] in
        let* id, st = call_builtin_function st GetAwaitable [Exp.Temp id] in
        let st = State.push st (Exp.Temp id) in
        Ok (st, None)
    | "BEFORE_ASYNC_WITH" ->
        (* See https://github.com/python/cpython/blob/3.8/Python/ceval.c#L3237 *)
        let* tos, st = State.pop_and_cast st in
        let st = State.push_symbol st (ContextManagerExit tos) in
        let id, st = call_method st Ident.Special.enter tos [] in
        let st = State.push st (Exp.Temp id) in
        Ok (st, None)
    | "SETUP_ASYNC_WITH" ->
        (* See https://github.com/python/cpython/blob/3.8/Python/ceval.c#L3261 *)
        (* This is nope operation until we translate exceptino throwing *)
        Ok (st, None)
    | "END_ASYNC_FOR" ->
        (* This instructions designates the end of an async for loop. Such a loop
           always ends with an exception.
           https://quentin.pradet.me/blog/using-asynchronous-for-loops-in-python.html
           https://superfastpython.com/asyncio-async-for/
           We model it like a throwing exception for now. This offset will not be reached
           by our DFS anyway since we don't model exceptional edges yet.
        *)
        Ok (st, Some (TerminatorBuilder.Throw Exp.none))
    | "RERAISE" | "WITH_EXCEPT_START" ->
        Ok (st, None)
    | "UNPACK_EX" ->
        (* The low byte of counts is the number of values before the list value, the high byte of
           counts the number of values after it. *)
        let to_int i = Exp.of_int i in
        let nr_before = arg land 0xff in
        let nr_after = (arg lsr 8) land 0xff in
        let* tos, st = State.pop_and_cast st in
        (* [UnpackEx m n exp] should unpack the [exp] collection into
           - [m] single values which are the first items in [exp]
           - [n] single values which are the latest items in [exp]
           - the rest stays into an iterable collection.
           We'll consider it returns a tuple of the right size that we can index
           to populate the stack *)
        let* res_id, st =
          call_builtin_function st UnpackEx [to_int nr_before; to_int nr_after; tos]
        in
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
    | "MATCH_CLASS" ->
        let* names, st = State.pop_and_cast st in
        let* type_, st = State.pop_and_cast st in
        let* subject, st = State.pop_and_cast st in
        let lhs, st = State.fresh_id st in
        let stmt = Stmt.Let {lhs; rhs= MatchClass {subject; type_; count= arg; names}} in
        let st = State.push_stmt st stmt in
        let st = State.push st (AttributesOfMatchClass (Temp lhs)) in
        let st =
          match version with
          | Python_3_10 ->
              State.push st (BoolOfMatchClass (Temp lhs))
          | Python_3_12 ->
              st
        in
        Ok (st, None)
    | "MATCH_SEQUENCE" ->
        let* tos = State.peek st in
        let* exp = State.cast_exp st tos in
        let st = State.push st (MatchSequence exp) in
        Ok (st, None)
    | "GET_LEN" ->
        let* tos = State.peek st in
        let* exp = State.cast_exp st tos in
        let st = State.push st (GetLen exp) in
        Ok (st, None)
    | "END_SEND" ->
        only_supported_from_python_3_12 opname version ;
        let* tos1, st = State.pop st in
        let* _, st = State.pop st in
        let st = State.push_symbol st tos1 in
        Ok (st, None)
    | "SEND" ->
        (* this instruction has 2 successors but the next instr (1st successor) is the entry of a loop
           we chose to not translate sincie it is just a a waiting loop "while not computation is not done" *)
        only_supported_from_python_3_12 opname version ;
        let {State.loc} = st in
        let* next_offset = Offset.get ~loc next_offset_opt in
        let* label = State.get_node_name st (next_offset + (2 * arg)) in
        let {State.stack} = st in
        let jump = TerminatorBuilder.mk_jump label stack in
        Ok (st, Some jump)
    | _ ->
        internal_error st (Error.UnsupportedOpcode opname)
  with Invalid_argument _ -> internal_error st (Error.IndexOutOfBound opname)


let get_successors_offset (version : FFI.version) {FFI.Instruction.opname; arg} =
  match opname with
  | "LOAD_ASSERTION_ERROR"
  | "LOAD_CONST"
  | "LOAD_NAME"
  | "LOAD_GLOBAL"
  | "LOAD_FAST"
  | "LOAD_ATTR"
  | "LOAD_CLASSDEREF"
  | "LOAD_DEREF"
  | "LOAD_FAST_CHECK"
  | "LOAD_FAST_AND_CLEAR"
  | "STORE_NAME"
  | "STORE_GLOBAL"
  | "STORE_FAST"
  | "STORE_ATTR"
  | "STORE_SUBSCR"
  | "STORE_DEREF"
  | "STORE_SLICE"
  | "CHECK_EXC_MATCH"
  | "CLEANUP_THROW"
  | "PUSH_EXC_INFO"
  | "RETURN_GENERATOR"
  | "KW_NAMES"
  | "CALL"
  | "CALL_FUNCTION"
  | "CALL_FUNCTION_KW"
  | "CALL_FUNCTION_EX"
  | "CALL_INTRINSIC_1"
  | "CALL_INTRINSIC_2"
  | "POP_TOP"
  | "COPY"
  | "SWAP"
  | "PUSH_NULL"
  | "BINARY_OP"
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
  | "BINARY_SLICE"
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
  | "GEN_START"
  | "SETUP_ANNOTATIONS"
  | "IMPORT_NAME"
  | "EAGER_IMPORT_NAME"
  | "IMPORT_FROM"
  | "IMPORT_STAR"
  | "COMPARE_OP"
  | "LOAD_LOCALS"
  | "LOAD_FROM_DICT_OR_DEREF"
  | "LOAD_SUPER_ATTR"
  | "LOAD_CLOSURE"
  | "RESUME"
  | "NOP"
  | "IS_OP"
  | "CONTAINS_OP"
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
  | "ROT_N"
  | "SETUP_WITH"
  | "BEFORE_WITH"
  | "SETUP_FINALLY"
  | "POP_FINALLY"
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
  | "LIST_EXTEND"
  | "LIST_TO_TUPLE"
  | "SET_ADD"
  | "SET_UPDATE"
  | "MAP_ADD"
  | "DICT_UPDATE"
  | "DICT_MERGE"
  | "DELETE_NAME"
  | "DELETE_GLOBAL"
  | "DELETE_FAST"
  | "DELETE_ATTR"
  | "DELETE_DEREF"
  | "DELETE_SUBSCR"
  | "END_FOR"
  | "GET_AWAITABLE"
  | "BEFORE_ASYNC_WITH"
  | "SETUP_ASYNC_WITH"
  | "RERAISE"
  | "WITH_EXCEPT_START"
  | "UNPACK_EX"
  | "MATCH_CLASS"
  | "MATCH_SEQUENCE"
  | "GET_LEN"
  | "MAKE_CELL"
  | "COPY_FREE_VARS"
  | "END_SEND" ->
      `NextInstrOnly
  | "RETURN_CONST" | "RETURN_VALUE" ->
      `Return
  | "POP_JUMP_IF_TRUE"
  | "POP_JUMP_IF_FALSE"
  | "JUMP_IF_NOT_EXC_MATCH"
  | "POP_JUMP_IF_NONE"
  | "POP_JUMP_IF_NOT_NONE" -> (
    match version with
    | Python_3_10 ->
        `NextInstrOrAbsolute (2 * arg)
    | Python_3_12 ->
        `NextInstrOrRelative (2 * arg) )
  | "JUMP_IF_TRUE_OR_POP" | "JUMP_IF_FALSE_OR_POP" ->
      `NextInstrWithPopOrAbsolute (2 * arg)
  | "FOR_ITER" -> (
    match version with
    | Python_3_10 ->
        `NextInstrOrRelativeWith2Pop (2 * arg)
    | Python_3_12 ->
        `NextInstrOrRelative (2 * arg) )
  | "JUMP_FORWARD" | "SEND" ->
      `Relative (2 * arg)
  | "JUMP_BACKWARD" | "JUMP_BACKWARD_NO_INTERRUPT" ->
      `Relative (-2 * arg)
  | "CALL_FINALLY" ->
      `CallFinallyRelative arg
  | "BEGIN_FINALLY" ->
      `BeginFinally
  | "END_FINALLY" ->
      `EndFinally
  | "JUMP_ABSOLUTE" ->
      `Absolute (2 * arg)
  | "END_ASYNC_FOR" | "RAISE_VARARGS" ->
      `Throw
  | _ ->
      `UnsupportedOpcode


let lookup_remaining = function
  | [] ->
      (None, false)
  | {FFI.Instruction.offset; is_jump_target; starts_line} :: _ ->
      (Some offset, is_jump_target || Option.is_some starts_line)


module Subroutine = struct
  (* We keep track of the subroutine call stack during CFG dfs.
     Then, we hit a END_FINALLY, the current frame is either [None]
     or [Some offset] and we can make a decision.
     This approach is rather fragile because a the order of return
     addresses in the real operand stack could **in theory** be messed
     up by some stack operation like ROT_TWO, ROT_THREE and so one.
     We do not try to detect such a strange situation for now.
  *)

  type call_stack = int option list (* stack of return offsets *)

  type edge =
    | Call of {return: int}  (** CALL_FINALLY *)
    | Tailcall  (** BEGIN_FINALLY *)
    | Return of {from: int}  (** END_FINALLY *)
    | Nop  (** No action on subroutines *)

  let exec dest stack edge =
    match edge with
    | Call {return} ->
        Ok (None, Some return :: stack)
    | Tailcall ->
        Ok (None, None :: stack)
    | Return {from} -> (
      match stack with
      | [] ->
          Error (L.InternalError, None, Error.SubroutineEmptyStack dest)
      | None :: stack ->
          Ok (None, stack)
      | Some return_offset :: stack ->
          Ok (Some (from, return_offset), stack) )
    | Nop ->
        Ok (None, stack)
end

type cfg_info =
  { successors: (Offset.t * int * Subroutine.edge) list
  ; predecessors: Offset.t list
  ; instructions: FFI.Instruction.t list }

let dummy_cfg_info = {successors= []; predecessors= []; instructions= []}

let build_cfg_skeleton_without_predecessors {FFI.Code.instructions; version} :
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
    match get_successors_offset version instr with
    | `NextInstrOnly when not next_is_jump_target ->
        Ok (map, action)
    | `NextInstrOnly ->
        let* next_offset = get_next_offset () in
        register_current_node [(next_offset, 0, Nop)]
    | `Throw | `Return ->
        register_current_node []
    | `NextInstrWithPopOrAbsolute other_offset ->
        let* next_offset = get_next_offset () in
        register_current_node [(next_offset, -1, Nop); (other_offset, 0, Nop)]
    | `NextInstrOrAbsolute other_offset ->
        let* next_offset = get_next_offset () in
        register_current_node [(next_offset, 0, Nop); (other_offset, 0, Nop)]
    | `NextInstrOrRelative delta ->
        let* next_offset = get_next_offset () in
        register_current_node [(next_offset, 0, Nop); (next_offset + delta, 0, Nop)]
    | `NextInstrOrRelativeWith2Pop delta ->
        let* next_offset = get_next_offset () in
        register_current_node [(next_offset, 0, Nop); (next_offset + delta, -2, Nop)]
    | `Relative delta ->
        let* next_offset = if delta > 0 then get_next_offset () else Ok (offset + 2) in
        register_current_node [(next_offset + delta, 0, Nop)]
    | `BeginFinally ->
        let* next_offset = get_next_offset () in
        register_current_node [(next_offset, 0, Tailcall)]
    | `CallFinallyRelative delta ->
        let* next_offset = get_next_offset () in
        let absolute_target = next_offset + delta in
        register_current_node [(absolute_target, 0, Call {return= next_offset})]
    | `EndFinally ->
        let* next_offset = get_next_offset () in
        (* we may modify these successors later *)
        register_current_node [(next_offset, 0, Return {from= current_node_offset})]
    | `Absolute offset ->
        register_current_node [(offset, 0, Nop)]
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

let set_predecessor ~pred ~succ map =
  let ({predecessors} as info) = IMap.find_opt succ map |> Option.value ~default:dummy_cfg_info in
  IMap.add succ {info with predecessors= pred :: predecessors} map


let optionally_set_predecessor ~from ~offset map =
  Option.value_map from ~default:map ~f:(fun from -> set_predecessor ~pred:from ~succ:offset map)


let change_successors_because_of_subroutine_return ~from ~return map =
  IMap.find_opt from map
  |> Option.value_map ~default:map ~f:(fun {instructions} ->
         IMap.add from {successors= [(return, 0, Nop)]; instructions; predecessors= []} map )


let build_topological_order cfg_skeleton_without_predecessors =
  (* weakly topological order *)
  let open IResult.Let_syntax in
  let rec visit ?from (call_stack : Subroutine.call_stack) (seen, map, post_visited)
      (offset, _, subroutine_edge) =
    if ISet.mem offset seen then
      let map = optionally_set_predecessor ~from ~offset map in
      Ok (seen, map, post_visited)
    else
      let* opt_succ, call_stack = Subroutine.exec offset call_stack subroutine_edge in
      match opt_succ with
      | None ->
          (* regular case *)
          let {successors} =
            IMap.find_opt offset cfg_skeleton_without_predecessors
            |> Option.value ~default:dummy_cfg_info
          in
          let seen = ISet.add offset seen in
          let* seen, map, post_visited =
            List.fold_result successors ~f:(visit ~from:offset call_stack)
              ~init:(seen, map, post_visited)
          in
          let map = optionally_set_predecessor ~from ~offset map in
          let post_visited = offset :: post_visited in
          Ok (seen, map, post_visited)
      | Some (from, return) ->
          (* this a subroutine indirect return edge: we need to change route *)
          let map = change_successors_because_of_subroutine_return ~from ~return map in
          visit ~from call_stack (seen, map, post_visited) (return, 0, Subroutine.Nop)
  in
  let+ _, cfg, post_visited =
    visit [] (ISet.empty, cfg_skeleton_without_predecessors, []) (0, 0, Nop)
  in
  (post_visited, cfg)


let constant_folding_ssa_params st succ_name {predecessors} =
  let open IResult.Let_syntax in
  let* st, status, predecessors, stacks =
    List.fold_result predecessors ~init:(st, `AllAvailable, [], [])
      ~f:(fun ((st, status, predecessors, stacks) as acc) predecessor ->
        match status with
        | `AllAvailable -> (
            let* last_of_predecessor = State.get_terminal_node st predecessor succ_name in
            match last_of_predecessor with
            | `NotYetThere _predecessor_name ->
                (* Because we use a topological order, if we reach a node with a predecessor
                   that has not been reached yet, it implies we are considering a back edge
                   in the DFS *)
                let* st = State.record_stack_at_loop_header st succ_name in
                Ok (st, `AtLeastOneNotAvailable, predecessors, stacks)
            | `AlreadyThere (predecessor_name, args) ->
                Ok (st, `AllAvailable, predecessor_name :: predecessors, args :: stacks) )
        | _ ->
            Ok acc )
  in
  let st, bottom_stack =
    match status with
    | `AtLeastOneNotAvailable ->
        let {State.stack} = st in
        let arity = List.length stack in
        let st =
          List.fold predecessors ~init:st ~f:(fun st pred_name ->
              State.drop_first_args_terminal_node st ~pred_name ~succ_name arity )
        in
        (st, stack)
    | `AllAvailable when List.length predecessors <= 0 ->
        (st, [])
    | `AllAvailable ->
        let stack0 =
          match stacks with
          | [] ->
              L.die InternalError "there should be at least one predecessor"
          | stack :: _ ->
              stack
        in
        let k = IList.k_first_columns_same_cell ~equal:Exp.equal_opstack_symbol stacks in
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


let process_node st code ~offset ~arity previous_last_loc ({instructions; successors} as info) =
  let open IResult.Let_syntax in
  let* name = State.get_node_name st offset in
  let* st, bottom_stack = constant_folding_ssa_params st name info in
  let st = State.enter_node st ~offset ~arity previous_last_loc bottom_stack in
  let ssa_parameters = State.get_ssa_parameters st in
  let {State.stack} = st in
  State.debug st "              %a@\n" (Stack.pp ~pp:Exp.pp_opstack_symbol) stack ;
  let show_completion () =
    State.debug st "Successors: %a@\n" (Pp.comma_seq F.pp_print_int) (List.map successors ~f:fst3) ;
    State.debug st "@\n"
  in
  let rec loop st = function
    | [] ->
        let {State.loc} = st in
        Error (L.InternalError, loc, Error.NextOffsetMissing)
    | instr :: remaining -> (
        let next_offset_opt, next_is_jump_target = lookup_remaining remaining in
        let* st, opt_terminator = parse_bytecode st code instr next_offset_opt in
        let {State.stack} = st in
        State.debug st "              %a@\n" (Stack.pp ~pp:Exp.pp_opstack_symbol) stack ;
        match opt_terminator with
        | Some last ->
            let {FFI.Instruction.offset} = instr in
            let* new_last = State.check_terminator_if_back_edge st offset last in
            let last = Option.value new_last ~default:last in
            let {State.loc= last_loc; first_loc} = st in
            let stmts = State.get_stmts st in
            let st = State.add_new_node st name ~first_loc ~last_loc ssa_parameters stmts last in
            show_completion () ;
            Ok st
        | None when next_is_jump_target ->
            let {State.loc= last_loc; first_loc; stack} = st in
            let stmts = State.get_stmts st in
            let* next_offset = Offset.get ~loc:last_loc next_offset_opt in
            let* next_name = State.get_node_name st next_offset in
            let last = TerminatorBuilder.mk_jump next_name stack in
            let st = State.add_new_node st name ~first_loc ~last_loc ssa_parameters stmts last in
            show_completion () ;
            Ok st
        | None ->
            loop st remaining )
  in
  loop st instructions


let build_cfg ~debug ~code_qual_name code =
  let open IResult.Let_syntax in
  let loc = Location.of_code code in
  let* cfg_skeleton_without_predecessors = build_cfg_skeleton_without_predecessors code in
  let* topological_order, cfg_skeleton =
    build_topological_order cfg_skeleton_without_predecessors
  in
  let get_info offset =
    match IMap.find_opt offset cfg_skeleton with
    | None ->
        Error (L.InternalError, loc, Error.MissingNodeInformation offset)
    | Some l ->
        Ok l
  in
  let visit (st, arity_and_loc_map) offset =
    let* ({successors} as info) = get_info offset in
    let* arity, previous_last_loc =
      match IMap.find_opt offset arity_and_loc_map with
      | Some (arity, loc) ->
          Ok (arity, loc)
      | None ->
          internal_error st (Error.MissingNodeInformation offset)
    in
    let* st = process_node st code ~offset ~arity previous_last_loc info in
    let arity = State.size st in
    let loc = st.State.loc in
    (* the last location we have seen *)
    let* arity_and_loc_map =
      List.fold_result successors ~init:arity_and_loc_map
        ~f:(fun arity_and_loc_map (succ, delta, _) ->
          let succ_arity = arity + delta in
          if IMap.mem succ arity_and_loc_map then
            let current_succ_arity, _ = IMap.find succ arity_and_loc_map in
            if not (Int.equal current_succ_arity succ_arity) then
              internal_error st (Error.IllFormedOpstack succ)
            else Ok arity_and_loc_map
          else Ok (IMap.add succ (succ_arity, loc) arity_and_loc_map) )
    in
    Ok (st, arity_and_loc_map)
  in
  let qual_name = code_qual_name code in
  let {FFI.Code.version} = code in
  let st = State.enter ~debug ~code_qual_name ~loc ~cfg_skeleton version qual_name in
  let* st, _ =
    List.fold_result topological_order ~init:(st, IMap.add 0 (0, None) IMap.empty) ~f:visit
  in
  let* cfg = CFG.of_builder st.State.cfg code in
  Ok cfg


module Module = struct
  type t = {name: Ident.t; toplevel: CFG.t; functions: CFG.t QualName.Map.t}

  let pp fmt {name; toplevel; functions} =
    F.fprintf fmt "@[<hv2>module %a:@\n@\n" Ident.pp name ;
    F.fprintf fmt "@[<hv2>%a@]@\n" (CFG.pp ~name:"toplevel") toplevel ;
    QualName.Map.iter
      (fun qual_name cfg ->
        let name = F.asprintf "%a" QualName.pp qual_name in
        F.fprintf fmt "@[<hv2>%a@]@\n" (CFG.pp ~name) cfg )
      functions ;
    F.fprintf fmt "@]@\n"
end

module CodeMap : Stdlib.Map.S with type key = FFI.Code.t = Stdlib.Map.Make (FFI.Code)

let build_code_object_unique_name module_name code =
  let rec visit map outer_name ({FFI.Code.co_consts} as code) =
    if CodeMap.mem code map then map
    else
      let map = CodeMap.add code outer_name map in
      Array.fold co_consts ~init:(map, IString.Map.empty) ~f:(fun (map, counter_map) constant ->
          match constant with
          | FFI.Constant.PYCCode code ->
              let name = code.FFI.Code.co_name in
              let counter = IString.Map.find_opt name counter_map |> Option.value ~default:0 in
              let outer_name = QualName.extend outer_name name counter in
              (visit map outer_name code, IString.Map.add name (counter + 1) counter_map)
          | _ ->
              (map, counter_map) )
      |> fst
  in
  let map = visit CodeMap.empty (QualName.init ~module_name) code in
  fun code -> CodeMap.find_opt code map


let fold_all_inner_codes_and_build_map ~code_qual_name ~(f : FFI.Code.t -> 'a pyresult) code :
    'a QualName.Map.t pyresult =
  let open IResult.Let_syntax in
  let read_code_qual_name code =
    let loc = Location.of_code code in
    match code_qual_name code with
    | Some qual_name ->
        Ok qual_name
    | None ->
        Error (L.InternalError, loc, Error.CodeWithoutQualifiedName code)
  in
  let fold_codes map co_consts =
    let rec visit map = function
      | FFI.Constant.PYCCode ({FFI.Code.co_consts} as code) ->
          let* qual_name = read_code_qual_name code in
          let* elt = f code in
          let map = QualName.Map.add qual_name elt map in
          Array.fold_result co_consts ~init:map ~f:visit
      | _ ->
          Ok map
    in
    Array.fold_result co_consts ~init:map ~f:visit
  in
  fold_codes QualName.Map.empty code.FFI.Code.co_consts


let test_cfg_skeleton ~show ~code_qual_name code =
  let test =
    let open IResult.Let_syntax in
    let* map = build_cfg_skeleton_without_predecessors code in
    build_topological_order map
  in
  match test with
  | Error (_, _, err) ->
      L.internal_error "IR error: %a@\n" Error.pp_kind err
  | Ok (topological_order, map) when show ->
      let qual_name = code_qual_name code |> Option.value_exn in
      F.printf "%a@\n" QualName.pp qual_name ;
      List.iter code.FFI.Code.instructions ~f:(F.printf "%a@\n" FFI.Instruction.pp) ;
      let pp_succ_and_delta fmt (succ, delta, _) =
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
      F.printf "topological order: %a@\n@\n" (Pp.seq ~sep:" " F.pp_print_int) topological_order
  | Ok _ ->
      ()


let module_name ~path_prefixes {FFI.Code.co_filename} =
  let remove_prefix str prefix =
    if String.is_prefix str ~prefix then
      let prefix_len = String.length prefix in
      String.sub str ~pos:prefix_len ~len:(String.length str - prefix_len)
    else str
  in
  let file_path = List.fold path_prefixes ~init:co_filename ~f:remove_prefix in
  (* if any prefixes have been removed, then also remove any leading / *)
  let file_path =
    if String.equal file_path co_filename then file_path else remove_prefix file_path "/"
  in
  Stdlib.Filename.remove_extension file_path |> String.substr_replace_all ~pattern:"/" ~with_:"::"


let mk ~debug ~path_prefix code =
  let open IResult.Let_syntax in
  let module_name =
    module_name ~path_prefixes:(List.concat [["./"]; Option.to_list path_prefix]) code
  in
  let code_qual_name = build_code_object_unique_name module_name code in
  let name = Ident.mk module_name in
  let f = build_cfg ~debug ~code_qual_name in
  let* toplevel = f code in
  let* functions = fold_all_inner_codes_and_build_map code ~code_qual_name ~f in
  Ok {Module.name; toplevel; functions}


let initialize ?(next_version = false) () =
  if not (Py.is_initialized ()) then
    let interpreter = if next_version then Version.python_next_exe else Version.python_exe in
    Py.initialize ~interpreter ()


let test_generator ~filename ~f source =
  initialize ~next_version:false () ;
  (* pass argument ~next_version:true if you want to activate unit tests on Python 3.12 *)
  let code =
    match FFI.from_string ~source ~filename with
    | Error (kind, err) ->
        L.die kind "FFI error: %a@\n" FFI.Error.pp_kind err
    | Ok code ->
        code
  in
  Py.finalize () ;
  match f code with
  | Error (kind, _loc, err) -> (
    match kind with
    | L.InternalError ->
        L.internal_error "IR error: %a@\n" Error.pp_kind err
    | L.UserError ->
        L.user_error "IR error: %a@\n" Error.pp_kind err
    | L.ExternalError ->
        L.external_error "IR error: %a@\n" Error.pp_kind err )
  | Ok () ->
      ()
  | exception (Py.E _ as e) ->
      L.die ExternalError "Pyml exception: %s@\n" (Exn.to_string e)


let test ?(filename = "dummy.py") ?(debug = false) ?run ?(show = false) source =
  let open IResult.Let_syntax in
  let f code =
    let+ module_ = mk ~debug ~path_prefix:None code in
    let run =
      Option.value run ~default:(fun module_ -> if show then F.printf "%a" Module.pp module_)
    in
    run module_
  in
  test_generator ~filename ~f source


let test_files ?(debug = false) ?run list =
  let open IResult.Let_syntax in
  let units = ref [] in
  let f code =
    let+ module_ = mk ~debug ~path_prefix:None code in
    units := module_ :: !units
  in
  List.iter list ~f:(fun (filename, source) -> test_generator ~filename ~f source) ;
  let run = Option.value run ~default:(List.iter ~f:(F.printf "%a" Module.pp)) in
  run (List.rev !units)


let test_cfg_skeleton ?(filename = "dummy.py") ?(show = false) source =
  let open IResult.Let_syntax in
  let f code =
    let file_path = module_name code ~path_prefixes:[] in
    let code_qual_name = build_code_object_unique_name file_path code in
    test_cfg_skeleton ~show ~code_qual_name code ;
    let f code = Ok (test_cfg_skeleton ~code_qual_name code) in
    let* _ = fold_all_inner_codes_and_build_map code ~code_qual_name ~f in
    Ok ()
  in
  test_generator ~filename ~f source
