(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging
module MakeFunctionFlags = PyCommon.MakeFunctionFlags
module Ident = PyCommon.Ident
module SMap = PyCommon.SMap

module Cell = struct
  type method_info =
    { raw_qualified_name: string
    ; code: FFI.Constant.t
    ; type_info: t SMap.t
    ; rev_signature: PyCommon.signature
    ; defaults: t list
    ; is_static: bool
    ; is_abstract: bool
    ; flags: MakeFunctionFlags.t }

  and t =
    | Atom of string
    | Path of Ident.t
    | Const of FFI.Constant.t
    | List of (PyBuiltin.builder * t list)
    | Map of t SMap.t
    | Call of t * t list
    | Apply of string * t
    | Method of method_info
    | ClassClosure

  (* Trimmed down version where we discard maps, method_info and code *)
  let rec equal c0 c1 =
    match (c0, c1) with
    | Atom a0, Atom a1 ->
        String.equal a0 a1
    | Path p0, Path p1 ->
        Int.equal 0 (Ident.compare p0 p1)
    | List (_, l0), List (_, l1) ->
        List.equal equal l0 l1
    | Apply (n0, c0), Apply (n1, c1) ->
        String.equal n0 n1 && equal c0 c1
    | ClassClosure, ClassClosure ->
        true
    | _, _ ->
        false


  let rec pp fmt = function
    | Atom a ->
        F.pp_print_string fmt a
    | List (_, lst) ->
        F.fprintf fmt "[@[%a@]]" (Pp.collection ~fold:List.fold ~sep:", " pp) lst
    | Map _map ->
        F.pp_print_string fmt "<Map>"
    | Call (f, args) ->
        F.fprintf fmt "%a(%a)" pp f (Pp.seq ~sep:", " pp) args
    | Apply (name, ty) ->
        F.fprintf fmt "%s[%a]" name pp ty
    | Const const ->
        F.fprintf fmt "Const(%a)" FFI.Constant.pp const
    | Path id ->
        Ident.pp fmt id
    | Method {raw_qualified_name; flags} ->
        F.fprintf fmt "Method(%s, %a)" raw_qualified_name MakeFunctionFlags.pp flags
    | ClassClosure ->
        F.pp_print_string fmt "__class__(closure)"
end

module Error = struct
  type cell_as = Atom | Path | Code | Tuple | Map

  type kind =
    | InvalidDecorator of Cell.t
    | UnexpectedOpcode of string
    | UnexpectedClosureName of string
    | UnexpectedClosure of Cell.t
    | EmptyStack
    | As of cell_as * Cell.t
    | UnknownStorage of string
    | AnnotationMismatch
    | ToCell of Cell.t

  let pp_kind fmt = function
    | InvalidDecorator typ ->
        F.fprintf fmt "unsupported decorator %a" Cell.pp typ
    | UnexpectedOpcode opcode ->
        F.fprintf fmt "unexpected opcode %s during class declaration" opcode
    | EmptyStack ->
        F.fprintf fmt "Trying to pop an empty stack during class declaration"
    | As (kind, cell) ->
        let kind =
          match kind with
          | Atom ->
              "an atom/string"
          | Path ->
              "a qualified name"
          | Code ->
              "a code object"
          | Tuple ->
              "a tuple"
          | Map ->
              "a map"
        in
        F.fprintf fmt "Expected %s but got '%a'" kind Cell.pp cell
    | UnknownStorage name ->
        F.fprintf fmt "STORE_SUBSCR in '%s' during class declaration" name
    | AnnotationMismatch ->
        F.fprintf fmt "Length mismatch when trying to build method signature"
    | UnexpectedClosureName name ->
        F.fprintf fmt "Unexpected closure name : %s" name
    | UnexpectedClosure cell ->
        F.fprintf fmt "Unexpected closure : %a" Cell.pp cell
    | ToCell cell ->
        F.fprintf fmt "Failed to generate a valid default value for %a" Cell.pp cell
end

let rec to_datastack_cell cell =
  let open IResult.Let_syntax in
  match (cell : Cell.t) with
  | Atom name ->
      Ok (PyEnv.DataStack.Name {global= false; name})
  | Path id ->
      Ok (PyEnv.DataStack.Path id)
  | List (b, l) ->
      let l = List.map l ~f:(fun c -> to_datastack_cell c) in
      let* l = Result.all l in
      Ok (PyEnv.DataStack.List (b, l))
  | Const c ->
      Ok (PyEnv.DataStack.Const c)
  | ClassClosure | Apply _ | Method _ | Map _ | Call _ ->
      Error (Error.ToCell cell)


module Stack = struct
  let empty = []

  let push stack cell = cell :: stack

  let pop stack = match stack with [] -> Error Error.EmptyStack | hd :: tl -> Ok (hd, tl)

  let pop_n stack n =
    let open IResult.Let_syntax in
    let rec aux n acc stack =
      if n > 0 then
        let* hd, tl = pop stack in
        aux (n - 1) (hd :: acc) tl
      else Ok (acc, stack)
    in
    aux n [] stack
end

module State = struct
  (* TODO: [raw_qualified_name] is not used at the moment. We might want to use it for some sanity
     checks. *)
  type method_info =
    { name: string
    ; raw_qualified_name: string
    ; code: FFI.Constant.t
    ; signature: PyCommon.signature
    ; defaults: PyEnv.DataStack.cell list
    ; is_static: bool
    ; is_abstract: bool
    ; flags: MakeFunctionFlags.t }

  type t =
    { members: PyCommon.annotated_name list
    ; methods: method_info list
    ; static_methods: method_info list
    ; has_annotations: bool
    ; has_init: PyCommon.annotated_name list option
    ; has_new: PyCommon.annotated_name list option }

  let empty =
    { members= []
    ; methods= []
    ; static_methods= []
    ; has_annotations= false
    ; has_init= None
    ; has_new= None }


  let has_annotations {has_annotations} = has_annotations
end

let rec cell_from_constant const =
  match (const : FFI.Constant.t) with
  | PYCBytes s ->
      let s = Bytes.to_string s in
      Cell.Atom s
  | PYCString str ->
      Cell.Atom str
  | PYCNone ->
      Cell.Atom "None"
  | PYCTuple arr ->
      let cells =
        Array.fold ~init:[] arr ~f:(fun cells const ->
            let cell = cell_from_constant const in
            cell :: cells )
      in
      Cell.List (PyBuiltin.Tuple, List.rev cells)
  | _ ->
      Cell.Const const


let try_as_atom = function Cell.Atom name -> Some name | _ -> None

let as_atom cell =
  match try_as_atom cell with Some name -> Ok name | None -> Error (Error.As (Atom, cell))


let as_path = function
  | Cell.Path id ->
      Ok id
  | Cell.Atom name ->
      Ok (Ident.mk name)
  | cell ->
      Printf.eprintf "Call stack:\n%s\n"
        (Caml.Printexc.get_callstack 10 |> Caml.Printexc.raw_backtrace_to_string) ;
      Error (Error.As (Path, cell))


let as_const = function Cell.Const const -> Ok const | cell -> Error (Error.As (Code, cell))

let try_as_method = function Cell.Method method_info -> Some method_info | _ -> None

let as_tuple = function Cell.List l -> Ok l | cell -> Error (Error.As (Tuple, cell))

let as_map = function Cell.Map map -> Ok map | cell -> Error (Error.As (Map, cell))

let make_function state stack flags =
  let open IResult.Let_syntax in
  let* raw_qualified_name, stack = Stack.pop stack in
  let* cell, stack = Stack.pop stack in
  let* raw_qualified_name = as_atom raw_qualified_name in
  let* code = as_const cell in
  let flags = MakeFunctionFlags.mk flags in
  let* stack, flags =
    if MakeFunctionFlags.mem flags Closure then
      let* cell, stack = Stack.pop stack in
      let* _, tuple = as_tuple cell in
      if List.equal Cell.equal [Cell.ClassClosure] tuple then
        let flags = MakeFunctionFlags.unset flags Closure in
        Ok (stack, flags)
      else Error (Error.UnexpectedClosure cell)
    else Ok (stack, flags)
  in
  let* type_info, stack =
    if MakeFunctionFlags.mem flags Annotations then
      let* type_info, stack = Stack.pop stack in
      let* type_info = as_map type_info in
      Ok (type_info, stack)
    else Ok (SMap.empty, stack)
  in
  let* stack, defaults =
    if MakeFunctionFlags.mem flags DefaultValues then
      let* cell, stack = Stack.pop stack in
      let* _, defaults = as_tuple cell in
      Ok (stack, defaults)
    else Ok (stack, [])
  in
  let* rev_signature, stack =
    let mk_formals_types code =
      let arguments = FFI.Code.get_arguments code in
      Array.fold arguments ~init:(Ok []) ~f:(fun args argname ->
          let* args in
          let* typ =
            match SMap.find_opt argname type_info with
            | Some typ ->
                (* TODO: support generics *)
                let* id = as_path typ in
                Ok (Some id)
            | None ->
                Ok None
          in
          let annotation = Option.value typ ~default:(Ident.mk "object") in
          let arg = {PyCommon.name= argname; annotation} in
          Ok (arg :: args) )
    in
    let* rev_formals_types =
      (* document this *)
      let actual_code = Option.value_exn @@ FFI.Constant.as_code code in
      mk_formals_types actual_code
    in
    Ok (rev_formals_types, stack)
  in
  let cell =
    Cell.Method
      { code
      ; type_info
      ; raw_qualified_name
      ; flags
      ; rev_signature
      ; defaults
      ; is_static= false
      ; is_abstract= false }
  in
  let stack = Stack.push stack cell in
  Ok (state, stack)


let store_name state stack class_name name method_info =
  let open IResult.Let_syntax in
  let { Cell.raw_qualified_name
      ; code
      ; rev_signature
      ; type_info
      ; is_static
      ; is_abstract
      ; flags
      ; defaults } =
    method_info
  in
  (* When __init__ calls super(), some closures are added to the fix, so we
     don't care if __init__ is marked as a closure. However for other methods,
     we currently don't really support that. Investigate if the warning ever
     fires *)
  if (not (String.equal PyCommon.init__ name)) && MakeFunctionFlags.mem flags Closure then
    L.user_warning "Unexpected closure flag on method %s of class %s@\n" name class_name ;
  (* fixing self parameter type *)
  let rev_signature =
    if is_static then rev_signature
    else
      let actual_code = Option.value_exn @@ FFI.Constant.as_code code in
      let arguments = FFI.Code.get_arguments actual_code in
      if Array.length arguments > 0 then
        let argname = arguments.(0) in
        (* [self] is the first argument of instance methods. If its type annotation is
           missing, use the current class *)
        List.map rev_signature ~f:(fun {PyCommon.name; annotation} ->
            let annotation =
              if String.equal name argname then Ident.mk class_name else annotation
            in
            {PyCommon.name; annotation} )
      else rev_signature
  in
  (* fixing return type *)
  let* rev_signature =
    match SMap.find_opt PyCommon.return type_info with
    | None ->
        (* __new__ returns an instance of the current class, __init__ just modifies it *)
        let annotation = if String.equal PyCommon.init__ name then "None" else "object" in
        let annotation = Ident.mk annotation in
        Ok ({PyCommon.name= PyCommon.return; annotation} :: rev_signature)
    | Some annotation ->
        let* id = as_path annotation in
        Ok ({PyCommon.name= PyCommon.return; annotation= id} :: rev_signature)
  in
  let signature = List.rev rev_signature in
  let defaults = List.map ~f:to_datastack_cell defaults in
  let* defaults = Result.all defaults in
  let method_info =
    {State.name; raw_qualified_name; code; signature; defaults; is_static; is_abstract; flags}
  in
  let {State.methods; has_init; has_new} = state in
  let has_init = if String.equal name PyCommon.init__ then Some signature else has_init in
  let has_new = if String.equal name PyCommon.new__ then Some signature else has_new in
  let methods = method_info :: methods in
  let state = {state with State.methods; has_init; has_new} in
  Ok (state, stack)


(* [CALL_FUNCTION] can happen while building the arguments of a decorator, or
   for applying a decorator itself.
   For now, we only support [@staticmethod] and [@abstractmethod], but still
   report this other so parsing can keep going *)
let call_function state stack argc =
  let open IResult.Let_syntax in
  let* args, stack = Stack.pop_n stack argc in
  let* callable, stack = Stack.pop stack in
  let default () =
    let call = Cell.Call (callable, args) in
    let stack = Stack.push stack call in
    Ok (state, stack)
  in
  (* TODO: support more complex decorators, with qualified path or generics *)
  match args with
  | [arg] -> (
    match try_as_method arg with
    | Some info ->
        (* We keep code as a Constant.t because PyTrans works with Arrays of
           Constant.t, not Code.t.
           We might revisit this decision if we allow building Constant.t outside of
           FFI *)
        let* info =
          match try_as_atom callable with
          | Some decorator_name ->
              if String.equal PyCommon.static_method decorator_name then
                Ok {info with Cell.is_static= true}
              else if String.equal PyCommon.ABC.abstract_method decorator_name then
                Ok {info with Cell.is_abstract= true}
              else Error (Error.InvalidDecorator callable)
          | None ->
              (* TODO: keep the decorators around and maybe add them as attributes *)
              L.user_warning "/!\\ Unsupported decorator '%a'@\n" Cell.pp callable ;
              Ok info
        in
        let cell = Cell.Method info in
        let stack = Stack.push stack cell in
        Ok (state, stack)
    | None ->
        default () )
  | _ ->
      default ()


(* In the same fashion as [PyTrans.run_instruction], here is a dedicated state
   machine to deal with class declaration.
   The goal is probably one day to merge into [PyTrans] but they don't work on
   the same state or generate the same types, so I'd rather have them
   separated for now. It makes updating them easier while we're in fast
   iteration dev mode *)
let run class_name state {FFI.Code.co_names; co_consts; co_cellvars; co_freevars} stack
    {FFI.Instruction.opname; arg} =
  let open IResult.Let_syntax in
  PyDebug.p "[%s] arg= %d\n" opname arg ;
  match opname with
  | "SETUP_ANNOTATIONS" ->
      let state = {state with State.has_annotations= true} in
      Ok (state, stack)
  | "LOAD_NAME" ->
      let cell = Cell.Atom co_names.(arg) in
      let stack = Stack.push stack cell in
      Ok (state, stack)
  | "LOAD_METHOD" | "LOAD_ATTR" ->
      let* prefix, stack = Stack.pop stack in
      let attr = co_names.(arg) in
      let* prefix = as_path prefix in
      let id = Ident.extend ~prefix attr in
      let cell = Cell.Path id in
      let stack = Stack.push stack cell in
      Ok (state, stack)
  | "LOAD_CONST" ->
      let const = co_consts.(arg) in
      let cell = cell_from_constant const in
      let stack = Stack.push stack cell in
      Ok (state, stack)
  | "BINARY_SUBSCR" ->
      let* inst, stack = Stack.pop stack in
      let* ty, stack = Stack.pop stack in
      let* name = as_atom ty in
      let cell = Cell.Apply (name, inst) in
      let stack = Stack.push stack cell in
      L.user_error "No support for generic types at the moment@\n" ;
      Ok (state, stack)
  | "BUILD_TUPLE" ->
      let* cells, stack = Stack.pop_n stack arg in
      let cell = Cell.List (Tuple, cells) in
      let stack = Stack.push stack cell in
      Ok (state, stack)
  | "BUILD_LIST" ->
      let* cells, stack = Stack.pop_n stack arg in
      let cell = Cell.List (List, cells) in
      let stack = Stack.push stack cell in
      Ok (state, stack)
  | "STORE_SUBSCR" ->
      let* ndx, stack = Stack.pop stack in
      let* dict, stack = Stack.pop stack in
      let* value, stack = Stack.pop stack in
      let* name = as_atom ndx in
      let* atom = as_atom dict in
      let* state =
        if State.has_annotations state then
          let* () =
            if String.equal PyCommon.annotations atom then Ok ()
            else Error (Error.UnknownStorage atom)
          in
          let* annotation = as_path value in
          let {State.members} = state in
          let members = {PyCommon.name; annotation} :: members in
          let state = {state with State.members} in
          Ok state
        else Error (Error.UnknownStorage atom)
      in
      Ok (state, stack)
  | "BUILD_CONST_KEY_MAP" ->
      let* cell, stack = Stack.pop stack in
      let* _, names = as_tuple cell in
      let* cells, stack = Stack.pop_n stack arg in
      let map =
        List.fold2 names cells ~init:(Ok SMap.empty) ~f:(fun map name cell ->
            let* map in
            let* name = as_atom name in
            let map = SMap.add name cell map in
            Ok map )
      in
      let* map =
        match map with
        | List.Or_unequal_lengths.Ok map ->
            map
        | List.Or_unequal_lengths.Unequal_lengths ->
            Error Error.AnnotationMismatch
      in
      let cell = Cell.Map map in
      let stack = Stack.push stack cell in
      Ok (state, stack)
  | "MAKE_FUNCTION" ->
      make_function state stack arg
  | "CALL_METHOD" | "CALL_FUNCTION" ->
      call_function state stack arg
  | "STORE_NAME" -> (
      let name = co_names.(arg) in
      (* If __init__ closure is involved, it will finish with
                    70 LOAD_CLOSURE             0 (__class__)
                    72 DUP_TOP
                    74 STORE_NAME               9 (__classcell__)
                    76 RETURN_VALUE
         So we'll just ignore these stores for now
      *)
      if String.equal PyCommon.classcell name then Ok (state, stack)
      else
        let* method_info, stack = Stack.pop stack in
        match try_as_method method_info with
        | Some method_info ->
            store_name state stack class_name name method_info
        | None ->
            L.user_warning "Default value for class members are not yet supported@\n" ;
            Ok (state, stack) )
  | "RETURN_VALUE" ->
      (* Often there will be useless "return None" in here so we just ignore
         return statements *)
      Ok (state, stack)
  | "LOAD_CLOSURE" ->
      let sz = Array.length co_cellvars in
      let name = if arg < sz then co_cellvars.(arg) else co_freevars.(arg - sz) in
      let* stack =
        if String.equal name PyCommon.class__ then Ok (Stack.push stack Cell.ClassClosure)
        else Error (Error.UnexpectedClosureName name)
      in
      Ok (state, stack)
  | "DUP_TOP" ->
      let* top, stack = Stack.pop stack in
      let stack = Stack.push stack top in
      let stack = Stack.push stack top in
      Ok (state, stack)
  | _ ->
      Error (Error.UnexpectedOpcode opname)


let rec parse_declaration class_name state code stack instructions =
  let open IResult.Let_syntax in
  match instructions with
  | [] ->
      Ok (state, stack)
  | instr :: instructions ->
      let* state, stack = run class_name state code stack instr in
      parse_declaration class_name state code stack instructions


let parse_class_declaration code class_name instructions =
  let open IResult.Let_syntax in
  (* strip the first 4 instructions which are always
              0 LOAD_NAME                0 (__name__)
              2 STORE_NAME               1 (__module__)
              4 LOAD_CONST               0 ('class_name')
              6 STORE_NAME               2 (__qualname__)
     TODO: deal with that at some point
  *)
  let _, instructions = List.split_n instructions 4 in
  let* state, _stack = parse_declaration class_name State.empty code Stack.empty instructions in
  Ok state
