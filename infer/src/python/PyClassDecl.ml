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

module Type = struct
  type t = Atom of string | List of t list | Apply of string * t

  let rec pp fmt = function
    | Atom a ->
        F.pp_print_string fmt a
    | List lst ->
        F.fprintf fmt "[@[%a@]]" (Pp.collection ~fold:List.fold ~sep:", " ~pp_item:pp) lst
    | Apply (name, ty) ->
        F.fprintf fmt "%s%a" name pp ty


  let show ty : string = F.asprintf "%a" pp ty
end

module Stack = struct
  let push stack ty = ty :: stack

  let pop stack = match stack with [] -> None | hd :: tl -> Some (hd, tl)

  let pop_n stack n =
    let rec aux n acc stack =
      if n > 0 then match stack with [] -> None | hd :: tl -> aux (n - 1) (hd :: acc) tl
      else Some (acc, stack)
    in
    aux n [] stack
end

(** Simple types like [int] are loaded in a single [LOAD_NAME] instruction, but more complex ones
    like [Callable\[\[int\], int\]] will require a more elaborate sequence. This function will do a
    best effort to parse type annotations, and will stop when it encounters the load of the special
    dictionary [__annotations__] (used to record types of class members) or [LOAD_CONST] trying to
    load a tuple (used to record method signatures). *)
let rec parse_tys stack ({FFI.Code.co_names; co_consts} as code) instructions =
  let open Option.Let_syntax in
  match instructions with
  | [] ->
      Some (stack, [])
  | hd :: tl -> (
      let {FFI.Instruction.opname; arg} = hd in
      (* TODO: support qualified types like [foo.Bar] *)
      match opname with
      | "LOAD_NAME" ->
          let name = co_names.(arg) in
          if String.equal "__annotations__" name then Some (stack, tl)
          else
            let stack = Stack.push stack (Type.Atom name) in
            parse_tys stack code tl
      | "LOAD_CONST" -> (
          let const = co_consts.(arg) in
          match (const : FFI.Constant.t) with
          | PYCNone ->
              let stack = Stack.push stack (Type.Atom "None") in
              parse_tys stack code tl
          | PYCString name ->
              let stack = Stack.push stack (Type.Atom name) in
              parse_tys stack code tl
          | PYCTuple _ ->
              Some (stack, instructions)
          | _ ->
              None )
      | "BUILD_TUPLE" | "BUILD_LIST" ->
          Stack.pop_n stack arg
          |> Option.bind ~f:(fun (elts, stack) ->
                 let ty = Type.List elts in
                 let stack = Stack.push stack ty in
                 parse_tys stack code tl )
      | "BINARY_SUBSCR" -> (
          Stack.pop stack
          >>= fun (ty, stack) ->
          Stack.pop stack
          >>= fun (name, stack) ->
          match name with
          | Type.Atom name ->
              let ty = Type.Apply (name, ty) in
              let stack = Stack.push stack ty in
              parse_tys stack code tl
          | _ ->
              None )
      | _ ->
          None )


let parse_member_name {FFI.Code.co_consts} instructions =
  match instructions with
  | [] ->
      None
  | {FFI.Instruction.opname; arg} :: tl ->
      if String.equal "LOAD_CONST" opname then
        let const = co_consts.(arg) in
        FFI.Constant.as_name const |> Option.map ~f:(fun name -> (name, tl))
      else None


let is_instruction name instructions =
  match instructions with
  | [] ->
      None
  | {FFI.Instruction.opname; arg} :: instructions ->
      if String.equal name opname then Some (arg, instructions) else None


let tys_as_ty = function [ty] -> Some ty | _ -> None

let load_tuple {FFI.Code.co_consts} = function
  | [] ->
      None
  | {FFI.Instruction.opname; arg} :: instructions ->
      if String.equal opname "LOAD_CONST" then
        let const = co_consts.(arg) in
        match const with FFI.Constant.PYCTuple arr -> Some (arr, instructions) | _ -> None
      else None


let load_named_tuple code instructions =
  let open IOption.Let_syntax in
  let rec aux = function
    | [] ->
        Some []
    | FFI.Constant.PYCString name :: tl ->
        aux tl >>| fun tl -> name :: tl
    | _ :: _ ->
        None
  in
  let* arr, instructions = load_tuple code instructions in
  let+ names = Array.to_list arr |> aux in
  (names, instructions)


(** A member declaration consists of a sequence of instructions to create its type, ending with a
    [STORE_SUBSCR] into the special [__annotations__] dictionary. *)
let parse_member_annotation code instructions =
  let open IOption.Let_syntax in
  let* tys, instructions = parse_tys [] code instructions in
  let* ty = tys_as_ty tys in
  let* name, instructions = parse_member_name code instructions in
  let+ _, instructions = is_instruction "STORE_SUBSCR" instructions in
  let annotation = Ident.mk @@ Type.show ty in
  ({PyCommon.name; annotation}, instructions)


let parse_member_annotations code instructions =
  let rec aux acc instructions =
    match parse_member_annotation code instructions with
    | None ->
        (acc, instructions)
    | Some (name_ty, instructions) ->
        aux (name_ty :: acc) instructions
  in
  aux [] instructions


(** Method type information (signature, return type) are similar to member annotations: multiple
    types are constructed on the stack, until a tuple holding the name of the parameters is pushed
    on the stack too. At this point, a map is built, which gives us the method signature. *)
let parse_method_type_info code stack instructions =
  let open Option.Let_syntax in
  parse_tys stack code instructions
  >>= fun (stack, instructions) ->
  load_named_tuple code instructions
  >>= fun (names, instructions) ->
  is_instruction "BUILD_CONST_KEY_MAP" instructions
  >>= fun (arg, instructions) ->
  let tys, stack = List.split_n stack arg in
  let res =
    List.map2 names
      (List.rev_map ~f:(fun ty -> Ident.mk @@ Type.show ty) tys)
      ~f:(fun name annotation -> {PyCommon.name; annotation})
  in
  match res with
  | List.Or_unequal_lengths.Ok signature ->
      Some (signature, stack, instructions)
  | Unequal_lengths ->
      None


let load_name co_names stack instructions =
  let open Option.Let_syntax in
  is_instruction "LOAD_NAME" instructions
  >>= fun (arg, instructions) ->
  let name = co_names.(arg) in
  Some (Stack.push stack (Type.Atom name), instructions)


(* Loads as many names as we can, as [Type.Atom]. They will be used to parse produce the type
   signature, but also the decorators of a methods *)
let rec load_names co_names stack instructions =
  match load_name co_names stack instructions with
  | None ->
      (stack, instructions)
  | Some (stack, instructions) ->
      load_names co_names stack instructions


module Decorators = struct
  exception Failure

  type t = {is_static: bool; is_abstract: bool; unsupported: string list}

  let has_static decorators = {decorators with is_static= true}

  let has_abstract decorators = {decorators with is_abstract= true}

  let unsupported name ({unsupported} as decorators) =
    {decorators with unsupported= name :: unsupported}


  let warn class_name name {unsupported} =
    if not (List.is_empty unsupported) then (
      L.user_warning "Left-over type information for method %s in class %a@\n" name Ident.pp
        class_name ;
      List.iter ~f:(L.user_warning "- %s@\n") unsupported )


  (* Each decorator will be processed at runtime by a [CALL_FUNCTION 1] operation. Since we only
     support builtin decorators, we can skip them for now. *)
  let skip_calls raw_qualified_name instructions {is_static; is_abstract; unsupported} =
    let nr_decorators = List.length unsupported in
    let nr_decorators = if is_static then nr_decorators + 1 else nr_decorators in
    let nr_decorators = if is_abstract then nr_decorators + 1 else nr_decorators in
    let rec skip n instructions =
      let open IOption.Let_syntax in
      if n > 0 then
        let opt =
          is_instruction "CALL_FUNCTION" instructions
          >>= fun (arg, instructions) ->
          if arg <> 1 then (
            L.external_error
              "[parse_method] invalid CALL_FUNCTION for decorated method in class %s@\n"
              raw_qualified_name ;
            raise Failure )
          else Some instructions
        in
        let instructions = Option.value ~default:instructions opt in
        skip (n - 1) instructions
      else instructions
    in
    skip nr_decorators instructions


  let make stack =
    let rec run acc = function
      | Type.Atom name :: decorators ->
          if String.equal PyCommon.static_method name then run (has_static acc) decorators
          else if String.equal PyCommon.ABC.abstract_method name then
            run (has_abstract acc) decorators
          else run (unsupported name acc) decorators
      | hd :: _ ->
          L.internal_error "Decorators.make spotted %a@\n" Type.pp hd ;
          raise Failure
      | [] ->
          acc
    in
    let decorators = {is_static= false; is_abstract= false; unsupported= []} in
    run decorators stack
end

(** Attempts to parse a special prelude to method declaration that can be present if __init__ is
    declared as closure, which might be the case when class inheritance is involved. For now we just
    record the presence of this prelude and don't use its content. *)
let parse_closure_prelude {FFI.Code.co_cellvars; co_freevars} instructions =
  let nr_cellvars = Array.length co_cellvars in
  let open Option.Let_syntax in
  is_instruction "LOAD_CLOSURE" instructions
  >>= fun (arg, instructions) ->
  let name = if arg < nr_cellvars then co_cellvars.(arg) else co_freevars.(arg - nr_cellvars) in
  is_instruction "BUILD_TUPLE" instructions
  >>= fun (arg, instructions) -> Some (name, arg, instructions)


(** Parses common bits of method declaration, both instance and static ones *)
let parse_method_core ({FFI.Code.co_consts} as code) instructions =
  let has_closure_prefix, instructions =
    match parse_closure_prelude code instructions with
    | None ->
        (false, instructions)
    | Some (name, tuple_size, instructions) ->
        if not (String.equal name "__class__") then
          L.user_warning "ill-formed method declaration. Closure mentioned cell '%s'@\n" name ;
        if not (Int.equal tuple_size 1) then
          L.user_warning "ill-formed method declaration. Tuple size is %d@\n" tuple_size ;
        (true, instructions)
  in
  let open Option.Let_syntax in
  is_instruction "LOAD_CONST" instructions
  >>= fun (arg, instructions) ->
  let method_code = co_consts.(arg) in
  is_instruction "LOAD_CONST" instructions
  >>= fun (arg, instructions) ->
  FFI.Constant.as_name co_consts.(arg)
  >>= fun raw_qualified_name ->
  is_instruction "MAKE_FUNCTION" instructions
  >>= fun (arg, instructions) ->
  let flags =
    (* TODO: the rest of the code doesn't nicely support closure, so we hide that bit, since
       method are not closures. This is an internal specificity of cpython we don't really care
       about.
       Revisit this decision once closures are well supported. *)
    let flags = MakeFunctionFlags.mk arg in
    if has_closure_prefix && not (MakeFunctionFlags.mem flags Closure) then (
      L.user_warning "ill-formed method declaration. Has closure prefix but invalid flags@\n" ;
      flags )
    else MakeFunctionFlags.unset flags Closure
  in
  Some (method_code, raw_qualified_name, flags, instructions)


(** After the optional method signature is added on the stack, the same 4 instructions will be used
    to declare a method, giving us its short name, (Python) qualified name, actual code object and
    the method flags.

    Method declaration starts with a bunch of [LOAD_NAME] instructions for any decorator (like
    [@staticmethod]) that might be here, and but also for any type (like [int]) that might be used
    in the type signature. There is no clear distinction between the two sets of names, so we'll
    load them all upfront, deal with the signature parsing, and use the left overs as source for the
    decorator information *)
let parse_method ({FFI.Code.co_names} as code) class_name instructions =
  let open Option.Let_syntax in
  (* First we parse all the names we can, to prepare for signature and decorator parsing *)
  let stack, instructions = load_names co_names [] instructions in
  (* Then we check if there's more structured type information or not *)
  let type_info, stack, instructions =
    match parse_method_type_info code stack instructions with
    | Some res ->
        res
    | None ->
        ([], stack, instructions)
  in
  (* when class inheritance is in the mix, __init__ has a special closure built here.
     Full bytecode is:

     NEW       42 LOAD_CLOSURE             0 (__class__)
     NEW       44 BUILD_TUPLE              1
               46 LOAD_CONST               7 (<code object __init__ at 0x7fe4d7b85190, file "parent.py", line 11>)
               48 LOAD_CONST               8 ('D.__init__')
     CHANGED   50 MAKE_FUNCTION           12 (annotations, closure)
               52 STORE_NAME               6 (__init__)
     For the time being, we'll skip these as they are only used for complex meta classes.
  *)
  parse_method_core code instructions
  >>= fun (method_code, raw_qualified_name, flags, instructions) ->
  (* Now that most of the method declaration is parsed, we should only have decorators left over
     on the stack, so let's parse the decorators *)
  let ({Decorators.is_static; is_abstract} as decorators) = Decorators.make stack in
  (* Actual method declaration bytecode *)
  let instructions = Decorators.skip_calls raw_qualified_name instructions decorators in
  is_instruction "STORE_NAME" instructions
  >>= fun (arg, instructions) ->
  let name = co_names.(arg) in
  let signature =
    let f target {PyCommon.name; annotation} =
      Option.some_if (String.equal target name) annotation
    in
    let mk_formals_types code =
      let arguments = FFI.Code.get_arguments code in
      Array.foldi arguments ~init:[] ~f:(fun ndx args argname ->
          let typ =
            match List.find_map type_info ~f:(f argname) with
            | Some typ ->
                Some typ
            | None ->
                (* [self] is the first argument of instance methods. If its type annotation is
                   missing, use the current class *)
                Option.some_if ((not is_static) && Int.equal ndx 0) class_name
          in
          let annotation = Option.value typ ~default:(Ident.mk "object") in
          let arg = {PyCommon.name= argname; annotation} in
          arg :: args )
    in
    let rev_formals_types =
      Option.value_map ~default:[] ~f:mk_formals_types (FFI.Constant.as_code method_code)
    in
    List.rev
    @@
    match List.find_map type_info ~f:(f PyCommon.return) with
    | None ->
        (* __new__ returns an instance of the current class, __init__ just modifies it *)
        let annotation = if String.equal PyCommon.init__ name then "None" else "object" in
        let annotation = Ident.mk annotation in
        {PyCommon.name= PyCommon.return; annotation} :: rev_formals_types
    | Some annotation ->
        {PyCommon.name= PyCommon.return; annotation} :: rev_formals_types
  in
  Decorators.warn class_name name decorators ;
  Some
    ( { PyCommon.name
      ; code= method_code
      ; signature
      ; is_static
      ; is_abstract
      ; flags
      ; raw_qualified_name }
    , instructions )


let parse_methods code class_name instructions =
  let rec aux method_infos static_method_infos has_init has_new instructions =
    match parse_method code class_name instructions with
    | Some (({is_static} as method_info), instructions) ->
        let {PyCommon.name; signature} = method_info in
        let has_init = if String.equal name PyCommon.init__ then Some signature else has_init in
        let has_new = if String.equal name PyCommon.new__ then Some signature else has_new in
        let method_infos, static_method_infos =
          if is_static then (method_infos, method_info :: static_method_infos)
          else (method_info :: method_infos, static_method_infos)
        in
        aux method_infos static_method_infos has_init has_new instructions
    | None ->
        (* If __init__ closure is involved, it will finish with
                      70 LOAD_CLOSURE             0 (__class__)
                      72 DUP_TOP
                      74 STORE_NAME               9 (__classcell__)
                      76 RETURN_VALUE

           instead of the usual "RETURN NONE" that we already ignore

            We can probably ignore it for now, mostly used by complex meta classes, but
            we should mention it
        *)
        (method_infos, static_method_infos, has_init, has_new, instructions)
  in
  aux [] [] None None instructions


type t =
  { members: PyCommon.annotated_name list
  ; methods: PyCommon.method_info list
  ; static_methods: PyCommon.method_info list
  ; has_init: PyCommon.annotated_name list option
  ; has_new: PyCommon.annotated_name list option }

let parse_class_declaration code class_name instructions =
  (* strip the first 4 instructions which are always
              0 LOAD_NAME                0 (__name__)
              2 STORE_NAME               1 (__module__)
              4 LOAD_CONST               0 ('class_name')
              6 STORE_NAME               2 (__qualname__)
     TODO: deal with that at some point
  *)
  let class_name = Ident.mk class_name in
  let _, instructions = List.split_n instructions 4 in
  (* Now we try to parse any member annotation *)
  let annotations, instructions =
    match is_instruction "SETUP_ANNOTATIONS" instructions with
    | None ->
        ([], instructions)
    | Some (_, instructions) ->
        parse_member_annotations code instructions
  in
  (* TODO: support Python method decorators *)
  (* Now we gather method declarations *)
  try
    let methods, static_methods, has_init, has_new, _ =
      parse_methods code class_name instructions
    in
    Ok {members= annotations; methods; static_methods; has_init; has_new}
  with Decorators.Failure -> Error ()
