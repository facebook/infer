(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

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
  let annotation = Type.show ty in
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


(** Method signatures are similar to member annotations: multiple types are constructed on the
    stack, until a tuple holding the name of the parameters is pushed on the stack too. At this
    point, a map is built, which gives us the method signature. *)
let parse_method_signature code stack instructions =
  (* TODO: if [self] (the first parameter) doesn't have a type, we could give it the current class. *)
  let open Option.Let_syntax in
  parse_tys stack code instructions
  >>= fun (stack, instructions) ->
  load_named_tuple code instructions
  >>= fun (names, instructions) ->
  is_instruction "BUILD_CONST_KEY_MAP" instructions
  >>= fun (arg, instructions) ->
  let tys, stack = List.split_n stack arg in
  let res =
    List.map2 names (List.rev_map ~f:Type.show tys) ~f:(fun name annotation ->
        {PyCommon.name; annotation} )
  in
  match res with
  | List.Or_unequal_lengths.Ok signature ->
      Some (signature, stack, instructions)
  | Unequal_lengths ->
      None


(** After the optional method signature is added on the stack, the same 4 instructions will be used
    to declare a method, giving us its short name, (Python) qualified name, actual code object and
    the method flags. *)
let parse_method ({FFI.Code.co_consts; co_names} as code) stack instructions =
  let open Option.Let_syntax in
  let signature, stack, instructions =
    match parse_method_signature code stack instructions with
    | Some res ->
        res
    | None ->
        ([], stack, instructions)
  in
  is_instruction "LOAD_CONST" instructions
  >>= fun (arg, instructions) ->
  let code = co_consts.(arg) in
  is_instruction "LOAD_CONST" instructions
  >>= fun (arg, instructions) ->
  FFI.Constant.as_name co_consts.(arg)
  >>= fun raw_qualified_name ->
  is_instruction "MAKE_FUNCTION" instructions
  >>= fun (flags, instructions) ->
  is_instruction "STORE_NAME" instructions
  >>= fun (arg, instructions) ->
  let name = co_names.(arg) in
  Some ({PyCommon.name; code; signature; flags; raw_qualified_name}, stack, instructions)


let parse_methods code instructions =
  let rec aux stack method_infos instructions =
    match parse_method code stack instructions with
    | Some (method_info, stack, instructions) ->
        aux stack (method_info :: method_infos) instructions
    | None ->
        (method_infos, instructions)
  in
  aux [] [] instructions


let parse_class_declaration code instructions =
  (* strip the first 4 instructions which are always
              0 LOAD_NAME                0 (__name__)
              2 STORE_NAME               1 (__module__)
              4 LOAD_CONST               0 ('class_name')
              6 STORE_NAME               2 (__qualname__)
     TODO: deal with that at some point
  *)
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
  let method_infos, _ = parse_methods code instructions in
  (annotations, method_infos)
