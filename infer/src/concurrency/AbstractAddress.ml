(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module F = Format
module L = Logging
module MF = MarkupFormatter

(** var type used only for printing, not comparisons *)
module IgnoreVar = struct
  type t = Var.t

  let compare _x _y = 0

  let equal _x _y = true
end

(** access path that does not ignore the type (like the original AccessPath.t) but which instead
    ignores the root variable for comparisons. *)
type path = (IgnoreVar.t * Typ.t) * AccessPath.access list [@@deriving compare, equal]

type t =
  | Global of {path: AccessPath.t}  (** [AccessPath] so as to include root var in comparison *)
  | Class of {typename: Typ.Name.t}  (** Java-only class object identified by typename *)
  | Parameter of {index: int; path: path}
      (** method parameter represented by its 0-indexed position *)
[@@deriving compare, equal]

let get_typ tenv =
  let class_type = Typ.(mk (Tstruct Name.Java.java_lang_class)) in
  let some_ptr_to_class_type = Some Typ.(mk (Tptr (class_type, Pk_pointer))) in
  function
  | Class _ ->
      some_ptr_to_class_type
  | Global {path} | Parameter {path} ->
      AccessPath.get_typ path tenv


let rec norm_path tenv ((typ, (accesses : AccessPath.access list)) as path) =
  match accesses with
  | (FieldAccess fieldname as access) :: rest when Fieldname.is_java_outer_instance fieldname -> (
    match AccessPath.get_access_type tenv typ access with
    | Some typ' ->
        norm_path tenv (typ', rest)
    | None ->
        path )
  | _ ->
      path


let equal_across_threads tenv t1 t2 =
  match (t1, t2) with
  | Parameter {path= (_, typ1), accesses1}, Parameter {path= (_, typ2), accesses2} ->
      (* parameter position/names can be ignored across threads, if types and accesses are equal *)
      let path1, path2 = (norm_path tenv (typ1, accesses1), norm_path tenv (typ2, accesses2)) in
      [%equal: Typ.t * AccessPath.access list] path1 path2
  | _, _ ->
      (* globals and class objects must be identical across threads *)
      equal t1 t2


let is_class_object = function Class _ -> true | _ -> false

let rec make formal_map (hilexp : HilExp.t) =
  match hilexp with
  | AccessExpression access_exp -> (
      let path = HilExp.AccessExpression.to_access_path access_exp in
      match fst (fst path) with
      | Var.LogicalVar _ ->
          (* ignore logical variables *)
          None
      | Var.ProgramVar pvar when Pvar.is_global pvar ->
          Some (Global {path})
      | Var.ProgramVar _ ->
          FormalMap.get_formal_index (fst path) formal_map
          (* ignores non-formals *)
          |> Option.map ~f:(fun index -> Parameter {index; path}) )
  | Constant (Cclass class_id) ->
      (* this is a synchronized(CLASSNAME.class) or class object construct *)
      let typename = Ident.name_to_string class_id |> Typ.Name.Java.from_string in
      Some (Class {typename})
  | Cast (_, hilexp) | Exception hilexp | UnaryOperator (_, hilexp, _) ->
      make formal_map hilexp
  | BinaryOperator _ | Closure _ | Constant _ | Sizeof _ ->
      None


let pp fmt t =
  let pp_path fmt ((var, typ), accesses) =
    F.fprintf fmt "(%a:%a)" Var.pp var (Typ.pp_full Pp.text) typ ;
    if not (List.is_empty accesses) then F.fprintf fmt ".%a" AccessPath.pp_access_list accesses
  in
  match t with
  | Global {path} ->
      F.fprintf fmt "G{%a}" pp_path path
  | Class {typename} ->
      F.fprintf fmt "C{%s}" (Typ.Name.name typename)
  | Parameter {index; path} ->
      F.fprintf fmt "P<%i>{%a}" index pp_path path


let root_class = function
  | Class {typename} ->
      Some typename
  | Global {path= (_, {desc}), _} | Parameter {path= (_, {desc}), _} -> (
    match desc with
    | Tstruct typename | Tptr ({desc= Tstruct typename}, _) ->
        Some typename
    | _ ->
        None )


let describe fmt t =
  let describe_root fmt t =
    root_class t |> Option.iter ~f:(F.fprintf fmt " in %a" (MF.wrap_monospaced Typ.Name.pp))
  in
  let describe_class_object fmt typename = F.fprintf fmt "%s.class" (Typ.Name.name typename) in
  match t with
  | Class {typename} ->
      MF.wrap_monospaced describe_class_object fmt typename
  | Global {path} | Parameter {path} ->
      F.fprintf fmt "%a%a" (MF.wrap_monospaced AccessPath.pp) path describe_root t


type subst = t option Array.t

let pp_subst fmt subst =
  PrettyPrintable.pp_collection fmt ~pp_item:(Pp.option pp) (Array.to_list subst)


let make_subst formal_map actuals =
  let actuals = Array.of_list actuals in
  let len =
    (* deal with var args functions *)
    Int.max (FormalMap.cardinal formal_map) (Array.length actuals)
  in
  let subst = Array.create ~len None in
  FormalMap.iter
    (fun _base idx ->
      if idx < Array.length actuals then subst.(idx) <- make formal_map actuals.(idx) )
    formal_map ;
  subst


let apply_subst (subst : subst) t =
  match t with
  | Global _ | Class _ ->
      Some t
  | Parameter {index; path= _, []} -> (
    try
      (* Special case for when the parameter is used without additional accesses, eg [x] as opposed to [x.f[].g]. *)
      subst.(index)
    with Invalid_argument _ -> None )
  | Parameter {index; path} -> (
    try
      (* Here we know that there are additional accesses on the parameter *)
      match subst.(index) with
      | None ->
          None
      | Some (Class _ as t') as c ->
          L.internal_error "Cannot dereference class object %a in path %a@." pp t' pp t ;
          c
      | Some (Parameter param) ->
          Some (Parameter {param with path= AccessPath.append param.path (snd path)})
      | Some (Global global) ->
          Some (Global {path= AccessPath.append global.path (snd path)})
    with Invalid_argument _ -> None )
