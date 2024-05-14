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

type access = HilExp.t option MemoryAccess.t [@@deriving compare, equal]

let get_access_typ tenv prev_typ (access : access) =
  let lookup tn = Tenv.lookup tenv tn in
  match access with
  | FieldAccess field_name ->
      Struct.get_field_type_and_annotation ~lookup field_name prev_typ |> Option.map ~f:fst
  | ArrayAccess (typ, _) ->
      Some typ
  | TakeAddress ->
      Some (Typ.mk (Tptr (prev_typ, Pk_pointer)))
  | Dereference -> (
    match prev_typ with {Typ.desc= Tptr (typ, _)} -> Some typ | _ -> None )


type access_list = access list [@@deriving compare, equal]

let get_typ tenv ((_, base_typ), accesses) =
  let f acc access = match acc with Some typ -> get_access_typ tenv typ access | None -> None in
  List.fold accesses ~init:(Some base_typ) ~f


let normalise_access_list (accesses : access_list) =
  let exception NormalisationFailure in
  let rec normalise_access_list_inner (accesses : access_list) =
    match accesses with
    | [] ->
        []
    | TakeAddress :: Dereference :: rest ->
        normalise_access_list_inner rest
    | (TakeAddress as _addr) :: (FieldAccess _ as field) :: rest ->
        field :: normalise_access_list_inner rest
    | TakeAddress :: _ :: _ ->
        (* an address can only be dereferenced *)
        raise NormalisationFailure
    | access :: rest ->
        access :: normalise_access_list_inner rest
  in
  try Some (normalise_access_list_inner accesses) with NormalisationFailure -> None


let pp_with_base pp_base fmt (base, accesses) =
  let rec pp_rev_accesses fmt (accesses : access_list) =
    match (accesses, !Language.curr_language) with
    | _, Erlang ->
        L.internal_error "Erlang not supported"
    | _, Hack ->
        L.internal_error "Hack not supported"
    | _, Python ->
        L.internal_error "Python not supported"
    | [], _ ->
        pp_base fmt base
    | ArrayAccess _ :: rest, _ ->
        F.fprintf fmt "%a[]" pp_rev_accesses rest
    | FieldAccess field_name :: Dereference :: rest, _ ->
        let op =
          match !Language.curr_language with
          | Clang ->
              "->"
          | Java ->
              "."
          | CIL ->
              "."
          | Erlang ->
              L.die InternalError "Erlang not supported"
          | Hack ->
              L.die InternalError "Hack not supported"
          | Python ->
              L.die InternalError "Python not supported"
        in
        F.fprintf fmt "%a%s%a" pp_rev_accesses rest op Fieldname.pp field_name
    | FieldAccess field_name :: rest, _ ->
        (* Java is allowed here only because the frontend is broken and generates
           [FieldAccess] without a [Dereference] for static fields *)
        F.fprintf fmt "%a.%a" pp_rev_accesses rest Fieldname.pp field_name
    | Dereference :: rest, Clang ->
        F.fprintf fmt "*(%a)" pp_rev_accesses rest
    | TakeAddress :: rest, Clang ->
        F.fprintf fmt "&(%a)" pp_rev_accesses rest
    | Dereference :: rest, CIL ->
        F.fprintf fmt "*(%a)" pp_rev_accesses rest
    | TakeAddress :: rest, CIL ->
        F.fprintf fmt "&(%a)" pp_rev_accesses rest
    | access :: rest, Java ->
        L.internal_error "Asked to print %a in Java mode@\n"
          (MemoryAccess.pp (fun _ _ -> ()))
          access ;
        pp_rev_accesses fmt rest
  in
  pp_rev_accesses fmt (List.rev accesses)


(* A wrapper that ignores ProgramVar.Global_var translation_unit in comparision
 * as we cannot add that ignore there due to issues with Siof 
 * similar hack to D51588007 *)
module SVar = struct
  include Var

  let compare x y =
    match (x, y) with
    | ProgramVar x, ProgramVar y when Pvar.is_global x && Pvar.is_global y ->
        [%compare: Mangled.t * Typ.template_spec_info]
          (Pvar.get_name x, Pvar.get_template_args x)
          (Pvar.get_name y, Pvar.get_template_args y)
    | ProgramVar x, _ when Pvar.is_global x ->
        -1
    | _, ProgramVar x when Pvar.is_global x ->
        1
    | _ ->
        Var.compare x y


  let equal x y =
    match (x, y) with
    | ProgramVar x, ProgramVar y when Pvar.is_global x && Pvar.is_global y ->
        [%equal: Mangled.t * Typ.template_spec_info]
          (Pvar.get_name x, Pvar.get_template_args x)
          (Pvar.get_name y, Pvar.get_template_args y)
    | ProgramVar x, _ when Pvar.is_global x ->
        false
    | _, ProgramVar x when Pvar.is_global x ->
        false
    | _ ->
        Var.equal x y
end

(** var type used only for printing, not comparisons *)
module IgnoreVar = struct
  type t = Var.t

  let compare _x _y = 0

  let equal _x _y = true
end

type raw_path = (SVar.t * Typ.t) * access_list [@@deriving compare, equal]

(** path-like type using [MemoryAccess] steps instead of [AccessPath.access]. It does not ignore the
    root variable type (like the original [AccessPath.t]) but instead ignores the root variable for
    comparisons. *)
type unrooted_path = (IgnoreVar.t * Typ.t) * access_list [@@deriving compare, equal]

type t =
  | Global of {path: raw_path}  (** root var is included in comparison *)
  | Class of {typename: Typ.Name.t}  (** Java-only class object identified by typename *)
  | Parameter of {index: int; path: unrooted_path}
      (** method parameter represented by its 0-indexed position, root var is not used in comparison *)
[@@deriving compare, equal]

let get_typ tenv = function
  | Class _ ->
      Some StdTyp.Java.pointer_to_java_lang_class
  | Global {path} | Parameter {path} ->
      get_typ tenv path


let append ~on_to:(base, accesses) (_, accesses') =
  match normalise_access_list (accesses @ accesses') with
  | Some accesses'' ->
      Some (base, accesses'')
  | None ->
      None


(* Remove initial prefix of synthetic java fields giving access to outer-class fields.
   This allows comparing as equal two paths generated by two different inner classes of the
   same outer class. *)
let rec inner_class_normalise tenv ((typ, (accesses : access_list)) as path) =
  match accesses with
  | (Dereference as access) :: (FieldAccess fieldname as access') :: rest
    when Fieldname.is_java_outer_instance fieldname -> (
    match get_access_typ tenv typ access with
    | Some typ' -> (
      match get_access_typ tenv typ' access' with
      | Some typ'' ->
          inner_class_normalise tenv (typ'', rest)
      | None ->
          path )
    | None ->
        path )
  | _ ->
      path


let equal_across_threads tenv t1 t2 =
  match (t1, t2) with
  | Parameter {path= (_, typ1), accesses1}, Parameter {path= (_, typ2), accesses2} ->
      (* parameter position/names can be ignored across threads, if types and accesses are equal *)
      let path1 = inner_class_normalise tenv (typ1, accesses1) in
      let path2 = inner_class_normalise tenv (typ2, accesses2) in
      [%equal: Typ.t * access_list] path1 path2
  | _, _ ->
      (* globals and class objects must be identical across threads *)
      equal t1 t2


let is_class_object = function Class _ -> true | _ -> false

let rec make formal_map (hilexp : HilExp.t) =
  let make_from_acc_exp acc_exp =
    match HilExp.AccessExpression.to_accesses acc_exp with
    | HilExp.AccessExpression.Base ((var, _) as base), accesses -> (
      match normalise_access_list accesses with
      | Some accesses -> (
          let path = (base, accesses) in
          match var with
          | Var.LogicalVar _ ->
              (* ignore logical variables *)
              None
          | Var.ProgramVar pvar when Pvar.is_global pvar ->
              Some (Global {path})
          | Var.ProgramVar _ ->
              FormalMap.get_formal_index base formal_map
              (* ignores non-formals *)
              |> Option.map ~f:(fun index -> Parameter {index; path}) )
      | _ ->
          None )
    | _ ->
        None
  in
  match hilexp with
  | AccessExpression access_exp ->
      make_from_acc_exp access_exp
  | Constant (Cclass class_id) ->
      (* this is a synchronized(CLASSNAME.class) or class object construct *)
      let typename = Ident.name_to_string class_id |> Typ.Name.Java.from_string in
      Some (Class {typename})
  | Cast (_, hilexp) | Exception hilexp | UnaryOperator (_, hilexp, _) ->
      make formal_map hilexp
  | BinaryOperator _ | Closure _ | Constant _ | Sizeof _ ->
      None


let pp fmt t =
  let pp_path fmt path =
    let pp_base fmt (var, typ) = F.fprintf fmt "(%a:%a)" Var.pp var (Typ.pp_full Pp.text) typ in
    pp_with_base pp_base fmt path
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
  let describe_path fmt path =
    let describe_base fmt (var, _) = Var.pp fmt var in
    pp_with_base describe_base fmt path
  in
  let describe_root fmt t =
    root_class t |> Option.iter ~f:(F.fprintf fmt " in %a" (MF.wrap_monospaced Typ.Name.pp))
  in
  let describe_class_object fmt typename = F.fprintf fmt "%s.class" (Typ.Name.name typename) in
  match t with
  | Class {typename} ->
      MF.wrap_monospaced describe_class_object fmt typename
  | Global {path} | Parameter {path} ->
      F.fprintf fmt "%a%a" (MF.wrap_monospaced describe_path) path describe_root t


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
      | Some (Parameter param) -> (
        match append ~on_to:param.path path with
        | Some path ->
            Some (Parameter {param with path})
        | None ->
            None )
      | Some (Global global) -> (
        match append ~on_to:global.path path with Some path -> Some (Global {path}) | None -> None )
    with Invalid_argument _ -> None )
