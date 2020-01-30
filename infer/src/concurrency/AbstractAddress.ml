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
    ignores the root variable for comparisons; this is taken care of by the root type *)
type path = (IgnoreVar.t * Typ.t) * AccessPath.access list [@@deriving compare, equal]

type root =
  | Global of Mangled.t
  | Class of Typ.name
  | Parameter of int  (** method parameter represented by its 0-indexed position *)
[@@deriving compare, equal]

type t = {root: root; path: path} [@@deriving compare, equal]

let get_typ tenv t = AccessPath.get_typ t.path tenv

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
  match (t1.root, t2.root) with
  | Global _, Global _ | Class _, Class _ ->
      (* globals and class objects must be identical across threads *)
      equal t1 t2
  | Parameter _, Parameter _ ->
      let ((_, typ1), accesses1), ((_, typ2), accesses2) = (t1.path, t2.path) in
      (* parameter position/names can be ignored across threads, if types and accesses are equal *)
      let path1, path2 = (norm_path tenv (typ1, accesses1), norm_path tenv (typ2, accesses2)) in
      [%equal: Typ.t * AccessPath.access list] path1 path2
  | _, _ ->
      false


let is_class_object = function {root= Class _} -> true | _ -> false

(* using an indentifier for a class object, create an access path representing that lock;
   this is for synchronizing on Java class objects only *)
let path_of_java_class =
  let typ = Typ.(mk (Tstruct Name.Java.java_lang_class)) in
  let typ' = Typ.(mk (Tptr (typ, Pk_pointer))) in
  fun class_id ->
    let ident = Ident.create_normal class_id 0 in
    AccessPath.of_id ident typ'


let make_global path mangled = {root= Global mangled; path}

let make_parameter path index = {root= Parameter index; path}

let make_class path typename = {root= Class typename; path}

(** convert an expression to a canonical form for a lock identifier *)
let rec make formal_map (hilexp : HilExp.t) =
  match hilexp with
  | AccessExpression access_exp -> (
      let path = HilExp.AccessExpression.to_access_path access_exp in
      match fst (fst path) with
      | Var.LogicalVar _ ->
          (* ignore logical variables *)
          None
      | Var.ProgramVar pvar when Pvar.is_global pvar ->
          Some (make_global path (Pvar.get_name pvar))
      | Var.ProgramVar _ ->
          FormalMap.get_formal_index (fst path) formal_map
          (* ignores non-formals *)
          |> Option.map ~f:(make_parameter path) )
  | Constant (Cclass class_id) ->
      (* this is a synchronized/lock(CLASSNAME.class) construct *)
      let path = path_of_java_class class_id in
      let typename = Ident.name_to_string class_id |> Typ.Name.Java.from_string in
      Some (make_class path typename)
  | Cast (_, hilexp) | Exception hilexp | UnaryOperator (_, hilexp, _) ->
      make formal_map hilexp
  | BinaryOperator _ | Closure _ | Constant _ | Sizeof _ ->
      None


let make_java_synchronized formals procname =
  match procname with
  | Procname.Java java_pname when Procname.Java.is_static java_pname ->
      (* this is crafted so as to match synchronized(CLASSNAME.class) constructs *)
      let typename = Procname.Java.get_class_type_name java_pname in
      let path = Typ.Name.name typename |> Ident.string_to_name |> path_of_java_class in
      Some (make_class path typename)
  | Procname.Java _ ->
      FormalMap.get_formal_base 0 formals |> Option.map ~f:(fun base -> make_parameter (base, []) 0)
  | _ ->
      L.die InternalError "Non-Java methods cannot be synchronized.@\n"


let pp fmt {root; path} =
  let pp_path fmt ((var, typ), accesses) =
    F.fprintf fmt "(%a:%a)" Var.pp var (Typ.pp_full Pp.text) typ ;
    if not (List.is_empty accesses) then F.fprintf fmt ".%a" AccessPath.pp_access_list accesses
  in
  match root with
  | Global mangled ->
      F.fprintf fmt "G<%a>{%a}" Mangled.pp mangled pp_path path
  | Class typename ->
      F.fprintf fmt "C<%a>{%a}" Typ.Name.pp typename pp_path path
  | Parameter idx ->
      F.fprintf fmt "P<%i>{%a}" idx pp_path path


let root_class {path= (_, {Typ.desc}), _} =
  match desc with Typ.Tstruct name | Typ.Tptr ({desc= Tstruct name}, _) -> Some name | _ -> None


let describe fmt lock =
  let describe_lock fmt lock = (MF.wrap_monospaced AccessPath.pp) fmt lock.path in
  let describe_typename = MF.wrap_monospaced Typ.Name.pp in
  let describe_owner fmt lock =
    root_class lock |> Option.iter ~f:(F.fprintf fmt " in %a" describe_typename)
  in
  F.fprintf fmt "%a%a" describe_lock lock describe_owner lock


let compare_wrt_reporting {path= (_, typ1), _} {path= (_, typ2), _} =
  (* use string comparison on types as a stable order to decide whether to report a deadlock *)
  String.compare (Typ.to_string typ1) (Typ.to_string typ2)


(** A substitution from formal position indices to actuals. Since we only care about locks, use
    [None] to denote an argument that cannot be resolved to a lock object. *)
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


let apply_subst (subst : subst) lock =
  match lock.root with
  | Global _ | Class _ ->
      Some lock
  | Parameter index -> (
    try
      match subst.(index) with
      | None ->
          None
      | Some actual ->
          Some {actual with path= AccessPath.append actual.path (snd lock.path)}
    with Invalid_argument _ -> None )
