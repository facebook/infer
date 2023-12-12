(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

(** invariant: if [package = Some str] then [not (String.equal str "")]. [classname] appears first
    so that the comparator fails earlier *)
type t = {classname: string; package: string option}
[@@deriving compare, equal, yojson_of, sexp, hash, normalize]

let make ~package ~classname =
  match package with Some "" -> {package= None; classname} | _ -> {package; classname}


let from_string str =
  match String.rsplit2 str ~on:'.' with
  | None ->
      {classname= str; package= None}
  | Some ("", _) ->
      L.die InternalError "Empty package path in Java qualified classname.@."
  | Some (pkg, classname) ->
      {classname; package= Some pkg}


let to_string = function
  | {classname; package= None} ->
      classname
  | {classname; package= Some pkg} ->
      String.concat ~sep:"." [pkg; classname]


let pp fmt = function
  | {classname; package= None} ->
      F.pp_print_string fmt classname
  | {classname; package= Some pkg} ->
      F.fprintf fmt "%s.%s" pkg classname


let package {package} = package

let classname {classname} = classname

let is_int s =
  try
    ignore (int_of_string s) ;
    true
  with Failure _ -> false


let get_outer_class_name {package; classname} =
  String.rsplit2 classname ~on:'$' |> Option.map ~f:(fun (outer, _) -> {package; classname= outer})


(*
 Anonymous classes have two forms:
 - classic anonymous classes: suffixes in form of $<int>.
 - classes corresponding to lambda-expressions: they are manifested as $Lambda$.
 - two forms above nested inside each other.
 Also non-anonymous (user-defined) name can be nested as well (Class$NestedClass).
 In general case anonymous class name looks something like
 Class$NestedClass$1$17$5$Lambda$_1_2, and we need to return Class$NestedClass *)
let get_user_defined_class_if_anonymous_inner {package; classname} =
  let is_anonymous_name = function
    | "Lambda" ->
        true
    | name when is_int name ->
        true
    | _ ->
        false
  in
  let pieces = String.split classname ~on:'$' in
  let first_anonymous_name = List.findi pieces ~f:(fun _index name -> is_anonymous_name name) in
  Option.bind first_anonymous_name ~f:(fun (index, _name) ->
      (* Everything before this index did not have anonymous prefixes. Deem it user defined. *)
      match List.take pieces index with
      | [] ->
          (* This is a weird situation - our class _starts_ with an anonymous name.
             This should not happen normally, but we can not rule this out completely since there is no physical limitations on
             the bytecode names.
             In this case, we return [None] because formally this is not an anonymous _inner_ class, but anonymous outermost class instead.
             TODO: redesign this API so this case is modelled directly
          *)
          None
      | list ->
          (* Assemble back all pieces together *)
          Some {package; classname= String.concat ~sep:"$" list} )


let is_anonymous_inner_class_name t = get_user_defined_class_if_anonymous_inner t |> is_some

let pp_with_verbosity ~verbose fmt t =
  if verbose then pp fmt t else F.pp_print_string fmt (classname t)
