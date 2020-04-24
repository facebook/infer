(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

(** invariant: if [package = Some str] then [not (String.equal str "")] *)
type t = {classname: string; package: string option} [@@deriving compare, equal]

module Map = Caml.Map.Make (struct
  type nonrec t = t

  let compare = compare
end)

module Set = Caml.Set.Make (struct
  type nonrec t = t

  let compare = compare
end)

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


(* Strips $<int> suffixes from the class name, and return how many were stripped *)
let strip_anonymous_suffixes_if_present classname =
  let rec strip_recursively classname nesting_level =
    match String.rsplit2 classname ~on:'$' with
    | Some (outer, suffix) when is_int suffix ->
        (* Suffix is an integer - that was an anonymous class.
           But it could be nested inside another anonymous class as well *)
        strip_recursively outer (nesting_level + 1)
    | _ ->
        (* Suffix is not an integer or not present - not an anonymous class *)
        (classname, nesting_level)
  in
  strip_recursively classname 0


(* Strips everything after $Lambda$ (if it is a lambda-class),
   and returns the result string together with if it was stripped *)
let strip_lambda_if_present classname =
  match String.substr_index classname ~pattern:"$Lambda$" with
  | Some index ->
      (String.prefix classname index, true)
  | None ->
      (classname, false)


(*
 Anonymous classes have two forms:
 - classic anonymous classes: suffixes in form of $<int>.
 - classes corresponding to lambda-expressions: they are manifested as $Lambda$.
 - two forms above nested inside each other.
 Also non-anonymous (user-defined) name can be nested as well (Class$NestedClass).
 In general case anonymous class name looks something like
 Class$NestedClass$1$17$5$Lambda$_1_2, and we need to return Class$NestedClass *)
let get_user_defined_class_if_anonymous_inner {package; classname} =
  let without_lambda, was_lambda_stripped = strip_lambda_if_present classname in
  let outer_class_name, nesting_level = strip_anonymous_suffixes_if_present without_lambda in
  let was_stripped = was_lambda_stripped || nesting_level > 0 in
  if was_stripped then Some {package; classname= outer_class_name} else None


let is_anonymous_inner_class_name t = get_user_defined_class_if_anonymous_inner t |> is_some

let is_external_via_config t =
  let package = package t in
  Option.exists ~f:Config.java_package_is_external package


let pp_with_verbosity ~verbose fmt t =
  if verbose then pp fmt t else F.pp_print_string fmt (classname t)
