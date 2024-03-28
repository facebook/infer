(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let template_arg = Str.regexp "<[^<>]*>"

let rec strip_template_args str =
  if
    (not (String.contains str '<'))
    || String.equal str Procname.Java.constructor_method_name
    || String.equal str Procname.Java.class_initializer_method_name
  then str
  else
    let result = Str.global_replace template_arg "" str in
    if String.equal result str then str else strip_template_args result


(** [call_matches <named args> C methods] builds a method matcher for calls [C.foo] where [foo] is
    in [methods]. Named arguments change behaviour:

    - [search_superclasses=true] will match calls [S.foo] where [S] is a superclass of [C].
    - [method_prefix=true] will match calls [C.foo] where [foo] is a prefix of a string in [methods]
    - [actuals_pred] is a predicate that runs on the expressions fed as arguments to the call, and
      which must return [true] for the matcher to return [true]. *)
let call_matches ~search_superclasses ~method_prefix ~actuals_pred clazz methods =
  let clazz = strip_template_args clazz in
  let methods = List.map methods ~f:strip_template_args in
  let method_matcher =
    if method_prefix then fun current_method target_method ->
      String.is_prefix ~prefix:target_method current_method
    else fun current_method target_method -> String.equal target_method current_method
  in
  let class_matcher =
    if search_superclasses then
      let target = "class " ^ clazz in
      let is_target tname _tstruct =
        Typ.Name.to_string tname |> strip_template_args |> String.equal target
      in
      fun tenv pname ->
        Procname.get_class_type_name pname
        |> Option.exists ~f:(PatternMatch.supertype_exists tenv is_target)
    else fun _tenv pname ->
      Procname.get_class_name pname |> Option.map ~f:strip_template_args
      |> Option.exists ~f:(String.equal clazz)
  in
  (fun tenv pn actuals ->
    actuals_pred actuals
    &&
    let mthd = Procname.get_method pn |> strip_template_args in
    List.exists methods ~f:(method_matcher mthd) && class_matcher tenv pn )
  |> Staged.stage


type t = Tenv.t -> Procname.t -> HilExp.t list -> bool

type record =
  { search_superclasses: bool
  ; method_prefix: bool
  ; actuals_pred: HilExp.t list -> bool
  ; classname: string
  ; methods: string list }

let of_record {search_superclasses; method_prefix; actuals_pred; classname; methods} =
  call_matches ~search_superclasses ~method_prefix ~actuals_pred classname methods |> Staged.unstage


let default =
  { search_superclasses= true
  ; method_prefix= false
  ; actuals_pred= (fun _ -> true)
  ; classname= ""
  ; methods= [] }


let of_list matchers tenv pn actuals = List.exists matchers ~f:(fun m -> m tenv pn actuals)

let of_records records = List.map ~f:of_record records |> of_list

let of_json top_json =
  let error json =
    L.(die UserError "Could not parse json matcher(s): %s" (Yojson.Safe.to_string json))
  in
  let make_matcher_from_json json =
    let parse_method_name = function `String methodname -> methodname | _ -> error json in
    let rec parse_fields assoclist acc =
      match assoclist with
      | ("search_superclasses", `Bool b) :: rest ->
          {acc with search_superclasses= b} |> parse_fields rest
      | ("method_prefix", `Bool b) :: rest ->
          {acc with method_prefix= b} |> parse_fields rest
      | ("classname", `String classname) :: rest ->
          {acc with classname} |> parse_fields rest
      | ("methods", `List methodnames) :: rest ->
          let methods = List.map methodnames ~f:parse_method_name in
          {acc with methods} |> parse_fields rest
      | [] ->
          if String.equal acc.classname "" || List.is_empty acc.methods then error json else acc
      | _ ->
          error json
    in
    match json with `Assoc fields -> parse_fields fields default | _ -> error json
  in
  match top_json with
  | `List matchers_json ->
      List.map matchers_json ~f:make_matcher_from_json |> of_records
  | _ ->
      error top_json
