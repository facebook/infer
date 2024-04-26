(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd
module L = Logging

(* TODO: do validation of the language-dependent part (that is, of constants) *)

type tag = string

type question = Question of {source: tag; sink: tag}

type position = Argument of int | Return

type value =
  | Position of position
  | Constant of Yojson.Safe.t (* constants are interpreted during translation, based on language *)

type eq_condition = Eq of {left: value; right: value}

type matcher =
  | Matcher of
      { tag: tag
      ; pattern: string (* interpretation may depend on language *)
      ; arity: int
            (* TODO: Required by Topl. Make it optional in Topl, then make it optinal here. *)
      ; position: position
      ; condition: eq_condition list }

type query = Query of {language: Language.t; matchers: matcher list; questions: question list}

let ignoring path json =
  L.user_error "@[<v>%s: Ignoring:@ @[%a@]@;@]" path (Yojson.Safe.pretty_print ~std:false) json


let parse_position ?(path = None) json : position option =
  match json with
  | `List [`String "arg"; `Int n] when Int.(n >= 0) ->
      Some (Argument n)
  | `List [`String "ret"] ->
      Some Return
  | _ ->
      let warn path =
        L.user_error
          "@[<v>%s: A position is either [\"arg\",n] or [\"ret\"], with nonnegative n.@;@]" path ;
        ignoring path json
      in
      Option.iter path ~f:warn ;
      None


let parse_value json : value =
  Option.value_map ~default:(Constant json)
    ~f:(fun position -> Position position)
    (parse_position json)


let parse_eq_condition path json =
  match json with
  | `List [`String "equal"; left_json; right_json] ->
      let left = parse_value left_json in
      let right = parse_value right_json in
      Some (Eq {left; right})
  | _ ->
      L.user_error "@[<v>%s: An equality condition should look like [\"equal\", left, right].@;@]"
        path ;
      ignoring path json ;
      None


let parse_condition path json =
  match json with
  | `List eq_condition_list ->
      List.filter_map ~f:(parse_eq_condition path) eq_condition_list
  | _ ->
      L.user_error "@[<v>%s: A condition should be a list (of equality conditions)@;@]" path ;
      ignoring path json ;
      []


(** [parse_assoc path record_name mandatory optionals assocs] parses [assocs] trying to find the
    values of [mandatory] and [optional] fields. Typically, [assocs] comes from an [`Assoc] value of
    type [Yojson.Safe.t].

    {[
      parse_assoc path record_name ["a"; "b"]
        [("c_opt", `Null); ("d_opt", `Int 1)]
        [("a", json_a); ("c_opt", json_c); ("b", json_b)]
    ]}

    returns

    {[
      Some ([("a", json_a); ("b", json_b)], [("c_opt", json_c); ("d_opt", `Int 1)])
    ]}

    Fields "a" and "b" are mandatory; fields "c_opt" and "d_opt" are optional, which is why the
    caller must provide a default (JSON) value for them. The output has the form
    [Some (mandatory_bindings, optional_bindings)] if parsing is successful, with both binding lists
    sorted; if there is a problem parsing, [L.user_error] is used to provide a diagnostic message,
    and [None] is returned. *)
let parse_assoc path record_name fields fields_with_default assocs =
  let pp_and_list pp = Fmt.(list ~sep:(any " and ")) pp in
  let fields_set = String.Set.of_list fields in
  if Set.length fields_set < List.length fields then
    L.die InternalError "precondition: fields are unique" ;
  let fields_with_default = String.Map.of_alist_exn fields_with_default in
  let () =
    (* Assert no overlap between mandatory and optional *)
    let both = Set.inter fields_set (Map.key_set fields_with_default) in
    if not (Set.is_empty both) then
      L.die InternalError "precondition: a field cannot be both mandatory and optional: %a"
        (pp_and_list Fmt.string) (Set.elements both)
  in
  let index : Yojson.Safe.t String.Map.t =
    (* Transform assocs into a map. If there are duplicates, drop them and warn. *)
    let index = String.Map.of_alist_multi assocs in
    let f ~key:field ~data:json_list =
      let extra = List.tl_exn json_list in
      ( if not (List.is_empty extra) then
          let pv f json =
            Format.fprintf f "\"%s\":%a" field (Yojson.Safe.pretty_print ~std:false) json
          in
          L.user_error "@[<v>%s: In %s, found repeated field. Ignoring %a@.;@]" path record_name
            (pp_and_list pv) extra ) ;
      List.hd_exn json_list
    in
    Map.mapi ~f index
  in
  let mandatory : Yojson.Safe.t String.Map.t = Map.filter_keys index ~f:(Set.mem fields_set) in
  let optional : Yojson.Safe.t String.Map.t =
    let f ~key:_ = function
      | `Left _given ->
          None
      | `Both (given, _default) ->
          Some given
      | `Right default ->
          Some default
    in
    Map.merge index fields_with_default ~f
  in
  let () =
    (* Warn on unexpected fields. *)
    let unexpected =
      let ( -- ) = Set.diff in
      Map.key_set index -- Map.key_set mandatory -- Map.key_set optional
    in
    if not (Set.is_empty unexpected) then
      L.user_error "@[<v>%s: In %s, there are unexpected fields: %a@;@]" path record_name
        (pp_and_list Fmt.string) (Set.elements unexpected)
  in
  if Map.length mandatory < Set.length fields_set then (
    (* Warn on missing mandatory fields, and return None to abort. *)
    let missing = List.filter fields ~f:(Fn.non @@ Map.mem mandatory) in
    L.user_error "@[<v>%s: In %s, there are missing mandatory fields: %a.@;@]" path record_name
      (pp_and_list Fmt.string) missing ;
    None )
  else
    Some
      (Map.to_alist ~key_order:`Increasing mandatory, Map.to_alist ~key_order:`Increasing optional)


let parse_string path name json =
  match json with
  | `String value ->
      Some value
  | _ ->
      L.user_error "@[<v>%s: for %s, expecting a string; found %a@;@]" path name Yojson.Safe.pp json ;
      None


let parse_arity path json =
  match json with
  | `Int value ->
      Some value
  | _ ->
      L.user_error "@[<v>%s: Arity should be a nonnegative integer; found %a@;@]" path
        Yojson.Safe.pp json ;
      None


let parse_matcher path matcher =
  let open IOption.Let_syntax in
  match matcher with
  | `List [`String tag; `String pattern; arity_json; position_json] ->
      let* position = parse_position ~path:(Some path) position_json in
      let+ arity = parse_arity path arity_json in
      Matcher {tag; pattern; arity; position; condition= []}
  | `List [`String tag; `String pattern; arity_json; position_json; condition_json] ->
      let* position = parse_position ~path:(Some path) position_json in
      let condition = parse_condition path condition_json in
      let+ arity = parse_arity path arity_json in
      Matcher {tag; pattern; arity; position; condition}
  | `Assoc matcher_fields -> (
    match
      parse_assoc path "matcher"
        ["tag"; "pattern"; "arity"; "position"]
        [("condition", `List [])]
        matcher_fields
    with
    | Some
        ( [ ("arity", arity_json)
          ; ("pattern", pattern_json)
          ; ("position", position_json)
          ; ("tag", tag_json) ]
        , [("condition", condition_json)] ) ->
        let* tag = parse_string path "tag" tag_json in
        let* pattern = parse_string path "pattern" pattern_json in
        let* arity = parse_arity path arity_json in
        let+ position = parse_position ~path:(Some path) position_json in
        let condition = parse_condition path condition_json in
        Matcher {tag; pattern; arity; position; condition}
    | None ->
        None
    | Some _ ->
        L.die InternalError "bug in parse_assoc" )
  | json ->
      L.user_error
        "@[<v>%s: A matcher should look like {\"tag\":..., \"pattern\":..., \"arity\":..., \
         \"condition\":...}@;\
         The condition is optional.@ It is also possible provide just the values, in a list, in \
         the order indicated.@;\
         @]"
        path ;
      ignoring path json ;
      None


let parse_language path json =
  match json with
  | `String "Erlang" | `String "erlang" ->
      Language.Erlang
  | _ ->
      L.user_error "@[<v>%s: Unrecognized language. Assuming Erlang.@;@]" path ;
      ignoring path json ;
      Language.Erlang


let parse_question path json =
  match json with
  | `Assoc fields -> (
    match parse_assoc path "question" ["sink"; "source"] [] fields with
    | Some ([("sink", sink_json); ("source", source_json)], []) ->
        let open IOption.Let_syntax in
        let* sink = parse_string path "sink" sink_json in
        let+ source = parse_string path "source" source_json in
        Question {sink; source}
    | Some _ ->
        L.die InternalError "bug in parse_assoc"
    | None ->
        None )
  | _ ->
      L.user_error "@[<v>%s: A question should look like {\"source\":\"TAG\", \"sink\":\"TAG\"}@;@]"
        path ;
      ignoring path json ;
      None


let parse_list name parse_element path json =
  match json with
  | `List list_json ->
      List.filter_map ~f:(parse_element path) list_json
  | _ ->
      L.user_error "@[<v>%s: Expecting a %s list. Assuming empty list.@;@]" path name ;
      ignoring path json ;
      []


let parse_json path json =
  let path = Filename.basename path in
  match json with
  | `Assoc fields -> (
    match
      parse_assoc path "query" ["matchers"; "questions"] [("language", `String "erlang")] fields
    with
    | Some
        ([("matchers", matchers_json); ("questions", questions_json)], [("language", language_json)])
      ->
        let language = parse_language path language_json in
        let matchers = parse_list "matcher" parse_matcher path matchers_json in
        let questions = parse_list "question" parse_question path questions_json in
        Some (Query {language; matchers; questions})
    | Some _ ->
        L.die InternalError "bug in parse_assoc"
    | None ->
        None )
  | _ ->
      L.user_error "@[<v>%s: Flow query should be a json object: {...}@;@]" path ;
      ignoring path json ;
      None


module Topl = struct
  let mk_arguments arity =
    let f arguments index = Printf.sprintf "Arg%d" index :: arguments in
    IContainer.forto_right arity ~init:["Ret"] ~f


  let mk_pattern (language : Language.t) pattern =
    match language with
    | Erlang ->
        (* for erlang, we assume [pattern] looks like "module:function" *)
        ToplAst.CallPattern {procedure_name_regex= ToplAst.mk_regex pattern; type_regexes= None}
    | Clang | CIL | Hack | Python | Java ->
        L.die InternalError "Unsupported language for data flow queries"


  let mk_vname arguments position =
    (* TODO: validate, so these don't throw *)
    match position with Return -> List.last_exn arguments | Argument n -> List.nth_exn arguments n


  let mk_eq_predicate (language : Language.t) arguments (Eq {left; right}) =
    let open IOption.Let_syntax in
    let+ left, right =
      match (language, left, right) with
      | _, Position left, Position right ->
          let b value = ToplAst.Binding (mk_vname arguments value) in
          Some (b left, b right)
      | Erlang, Position position, Constant (`List [`String "atom"; `String atom])
      | Erlang, Constant (`List [`String "atom"; `String atom]), Position position ->
          let left =
            let value = ToplAst.Binding (mk_vname arguments position) in
            let class_name = ErlangTypeName.to_string Atom in
            ToplAst.FieldAccess {value; class_name; field_name= ErlangTypeName.atom_hash}
          in
          let right = ToplAst.Constant (LiteralInt (ErlangTypeName.calculate_hash atom)) in
          Some (left, right)
      | Erlang, Position _position, Constant (`List [`String "string"; `String s])
      | Erlang, Constant (`List [`String "string"; `String s]), Position _position ->
          (* TODO: add translation *)
          L.internal_error "@[<v>For now, ignoring side-condition using string constant '%s'@;@]" s ;
          None
      | _ ->
          L.die InternalError "unsupported flow query predicate"
    in
    ToplAst.Binop (OpEq, left, right)


  let mk_side_condition language arguments eq_condition_list =
    List.filter_map ~f:(mk_eq_predicate language arguments) eq_condition_list


  let start_state = ToplAutomaton.start_name

  let error_state = ToplAutomaton.error_name

  let leaked_state = "leaked"

  let tracking_state = "tracking"

  let tracked_register = "tracked"

  let mk_save arguments position = [(tracked_register, mk_vname arguments position)]

  let mk_check arguments position =
    [ToplAst.Binop (LeadsTo, Binding (mk_vname arguments position), Register tracked_register)]


  let from_matcher language source sink (Matcher {tag; pattern; arity; position; condition}) :
      ToplAst.transition list =
    (*
       We build
        start --t1--> tracking --t2--> error
       and
        start --t3--> leaked --t4 --> error

       where
         t1 matches on a source pattern (and saves value seen at source)
         t2 matches on a sink pattern (and checks value seen at sink)
         t3 matches on a sink pattern (and saves value seen at sink)
         t4 matches on a source pattern (and checks the value seen at source)
       Transitions t3&t4 are unintuitive. They capture the situation in which something is leaked
       and we find out only after that it was sensitive information. This probably happens rarely.
     *)
    let mk_transitions stateA stateB =
      (* [mk_transitions "tracking" "leaked"] makes t1&t4*)
      (* [mk_transitions "leaked" "tracking"] makes t3&t2*)
      let arguments = mk_arguments arity in
      let pattern = mk_pattern language pattern in
      let condition = mk_side_condition language arguments condition in
      let action = mk_save arguments position in
      let label = Some {ToplAst.arguments= Some arguments; condition; action; pattern} in
      let transition_odd = {ToplAst.source= start_state; target= stateA; label} in
      let action = [] in
      let condition = mk_check arguments position @ condition in
      let label = Some {ToplAst.arguments= Some arguments; condition; action; pattern} in
      let transition_even = {ToplAst.source= stateB; target= error_state; label} in
      [transition_odd; transition_even]
    in
    if String.(tag = source) then mk_transitions tracking_state leaked_state
    else if String.(tag = sink) then mk_transitions leaked_state tracking_state
    else []


  let from_question path language matchers (Question {source; sink}) : ToplAst.t =
    let name = Filename.basename path in
    let message = Some (Printf.sprintf "Flow query from %s" path) in
    let transitions =
      {ToplAst.source= start_state; target= start_state; label= None}
      :: List.concat_map ~f:(from_matcher language source sink) matchers
    in
    {ToplAst.name; message; prefixes= []; transitions}
end

let convert_one_to_topl path =
  L.debug Analysis Medium "@[<v>DataFlowQuery: Parsing %s@;@]" path ;
  match Utils.read_json_file path with
  | Ok json -> (
    match parse_json path json with
    | Some (Query {language; matchers; questions}) ->
        List.map ~f:(Topl.from_question path language matchers) questions
    | None ->
        [] )
  | Error message ->
      L.user_error "@[<v>%s: Failed to parse json:@ @[%s@]@;@]" path message ;
      []


let convert_to_topl query_files =
  let result = List.concat_map ~f:convert_one_to_topl query_files in
  let debug topl =
    L.debug Analysis Medium "@[<v>Topl generated from FlowQuery@ %s@;@]" (ToplAst.show topl)
  in
  List.iter ~f:debug result ;
  result
