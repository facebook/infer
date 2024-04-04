(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** To be used in 'list_constraint *)
type accept_more

and end_of_list

(** To be used in 'emptyness *)
type empty

and non_empty

(* Type shorthands *)

type typ = Typ.t

type c = Procname.C.t

type csharp = Procname.CSharp.t

type objc_cpp = Procname.ObjC_Cpp.t

type erlang = Procname.Erlang.t

type hack = Procname.Hack.t

type java = Procname.Java.t

type qual_name = QualifiedCppName.t

type templated_name = qual_name * Typ.template_arg list

(* Typ helpers *)

let template_args_of_template_spec_info = function
  | Typ.NoTemplate ->
      []
  | Typ.Template {args} ->
      args


let templated_name_of_class_name class_name =
  let open Typ in
  match class_name with
  | CStruct qual_name | CUnion qual_name | ObjcProtocol qual_name | ObjcClass qual_name ->
      (qual_name, [])
  | CppClass {name= qual_name; template_spec_info} ->
      (qual_name, template_args_of_template_spec_info template_spec_info)
  | JavaClass mangled_name ->
      (QualifiedCppName.of_list [JavaClassName.to_string mangled_name], [])
  | CSharpClass mangled_name ->
      (QualifiedCppName.of_list [CSharpClassName.to_string mangled_name], [])
  | ErlangType mangled_name ->
      (QualifiedCppName.of_list [ErlangTypeName.to_string mangled_name], [])
  | HackClass mangled_name ->
      (QualifiedCppName.of_list (HackClassName.components mangled_name), [])
  | PythonClass mangled_name ->
      (QualifiedCppName.of_list (PythonClassName.components mangled_name), [])
  | ObjcBlock bsig ->
      (QualifiedCppName.of_list [bsig.name], [])
  | CFunction csig ->
      (csig.c_name, [])


let templated_name_of_hack hack =
  Option.map (Procname.Hack.get_class_name_as_a_string hack) ~f:(fun class_name ->
      let qual_name = QualifiedCppName.of_list [class_name; hack.Procname.Hack.function_name] in
      (qual_name, []) )


let templated_name_of_java java =
  let qual_name =
    QualifiedCppName.of_list [Procname.Java.get_class_name java; Procname.Java.get_method java]
  in
  (qual_name, [])


let templated_name_of_csharp csharp =
  let qual_name =
    QualifiedCppName.of_list
      [Procname.CSharp.get_class_name csharp; Procname.CSharp.get_method csharp]
  in
  (qual_name, [])


let templated_name_of_erlang (erlang : erlang) =
  let qual_name = QualifiedCppName.of_list [erlang.module_name; erlang.function_name] in
  (qual_name, [])


(* Intermediate matcher types *)

type ('context, 'f_in, 'f_out, 'arg_payload) name_matcher =
  { on_objc_cpp: 'context -> 'f_in -> objc_cpp -> 'f_out option
  ; on_qual_name: 'context -> 'f_in -> qual_name -> 'f_out option }

type ('f_in, 'f_out, 'list_constraint) template_arg =
  {eat_template_arg: 'f_in * Typ.template_arg list -> ('f_out * Typ.template_arg list) option}

type ('context, 'f_in, 'f_out, 'list_constraint, 'arg_payload) templ_matcher =
  { on_objc_cpp: 'context -> 'f_in -> objc_cpp -> ('f_out * Typ.template_arg list) option
  ; on_templated_name:
      'context -> 'f_in -> templated_name -> ('f_out * Typ.template_arg list) option }

type ('context, 'f_in, 'f_out, 'emptyness) path_extra =
  | PathEmpty : ('context, 'f, 'f, empty) path_extra
  | PathNonEmpty :
      {on_objc_cpp: 'context -> 'f_in -> objc_cpp -> 'f_out option}
      -> ('context, 'f_in, 'f_out, non_empty) path_extra

type ('context, 'f_in, 'f_out, 'emptyness, 'arg_payload) path_matcher =
  { on_templated_name: 'context -> 'f_in -> templated_name -> 'f_out option
  ; path_extra: ('context, 'f_in, 'f_out, 'emptyness) path_extra }

type typ_matcher = typ -> bool

(* Combinators *)

let empty : ('context, 'f, 'f, empty, 'arg_payload) path_matcher =
  let on_templated_name _context f (qual_name, template_args) =
    match (QualifiedCppName.extract_last qual_name, template_args) with
    | None, [] ->
        Some f
    | None, _ ->
        assert false
    | Some _, _ ->
        None
  in
  {on_templated_name; path_extra= PathEmpty}


let name_cons :
       ('context, 'f_in, 'f_out, _, 'arg_payload) path_matcher
    -> string
    -> ('context, 'f_in, 'f_out, 'arg_payload) name_matcher =
 fun m name ->
  let {on_templated_name} = m in
  let match_fuzzy_name =
    let fuzzy_name_regexp = name |> Str.quote |> Printf.sprintf "^%s\\(<.+>\\)?$" |> Str.regexp in
    fun s -> Str.string_match fuzzy_name_regexp s 0
  in
  let on_qual_name context f qual_name =
    match QualifiedCppName.extract_last qual_name with
    | Some (last, rest) when match_fuzzy_name last ->
        on_templated_name context f (rest, [])
    | _ ->
        None
  in
  let on_objc_cpp context f (objc_cpp : Procname.ObjC_Cpp.t) =
    if match_fuzzy_name objc_cpp.method_name then
      on_templated_name context f (templated_name_of_class_name objc_cpp.class_name)
    else None
  in
  {on_objc_cpp; on_qual_name}


let name_cons_f :
       ('context, 'f_in, 'f_out, _, 'arg_payload) path_matcher
    -> ('context -> string -> bool)
    -> ('context, 'f_in, 'f_out, 'arg_payload) name_matcher =
 fun m f_name ->
  let {on_templated_name} = m in
  let on_qual_name context f qual_name =
    match QualifiedCppName.extract_last qual_name with
    | Some (last, rest) when f_name context last ->
        on_templated_name context f (rest, [])
    | _ ->
        None
  in
  let on_objc_cpp context f (objc_cpp : Procname.ObjC_Cpp.t) =
    if f_name context objc_cpp.method_name then
      on_templated_name context f (templated_name_of_class_name objc_cpp.class_name)
    else None
  in
  {on_objc_cpp; on_qual_name}


let all_names_cons :
       ('context, 'f_in, 'f_out, non_empty, 'arg_payload) path_matcher
    -> ('context, 'f_in, 'f_out, non_empty, 'arg_payload) path_matcher =
 fun m ->
  let {on_templated_name; path_extra= PathNonEmpty {on_objc_cpp}} = m in
  let rec on_templated_name_rec context f templated_name =
    match on_templated_name context f templated_name with
    | Some _ as some ->
        some
    | None -> (
        let qual_name, _template_args = templated_name in
        match QualifiedCppName.extract_last qual_name with
        | None ->
            None
        | Some (_last, rest) ->
            on_templated_name_rec context f (rest, []) )
  in
  let on_templated_name = on_templated_name_rec in
  let on_objc_cpp context f (objc_cpp : Procname.ObjC_Cpp.t) =
    match on_objc_cpp context f objc_cpp with
    | Some _ as some ->
        some
    | None ->
        on_templated_name context f (templated_name_of_class_name objc_cpp.class_name)
  in
  {on_templated_name; path_extra= PathNonEmpty {on_objc_cpp}}


let templ_begin :
       ('context, 'f_in, 'f_out, 'arg_payload) name_matcher
    -> ('context, 'f_in, 'f_out, accept_more, 'arg_payload) templ_matcher =
 fun m ->
  let {on_objc_cpp; on_qual_name} = m in
  let on_templated_name context f (qual_name, template_args) =
    match on_qual_name context f qual_name with None -> None | Some f -> Some (f, template_args)
  in
  let on_objc_cpp context f (objc_cpp : Procname.ObjC_Cpp.t) =
    match on_objc_cpp context f objc_cpp with
    | None ->
        None
    | Some f ->
        let template_args = template_args_of_template_spec_info objc_cpp.template_args in
        Some (f, template_args)
  in
  {on_objc_cpp; on_templated_name}


let templ_cons :
       ('context, 'f_in, 'f_interm, accept_more, 'arg_payload) templ_matcher
    -> ('f_interm, 'f_out, 'lc) template_arg
    -> ('context, 'f_in, 'f_out, 'lc, 'arg_payload) templ_matcher =
 fun m template_arg ->
  let {on_objc_cpp; on_templated_name} = m in
  let {eat_template_arg} = template_arg in
  let on_templated_name context f templated_name =
    on_templated_name context f templated_name |> Option.bind ~f:eat_template_arg
  in
  let on_objc_cpp context f objc_cpp =
    on_objc_cpp context f objc_cpp |> Option.bind ~f:eat_template_arg
  in
  {on_objc_cpp; on_templated_name}


let templ_end :
       ('context, 'f_in, 'f_out, _, 'arg_payload) templ_matcher
    -> ('context, 'f_in, 'f_out, non_empty, 'arg_payload) path_matcher =
  let match_empty_templ_args (f, template_args) =
    match template_args with [] -> Some f | _ -> None
  in
  fun m ->
    let {on_objc_cpp; on_templated_name} = m in
    let on_templated_name context f templated_name =
      on_templated_name context f templated_name |> Option.bind ~f:match_empty_templ_args
    in
    let on_objc_cpp context f objc_cpp =
      on_objc_cpp context f objc_cpp |> Option.bind ~f:match_empty_templ_args
    in
    {on_templated_name; path_extra= PathNonEmpty {on_objc_cpp}}


module type Common = sig
  type ('context, 'f, 'arg_payload) matcher

  type ('context, 'f, 'arg_payload) dispatcher

  val make_dispatcher :
    ('context, 'f, 'arg_payload) matcher list -> ('context, 'f, 'arg_payload) dispatcher

  val map_matcher :
    ('context, 'f1, 'arg_payload) matcher -> f:('f1 -> 'f2) -> ('context, 'f2, 'arg_payload) matcher

  (* Template arguments *)

  val any_typ : ('f, 'f, accept_more) template_arg
  (** Eats a type *)

  val capt_typ : (Typ.t -> 'f, 'f, accept_more) template_arg
  (** Captures a type *)

  val capt_int : (Int64.t -> 'f, 'f, accept_more) template_arg
  (** Captures an int *)

  val capt_all : (Typ.template_arg list -> 'f, 'f, end_of_list) template_arg
  (** Captures all template args *)

  val ( ~- ) : string -> ('context, 'f, 'f, 'arg_payload) name_matcher
  (** Starts a path with a name *)

  val ( ~+ ) : ('context -> string -> bool) -> ('context, 'f, 'f, 'arg_payload) name_matcher
  (** Starts a path with a matching name that satisfies the given function *)

  val startsWith : string -> 'context -> string -> bool

  val endsWith : string -> 'context -> string -> bool

  val ( &+ ) :
       ('context, 'f_in, 'f_interm, accept_more, 'arg_payload) templ_matcher
    -> ('f_interm, 'f_out, 'lc) template_arg
    -> ('context, 'f_in, 'f_out, 'lc, 'arg_payload) templ_matcher
  (** Separate template arguments *)

  val ( < ) :
       ('context, 'f_in, 'f_interm, 'arg_payload) name_matcher
    -> ('f_interm, 'f_out, 'lc) template_arg
    -> ('context, 'f_in, 'f_out, 'lc, 'arg_payload) templ_matcher
  (** Starts template arguments after a name *)

  val ( >:: ) :
       ('context, 'f_in, 'f_out, _, 'arg_payload) templ_matcher
    -> string
    -> ('context, 'f_in, 'f_out, 'arg_payload) name_matcher
  (** Ends template arguments and starts a name *)

  val ( >::+ ) :
       ('context, 'f_in, 'f_out, 'lc, 'arg_payload) templ_matcher
    -> ('context -> string -> bool)
    -> ('context, 'f_in, 'f_out, 'arg_payload) name_matcher

  val ( &+...>:: ) :
       ('context, 'f_in, 'f_out, accept_more, 'arg_payload) templ_matcher
    -> string
    -> ('context, 'f_in, 'f_out, 'arg_payload) name_matcher
  (** Ends template arguments with eats-ALL and starts a name *)

  val ( &:: ) :
       ('context, 'f_in, 'f_out, 'arg_payload) name_matcher
    -> string
    -> ('context, 'f_in, 'f_out, 'arg_payload) name_matcher
  (** Separates names (accepts ALL template arguments on the left one) *)

  val ( &::+ ) :
       ('context, 'f_in, 'f_out, 'arg_payload) name_matcher
    -> ('context -> string -> bool)
    -> ('context, 'f_in, 'f_out, 'arg_payload) name_matcher

  val ( <>:: ) :
       ('context, 'f_in, 'f_out, 'arg_payload) name_matcher
    -> string
    -> ('context, 'f_in, 'f_out, 'arg_payload) name_matcher
  (** Separates names (accepts NO template arguments on the left one) *)
end

module Common = struct
  (* Template arguments *)

  (** Eats all template args *)
  let any_template_args : ('f, 'f, end_of_list) template_arg =
    let eat_template_arg (f, _) = Some (f, []) in
    {eat_template_arg}


  (** Eats a type *)
  let any_typ : ('f, 'f, accept_more) template_arg =
    let eat_template_arg (f, template_args) =
      match template_args with Typ.TType _ :: rest -> Some (f, rest) | _ -> None
    in
    {eat_template_arg}


  (** Captures a type *)
  let capt_typ : (Typ.t -> 'f, 'f, accept_more) template_arg =
    let eat_template_arg (f, template_args) =
      match template_args with Typ.TType ty :: rest -> Some (f ty, rest) | _ -> None
    in
    {eat_template_arg}


  (** Captures an int *)
  let capt_int : (Int64.t -> 'f, 'f, accept_more) template_arg =
    let eat_template_arg (f, template_args) =
      match template_args with Typ.TInt i :: rest -> Some (f i, rest) | _ -> None
    in
    {eat_template_arg}


  (** Captures all template args *)
  let capt_all : (Typ.template_arg list -> 'f, 'f, end_of_list) template_arg =
    let eat_template_arg (f, template_args) = Some (f template_args, []) in
    {eat_template_arg}


  let ( <! ) name_matcher () = templ_begin name_matcher

  let ( >! ) templ_matcher () = templ_end templ_matcher

  let ( &::! ) path_matcher name = name_cons path_matcher name

  let ( &::+! ) path_matcher f_name = name_cons_f path_matcher f_name

  let ( &::.*! ) path_matcher () = all_names_cons path_matcher

  let ( ~- ) name = empty &::! name

  let ( ~+ ) f_name = name_cons_f empty f_name

  let startsWith prefix _ s = String.is_prefix ~prefix s

  let endsWith suffix _ s = String.is_suffix ~suffix s

  let ( &+ ) templ_matcher template_arg = templ_cons templ_matcher template_arg

  let ( &+...>! ) templ_matcher () = templ_matcher &+ any_template_args >! ()

  let ( <...>! ) name_matcher () = name_matcher <! () &+...>! ()

  let ( < ) name_matcher template_arg = name_matcher <! () &+ template_arg

  let ( >:: ) templ_matcher name = templ_matcher >! () &::! name

  let ( >::+ ) templ_matcher f_name = templ_matcher >! () &::+! f_name

  let ( &+...>:: ) templ_matcher name = templ_matcher &+...>! () &::! name

  let ( &:: ) name_matcher name = name_matcher <...>! () &::! name

  let ( &::+ ) name_matcher f_name = name_matcher <...>! () &::+! f_name

  let ( <>:: ) name_matcher name = name_matcher <! () >:: name
end

module Call = struct
  include Common

  (** Little abstraction over arguments: currently actual args, we'll want formal args later *)
  module FuncArg = struct
    type 'arg_payload t = {exp: Exp.t; typ: Typ.t; arg_payload: 'arg_payload}

    let typ {typ} = typ

    let exp {exp} = exp

    let arg_payload {arg_payload} = arg_payload

    let is_var {exp} = match exp with Var _ -> true | _ -> false

    let map_payload ~f ({arg_payload} as func_arg) = {func_arg with arg_payload= f arg_payload}

    let get_var_exn {exp; typ} =
      match exp with
      | Exp.Var v ->
          v
      | e ->
          Logging.(die InternalError) "Expected Lvar, got %a:%a" Exp.pp e (Typ.pp Pp.text) typ
  end

  type ('context, 'f_in, 'f_out) proc_matcher =
    { on_objc_cpp: 'context -> 'f_in -> objc_cpp -> 'f_out option
    ; on_c: 'context -> 'f_in -> c -> 'f_out option
    ; on_hack: 'context -> 'f_in -> hack -> 'f_out option
    ; on_java: 'context -> 'f_in -> java -> 'f_out option
    ; on_erlang: 'context -> 'f_in -> erlang -> 'f_out option
    ; on_csharp: 'context -> 'f_in -> csharp -> 'f_out option }

  type ('context, 'f_in, 'f_out, 'arg_payload) on_args =
    'context -> 'f_in * 'arg_payload FuncArg.t list -> ('f_out * 'arg_payload FuncArg.t list) option

  type ('context, 'f_in, 'f_proc_out, 'f_out, 'arg_payload) args_matcher =
    { on_proc: ('context, 'f_in, 'f_proc_out) proc_matcher
    ; on_args: ('context, 'f_proc_out, 'f_out, 'arg_payload) on_args }

  type ('context, 'arg_payload) one_arg_matcher =
    {match_arg: 'context -> 'arg_payload FuncArg.t -> bool}

  type ('arg_in, 'arg_out, 'f_in, 'f_out, 'arg_payload) arg_capture =
    {get_captured_value: 'arg_payload FuncArg.t -> 'arg_in; do_capture: 'f_in -> 'arg_out -> 'f_out}

  type ('context, 'arg_in, 'arg_out, 'f_in, 'f_out, 'arg_payload) one_arg =
    { one_arg_matcher: ('context, 'arg_payload) one_arg_matcher
    ; capture: ('arg_in, 'arg_out, 'f_in, 'f_out, 'arg_payload) arg_capture }

  type ('arg_in, 'arg_out, 'f_in, 'f_out, 'arg_payload) arg_preparer =
    { on_empty:
        ('f_in -> 'arg_out -> 'f_out) -> 'f_in -> ('f_out * 'arg_payload FuncArg.t list) option
    ; wrapper: 'arg_in -> 'arg_out }

  type ('context, 'f_in, 'f_out, 'arg_payload) func_arg =
    {eat_func_arg: ('context, 'f_in, 'f_out, 'arg_payload) on_args}

  type ('context, 'f, 'arg_payload) matcher =
    { on_objc_cpp: 'context -> objc_cpp -> 'arg_payload FuncArg.t list -> 'f option
    ; on_c: 'context -> c -> 'arg_payload FuncArg.t list -> 'f option
    ; on_hack: 'context -> hack -> 'arg_payload FuncArg.t list -> 'f option
    ; on_java: 'context -> java -> 'arg_payload FuncArg.t list -> 'f option
    ; on_erlang: 'context -> erlang -> 'arg_payload FuncArg.t list -> 'f option
    ; on_csharp: 'context -> csharp -> 'arg_payload FuncArg.t list -> 'f option }

  let map_matcher matcher ~f : (_, _, _) matcher =
    let transform_for_lang lang_matcher ctx lang args =
      lang_matcher ctx lang args |> Option.map ~f
    in
    let ({on_objc_cpp; on_c; on_hack; on_java; on_erlang; on_csharp} : (_, _, _) matcher) =
      matcher
    in
    { on_objc_cpp= transform_for_lang on_objc_cpp
    ; on_c= transform_for_lang on_c
    ; on_hack= transform_for_lang on_hack
    ; on_java= transform_for_lang on_java
    ; on_erlang= transform_for_lang on_erlang
    ; on_csharp= transform_for_lang on_csharp }


  let contramap_arg_payload matcher ~f =
    let map_args args = List.map ~f:(FuncArg.map_payload ~f) args in
    let transform_for_lang lang_matcher ctx lang args = lang_matcher ctx lang (map_args args) in
    { on_objc_cpp= transform_for_lang matcher.on_objc_cpp
    ; on_c= transform_for_lang matcher.on_c
    ; on_hack= transform_for_lang matcher.on_hack
    ; on_java= transform_for_lang matcher.on_java
    ; on_erlang= transform_for_lang matcher.on_erlang
    ; on_csharp= transform_for_lang matcher.on_csharp }


  type ('context, 'f, 'arg_payload) pre_result =
    | DoesNotMatch
    | Matches of 'f
    | RetryWith of ('context, 'f, 'arg_payload) matcher

  let pre_bind_opt opt ~f = match opt with None -> DoesNotMatch | Some x -> f x

  let pre_map_opt opt ~f = match opt with None -> DoesNotMatch | Some x -> Matches (f x)

  type ('context, 'f_in, 'f_out, 'arg_payload) func_args_end =
       on_args:('context, 'f_in, 'f_out, 'arg_payload) on_args
    -> 'context
    -> 'arg_payload FuncArg.t list
    -> 'f_in
    -> ('context, 'f_out, 'arg_payload) pre_result

  type ('context, 'f_in, 'f_out, 'arg_payload) all_args_matcher =
    { on_objc_cpp:
           'context
        -> 'f_in
        -> objc_cpp
        -> 'arg_payload FuncArg.t list
        -> ('context, 'f_out, 'arg_payload) pre_result
    ; on_c:
           'context
        -> 'f_in
        -> c
        -> 'arg_payload FuncArg.t list
        -> ('context, 'f_out, 'arg_payload) pre_result
    ; on_hack:
           'context
        -> 'f_in
        -> hack
        -> 'arg_payload FuncArg.t list
        -> ('context, 'f_out, 'arg_payload) pre_result
    ; on_java:
           'context
        -> 'f_in
        -> java
        -> 'arg_payload FuncArg.t list
        -> ('context, 'f_out, 'arg_payload) pre_result
    ; on_erlang:
           'context
        -> 'f_in
        -> erlang
        -> 'arg_payload FuncArg.t list
        -> ('context, 'f_out, 'arg_payload) pre_result
    ; on_csharp:
           'context
        -> 'f_in
        -> csharp
        -> 'arg_payload FuncArg.t list
        -> ('context, 'f_out, 'arg_payload) pre_result }

  type ('context, 'f, 'arg_payload) dispatcher =
    'context -> Procname.t -> 'arg_payload FuncArg.t list -> 'f option

  let args_begin :
         ('context, 'f_in, 'f_out, non_empty, 'arg_payload) path_matcher
      -> ('context, 'f_in, 'f_out, 'f_out, 'arg_payload) args_matcher =
    let on_args _context f_args = Some f_args in
    fun m ->
      let {on_templated_name; path_extra= PathNonEmpty {on_objc_cpp}} = m in
      let on_c context f (c : c) =
        let template_args = template_args_of_template_spec_info c.c_template_args in
        on_templated_name context f (c.c_name, template_args)
      in
      let on_java context f (java : java) =
        on_templated_name context f (templated_name_of_java java)
      in
      let on_hack context f (hack : hack) =
        Option.value_map (templated_name_of_hack hack) ~default:None
          ~f:(on_templated_name context f)
      in
      let on_erlang context f (erlang : erlang) =
        on_templated_name context f (templated_name_of_erlang erlang)
      in
      let on_csharp context f (csharp : csharp) =
        on_templated_name context f (templated_name_of_csharp csharp)
      in
      let on_objc_cpp context f objc_cpp = on_objc_cpp context f objc_cpp in
      let on_proc : _ proc_matcher = {on_objc_cpp; on_c; on_hack; on_java; on_erlang; on_csharp} in
      {on_proc; on_args}


  let args_cons :
         ('context, 'f_in, 'f_proc_out, 'f_interm, 'arg_payload) args_matcher
      -> ('context, 'f_interm, 'f_out, 'arg_payload) func_arg
      -> ('context, 'f_in, 'f_proc_out, 'f_out, 'arg_payload) args_matcher =
   fun m func_arg ->
    let {on_proc; on_args} = m in
    let {eat_func_arg} = func_arg in
    let on_args context f_args = on_args context f_args |> Option.bind ~f:(eat_func_arg context) in
    {on_proc; on_args}


  let args_end :
         ('context, 'f_in, 'f_proc_out, 'f_out, 'arg_payload) args_matcher
      -> ('context, 'f_proc_out, 'f_out, 'arg_payload) func_args_end
      -> ('context, 'f_in, 'f_out, 'arg_payload) all_args_matcher =
   fun m func_args_end ->
    let {on_proc= {on_c; on_hack; on_java; on_erlang; on_csharp; on_objc_cpp}; on_args} = m in
    let on_c context f c args =
      on_c context f c |> pre_bind_opt ~f:(func_args_end ~on_args context args)
    in
    let on_hack context f hack args =
      on_hack context f hack |> pre_bind_opt ~f:(func_args_end ~on_args context args)
    in
    let on_java context f java args =
      on_java context f java |> pre_bind_opt ~f:(func_args_end ~on_args context args)
    in
    let on_erlang context f erlang args =
      on_erlang context f erlang |> pre_bind_opt ~f:(func_args_end ~on_args context args)
    in
    let on_csharp context f csharp args =
      on_csharp context f csharp |> pre_bind_opt ~f:(func_args_end ~on_args context args)
    in
    let on_objc_cpp context f objc_cpp args =
      on_objc_cpp context f objc_cpp |> pre_bind_opt ~f:(func_args_end ~on_args context args)
    in
    {on_c; on_hack; on_java; on_erlang; on_csharp; on_objc_cpp}


  let make_matcher :
         ('context, 'f_in, 'f_out, 'arg_payload) all_args_matcher
      -> 'f_in
      -> ('context, 'f_out, 'arg_payload) matcher =
   fun m f ->
    let ({on_c; on_hack; on_java; on_erlang; on_csharp; on_objc_cpp} : (_, _, _, _) all_args_matcher)
        =
      m
    in
    let on_objc_cpp context objc_cpp args =
      match on_objc_cpp context f objc_cpp args with
      | DoesNotMatch ->
          None
      | Matches res ->
          Some res
      | RetryWith {on_objc_cpp} ->
          on_objc_cpp context objc_cpp args
    in
    let on_c context c args =
      match on_c context f c args with
      | DoesNotMatch ->
          None
      | Matches res ->
          Some res
      | RetryWith {on_c} ->
          on_c context c args
    in
    let on_hack context hack args =
      match on_hack context f hack args with
      | DoesNotMatch ->
          None
      | Matches res ->
          Some res
      | RetryWith {on_hack} ->
          on_hack context hack args
    in
    let on_java context java args =
      match on_java context f java args with
      | DoesNotMatch ->
          None
      | Matches res ->
          Some res
      | RetryWith {on_java} ->
          on_java context java args
    in
    let on_erlang context erlang args =
      match on_erlang context f erlang args with
      | DoesNotMatch ->
          None
      | Matches res ->
          Some res
      | RetryWith {on_erlang} ->
          on_erlang context erlang args
    in
    let on_csharp context csharp args =
      match on_csharp context f csharp args with
      | DoesNotMatch ->
          None
      | Matches res ->
          Some res
      | RetryWith {on_csharp} ->
          on_csharp context csharp args
    in
    {on_objc_cpp; on_c; on_hack; on_java; on_erlang; on_csharp}


  (** Simple implementation of a dispatcher, could be optimized later *)
  let make_dispatcher :
      ('context, 'f, 'arg_payload) matcher list -> ('context, 'f, 'arg_payload) dispatcher =
   fun matchers ->
    let on_objc_cpp context objc_cpp args =
      List.find_map matchers ~f:(fun (matcher : _ matcher) ->
          matcher.on_objc_cpp context objc_cpp args )
    in
    let on_c context c args =
      List.find_map matchers ~f:(fun (matcher : _ matcher) -> matcher.on_c context c args)
    in
    let on_hack context hack args =
      List.find_map matchers ~f:(fun (matcher : _ matcher) -> matcher.on_hack context hack args)
    in
    let on_java context java args =
      List.find_map matchers ~f:(fun (matcher : _ matcher) -> matcher.on_java context java args)
    in
    let on_erlang context erlang args =
      List.find_map matchers ~f:(fun (matcher : _ matcher) -> matcher.on_erlang context erlang args)
    in
    let on_csharp context csharp args =
      List.find_map matchers ~f:(fun (matcher : _ matcher) -> matcher.on_csharp context csharp args)
    in
    fun context procname args ->
      let match_procname procname =
        match (procname : Procname.t) with
        | ObjC_Cpp objc_cpp ->
            on_objc_cpp context objc_cpp args
        | C c ->
            on_c context c args
        | Hack hack ->
            on_hack context hack args
        | Java java ->
            on_java context java args
        | Erlang erlang ->
            on_erlang context erlang args
        | CSharp csharp ->
            on_csharp context csharp args
        | _ ->
            None
      in
      match_procname procname


  let merge_dispatchers :
         ('context, 'f, 'arg_payload) dispatcher
      -> ('context, 'f, 'arg_payload) dispatcher
      -> ('context, 'f, 'arg_payload) dispatcher =
   fun dispatcher1 dispatcher2 context procname args ->
    match dispatcher1 context procname args with
    | Some _ as r ->
        r
    | None ->
        dispatcher2 context procname args


  (* Function args *)

  (** Matches any arg *)
  let match_any_arg : _ one_arg_matcher =
    let match_arg _context _arg = true in
    {match_arg}


  (** Matches var *)
  let match_var_arg : _ one_arg_matcher =
    let match_arg _context arg = FuncArg.is_var arg in
    {match_arg}


  (** Matches the type matched by the given path_matcher *)
  let match_typ :
         ('context, _, _, non_empty, 'arg_payload) path_matcher
      -> ('context, 'arg_payload) one_arg_matcher =
   fun m ->
    let ({on_templated_name} : (_, _, _, non_empty, 'arg_payload) path_matcher) = m in
    let rec match_typ context typ =
      match typ with
      | {Typ.desc= Tstruct name} ->
          name |> templated_name_of_class_name |> on_templated_name context () |> Option.is_some
      | {Typ.desc= Tptr (typ, _ptr_kind)} ->
          match_typ context typ
      | _ ->
          false
    in
    let match_arg context arg = match_typ context (FuncArg.typ arg) in
    {match_arg}


  (** Matches the type matched by any of the given path_matchers *)
  let match_typ_exists :
         ('context, _, _, non_empty, 'arg_payload) path_matcher list
      -> ('context, 'arg_payload) one_arg_matcher =
   fun matchers ->
    let matchers = List.map matchers ~f:(fun m -> (match_typ m).match_arg) in
    let match_arg context arg = List.exists matchers ~f:(fun matcher -> matcher context arg) in
    {match_arg}


  (** Matches the type matched by the given typ_matcher *)
  let match_prim_typ : typ_matcher -> _ one_arg_matcher =
   fun on_typ ->
    let match_arg _context arg = on_typ (FuncArg.typ arg) in
    {match_arg}


  (* Function argument capture *)

  (** Do not capture this argument *)
  let no_capture : (_, _, 'f, 'f, _) arg_capture =
    let get_captured_value _arg = () in
    let do_capture f _v = f in
    {get_captured_value; do_capture}


  (** Capture the argument *)
  let capture_arg :
      ('arg_payload FuncArg.t, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, 'arg_payload) arg_capture =
    let get_captured_value arg = arg in
    let do_capture f v = f v in
    {get_captured_value; do_capture}


  (** Capture the argument value *)
  let capture_arg_val :
      ('arg_payload, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, 'arg_payload) arg_capture =
    let get_captured_value arg = FuncArg.arg_payload arg in
    let do_capture f v = f v in
    {get_captured_value; do_capture}


  (** Capture the argument expression *)
  let capture_arg_exp : (Exp.t, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, _) arg_capture =
    let get_captured_value arg = FuncArg.exp arg in
    let do_capture f v = f v in
    {get_captured_value; do_capture}


  (** Capture the argument local var or fail *)
  let capture_arg_var_exn : (Ident.t, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, _) arg_capture =
    let get_captured_value arg = FuncArg.get_var_exn arg in
    let do_capture f v = f v in
    {get_captured_value; do_capture}


  let mandatory_arg =
    let on_empty _do_capture _f = None in
    let wrapper = Fn.id in
    {on_empty; wrapper}


  let optional_arg =
    let on_empty do_capture f = Some (do_capture f None, []) in
    let wrapper = Option.some in
    {on_empty; wrapper}


  let make_arg :
         ('arg_in, 'arg_out, 'f_in, 'f_out, 'arg_payload) arg_preparer
      -> ('context, 'arg_in, 'arg_out, 'f_in, 'f_out, 'arg_payload) one_arg
      -> ('context, 'f_in, 'f_out, 'arg_payload) func_arg =
   fun arg_preparer one_arg ->
    let {on_empty; wrapper} = arg_preparer in
    let {one_arg_matcher; capture} = one_arg in
    let {match_arg} = one_arg_matcher in
    let {get_captured_value; do_capture} = capture in
    let eat_func_arg context (f, args) =
      match args with
      | [] ->
          on_empty do_capture f
      | arg :: rest when match_arg context arg ->
          Some (arg |> get_captured_value |> wrapper |> do_capture f, rest)
      | _ ->
          None
    in
    {eat_func_arg}


  let any_arg : ('context, unit, _, 'f, 'f, 'arg_payload) one_arg =
    {one_arg_matcher= match_any_arg; capture= no_capture}


  let capt_arg :
      ('context, 'arg_payload FuncArg.t, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, 'arg_payload) one_arg
      =
    {one_arg_matcher= match_any_arg; capture= capture_arg}


  let all_args : ('context, 'arg_payload FuncArg.t list -> 'f_out, 'f_out, 'arg_payload) func_arg =
    let eat_func_arg _context (f, args) = Some (f args, []) in
    {eat_func_arg}


  let all_arg_payloads : ('context, 'arg_payload list -> 'f_out, 'f_out, 'arg_payload) func_arg =
    let eat_func_arg _context (f, args) = Some (f (List.map args ~f:FuncArg.arg_payload), []) in
    {eat_func_arg}


  let capt_arg_payload :
      ('context, 'arg_payload, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, 'arg_payload) one_arg =
    {one_arg_matcher= match_any_arg; capture= capture_arg_val}


  let capt_exp : ('context, Exp.t, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, 'arg_payload) one_arg =
    {one_arg_matcher= match_any_arg; capture= capture_arg_exp}


  let capt_var : ('context, Ident.t, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, 'arg_payload) one_arg =
    {one_arg_matcher= match_var_arg; capture= capture_arg_var_exn}


  let any_arg_of_typ m = {one_arg_matcher= match_typ (m <...>! ()); capture= no_capture}

  let any_arg_of_typ_exists m =
    {one_arg_matcher= match_typ_exists (List.map m ~f:(fun m -> m <...>! ())); capture= no_capture}


  let capt_arg_of_typ m = {one_arg_matcher= match_typ (m <...>! ()); capture= capture_arg}

  let capt_arg_of_typ_exists m =
    {one_arg_matcher= match_typ_exists (List.map m ~f:(fun m -> m <...>! ())); capture= capture_arg}


  let capt_arg_payload_of_typ m =
    {one_arg_matcher= match_typ (m <...>! ()); capture= capture_arg_val}


  let capt_arg_payload_of_typ_exists m =
    { one_arg_matcher= match_typ_exists (List.map m ~f:(fun m -> m <...>! ()))
    ; capture= capture_arg_val }


  let capt_exp_of_typ m = {one_arg_matcher= match_typ (m <...>! ()); capture= capture_arg_exp}

  let capt_exp_of_typ_exists m =
    { one_arg_matcher= match_typ_exists (List.map m ~f:(fun m -> m <...>! ()))
    ; capture= capture_arg_exp }


  let one_arg_matcher_of_prim_typ typ =
    let on_typ typ' = Typ.equal_ignore_quals typ typ' in
    match_prim_typ on_typ


  let any_arg_of_prim_typ typ =
    {one_arg_matcher= one_arg_matcher_of_prim_typ typ; capture= no_capture}


  let capt_arg_payload_of_prim_typ typ =
    {one_arg_matcher= one_arg_matcher_of_prim_typ typ; capture= capture_arg_val}


  let capt_exp_of_prim_typ typ =
    {one_arg_matcher= one_arg_matcher_of_prim_typ typ; capture= capture_arg_exp}


  (* Function args end *)

  (** Matches if there is no function arguments left *)
  let no_args_left : ('context, _, _, 'arg_payload) func_args_end =
    let match_empty_args = function Some (f, []) -> Matches f | _ -> DoesNotMatch in
    fun ~on_args context args f -> on_args context (f, args) |> match_empty_args


  (** Matches any function arguments *)
  let any_func_args : ('context, _, _, 'arg_payload) func_args_end =
   fun ~on_args context args f -> on_args context (f, args) |> pre_map_opt ~f:fst


  (** If [func_args_end1] does not match, use [func_args_end2] *)
  let alternative_args_end :
         ('context, 'f_in, 'f_out, 'arg_payload) func_args_end
      -> ('context, 'f_in, 'f_out, 'arg_payload) func_args_end
      -> ('context, 'f_in, 'f_out, 'arg_payload) func_args_end =
   fun func_args_end1 func_args_end2 ~on_args context args f_capt ->
    match func_args_end1 ~on_args context args f_capt with
    | DoesNotMatch ->
        func_args_end2 ~on_args context args f_capt
    | otherwise ->
        otherwise


  (** Retries matching with another matcher *)
  let args_end_retry : _ matcher -> ('context, _, _, 'arg_payload) func_args_end =
   fun m ~on_args:_ _context _args _f_capt -> RetryWith m


  (** Retries matching with another matcher if the function does not have the exact number/types of
      args *)
  let exact_args_or_retry :
      ('context, 'f, 'arg_payload) matcher -> ('context, _, _, 'arg_payload) func_args_end =
   fun m -> alternative_args_end no_args_left (args_end_retry m)


  let wrong_args_internal_error : _ matcher =
    let on_procname procname =
      Logging.(die InternalError) "Unexpected number/types of arguments for %a" Procname.pp procname
    in
    let on_c _context c _args = on_procname (C c) in
    let on_hack _context hack _args = on_procname (Hack hack) in
    let on_java _context java _args = on_procname (Java java) in
    let on_erlang _context erlang _args = on_procname (Erlang erlang) in
    let on_objc_cpp _context objc_cpp _args = on_procname (ObjC_Cpp objc_cpp) in
    let on_csharp _context csharp _args = on_procname (CSharp csharp) in
    {on_c; on_hack; on_java; on_erlang; on_csharp; on_objc_cpp}


  let ( $! ) path_matcher () = args_begin path_matcher

  let ( >$! ) templ_matcher () = templ_matcher >! () $! ()

  let ( $*--> ) all_args_matcher f = make_matcher all_args_matcher f

  let ( $+! ) args_matcher func_arg = args_cons args_matcher func_arg

  let ( $!! ) one_arg () = make_arg mandatory_arg one_arg

  let ( $?! ) one_arg () = make_arg optional_arg one_arg

  let ( $+ ) args_matcher one_arg = args_matcher $+! (one_arg $!! ())

  let ( $++ ) args_matcher () = args_matcher $+! all_args

  let ( $+++ ) args_matcher () = args_matcher $+! all_arg_payloads

  let ( $+? ) args_matcher one_arg = args_matcher $+! (one_arg $?! ())

  let ( >$ ) templ_matcher one_arg = templ_matcher >$! () $+ one_arg

  let ( $* ) args_matcher func_args_end = args_end args_matcher func_args_end

  let ( $--> ) args_matcher f = args_matcher $* no_args_left $*--> f

  let ( $ ) name_matcher func_arg = name_matcher <...>! () $! () $+ func_arg

  let ( <>$ ) name_matcher one_arg = name_matcher <! () >$ one_arg

  let ( $+...$--> ) args_matcher f = args_matcher $* any_func_args $*--> f

  let ( $++$--> ) args_matcher f = args_matcher $++ () $--> f

  let ( $+++$--> ) args_matcher f = args_matcher $+++ () $--> f

  let ( >--> ) templ_matcher f = templ_matcher >$! () $+...$--> f

  let ( >$$--> ) templ_matcher f = templ_matcher >$! () $--> f

  let ( $$--> ) name_matcher f = name_matcher <...>! () $! () $--> f

  let ( <>$$--> ) name_matcher f = name_matcher <! () >$$--> f

  let ( &--> ) name_matcher f = name_matcher <...>! () $! () $+...$--> f

  let ( &++> ) name_matcher f = name_matcher <...>! () $! () $++$--> f

  let ( <>--> ) name_matcher f = name_matcher <! () >--> f

  let ( &::.*--> ) name_matcher f = name_matcher <...>! () &::.*! () $! () $+...$--> f

  let ( &::.*++> ) name_matcher f = name_matcher <...>! () &::.*! () $! () $++$--> f

  let ( &::.*+++> ) name_matcher f = name_matcher <...>! () &::.*! () $! () $+++$--> f

  let ( $!--> ) args_matcher f =
    args_matcher $* exact_args_or_retry wrong_args_internal_error $*--> f
end

module type NameCommon = sig
  include Common

  val ( >--> ) :
       ('context, 'f_in, 'f_out, _, 'arg_payload) templ_matcher
    -> 'f_in
    -> ('context, 'f_out, 'arg_payload) matcher

  val ( &+...>--> ) :
       ('context, 'f_in, 'f_out, accept_more, 'arg_payload) templ_matcher
    -> 'f_in
    -> ('context, 'f_out, 'arg_payload) matcher

  val ( <>--> ) :
       ('context, 'f_in, 'f_out, 'arg_payload) name_matcher
    -> 'f_in
    -> ('context, 'f_out, 'arg_payload) matcher

  val ( &--> ) :
       ('context, 'f_in, 'f_out, 'arg_payload) name_matcher
    -> 'f_in
    -> ('context, 'f_out, 'arg_payload) matcher

  val ( &::.*--> ) :
       ('context, 'f_in, 'f_out, 'arg_payload) name_matcher
    -> 'f_in
    -> ('context, 'f_out, 'arg_payload) matcher
  (** After a name, accepts ALL template arguments, accepts ALL path tails (names, templates),
      accepts ALL function arguments, binds the function *)
end

module NameCommon = struct
  include Common

  type ('context, 'f, 'arg_payload) matcher =
    { on_templated_name: 'context -> templated_name -> 'f option
    ; on_objc_cpp: 'context -> objc_cpp -> 'f option }

  let make_matcher :
         ('context, 'f_in, 'f_out, non_empty, 'arg_payload) path_matcher
      -> 'f_in
      -> ('context, 'f_out, 'arg_payload) matcher =
   fun m f ->
    let ({on_templated_name; path_extra= PathNonEmpty {on_objc_cpp}}
          : ('context, 'f_in, 'f_out, non_empty, 'arg_payload) path_matcher ) =
      m
    in
    let on_templated_name context templated_name = templated_name |> on_templated_name context f in
    let on_objc_cpp context objc_cpp = objc_cpp |> on_objc_cpp context f in
    {on_templated_name; on_objc_cpp}


  let map_matcher matcher ~f : (_, _, _) matcher =
    let transform_for_lang lang_matcher ctx lang = lang_matcher ctx lang |> Option.map ~f in
    let ({on_objc_cpp; on_templated_name} : (_, _, _) matcher) = matcher in
    { on_objc_cpp= transform_for_lang on_objc_cpp
    ; on_templated_name= transform_for_lang on_templated_name }


  let ( &-->! ) path_matcher f = make_matcher path_matcher f

  let ( >--> ) templ_matcher f = templ_matcher >! () &-->! f

  let ( &+...>--> ) templ_matcher f = templ_matcher &+...>! () &-->! f

  let ( <>--> ) name_matcher f = name_matcher <! () >--> f

  let ( &--> ) name_matcher f = name_matcher <...>! () &-->! f

  let ( &::.*--> ) name_matcher f = name_matcher <...>! () &::.*! () &-->! f
end

module ProcName = struct
  include NameCommon

  type ('context, 'f, 'arg_payload) dispatcher = 'context -> Procname.t -> 'f option

  let make_dispatcher :
      ('context, 'f, 'arg_payload) matcher list -> ('context, 'f, 'arg_payload) dispatcher =
   fun matchers ->
    let on_objc_cpp context objc_cpp =
      List.find_map matchers ~f:(fun (matcher : _ matcher) -> matcher.on_objc_cpp context objc_cpp)
    in
    let on_templated_name context templated_name =
      List.find_map matchers ~f:(fun (matcher : _ matcher) ->
          matcher.on_templated_name context templated_name )
    in
    let on_erlang context (erlang : Procname.Erlang.t) =
      let templated_name = templated_name_of_erlang erlang in
      on_templated_name context templated_name
    in
    let on_hack context (hack : Procname.Hack.t) =
      Option.value_map (templated_name_of_hack hack) ~default:None ~f:(on_templated_name context)
    in
    let on_java context (java : Procname.Java.t) =
      let templated_name = templated_name_of_java java in
      on_templated_name context templated_name
    in
    let on_csharp context (csharp : Procname.CSharp.t) =
      let templated_name = templated_name_of_csharp csharp in
      on_templated_name context templated_name
    in
    let on_c context (c : c) =
      let template_args = template_args_of_template_spec_info c.c_template_args in
      let templated_name = (c.c_name, template_args) in
      on_templated_name context templated_name
    in
    fun context procname ->
      match procname with
      | ObjC_Cpp objc_cpp ->
          on_objc_cpp context objc_cpp
      | C c ->
          on_c context c
      | Erlang erlang ->
          on_erlang context erlang
      | Hack hack ->
          on_hack context hack
      | Java java ->
          on_java context java
      | CSharp cs ->
          on_csharp context cs
      | _ ->
          None
end

module TypName = struct
  include NameCommon

  type ('context, 'f, 'arg_payload) dispatcher = 'context -> Typ.name -> 'f option

  let make_dispatcher :
      ('context, 'f, 'arg_payload) matcher list -> ('context, 'f, 'arg_payload) dispatcher =
   fun matchers context typname ->
    let templated_name = templated_name_of_class_name typname in
    List.find_map matchers ~f:(fun (matcher : _ matcher) ->
        matcher.on_templated_name context templated_name )
end
