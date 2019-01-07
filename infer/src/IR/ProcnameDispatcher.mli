(*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

(** To be used in 'list_constraint *)
type accept_more
 and end_of_list

(* Markers are a fool-proofing mechanism to avoid mistaking captured types.
  Template argument types can be captured with [capt_typ] to be referenced later
  by their position [typ1], [typ2], [typ3], ...
  To avoid mixing them, give a different name to each captured type, using whatever
  type/value you want and reuse it when referencing the captured type, e.g.
  [capt_typ `T &+ capt_typ `A], then use [typ1 `T], [typ2 `A].
  If you get them wrong, you will get a typing error at compile-time or an
  assertion failure at matcher-building time.
*)

type 'marker mtyp = Typ.t

(* Intermediate matcher types *)

type ('context, 'f_in, 'f_out, 'captured_types, 'markers_in, 'markers_out) name_matcher

type ( 'f_in
     , 'f_out
     , 'captured_types_in
     , 'captured_types_out
     , 'markers_in
     , 'markers_out
     , 'list_constraint ) template_arg

type ('context, 'f_in, 'f_out, 'captured_types, 'markers_in, 'markers_out, 'list_constraint) templ_matcher

(* A matcher is a rule associating a function [f] to a [C/C++ function/method]:
  - [C/C++ function/method] --> [f]

  The goal is to write the C/C++ function/method as naturally as possible, with the following
  exceptions:
    Start with -
    Use $ instead of parentheses for function arguments
    Use + instead of comma to separate function/template arguments
    Concatenate consecutive symbols (e.g. >:: instead of > ::)
    Operators must start with & $ < >

    E.g. std::vector<T, A>::vector(A) --> f becomes
    -"std" &:: "vector" < capt_typ T &+ capt_typ A >:: "vector" $ typ2 A $--> f
*)

module type Common = sig
  type ('context, 'f) matcher

  type ('context, 'f) dispatcher

  val make_dispatcher : ('context, 'f) matcher list -> ('context, 'f) dispatcher
  (** Combines matchers to create a dispatcher *)

  (* Template arguments *)

  val any_typ :
    ('f, 'f, 'captured_types, 'captured_types, 'markers, 'markers, accept_more) template_arg
  (** Eats a type *)

  val capt_typ :
       'marker
    -> ( 'marker mtyp -> 'f
       , 'f
       , 'captured_types
       , 'marker mtyp * 'captured_types
       , 'markers
       , 'marker * 'markers
       , accept_more )
       template_arg
  (** Captures a type than can be back-referenced *)

  val capt_int :
    ( Int64.t -> 'f
    , 'f
    , 'captured_types
    , 'captured_types
    , 'markers
    , 'markers
    , accept_more )
    template_arg
  (** Captures an int *)

  val capt_all :
    ( Typ.template_arg list -> 'f
    , 'f
    , 'captured_types
    , 'captured_types
    , 'markers
    , 'markers
    , end_of_list )
    template_arg
  (** Captures all template args *)

  val ( ~- ) : string -> ('context, 'f, 'f, unit, 'markers, 'markers) name_matcher
  (** Starts a path with a name *)

  val ( ~+ ) :
    ('context -> string -> bool) -> ('context, 'f, 'f, unit, 'markers, 'markers) name_matcher
  (** Starts a path with a matching name that satisfies the given function *)

  val ( &+ ) :
       ( 'context
       , 'f_in
       , 'f_interm
       , 'captured_types_in
       , 'markers_interm
       , 'markers_out
       , accept_more )
       templ_matcher
    -> ( 'f_interm
       , 'f_out
       , 'captured_types_in
       , 'captured_types_out
       , 'markers_in
       , 'markers_interm
       , 'lc )
       template_arg
    -> ('context, 'f_in, 'f_out, 'captured_types_out, 'markers_in, 'markers_out, 'lc) templ_matcher
  (** Separate template arguments *)

  val ( < ) :
       ('context, 'f_in, 'f_interm, 'captured_types_in, 'markers_interm, 'markers_out) name_matcher
    -> ( 'f_interm
       , 'f_out
       , 'captured_types_in
       , 'captured_types_out
       , 'markers_in
       , 'markers_interm
       , 'lc )
       template_arg
    -> ('context, 'f_in, 'f_out, 'captured_types_out, 'markers_in, 'markers_out, 'lc) templ_matcher
  (** Starts template arguments after a name *)

  val ( >:: ) :
       ('context, 'f_in, 'f_out, 'captured_types, 'markers_in, 'markers_out, _) templ_matcher
    -> string
    -> ('context, 'f_in, 'f_out, 'captured_types, 'markers_in, 'markers_out) name_matcher
  (** Ends template arguments and starts a name *)

  val ( &+...>:: ) :
       ( 'context
       , 'f_in
       , 'f_out
       , 'captured_types
       , 'markers_in
       , 'markers_out
       , accept_more )
       templ_matcher
    -> string
    -> ('context, 'f_in, 'f_out, 'captured_types, 'markers_in, 'markers_out) name_matcher
  (** Ends template arguments with eats-ALL and starts a name *)

  val ( &:: ) :
       ('context, 'f_in, 'f_out, 'captured_types, 'markers_in, 'markers_out) name_matcher
    -> string
    -> ('context, 'f_in, 'f_out, 'captured_types, 'markers_in, 'markers_out) name_matcher
  (** Separates names (accepts ALL template arguments on the left one) *)

  val ( &::+ ) :
       ('context, 'f_in, 'f_out, 'captured_types, 'markers_in, 'markers_out) name_matcher
    -> ('context -> string -> bool)
    -> ('context, 'f_in, 'f_out, 'captured_types, 'markers_in, 'markers_out) name_matcher
  (** Separates names that satisfies the given function (accepts ALL
     template arguments on the left one) *)

  val ( <>:: ) :
       ('context, 'f_in, 'f_out, 'captured_types, 'markers_in, 'markers_out) name_matcher
    -> string
    -> ('context, 'f_in, 'f_out, 'captured_types, 'markers_in, 'markers_out) name_matcher
  (** Separates names (accepts NO template arguments on the left one) *)
end

module type NameCommon = sig
  include Common

  val ( >--> ) :
       ('context, 'f_in, 'f_out, 'captured_types, unit, 'markers, _) templ_matcher
    -> 'f_in
    -> ('context, 'f_out) matcher

  val ( <>--> ) :
       ('context, 'f_in, 'f_out, 'captured_types, unit, 'markers) name_matcher
    -> 'f_in
    -> ('context, 'f_out) matcher

  val ( &--> ) :
       ('context, 'f_in, 'f_out, 'captured_types, unit, 'markers) name_matcher
    -> 'f_in
    -> ('context, 'f_out) matcher

  val ( &::.*--> ) :
       ('context, 'f_in, 'f_out, 'captured_types, unit, 'markers) name_matcher
    -> 'f_in
    -> ('context, 'f_out) matcher
  (** After a name, accepts ALL template arguments, accepts ALL path tails (names, templates),
        accepts ALL function arguments, binds the function *)
end

module ProcName :
  NameCommon with type ('context, 'f) dispatcher = 'context -> Typ.Procname.t -> 'f option

module TypName : NameCommon with type ('context, 'f) dispatcher = 'context -> Typ.name -> 'f option

module Call : sig
  (** Little abstraction over arguments: currently actual args, we'll want formal args later *)
  module FuncArg : sig
    type t = Exp.t * Typ.t
  end

  include
    Common
    with type ('context, 'f) dispatcher = 'context -> Typ.Procname.t -> FuncArg.t list -> 'f option

  type ('context, 'f_in, 'f_proc_out, 'f_out, 'captured_types, 'markers) args_matcher

  type ('context, 'arg_in, 'arg_out, 'f_in, 'f_out, 'captured_types, 'markers) one_arg

  (* Function args *)

  val any_arg : ('context, unit, _, 'f, 'f, _, _) one_arg
  (** Eats one arg *)

  val capt_arg : ('context, FuncArg.t, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, _, _) one_arg
  (** Captures one arg *)

  val capt_exp : ('context, Exp.t, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, _, _) one_arg
  (** Captures one arg expression *)

  val any_arg_of_typ :
    ('context, unit, _, unit, unit, unit) name_matcher -> ('context, unit, _, 'f, 'f, _, _) one_arg
  (** Eats one arg of the given type *)

  val capt_arg_of_typ :
       ('context, unit, _, unit, unit, unit) name_matcher
    -> ('context, FuncArg.t, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, _, _) one_arg
  (** Captures one arg of the given type *)

  val capt_exp_of_typ :
       ('context, unit, _, unit, unit, unit) name_matcher
    -> ('context, Exp.t, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, _, _) one_arg
  (** Captures one arg expression of the given type *)

  val any_arg_of_prim_typ : Typ.t -> ('context, unit, _, 'f, 'f, _, _) one_arg
  (** Eats one arg of the given primitive type *)

  val capt_exp_of_prim_typ :
    Typ.t -> ('context, Exp.t, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, _, _) one_arg
  (** Captures one arg expression of the given primitive type *)

  val capt_var_exn : ('context, Ident.t, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, _, _) one_arg
  (** Captures one arg Var. Fails with an internal error if the expression is not a Var *)

  val typ1 : 'marker -> ('context, unit, _, 'f, 'f, 'marker mtyp * _, 'marker * _) one_arg
  (** Matches first captured type *)

  val typ2 :
    'marker -> ('context, unit, _, 'f, 'f, _ * ('marker mtyp * _), _ * ('marker * _)) one_arg
  (** Matches second captured type *)

  val typ3 :
       'marker
    -> ('context, unit, _, 'f, 'f, _ * (_ * ('marker mtyp * _)), _ * (_ * ('marker * _))) one_arg
  (** Matches third captured type *)

  val ( $+ ) :
       ('context, 'f_in, 'f_proc_out, 'f_interm, 'captured_types, 'markers) args_matcher
    -> ('context, 'arg, 'arg, 'f_interm, 'f_out, 'captured_types, 'markers) one_arg
    -> ('context, 'f_in, 'f_proc_out, 'f_out, 'captured_types, 'markers) args_matcher
  (** Separate function arguments *)

  val ( $+? ) :
       ('context, 'f_in, 'f_proc_out, 'f_interm, 'captured_types, 'markers) args_matcher
    -> ('context, 'arg, 'arg option, 'f_interm, 'f_out, 'captured_types, 'markers) one_arg
    -> ('context, 'f_in, 'f_proc_out, 'f_out, 'captured_types, 'markers) args_matcher
  (** Add an optional argument *)

  val ( >$ ) :
       ('context, 'f_in, 'f_proc_out, 'ct, unit, 'cm, _) templ_matcher
    -> ('context, 'arg, 'arg, 'f_proc_out, 'f_out, 'ct, 'cm) one_arg
    -> ('context, 'f_in, 'f_proc_out, 'f_out, 'ct, 'cm) args_matcher
  (** Ends template arguments and starts function arguments *)

  val ( $--> ) :
       ('context, 'f_in, _, 'f_out, 'captured_types, 'markers) args_matcher
    -> 'f_in
    -> ('context, 'f_out) matcher
  (** Ends function arguments, binds the function *)

  val ( $ ) :
       ('context, 'f_in, 'f_proc_out, 'captured_types, unit, 'markers) name_matcher
    -> ('context, 'arg, 'arg, 'f_proc_out, 'f_out, 'captured_types, 'markers) one_arg
    -> ('context, 'f_in, 'f_proc_out, 'f_out, 'captured_types, 'markers) args_matcher
  (** Ends a name with accept-ALL template arguments and starts function arguments *)

  val ( <>$ ) :
       ('context, 'f_in, 'f_proc_out, 'captured_types, unit, 'markers) name_matcher
    -> ('context, 'arg, 'arg, 'f_proc_out, 'f_out, 'captured_types, 'markers) one_arg
    -> ('context, 'f_in, 'f_proc_out, 'f_out, 'captured_types, 'markers) args_matcher
  (** Ends a name with accept-NO template arguments and starts function arguments *)

  val ( >--> ) :
       ('context, 'f_in, 'f_out, 'captured_types, unit, 'markers, _) templ_matcher
    -> 'f_in
    -> ('context, 'f_out) matcher
  (** Ends template arguments, accepts ALL function arguments, binds the function *)

  val ( $+...$--> ) :
       ('context, 'f_in, _, 'f_out, 'captured_types, 'markers) args_matcher
    -> 'f_in
    -> ('context, 'f_out) matcher
  (** Ends function arguments with eats-ALL and binds the function *)

  val ( >$$--> ) :
       ('context, 'f_in, 'f_out, 'captured_types, unit, 'markers, _) templ_matcher
    -> 'f_in
    -> ('context, 'f_out) matcher
  (** Ends template arguments, accepts NO function arguments, binds the function *)

  val ( $$--> ) :
       ('context, 'f_in, 'f_out, 'captured_types, unit, 'markers) name_matcher
    -> 'f_in
    -> ('context, 'f_out) matcher
  (** After a name, accepts ALL template arguments, accepts NO function arguments, binds the function *)

  val ( <>$$--> ) :
       ('context, 'f_in, 'f_out, 'captured_types, unit, 'markers) name_matcher
    -> 'f_in
    -> ('context, 'f_out) matcher
  (** After a name, accepts NO template arguments, accepts NO function arguments, binds the function *)

  val ( &--> ) :
       ('context, 'f_in, 'f_out, 'captured_types, unit, 'markers) name_matcher
    -> 'f_in
    -> ('context, 'f_out) matcher
  (** After a name, accepts ALL template arguments, accepts ALL function arguments, binds the function *)

  val ( <>--> ) :
       ('context, 'f_in, 'f_out, 'captured_types, unit, 'markers) name_matcher
    -> 'f_in
    -> ('context, 'f_out) matcher
  (** After a name, accepts NO template arguments, accepts ALL function arguments, binds the function *)

  val ( &::.*--> ) :
       ('context, 'f_in, 'f_out, 'captured_types, unit, 'markers) name_matcher
    -> 'f_in
    -> ('context, 'f_out) matcher
  (** After a name, accepts ALL template arguments, accepts ALL path tails (names, templates),
      accepts ALL function arguments, binds the function *)

  val ( $!--> ) :
       ('context, 'f_in, 'f_proc_out, 'f_out, 'captured_types, 'markers) args_matcher
    -> 'f_in
    -> ('context, 'f_out) matcher
  (** Ends function arguments, accepts NO more function arguments.
    If the args do not match, raise an internal error.
 *)
end
[@@warning "-32"]
