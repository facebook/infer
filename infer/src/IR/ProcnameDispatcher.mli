(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

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

type ('f_in, 'f_out, 'captured_types, 'markers_in, 'markers_out) name_matcher

type ( 'f_in
     , 'f_out
     , 'captured_types_in
     , 'captured_types_out
     , 'markers_in
     , 'markers_out
     , 'list_constraint ) template_arg

type ('f_in, 'f_out, 'captured_types, 'markers_in, 'markers_out, 'list_constraint) templ_matcher

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
  type 'f matcher

  type 'f dispatcher

  val make_dispatcher : 'f matcher list -> 'f dispatcher
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

  val ( ~- ) : string -> ('f, 'f, unit, 'markers, 'markers) name_matcher
  (** Starts a path with a name *)

  val ( &+ ) :
    ( 'f_in
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
    -> ('f_in, 'f_out, 'captured_types_out, 'markers_in, 'markers_out, 'lc) templ_matcher
  (** Separate template arguments *)

  val ( < ) :
    ('f_in, 'f_interm, 'captured_types_in, 'markers_interm, 'markers_out) name_matcher
    -> ( 'f_interm
       , 'f_out
       , 'captured_types_in
       , 'captured_types_out
       , 'markers_in
       , 'markers_interm
       , 'lc )
       template_arg
    -> ('f_in, 'f_out, 'captured_types_out, 'markers_in, 'markers_out, 'lc) templ_matcher
  (** Starts template arguments after a name *)

  val ( >:: ) :
    ('f_in, 'f_out, 'captured_types, 'markers_in, 'markers_out, _) templ_matcher -> string
    -> ('f_in, 'f_out, 'captured_types, 'markers_in, 'markers_out) name_matcher
  (** Ends template arguments and starts a name *)

  val ( &+...>:: ) :
    ('f_in, 'f_out, 'captured_types, 'markers_in, 'markers_out, accept_more) templ_matcher
    -> string -> ('f_in, 'f_out, 'captured_types, 'markers_in, 'markers_out) name_matcher
  (** Ends template arguments with eats-ALL and starts a name *)

  val ( &:: ) :
    ('f_in, 'f_out, 'captured_types, 'markers_in, 'markers_out) name_matcher -> string
    -> ('f_in, 'f_out, 'captured_types, 'markers_in, 'markers_out) name_matcher
  (** Separates names (accepts ALL template arguments on the left one) *)

  val ( <>:: ) :
    ('f_in, 'f_out, 'captured_types, 'markers_in, 'markers_out) name_matcher -> string
    -> ('f_in, 'f_out, 'captured_types, 'markers_in, 'markers_out) name_matcher
  (** Separates names (accepts NO template arguments on the left one) *)
end

module Call : sig
  (** Little abstraction over arguments: currently actual args, we'll want formal args later *)
  module FuncArg : sig
    type t = Exp.t * Typ.t
  end

  include Common with type 'f dispatcher = Typ.Procname.t -> FuncArg.t list -> 'f option

  type ('f_in, 'f_proc_out, 'f_out, 'captured_types, 'markers) args_matcher

  type ('arg_in, 'arg_out, 'f_in, 'f_out, 'captured_types, 'markers) one_arg

  (* Function args *)

  val any_arg : (unit, _, 'f, 'f, _, _) one_arg
  (** Eats one arg *)

  val capt_arg : (FuncArg.t, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, _, _) one_arg
  (** Captures one arg *)

  val capt_exp : (Exp.t, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, _, _) one_arg
  (** Captures one arg expression *)

  val capt_arg_of_typ :
    (unit, _, unit, unit, unit) name_matcher
    -> (FuncArg.t, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, _, _) one_arg
  (** Captures one arg of the given type *)

  val capt_exp_of_typ :
    (unit, _, unit, unit, unit) name_matcher
    -> (Exp.t, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, _, _) one_arg
  (** Captures one arg expression of the given type *)

  val typ1 : 'marker -> (unit, _, 'f, 'f, 'marker mtyp * _, 'marker * _) one_arg
  (** Matches first captured type *)

  val typ2 : 'marker -> (unit, _, 'f, 'f, _ * ('marker mtyp * _), _ * ('marker * _)) one_arg
  (** Matches second captured type *)

  val typ3 :
    'marker -> (unit, _, 'f, 'f, _ * (_ * ('marker mtyp * _)), _ * (_ * ('marker * _))) one_arg
  (** Matches third captured type *)

  val ( $+ ) :
    ('f_in, 'f_proc_out, 'f_interm, 'captured_types, 'markers) args_matcher
    -> ('arg, 'arg, 'f_interm, 'f_out, 'captured_types, 'markers) one_arg
    -> ('f_in, 'f_proc_out, 'f_out, 'captured_types, 'markers) args_matcher
  (** Separate function arguments *)

  val ( $+? ) :
    ('f_in, 'f_proc_out, 'f_interm, 'captured_types, 'markers) args_matcher
    -> ('arg, 'arg option, 'f_interm, 'f_out, 'captured_types, 'markers) one_arg
    -> ('f_in, 'f_proc_out, 'f_out, 'captured_types, 'markers) args_matcher
  (** Add an optional argument *)

  val ( >$ ) :
    ('f_in, 'f_proc_out, 'ct, unit, 'cm, _) templ_matcher
    -> ('arg, 'arg, 'f_proc_out, 'f_out, 'ct, 'cm) one_arg
    -> ('f_in, 'f_proc_out, 'f_out, 'ct, 'cm) args_matcher
  (** Ends template arguments and starts function arguments *)

  val ( $--> ) :
    ('f_in, _, 'f_out, 'captured_types, 'markers) args_matcher -> 'f_in -> 'f_out matcher
  (** Ends function arguments, binds the function *)

  val ( $ ) :
    ('f_in, 'f_proc_out, 'captured_types, unit, 'markers) name_matcher
    -> ('arg, 'arg, 'f_proc_out, 'f_out, 'captured_types, 'markers) one_arg
    -> ('f_in, 'f_proc_out, 'f_out, 'captured_types, 'markers) args_matcher
  (** Ends a name with accept-ALL template arguments and starts function arguments *)

  val ( <>$ ) :
    ('f_in, 'f_proc_out, 'captured_types, unit, 'markers) name_matcher
    -> ('arg, 'arg, 'f_proc_out, 'f_out, 'captured_types, 'markers) one_arg
    -> ('f_in, 'f_proc_out, 'f_out, 'captured_types, 'markers) args_matcher
  (** Ends a name with accept-NO template arguments and starts function arguments *)

  val ( >--> ) :
    ('f_in, 'f_out, 'captured_types, unit, 'markers, _) templ_matcher -> 'f_in -> 'f_out matcher
  (** Ends template arguments, accepts ALL function arguments, binds the function *)

  val ( $+...$--> ) :
    ('f_in, _, 'f_out, 'captured_types, 'markers) args_matcher -> 'f_in -> 'f_out matcher
  (** Ends function arguments with eats-ALL and binds the function *)

  val ( >$$--> ) :
    ('f_in, 'f_out, 'captured_types, unit, 'markers, _) templ_matcher -> 'f_in -> 'f_out matcher
  (** Ends template arguments, accepts NO function arguments, binds the function *)

  val ( $$--> ) :
    ('f_in, 'f_out, 'captured_types, unit, 'markers) name_matcher -> 'f_in -> 'f_out matcher
  (** After a name, accepts ALL template arguments, accepts NO function arguments, binds the function *)

  val ( <>$$--> ) :
    ('f_in, 'f_out, 'captured_types, unit, 'markers) name_matcher -> 'f_in -> 'f_out matcher
  (** After a name, accepts NO template arguments, accepts NO function arguments, binds the function *)

  val ( &--> ) :
    ('f_in, 'f_out, 'captured_types, unit, 'markers) name_matcher -> 'f_in -> 'f_out matcher
  (** After a name, accepts ALL template arguments, accepts ALL function arguments, binds the function *)

  val ( <>--> ) :
    ('f_in, 'f_out, 'captured_types, unit, 'markers) name_matcher -> 'f_in -> 'f_out matcher
  (** After a name, accepts NO template arguments, accepts ALL function arguments, binds the function *)

  val ( &::.*--> ) :
    ('f_in, 'f_out, 'captured_types, unit, 'markers) name_matcher -> 'f_in -> 'f_out matcher
  (** After a name, accepts ALL template arguments, accepts ALL path tails (names, templates),
      accepts ALL function arguments, binds the function *)

  val ( $!--> ) :
    ('f_in, 'f_proc_out, 'f_out, 'captured_types, 'markers) args_matcher -> 'f_in -> 'f_out matcher
  (** Ends function arguments, accepts NO more function arguments.
    If the args do not match, raise an internal error.
 *)
end
[@@warning "-32"]

module TypName : sig
  include Common with type 'f dispatcher = Typ.name -> 'f option

  val ( >--> ) :
    ('f_in, 'f_out, 'captured_types, unit, 'markers, _) templ_matcher -> 'f_in -> 'f_out matcher

  val ( <>--> ) :
    ('f_in, 'f_out, 'captured_types, unit, 'markers) name_matcher -> 'f_in -> 'f_out matcher

  val ( &--> ) :
    ('f_in, 'f_out, 'captured_types, unit, 'markers) name_matcher -> 'f_in -> 'f_out matcher

  val ( &::.*--> ) :
    ('f_in, 'f_out, 'captured_types, unit, 'markers) name_matcher -> 'f_in -> 'f_out matcher
  (** After a name, accepts ALL template arguments, accepts ALL path tails (names, templates),
        accepts ALL function arguments, binds the function *)
end
[@@warning "-32"]
