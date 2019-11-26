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

(* Intermediate matcher types *)

type ('context, 'f_in, 'f_out, 'value) name_matcher

type ('f_in, 'f_out_in_out, 'list_constraint) template_arg

type ('context, 'f_in, 'f_out, 'list_constraint, 'value) templ_matcher

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
  type ('context, 'f, 'value) matcher

  type ('context, 'f, 'value) dispatcher

  val make_dispatcher : ('context, 'f, 'value) matcher list -> ('context, 'f, 'value) dispatcher
  (** Combines matchers to create a dispatcher *)

  (* Template arguments *)

  val any_typ : ('f, 'f, accept_more) template_arg
  (** Eats a type *)

  val capt_typ : (Typ.t -> 'f, 'f, accept_more) template_arg
  (** Captures a type *)

  val capt_int : (Int64.t -> 'f, 'f, accept_more) template_arg
  (** Captures an int *)

  val capt_all : (Typ.template_arg list -> 'f, 'f, end_of_list) template_arg
  (** Captures all template args *)

  val ( ~- ) : string -> ('context, 'f, 'f, 'value) name_matcher
  (** Starts a path with a name *)

  val ( ~+ ) : ('context -> string -> bool) -> ('context, 'f, 'f, 'value) name_matcher
  (** Starts a path with a matching name that satisfies the given function *)

  val ( &+ ) :
       ('context, 'f_in, 'f_interm, accept_more, 'value) templ_matcher
    -> ('f_interm, 'f_out, 'lc) template_arg
    -> ('context, 'f_in, 'f_out, 'lc, 'value) templ_matcher
  (** Separate template arguments *)

  val ( < ) :
       ('context, 'f_in, 'f_interm, 'value) name_matcher
    -> ('f_interm, 'f_out, 'lc) template_arg
    -> ('context, 'f_in, 'f_out, 'lc, 'value) templ_matcher
  (** Starts template arguments after a name *)

  val ( >:: ) :
       ('context, 'f_in, 'f_out, _, 'value) templ_matcher
    -> string
    -> ('context, 'f_in, 'f_out, 'value) name_matcher
  (** Ends template arguments and starts a name *)

  val ( >::+ ) :
       ('context, 'f_in, 'f_out, 'lc, 'value) templ_matcher
    -> ('context -> string -> bool)
    -> ('context, 'f_in, 'f_out, 'value) name_matcher

  val ( &+...>:: ) :
       ('context, 'f_in, 'f_out, accept_more, 'value) templ_matcher
    -> string
    -> ('context, 'f_in, 'f_out, 'value) name_matcher
  (** Ends template arguments with eats-ALL and starts a name *)

  val ( &:: ) :
       ('context, 'f_in, 'f_out, 'value) name_matcher
    -> string
    -> ('context, 'f_in, 'f_out, 'value) name_matcher
  (** Separates names (accepts ALL template arguments on the left one) *)

  val ( &::+ ) :
       ('context, 'f_in, 'f_out, 'value) name_matcher
    -> ('context -> string -> bool)
    -> ('context, 'f_in, 'f_out, 'value) name_matcher
  (** Separates names that satisfies the given function (accepts ALL
     template arguments on the left one) *)

  val ( <>:: ) :
       ('context, 'f_in, 'f_out, 'value) name_matcher
    -> string
    -> ('context, 'f_in, 'f_out, 'value) name_matcher
  (** Separates names (accepts NO template arguments on the left one) *)
end

module type NameCommon = sig
  include Common

  val ( >--> ) :
       ('context, 'f_in, 'f_out, _, 'value) templ_matcher
    -> 'f_in
    -> ('context, 'f_out, 'value) matcher

  val ( <>--> ) :
    ('context, 'f_in, 'f_out, 'value) name_matcher -> 'f_in -> ('context, 'f_out, 'value) matcher

  val ( &--> ) :
    ('context, 'f_in, 'f_out, 'value) name_matcher -> 'f_in -> ('context, 'f_out, 'value) matcher

  val ( &::.*--> ) :
    ('context, 'f_in, 'f_out, 'value) name_matcher -> 'f_in -> ('context, 'f_out, 'value) matcher

  (** After a name, accepts ALL template arguments, accepts ALL path tails (names, templates),
        accepts ALL function arguments, binds the function *)
end

module ProcName :
  NameCommon with type ('context, 'f, 'value) dispatcher = 'context -> Typ.Procname.t -> 'f option

module TypName :
  NameCommon with type ('context, 'f, 'value) dispatcher = 'context -> Typ.name -> 'f option

module Call : sig
  (** Little abstraction over arguments: currently actual args, we'll want formal args later *)
  module FuncArg : sig
    type 'value t = {exp: Exp.t; typ: Typ.t; value: 'value}
  end

  include
    Common
      with type ('context, 'f, 'value) dispatcher =
            'context -> Typ.Procname.t -> 'value FuncArg.t list -> 'f option

  val merge_dispatchers :
       ('context, 'f, 'value) dispatcher
    -> ('context, 'f, 'value) dispatcher
    -> ('context, 'f, 'value) dispatcher
  (** Merges two dispatchers into a dispatcher *)

  type ('context, 'f_in, 'f_proc_out, 'f_out, 'value) args_matcher

  type ('context, 'arg_in, 'arg_out, 'f_in, 'f_out, 'value) one_arg

  (* Function args *)

  val any_arg : ('context, unit, _, 'f, 'f, 'value) one_arg
  (** Eats one arg *)

  val capt_arg : ('context, 'value FuncArg.t, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, 'value) one_arg
  (** Captures one arg *)

  val capt_value : ('context, 'value, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, 'value) one_arg
  (** Captures the value of one arg at current state  *)

  val capt_exp : ('context, Exp.t, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, 'value) one_arg
  (** Captures one arg expression *)

  val any_arg_of_typ :
    ('context, unit, _, 'value) name_matcher -> ('context, unit, _, 'f, 'f, 'value) one_arg
  (** Eats one arg of the given type *)

  val capt_arg_of_typ :
       ('context, unit, _, 'value) name_matcher
    -> ('context, 'value FuncArg.t, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, 'value) one_arg
  (** Captures one arg of the given type *)

  val capt_exp_of_typ :
       ('context, unit, _, 'value) name_matcher
    -> ('context, Exp.t, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, 'value) one_arg
  (** Captures one arg expression of the given type *)

  val any_arg_of_prim_typ : Typ.t -> ('context, unit, _, 'f, 'f, 'value) one_arg
  (** Eats one arg of the given primitive type *)

  val capt_exp_of_prim_typ :
    Typ.t -> ('context, Exp.t, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, 'value) one_arg
  (** Captures one arg expression of the given primitive type *)

  val capt_var_exn : ('context, Ident.t, 'wrapped_arg, 'wrapped_arg -> 'f, 'f, 'value) one_arg
  (** Captures one arg Var. Fails with an internal error if the expression is not a Var *)

  val ( $+ ) :
       ('context, 'f_in, 'f_proc_out, 'f_interm, 'value) args_matcher
    -> ('context, 'arg, 'arg, 'f_interm, 'f_out, 'value) one_arg
    -> ('context, 'f_in, 'f_proc_out, 'f_out, 'value) args_matcher
  (** Separate function arguments *)

  val ( $+? ) :
       ('context, 'f_in, 'f_proc_out, 'f_interm, 'value) args_matcher
    -> ('context, 'arg, 'arg option, 'f_interm, 'f_out, 'value) one_arg
    -> ('context, 'f_in, 'f_proc_out, 'f_out, 'value) args_matcher
  (** Add an optional argument *)

  val ( >$ ) :
       ('context, 'f_in, 'f_proc_out, 'ct, 'value) templ_matcher
    -> ('context, 'arg, 'arg, 'f_proc_out, 'f_out, 'value) one_arg
    -> ('context, 'f_in, 'f_proc_out, 'f_out, 'value) args_matcher
  (** Ends template arguments and starts function arguments *)

  val ( $--> ) :
    ('context, 'f_in, _, 'f_out, 'value) args_matcher -> 'f_in -> ('context, 'f_out, 'value) matcher
  (** Ends function arguments, binds the function *)

  val ( $ ) :
       ('context, 'f_in, 'f_proc_out, 'value) name_matcher
    -> ('context, 'arg, 'arg, 'f_proc_out, 'f_out, 'value) one_arg
    -> ('context, 'f_in, 'f_proc_out, 'f_out, 'value) args_matcher
  (** Ends a name with accept-ALL template arguments and starts function arguments *)

  val ( <>$ ) :
       ('context, 'f_in, 'f_proc_out, 'value) name_matcher
    -> ('context, 'arg, 'arg, 'f_proc_out, 'f_out, 'value) one_arg
    -> ('context, 'f_in, 'f_proc_out, 'f_out, 'value) args_matcher
  (** Ends a name with accept-NO template arguments and starts function arguments *)

  val ( >--> ) :
       ('context, 'f_in, 'f_out, _, 'value) templ_matcher
    -> 'f_in
    -> ('context, 'f_out, 'value) matcher
  (** Ends template arguments, accepts ALL function arguments, binds the function *)

  val ( $+...$--> ) :
    ('context, 'f_in, _, 'f_out, 'value) args_matcher -> 'f_in -> ('context, 'f_out, 'value) matcher
  (** Ends function arguments with eats-ALL and binds the function *)

  val ( >$$--> ) :
       ('context, 'f_in, 'f_out, _, 'value) templ_matcher
    -> 'f_in
    -> ('context, 'f_out, 'value) matcher
  (** Ends template arguments, accepts NO function arguments, binds the function *)

  val ( $$--> ) :
    ('context, 'f_in, 'f_out, 'value) name_matcher -> 'f_in -> ('context, 'f_out, 'value) matcher
  (** After a name, accepts ALL template arguments, accepts NO function arguments, binds the function *)

  val ( <>$$--> ) :
    ('context, 'f_in, 'f_out, 'value) name_matcher -> 'f_in -> ('context, 'f_out, 'value) matcher
  (** After a name, accepts NO template arguments, accepts NO function arguments, binds the function *)

  val ( &--> ) :
    ('context, 'f_in, 'f_out, 'value) name_matcher -> 'f_in -> ('context, 'f_out, 'value) matcher
  (** After a name, accepts ALL template arguments, accepts ALL function arguments, binds the function *)

  val ( <>--> ) :
    ('context, 'f_in, 'f_out, 'value) name_matcher -> 'f_in -> ('context, 'f_out, 'value) matcher
  (** After a name, accepts NO template arguments, accepts ALL function arguments, binds the function *)

  val ( &::.*--> ) :
    ('context, 'f_in, 'f_out, 'value) name_matcher -> 'f_in -> ('context, 'f_out, 'value) matcher
  (** After a name, accepts ALL template arguments, accepts ALL path tails (names, templates),
      accepts ALL function arguments, binds the function *)

  val ( $!--> ) :
       ('context, 'f_in, 'f_proc_out, 'f_out, 'value) args_matcher
    -> 'f_in
    -> ('context, 'f_out, 'value) matcher
  (** Ends function arguments, accepts NO more function arguments.
    If the args do not match, raise an internal error. *)
end
[@@warning "-32"]
