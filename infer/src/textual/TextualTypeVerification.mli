(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open Textual

(** This module provides basic type verification for Textual programs.

    Each function is type-checked independently, starting with the declared types of globals,
    parameters and locals.

    The type of logical variables is infered by traversing the instructions in a suitable order
    (dfs). This may fail if a logical variable is assigned twice with two different types. *)

(*** Subtyping *)
(** Subtyping is quite limiting because we do no try to understand the class hierarchy. It can be
    specified by the following rules

    {[
      ----------------------------
             t <= t

      ----------------------------
           t <= Void
                                   ( Void acts as an escape hatch; we may make the
      ----------------------------   type system stricter later on)
            Void <= t

      ----------------------------
          Null <= *t

            t1 <= t2
      ----------------------------
           *t1 <= *t2

            t1 <= t2
      ---------------------------- (to please the Java type system but we may need
          t1[] <= t2[]              an othe decision when dealing with more langages)


      ---------------------------- (this is where the type system becomes weak)
      Struct name1 <= Struct name2
    ]}

    See function [compat ~assigned ~given] that implements [given <= assigned]. *)

(*** expression typing *)
(** We can compute a type for each well-typed expression, using

    - the type of program variable (given by G)
    - the type of logical variables (given L, and inferred during instruction typing)

    {[
      ------------------------------------------------
                     G, L |- Int _ : Int

      ------------------------------------------------
                   G, L |- Float _ : Float

      ------------------------------------------------ (this is the purpose of the type Null
                    G, L |- Null : Null                 we don't expect the programmer to write
                                                        this type himself )

      ------------------------------------------------
                    G, L |- Str _ : *String

      ------------------------------------------------
                   G, L |- Var id : L[id]

      ------------------------------------------------ ([Lvar] constructor)
                   G, L |- &var : *G[id]

              G, L |- exp : t t <= *(Struct name)
      ------------------------------------------------ (we can't  check if the field exists in
                G, L |- exp.f : *typeof(f)               name of or of it superclass)

          G, L |- exp1 : t1       t1 <= *t[]
                    G, L |- exp2 : Int
      ------------------------------------------------
                   G, L |- exp1[exp2] : *t

           proc has type [x1:t1'; ...; xn:tn'] -> t
           G, L |- exp1 : t1          t1 <= t1'
                       ...
           G, L |- expn : tn          tn <= tn'
      ------------------------------------------------ (we have special cases for builtins)
             G, L |- proc(exp1, ..., expn) : t

      ------------------------------------------------
           G, L |- Typ t : the_type_of_all_types
    ]}

    See function [typeof_exp]. *)

(*** instruction typing *)
(** The judgement is of the form G, L1 |- instr : L2 because we compute logical variable types in a
    flow sensitive way.

    {[
            G, L |- exp : t'       t' <= t
                  L[id] = t or undefined
      -----------------------------------------------
            G, L |- id: t = load exp : L[id=t]

           G, L |- exp1 : t1        *t <= t1
           G, L |- exp2 : t2        t2 <= t
      -----------------------------------------------
          G, L |- store exp1 <- exp2: t : L

                   G, L |- exp : Int
      -----------------------------------------------
               G, L |- Prune exp : L

                  G, L |- exp : t
      -----------------------------------------------
             G, L |- id = exp : L[id=t]
    ]} *)

(*** terminator typing *)
(** The judgement is of the form G, L, N, rt |- terminator with

    - rt the declared result type of the current function
    - and N the types of each node SSA parameters (if any).

    {[
                G, L |- exp : t          t <= rt
       -----------------------------------------------------
                      G, L, N, rt |- Ret exp

          lexp1 = exp1, ..., expk
              G, L |- exp1 : t1           t1 <= N[lab1][1]
                             ...
              G, L |- expk : tk           tk <= N[lab1][k]
                             ...
          lexpn = ...
                             ...
      ------------------------------------------------------
          G, L, rt, N |- Jump lab1(lexp1), ..., labn(lexpn)

                         G, L |- exp : *t
      -----------------------------------------------------
                  G, L, N, rt |- Throw exp

      -----------------------------------------------------
                  G, L, N, rt |- Unreachable
    ]} *)

type error

val pp_error : SourceFile.t -> Format.formatter -> error -> unit

(* typechecks all the module and, if no errors, returns a fresh copy where
   all load/store type annotations are filled now *)
val run : Module.t -> TextualDecls.t -> (Module.t, error list) Result.t

type type_check_result =
  | Ok of Module.t
  | Decl_errors of TextualDecls.error list
  | Type_errors of error list

(* Called from tests *)
val type_check : Module.t -> type_check_result [@@warning "-unused-value-declaration"]
