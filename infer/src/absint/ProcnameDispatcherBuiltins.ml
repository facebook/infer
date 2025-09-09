(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module L = Logging

let expect_1_arg args builtin =
  match args with
  | [arg] ->
      arg
  | _ ->
      L.die InternalError "builtin %s was given %d arguments while 1 was expected" builtin
        (List.length args)


let expect_2_args args builtin =
  match args with
  | [arg1; arg2] ->
      (arg1, arg2)
  | _ ->
      L.die InternalError "builtin %s was given %d arguments while 2 were expected" builtin
        (List.length args)


let expect_3_args args builtin =
  match args with
  | [arg1; arg2; arg3] ->
      (arg1, arg2, arg3)
  | _ ->
      L.die InternalError "builtin %s was given %d arguments while 3 were expected" builtin
        (List.length args)


let expect_4_args args builtin =
  match args with
  | [arg1; arg2; arg3; arg4] ->
      (arg1, arg2, arg3, arg4)
  | _ ->
      L.die InternalError "builtin %s was given %d arguments while 4 were expected" builtin
        (List.length args)


let expect_5_args args builtin =
  match args with
  | [arg1; arg2; arg3; arg4; arg5] ->
      (arg1, arg2, arg3, arg4, arg5)
  | _ ->
      L.die InternalError "builtin %s was given %d arguments while 5 were expected" builtin
        (List.length args)


let expect_at_least_1_arg args builtin =
  match args with
  | arg1 :: args ->
      (arg1, args)
  | _ ->
      L.die InternalError "builtin %s was given %d arguments while at least 1 were expected" builtin
        (List.length args)


let expect_at_least_2_args args builtin =
  match args with
  | arg1 :: arg2 :: args ->
      (arg1, arg2, args)
  | _ ->
      L.die InternalError "builtin %s was given %d arguments while at least 2 were expected" builtin
        (List.length args)


let expect_at_least_3_args args builtin =
  match args with
  | arg1 :: arg2 :: arg3 :: args ->
      (arg1, arg2, arg3, args)
  | _ ->
      L.die InternalError "builtin %s was given %d arguments while at least 3 were expected" builtin
        (List.length args)
