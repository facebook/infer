(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type t = FullNameOnly | NameOnly | Non_verbose | Simple | Verbose

val is_verbose : t -> bool
