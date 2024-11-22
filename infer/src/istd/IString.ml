(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module T = String
module Map = PrettyPrintable.MakeHashSexpPPMap (T)
module Set = PrettyPrintable.MakeHashSexpPPSet (T)
module Hash = Stdlib.Hashtbl.Make (T)
