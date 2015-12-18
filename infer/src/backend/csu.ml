(*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Utils

(** Internal representation of data structure for Java, Objective-C and C++ classes,
    C-style structs struct and union,
    And Objective C protocol *)

type t =
  | Class
  | Struct
  | Union
  | Protocol

let name = function
  | Class -> "class"
  | Struct -> "struct"
  | Union -> "union"
  | Protocol -> "protocol"

let compare dstruct1 dstruct2 =
  match dstruct1, dstruct2 with
  | Class, Class -> 0
  | Class, _ -> -1
  | _, Class -> 1
  | Struct, Struct -> 0
  | Struct, _ -> -1
  | _, Struct -> 1
  | Union, Union -> 0
  | Union, _ -> -1
  | _, Union -> 1
  | Protocol, Protocol -> 0

let equal tn1 tn2 =
  compare tn1 tn2 = 0
