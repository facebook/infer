/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! IStd;


/** Internal representation of data structure for Java, Objective-C and C++ classes,
    C-style structs struct and union,
    And Objective C protocol */
type class_kind =
  | CPP
  | Java
  | Objc
[@@deriving compare];

let equal_class_kind: class_kind => class_kind => bool;

type t =
  | Class class_kind
  | Struct
  | Union
  | Protocol
[@@deriving compare];

let equal: t => t => bool;

let name: t => string;
