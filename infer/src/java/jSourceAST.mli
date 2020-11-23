(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type location = {line: int; col: int}

type class_kind = Class of string | Interface of string | AnonymousClass | Enum of string

type class_or_interface =
  {location: location; kind: class_kind; inner_elements: class_or_interface list}

type file_content = {package: string option; classes: class_or_interface list}

val iter_on_declarations :
  action_on_class_location:(classname:string -> col:int -> line:int -> unit) -> file_content -> unit
