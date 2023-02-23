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

type context = {prefix: string; mutable counter: int}

let name_of_kind context = function
  | Class id | Interface id | Enum id ->
      id
  | AnonymousClass ->
      context.counter <- context.counter + 1 ;
      string_of_int context.counter


let rec iter ~action_on_class_location context {location; kind; inner_elements} =
  let previous_prefix = context.prefix in
  let name = name_of_kind context kind in
  let context = {prefix= Printf.sprintf "%s%s$" context.prefix name; counter= 0} in
  let classname = previous_prefix ^ name in
  let col = location.col in
  let line = location.line in
  action_on_class_location ~classname ~col ~line ;
  List.iter inner_elements ~f:(iter ~action_on_class_location context)


let iter_on_declarations ~action_on_class_location {package; classes} =
  let prefix = Option.fold ~init:"" ~f:(fun _ s -> s ^ ".") package in
  let context = {prefix; counter= 0} in
  List.iter classes ~f:(iter ~action_on_class_location context)
