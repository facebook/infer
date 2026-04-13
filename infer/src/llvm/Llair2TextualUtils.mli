(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open! IStd

val demangle_swift_class_name : string -> string

val extract_type_from_mangled_proc : string -> string option
