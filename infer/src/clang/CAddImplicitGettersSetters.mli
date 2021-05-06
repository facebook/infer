(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val process : Cfg.t -> Tenv.t -> unit
(** In Objective-C when properties are created in the interface of a class, the compiler creates
    automatically the instance variable for it and also the getter and setter in the implementation
    of the class. In the frontend we collect the information about which method is the implicit
    getter and setter of which instance variable (we get the method declaration but not the
    implementation), and here we add the implicit implementation. *)
