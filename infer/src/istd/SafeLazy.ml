(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

type 'a t =
  | Eager of 'a  (** avoid allocating a mutex with [from_val] *)
  | Lazy of {mutex: IMutex.t; v: 'a Lazy.t}

let make v = Lazy {mutex= IMutex.create (); v}

let force = function
  | Eager v ->
      v
  | Lazy t ->
      IMutex.critical_section t.mutex ~f:(fun () -> Lazy.force t.v)


let force_option v_opt = Option.map ~f:force v_opt

let from_val v = Eager v

let from_val_option v = Option.map ~f:from_val v

let freeze t = from_val @@ force t
