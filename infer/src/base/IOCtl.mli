(*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

(** bindings to ioctl(2) that only capture what we need *)


open! IStd
open Ctypes

type winsize
val winsize : winsize structure typ
val ws_col : (Unsigned.ushort, winsize structure) field
val ws_row : (Unsigned.ushort, winsize structure) field
val ws_xpixel : (Unsigned.ushort, winsize structure) field
val ws_ypixel : (Unsigned.ushort, winsize structure) field

module Types : functor (F : Cstubs.Types.TYPE) -> sig
  module Request : sig val request_TIOCGWINSZ : Unsigned.ulong F.const end
end

module Bindings : functor (F : Cstubs.FOREIGN) -> sig
  val ioctl_winsize :
    (int ->
     Unsigned.ulong -> winsize structure Ctypes_static.ptr -> int F.return)
      F.result
end
