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
(* as found in asm-generic/termios.h *)
let winsize : winsize structure typ = structure "winsize"
let ws_row = field winsize "ws_row" ushort
let ws_col = field winsize "ws_col" ushort
let ws_xpixel = field winsize "ws_xpixel" ushort
let ws_ypixel = field winsize "ws_ypixel" ushort
let () = seal winsize

(* as found in asm-generic/ioctls.h *)
let request_TIOCGWINSZ = Unsigned.ULong.of_int 0x5413

(* ioctl(2) is a variadic function, so cross our fingers that the calling convention works the same
   as non-variadic functions and define different ioctl_* functions for each need *)

let ioctl_winsize =
  Foreign.foreign "ioctl"
    (int @-> ulong @-> ptr winsize @-> returning int)

(** high-level function *)
let terminal_width = lazy(
  let winsize = make winsize in
  let return = ioctl_winsize 0 request_TIOCGWINSZ (addr winsize) in
  if return >= 0 then
    Ok (Unsigned.UShort.to_int (getf winsize ws_col))
  else
    Error return
)
