(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
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

module Types (F: Cstubs.Types.TYPE) = struct
  module Request = struct
    let request_TIOCGWINSZ = F.constant "TIOCGWINSZ" F.ulong
  end
end

module Bindings (F : Cstubs.FOREIGN) = struct
  let (@->) = F.(@->) (* shadow Ctypes' operator *)

  (* ioctl(2) is a variadic function, so cross our fingers that the calling convention works the
     same as non-variadic functions and define different ioctl_* functions for each need *)

  let ioctl_winsize = F.foreign "ioctl" (int @-> ulong @-> ptr winsize @-> F.returning int)
end
