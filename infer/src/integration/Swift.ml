(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

let capture ~command ~args =
  Logging.debug Capture Quiet "processed swiftc command is %s, args are %a@\n" command
    (Pp.comma_seq F.pp_print_string) args
