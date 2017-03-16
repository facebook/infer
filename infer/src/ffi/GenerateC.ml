(*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)
let () =
  print_endline "#include <sys/ioctl.h>";
  Cstubs.write_c Format.std_formatter ~prefix:GenerateUtils.prefix (module IOCtl.Bindings)
