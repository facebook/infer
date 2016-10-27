/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
open! Utils;

let () = {
  let xx_suffix =
    if (string_is_suffix "++" Sys.argv.(0)) {
      "++"
    } else {
      try (Sys.getenv "INFER_XX") {
      | Not_found => ""
      }
    };
  let args = Array.copy Sys.argv;
  /* make sure we don't call ourselves recursively */
  args.(0) = CFrontend_config.clang_bin xx_suffix;
  ClangWrapper.exe args xx_suffix
};
