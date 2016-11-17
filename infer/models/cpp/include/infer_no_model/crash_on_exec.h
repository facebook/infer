/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#pragma once

#include <cstdlib>
#include <cstdio>

// NOTE this header shouldn't be translated by infer - it's here to disallow
// running binaries built with infer headers
namespace infer_model {
// code compiled with infer headers is not supposed to be executed
struct AbortWhenRun {
  AbortWhenRun() { __infer_skip__(); }
  // will be skipped by analyzer
  void __infer_skip__() {
    fprintf(stderr,
            "!!! This program must not be run !!!\n"
            "This code was compiled to be analyzed by Infer.\n"
            "To run this program, recompile it without Infer.\n");
    std::abort();
  }
};

static AbortWhenRun a{};
}
