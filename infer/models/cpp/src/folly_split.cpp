/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#include <vector>

namespace folly {

#define TYPEVAR(Name) \
  struct __attribute__((annotate("__infer_type_var"))) Name {};

TYPEVAR(a1);
TYPEVAR(a2);
TYPEVAR(a3);

template <class Delim, class String, class OutputType>
void split(const Delim& delimiter,
           const String& input,
           std::vector<OutputType>& out,
           bool ignoreEmpty = false) {
  out.resize(1);
  return;
}

template __attribute__((annotate("__infer_generic_model"))) void
split<a1, a2, a3>(const a1& delimiter,
                  const a2& input,
                  std::vector<a3>& out,
                  const bool ignoreEmpty);
}
