/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <memory>
#include <string>

struct mystruct {
  char* str;
  int i;
};

extern mystruct* __infer_taint_source();
extern void __infer_taint_sink(mystruct);

namespace structs {

void struct_source_bad() {
  mystruct* source = __infer_taint_source();
  __infer_taint_sink(*source);
}

void struct_field_source_unique_pointer_bad() {
  std::unique_ptr<mystruct> source(__infer_taint_source());
  __infer_taint_sink(*source);
}

void struct_field_source_bad() {
  mystruct source;
  source.str = std::getenv("var");
  __infer_taint_sink(source);
}

void read_from_struct_source_field_bad() {
  mystruct* source = __infer_taint_source();
  system(source->str);
}

} // namespace structs
