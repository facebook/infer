/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
  mystruct* tainted = __infer_taint_source();
  mystruct source;
  source.str = tainted->str;
  source.i = 0;
  __infer_taint_sink(source);
}

void read_from_struct_source_field_bad_FN() {
  mystruct* source = __infer_taint_source();
  system(source->str);
}

} // namespace structs
