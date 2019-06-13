/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

extern int __infer_taint_source();
extern void __infer_taint_sink(int);

namespace expressions {

void propagate_via_unop1_bad() {
  int source = __infer_taint_source();
  int laundered = ~source;
  __infer_taint_sink(laundered);
}

void propagate_via_unop2_bad() {
  int source = __infer_taint_source();
  __infer_taint_sink(~source);
}

void propagate_via_binop1_bad() {
  int source = __infer_taint_source();
  int laundered = 5 + source % 7;
  __infer_taint_sink(laundered);
}

void propagate_via_binop2_bad() {
  int source1 = __infer_taint_source();
  int source2 = __infer_taint_source();
  int laundered = 1 + source1 / 2 + source2 * 7;
  // should report twice
  __infer_taint_sink(laundered);
}

void propagate_via_binop3_bad() {
  int source = __infer_taint_source();
  __infer_taint_sink(source - 3);
}

void call_sink_nested(int formal) { __infer_taint_sink(formal); }

void propagate_via_binop_nested1_bad() {
  int source = ~(__infer_taint_source());
  call_sink_nested(source);
}

void propagate_via_binop_nested2_bad() {
  int source = __infer_taint_source();
  call_sink_nested(~source);
}

} // namespace expressions
