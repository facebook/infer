/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

extern void* __infer_taint_source();
extern void __infer_taint_sink(void*);

namespace basics {

class Obj {
 public:
  int field;
};

void* returnSource() { return __infer_taint_source(); }

void callSink(void* param) { __infer_taint_sink(param); }

void* id(void* param) { return param; }

void sourceToSinkDirectBad() {
  void* source = __infer_taint_source();
  __infer_taint_sink(source);
}

void returnSourceToSinkBad() {
  void* source = returnSource();
  __infer_taint_sink(source);
}

void sourceThenCallSinkBad() {
  void* source = __infer_taint_source();
  callSink(source);
}

void propagateBad() {
  void* source = __infer_taint_source();
  void* launderedSource = id(source);
  callSink(launderedSource);
}
}
