/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace boxed_ptr {

struct X {
  int field;
  int* getPtr() { return &field; }
  int* getNull() { return nullptr; }
};

struct SmartPtr {
  SmartPtr() : data(nullptr) {}
  X* data;
  X* get() { return data; }
};

void smart_ptr_null_field_deref() {
  SmartPtr p;
  int f = p.get()->field;
}

void smart_ptr_null_method_deref() {
  SmartPtr p;
  int* f = p.get()->getPtr();
}

void smart_ptr_null_method_deref2() {
  SmartPtr p;
  int* f = p.get()->getNull();
}

void smart_ptr_ok_field_deref() {
  SmartPtr p;
  X x;
  p.data = &x;
  int f = p.get()->field;
}

void smart_ptr_ok_method_deref() {
  SmartPtr p;
  X x;
  p.data = &x;
  int* f = p.get()->getNull();
  int* g = f;
}

void smart_ptr_result_method_null_deref() {
  SmartPtr p;
  X x;
  p.data = &x;
  int f = *(p.get()->getNull());
}

void smart_ptr_result_method_ok_deref() {
  SmartPtr p;
  X x;
  p.data = &x;
  int f = *(p.get()->getPtr());
}
} // namespace boxed_ptr
