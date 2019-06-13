/*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// models for shared_ptr

extern "C" void __method_set_ignore_attribute(void** self, void* arg);

// standard constructor
extern "C" void __infer_shared_ptr(void** self, void* arg) {
  __method_set_ignore_attribute(self, arg); // auto memory management
  *self = arg;
}

// constructor taking a reference to a shared_ptr
extern "C" void __infer_shared_ptr_ref(void** arg1, void** arg2) {
  *arg1 = *arg2;
}

// operator=
extern "C" void** __infer_shared_ptr_eq(void** arg1, void** arg2) {
  *arg1 = *arg2;
  return arg1;
}

// operator==
extern "C" int __infer_shared_ptr_eqeq(void** arg1, void** arg2) {
  return (*arg1 == *arg2);
}

// operator->
extern "C" void* __infer_shared_ptr_arrow(void** arg) { return *arg; }

// destructor
extern "C" void __infer_shared_ptr_destructor(void** arg) {}
