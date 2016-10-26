/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

// header required by TranslateAsPtr class. It's not in common header search
// path when running clang without infer (clang wrappers add it)
// Add -isystem '/path/to/infer/repo' to clang invocation to work around
// compilation problem
#include <infer/models/cpp/include/infer_model/infer_traits.h>

/* Test for passing function attributes to infer via __deprecated__ attribute */

// basic test of C function with __infer_replace_with_deref_first_arg attribute
int derefFirstArg(int* a, int* b) INFER_MODEL_AS_DEREF_FIRST_ARG;

// test directly with deprecated attribute
int derefFirstArg2(int* a, int* b)
    __attribute__((deprecated("__infer_replace_with_deref_first_arg"))) {
  /* equivalent in real code:
  return *a; */
  return *b; // body is in conflict with the attribute, attribute semantics
  // should be used
}

// test with wrong deprecated attribute
int derefFirstArg3(int* a, int* b) __attribute__((deprecated("__infer_typo"))) {
  /* equivalent in real code: */
  return *b; // there isn't any known attribute with this name, use semantics
  // from the body
}

int derefFirstArg_null_deref() {
  int a = 0;
  return derefFirstArg(nullptr, &a);
}

int derefFirstArg_ok_deref() {
  int a = 0;
  return derefFirstArg(&a, nullptr);
}

int derefFirstArg2_null_deref() {
  int a = 0;
  return derefFirstArg2(nullptr, &a);
}

int derefFirstArg2_ok_deref() {
  int a = 0;
  return derefFirstArg2(&a, nullptr);
}

int derefFirstArg3_ok_deref() {
  int a = 0;
  return derefFirstArg3(nullptr, &a);
}

int derefFirstArg3_null_deref() {
  int a = 0;
  return derefFirstArg3(&a, nullptr);
}

// more involving test of __infer_replace_with_deref_first_arg attribute in
// context of C++
// methods
/* This class be translated as T* by infer frontend - same as shared_ptr
   This class has different API in order to better test __deprecated__ attribute
   handling by the frontend. */
template <class T>
struct TranslateAsPtr {
  friend class infer_traits::TranslateAsType<T*>;
  TranslateAsPtr(T* t = nullptr) { setPtr(t); }
  /* calls to those functions are supposed to be translated as `*this` */
  T* getPtr() INFER_MODEL_AS_DEREF_FIRST_ARG;
  T* getPtr(int a, int b) INFER_MODEL_AS_DEREF_FIRST_ARG;
  /* calls to those functions are supposed to be translated as `**this` */
  T& operator*() INFER_MODEL_AS_DEREF_FIRST_ARG;
  T& getRef() INFER_MODEL_AS_DEREF_FIRST_ARG;
  T& getRef(int a, int b) INFER_MODEL_AS_DEREF_FIRST_ARG;

  // same trick we do for setting value of shared_ptr, look there for details
  void setPtr(T* v) { *((void**)(this)) = v; }
};

int getPtr_null_deref1() {
  TranslateAsPtr<int> t;
  t.setPtr(nullptr);
  return *t.getPtr();
}

int getPtr_null_deref2() {
  TranslateAsPtr<int> t;
  t.setPtr(nullptr);
  return *t.getPtr(1, 2);
}

int getPtr_ok_deref() {
  int a = 0;
  TranslateAsPtr<int> t;
  t.setPtr(&a);
  return *t.getPtr();
}

int operator_star_null_deref1() {
  TranslateAsPtr<int> t;
  t.setPtr(nullptr);
  return *t; // call operator* via operator call
}

int operator_star_null_deref2() {
  TranslateAsPtr<int> t;
  t.setPtr(nullptr);
  return t.operator*(); // call operator* via regular method call
}

int operator_star_ok_deref() {
  int a;
  TranslateAsPtr<int> t;
  t.setPtr(&a);
  return t.operator*(); // call operator* via regular method call
}

int getRef_null_deref1() {
  TranslateAsPtr<int> t;
  t.setPtr(nullptr);
  return t.getRef();
}

int getRef_null_deref2() {
  TranslateAsPtr<int> t;
  t.setPtr(nullptr);
  return t.getRef(1, 2);
}

int getRef_ok_deref() {
  int a = 0;
  TranslateAsPtr<int> t;
  t.setPtr(&a);
  return t.getRef();
}
