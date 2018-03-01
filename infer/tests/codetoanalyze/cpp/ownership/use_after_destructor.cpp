/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <iostream>
#include <memory>
#include <string>

struct S {
  int f;

  ~S() {}
};

// destructor called at end of function, no issues
void normal_scope_destructor_ok() { S s{1}; }

void nested_scope_destructor_ok() {
  { S s{1}; }
}

int reinit_after_explicit_destructor_ok() {
  S s{1};
  s.~S();
  s = S{2};
  return s.f;
}

void placement_new_explicit_destructor_ok() {
  char buf[sizeof(S)];
  {
    S* s = new (buf) S;
    s->~S();
  }
  {
    // this use of [buf] shouldn't be flagged
    S* s = new (buf) S;
    s->~S();
  }
}

void double_destructor_bad() {
  S s{1};
  s.~S();
  // destructor will be called again after S goes out of scope, which is
  // undefined behavior
}

int use_after_destructor_bad() {
  S s{1};
  s.~S();
  int ret = s.f;
  s = S{2};
  return ret;
}

void use_after_scope1_bad() {
  S s;
  {
    S tmp{1};
    s = tmp;
  } // destructor for tmp runs here
  // destructor for s here; second time the value held by s has been desructed
}

void FN_use_after_scope2_bad() {
  S s;
  {
    s = S{1};
  } // destructor runs here, but our frontend currently doesn't insert it
}
