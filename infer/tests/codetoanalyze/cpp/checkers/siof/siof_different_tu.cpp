/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include "siof_types.h"

SomeNonPODObject global_object2;

int access_to_non_pod() {
  global_object2.some_method();
  return 5;
}

SomeTemplatedNonPODObject<int> global_object3;

int access_to_templated_non_pod() { return global_object3.some_method(); }

SomeNonPODObject& getFunctionStaticNonPOD() {
  static SomeNonPODObject instance;
  return instance;
}

SomeNonPODObject& getGlobalNonPOD() { return global_object2; }

// initialise static class field
SomeConstexprObject SomeConstexprObject::instance_;

SomeConstexprObject& getGlobalConstexpr() {
  return SomeConstexprObject::singletonMethod();
}

// initialise static class field
template <typename T>
SomeTemplatedConstexprObject<T> SomeTemplatedConstexprObject<T>::instance_;
SomeTemplatedConstexprObject<int>& getGlobalTemplatedConstexpr() {
  return SomeTemplatedConstexprObject<int>::singletonMethod();
}
