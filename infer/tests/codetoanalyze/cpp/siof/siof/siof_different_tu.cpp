/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "siof_types.h"

SomeNonPODObject global_object2;
SomeNonPODObject some_other_global_object2;

int access_to_non_pod() {
  global_object2.some_method();
  // access several global objects to check that we group the reports together
  some_other_global_object2.some_method();
  return 5;
}

SomeTemplatedNonPODObject<int> global_object3;

int access_to_templated_non_pod() { return global_object3.some_method(); }

SomeNonPODObject& getFunctionStaticNonPOD() {
  static SomeNonPODObject instance;
  return instance;
}

SomeNonPODObject& getGlobalNonPOD() {
  some_other_global_object2.some_method();
  return global_object2;
}

SomeNonPODObject& getGlobalNonPODWhitelisted() {
  some_other_global_object2.some_method();
  return global_object2;
}

// initialise static class field
SomeConstexprObject SomeConstexprObject::instance_;

SomeConstexprObject& getGlobalConstexpr() {
  return SomeConstexprObject::singletonMethod();
}

namespace allow_listed {

SomeNonPODObject& getGlobalNonPOD() {
  some_other_global_object2.some_method();
  return global_object2;
}

template <typename T>
SomeNonPODObject& TemplatedObject<T>::getGlobalNonPOD() {
  some_other_global_object2.some_method();
  return global_object2;
}

// instantiate template so that infer analyses it
template struct TemplatedObject<int>;
} // namespace allow_listed

// initialize static class field
template <typename T>
SomeTemplatedConstexprObject<T> SomeTemplatedConstexprObject<T>::instance_;
SomeTemplatedConstexprObject<int>& getGlobalTemplatedConstexpr() {
  return SomeTemplatedConstexprObject<int>::singletonMethod();
}
