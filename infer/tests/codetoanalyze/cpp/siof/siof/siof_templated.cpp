/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "siof_types.h"

extern SomeTemplatedNonPODObject<int> extern_global_object;

SomeTemplatedNonPODObject<int> global_template_object;

template <typename T>
struct SomeOtherTemplatedNonPODObject {
  SomeOtherTemplatedNonPODObject() {
    global_template_object.some_method(); // OK, same translation unit
    extern_global_object.some_method(); // bad, different translation unit
  }

  SomeOtherTemplatedNonPODObject(int i) {
    global_template_object.some_method(); // OK, same translation unit
  }

  SomeOtherTemplatedNonPODObject(int i, int j) {
    // OK: declared in another file but instantiated in this translation unit
    someTemplatedStatic<int>.some_method();
  }
};

SomeOtherTemplatedNonPODObject<bool> another_templated_global_object_bad;
SomeOtherTemplatedNonPODObject<bool> another_templated_global_object2_bad(
    access_to_non_pod());
SomeOtherTemplatedNonPODObject<bool> another_templated_global_object3_bad(
    access_to_templated_non_pod());
SomeOtherTemplatedNonPODObject<bool> another_templated_global_object4_good(42);
SomeOtherTemplatedNonPODObject<bool> access_variable_template_ok(32, 52);
