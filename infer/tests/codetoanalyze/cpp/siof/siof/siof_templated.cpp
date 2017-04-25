/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include "siof_types.h"

extern SomeTemplatedNonPODObject<int> extern_global_object;

SomeTemplatedNonPODObject<int> global_template_object;

template <typename T>
struct SomeOtherTemplatedNonPODObject {
  SomeOtherTemplatedNonPODObject() {
    global_template_object.some_method(); // OK, same translation unit
    extern_global_object.some_method(); // bad, different translation unit
  };

  SomeOtherTemplatedNonPODObject(int i) {
    global_template_object.some_method(); // OK, same translation unit
  };
};

SomeOtherTemplatedNonPODObject<bool> another_templated_global_object; // SIOF!
SomeOtherTemplatedNonPODObject<bool> another_templated_global_object2(
    access_to_non_pod()); // SIOF!
SomeOtherTemplatedNonPODObject<bool> another_templated_global_object3(
    access_to_templated_non_pod()); // SIOF!
SomeOtherTemplatedNonPODObject<bool> another_templated_global_object4(42); // OK
