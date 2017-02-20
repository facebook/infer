/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include "siof_types.h"

extern SomeNonPODObject extern_global_object;
SomeNonPODObject global_object;
extern int access_to_non_pod();

struct SomeOtherNonPODObject {
  SomeOtherNonPODObject() {
    global_object.some_method(); // OK, same translation unit
    extern_global_object.some_method(); // bad, different translation unit
  };

  SomeOtherNonPODObject(int i) {
    global_object.some_method(); // OK, same translation unit
  };
};

SomeOtherNonPODObject another_global_object; // SIOF!
SomeOtherNonPODObject another_global_object2(access_to_non_pod()); // SIOF!
SomeOtherNonPODObject another_global_object3(
    access_to_templated_non_pod()); // SIOF!
SomeOtherNonPODObject another_global_object4(42); // OK

int pod_accesses_non_pod = access_to_non_pod(); // SIOF!

struct X {
  static int static_pod_accesses_non_pod;
};

int X::static_pod_accesses_non_pod = access_to_non_pod(); // SIOF!

SomeNonPODObject initWithStatic = getFunctionStaticNonPOD(); // OK
SomeNonPODObject initWithGlobal = getGlobalNonPOD(); // SIOF!

SomeNonPODObject initWithGlobalWhitelisted = getGlobalNonPODWhitelisted(); // OK

SomeNonPODObject initWithGlobalWhitelistedNamespaced =
    whitelisted::getGlobalNonPOD(); // OK

SomeNonPODObject initWithGlobalWhitelistedTemplated =
    whitelisted::TemplatedObject<int>::getGlobalNonPOD(); // OK

extern SomeConstexprObject& getGlobalConstexpr();
SomeConstexprObject initWithConstexpr = getGlobalConstexpr();

extern SomeTemplatedConstexprObject<int>& getGlobalTemplatedConstexpr();
SomeTemplatedConstexprObject<int> initWithTemplatedConstexpr =
    getGlobalTemplatedConstexpr();
