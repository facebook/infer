/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "siof_types.h"

extern SomeNonPODObject extern_global_object;
SomeNonPODObject global_object;
extern int access_to_non_pod();
void safe_streams();

struct SomeOtherNonPODObject {
  SomeOtherNonPODObject() {
    global_object.some_method(); // OK, same translation unit
    extern_global_object.some_method(); // bad, different translation unit
  };

  SomeOtherNonPODObject(int i) {
    global_object.some_method(); // OK, same translation unit
    safe_streams(); // OK, that function is SIOF safe
  };
};

SomeOtherNonPODObject another_global_object_bad;
SomeOtherNonPODObject another_global_object2_bad(access_to_non_pod());
SomeOtherNonPODObject another_global_object3_bad(access_to_templated_non_pod());
SomeOtherNonPODObject another_global_object4_good(42);

int pod_accesses_non_pod_bad = access_to_non_pod();

struct X {
  static int static_pod_accesses_non_pod_bad;
};

int X::static_pod_accesses_non_pod_bad = access_to_non_pod();

SomeNonPODObject initWithStatic_good = getFunctionStaticNonPOD();
SomeNonPODObject initWithGlobal_bad = getGlobalNonPOD();

SomeNonPODObject initWithGlobalWhitelisted_good = getGlobalNonPODWhitelisted();

SomeNonPODObject initWithGlobalWhitelistedNamespaced_good =
    whitelisted::getGlobalNonPOD();

SomeNonPODObject initWithGlobalWhitelistedTemplated_good =
    whitelisted::TemplatedObject<int>::getGlobalNonPOD();

extern SomeConstexprObject& getGlobalConstexpr();
SomeConstexprObject initWithConstexpr_good = getGlobalConstexpr();

extern SomeTemplatedConstexprObject<int>& getGlobalTemplatedConstexpr();
SomeTemplatedConstexprObject<int> initWithTemplatedConstexpr_good =
    getGlobalTemplatedConstexpr();
