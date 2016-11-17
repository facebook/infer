/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

struct SomeNonPODObject {
  virtual void some_method();
};

template<typename T>
struct SomeTemplatedNonPODObject {
  virtual T some_method();
};

int access_to_templated_non_pod();
int access_to_non_pod();
SomeNonPODObject& getFunctionStaticNonPOD();
SomeNonPODObject& getGlobalNonPOD();
