/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#pragma once

// Some funkiness in infer's models of smart pointers make it add extra
// instructions after calls to the constructors, which trips SIOF. This tests
// the custom SIOF logic that unhacks the hack.

#include <memory>

class InitWithConstexprStaticOK {
 public:
  InitWithConstexprStaticOK() {
    if (foo_smart_pointer_) {
    }
  }

 private:
  static std::unique_ptr<int> foo_smart_pointer_;
};
