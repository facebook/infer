/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

struct SomeObject {
  void some_method();
};

extern SomeObject global_object;

struct SomeOtherObject {
  SomeOtherObject() { global_object.some_method(); };
};

// BAD: report SIOF here
SomeOtherObject another_global_object;
