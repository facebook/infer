/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

// This file exists only so that the SIOF checkers sees global_object
// being initialized via a method call. The SIOF checker could be
// improved to know that all non-POD types require initialization in
// C++.

struct SomeObject {
  void some_method();
};

SomeObject global_object;
