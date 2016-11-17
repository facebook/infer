/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#pragma once
namespace internal {
int fun(int a) { return a; }

// function shouldn't be translated if it's not used in source file
int unused(int a) { return a; }
}
