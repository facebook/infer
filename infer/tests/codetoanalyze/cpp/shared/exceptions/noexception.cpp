/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

void throw1(){};

void no_throw() noexcept {};

int noexcept_in_no_throw_is_true() { return noexcept(no_throw()); }

int noexcept_in_throw1_is_false() { return noexcept(throw1()); }
