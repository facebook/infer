/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void throw1(){};

void no_throw() noexcept {};

int noexcept_in_no_throw_is_true() { return noexcept(no_throw()); }

int noexcept_in_throw1_is_false() { return noexcept(throw1()); }
