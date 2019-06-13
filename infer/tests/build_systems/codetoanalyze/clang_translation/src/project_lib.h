/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once
namespace internal {
int fun(int a) { return a; }

// function shouldn't be translated if it's not used in source file
int unused(int a) { return a; }

// function should be translated because it's used in header corresponding
// to source file
int used_in_main_header(int a) { return a; }
} // namespace internal
