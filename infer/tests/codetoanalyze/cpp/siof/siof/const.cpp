/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
int mult(int a, int b);

const int const_y = 32; // harmless global
const int const_x = 52 * const_y; // harmless
int constexpr z =
    const_x / const_y + 1; // user guarantees it is harmless with constexpr
int u = mult(32, 52); // potentially in need of initialization
