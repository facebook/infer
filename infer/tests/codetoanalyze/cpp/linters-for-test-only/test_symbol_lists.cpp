/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

extern void AllowedSymbol1();
extern void AllowedSymbol2();
extern void DisallowedSymbol();

void foo() {
  AllowedSymbol1();
  AllowedSymbol2();
  DisallowedSymbol();
}
