/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
static const int ConstantGlobal[] = {10};

void access_constant_constant() {
  for (int i = 0; i <= ConstantGlobal[0]; i++) {
  }
}
