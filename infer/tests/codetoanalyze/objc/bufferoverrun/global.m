/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
static const int ConstantGlobal[] = {8};

void access_constant_global_Bad() {
  int a[5];
  a[ConstantGlobal[0]] = 3;
}

int access_constant_global_via_assignment_Bad() {
  int a[5];
  const int* arr = ConstantGlobal;
  return a[arr[0]];
}
