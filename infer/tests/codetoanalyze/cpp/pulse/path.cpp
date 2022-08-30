/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int* may_return_null(int x) {
  if (x == 42) {
    return nullptr;
  }
  return new int();
}

void FP_only_bad_on_42_latent(int x) {
  int* p = may_return_null(x);
  *p = 12;
  delete p;
}

void FN_faulty_call_bad() { FP_only_bad_on_42_latent(42); }

void call_not_with_42_ok() { FP_only_bad_on_42_latent(41); }
