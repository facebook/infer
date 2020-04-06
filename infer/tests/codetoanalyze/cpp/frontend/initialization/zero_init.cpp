/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace zero_init {

struct X {
  // ridiculously large array's initialization should not be translated index by
  // index by the frontend
  static constexpr unsigned int kMaxKeys = 1UL << 16;
  void* keys_[kMaxKeys];
  constexpr X() : keys_() {}
};

struct Y {
  // this is small and can be translated index by index to save modelling burden
  // in analyses
  void* keys_[3];
  constexpr Y() : keys_() {}
};

} // namespace zero_init
