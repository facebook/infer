/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
namespace library {

namespace safewrapper {

struct Wrapper {
  Wrapper();
  ~Wrapper();
};

void wrapper();
} // namespace safewrapper

namespace details {
void low_level();

struct LowLevel {
  LowLevel();
  ~LowLevel();
};
} // namespace details
} // namespace library
