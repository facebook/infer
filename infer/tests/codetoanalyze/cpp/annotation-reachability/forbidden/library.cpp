/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace library {

// low-level implementation, clients shouldn't use directly
namespace details {

struct LowLevel {
  LowLevel() {}
  ~LowLevel() {}
};

void low_level() {}
} // namespace details

// calls into this namespace from client code are allowed
namespace safewrapper {

struct Wrapper {
  details::LowLevel wrapped;

  Wrapper() : wrapped() {}
  ~Wrapper() {}
};

void wrapper() { details::low_level(); }

} // namespace safewrapper
} // namespace library
