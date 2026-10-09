/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>

namespace weak_from_this {

struct Session : std::enable_shared_from_this<Session> {
  int x = 0;
};

// weak_from_this() is not modelled: lock() on the weak_ptr it returns is
// assumed to be able to return an empty shared_ptr, even though the object is
// owned
int FP_weak_from_this_lock_owned_ok() {
  auto s = std::make_shared<Session>();
  return s->weak_from_this().lock()->x;
}

} // namespace weak_from_this
