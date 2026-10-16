/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>

void lock_after_owner_destroyed_bad() {
  std::weak_ptr<int> p;
  {
    auto s = std::make_shared<int>(0);
    p = s;
  }
  *p.lock() = 1;
}
