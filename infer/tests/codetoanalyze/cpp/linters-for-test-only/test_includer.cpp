/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "test_included.h"

struct Bazowey {
  void frazzle() { Bazoo().fibble(); }
};

namespace Foo {
using ::rabble;
} // namespace Foo

void zowie() { Foo::rabble(); }
