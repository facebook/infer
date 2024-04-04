// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace GlobalAccess;

class Unknown {}

class VeryUnsafe {
  public function suspicious(): int {
    return 42;
  }
}

class Fine {
  public function suspicious(): int {
    return 42;
  }
}

class ContainsABadPatternInside {
  public function foo(): int {
    return 42;
  }
}
