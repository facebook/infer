// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace IntraFile;

class SensitiveClass {
  public function getDerived(): int {
    return 42;
  }
}

class HackMixed {
  public static function explicitSinkAllArgs(SensitiveClass $sc): void {}
}
