// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace VecIdxTests;

class Main {

  // Flow
  public function idx_found_1_BAD(): void {
    $tainted = \Level1\taintSource();

    $v = vec[1, 2, 3];

    if (idx($v, 0, 42) == 1) {
      \Level1\taintSink($tainted);
    }
  }

  // Flow
  public function idx_found_2_BAD(): void {
    $tainted = \Level1\taintSource();

    $v = vec[1, 2, 3];

    if (idx($v, 1, 42) == 2) {
      \Level1\taintSink($tainted);
    }
  }

  // Should be a flow, but vec is approximated with the first two values only
  public function idx_found_3_BAD_FN(): void {
    $tainted = \Level1\taintSource();

    $v = vec[1, 2, 3];

    if (idx($v, 2, 42) == 3) {
      \Level1\taintSink($tainted);
    }
  }

  // Flow
  public function idx_default_1_BAD(): void {
    $tainted = \Level1\taintSource();

    $v = vec[1, 2, 3];

    if (idx($v, 4) == null) {
      \Level1\taintSink($tainted);
    }
  }

  // Not a flow
  public function idx_default_2_OK(): void {
    $tainted = \Level1\taintSource();

    $v = vec[1, 2, 3];

    if (idx($v, 4) == 66) {
      \Level1\taintSink($tainted);
    }
  }

}
