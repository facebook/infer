// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace DictIdxTests;

class Main {

  // Flow: the key is found, 1 on the left, 1 \/ 42 on the right
  public function idx_as_get_1_BAD(): void {
    $tainted = \Level1\taintSource();

    $w = dict['a' => 1];

    if ($w['a'] == idx($w, 'a', 42)) {
      \Level1\taintSink($tainted);
    }
  }

  // Flow: The taint sink is unreachable since $w['b'] should raise an exception.
  public function idx_as_get_2_OK_FP(): void {
    $tainted = \Level1\taintSource();

    $w = dict['a' => 1];

    if ($w['b'] == idx($w, 'b', 42)) {
      \Level1\taintSink($tainted);
    }
  }

  // Flow
  public function idx_defined_not_default_1_BAD(): void {
    $tainted = \Level1\taintSource();

    $w = dict['a' => 1];

    if (idx($w, 'a', 42) != 42) {
      \Level1\taintSink($tainted);
    }
  }

  // Flow: false positive because dict do not track the defined fields
  public function idx_defined_not_default_2_OK_FP(): void {
    $tainted = \Level1\taintSource();

    $w = dict['a' => 1];

    if (idx($w, 'a', 42) == 42) {
      \Level1\taintSink($tainted);
    }
  }

  // Flow
  public function idx_not_defined_1_BAD(): void {
    $tainted = \Level1\taintSource();

    $w = dict['a' => 1];

    if (idx($w, 'b', 42) == 42) {
      \Level1\taintSink($tainted);
    }
  }

  // Flow: false positive because dict do not track the defined fields
  public function idx_not_defined_2_OK_FP(): void {
    $tainted = \Level1\taintSource();

    $w = dict['a' => 1];

    if (idx($w, 'b', 42) == 37) {
      \Level1\taintSink($tainted);
    }
  }

  public function idx_get_BAD(string $key): void {
    $tainted = \Level1\taintSource();

    $w = dict['a' => 1];

    if (idx($w, $key, 42) == $w[$key]) {
      \Level1\taintSink($tainted);
    }
  }

}
