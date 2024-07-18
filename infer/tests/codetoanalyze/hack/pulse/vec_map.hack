// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace VecMap;

class Main {

  public function map_size_0_test_fst_bad(): void {
    $tainted = \Level1\taintSource();

    $vec1 = vec[];
    $f = $x ==> $x + 1;
    $vec2 = Vec\map($vec1, $f);
    if (idx($vec2, 0, 42) == 42) {
      \Level1\taintSink($tainted);
    }
  }

  public function map_size_0_test_fst_ok(): void {
    $tainted = \Level1\taintSource();

    $vec1 = vec[];
    $f = $x ==> $x + 1;
    $vec2 = Vec\map($vec1, $f);
    if (idx($vec2, 0, 42) != 42) {
      \Level1\taintSink($tainted);
    }
  }

  public function map_size_1_test_fst_bad(): void {
    $tainted = \Level1\taintSource();

    $vec1 = vec[1];
    $f = $x ==> $x + 1;
    $vec2 = Vec\map($vec1, $f);
    if ($vec2[0] == 2) {
      \Level1\taintSink($tainted);
    }
  }

  public function map_size_1_test_fst_ok(): void {
    $tainted = \Level1\taintSource();

    $vec1 = vec[1];
    $f = $x ==> $x + 1;
    $vec2 = Vec\map($vec1, $f);
    if ($vec2[0] != 2) {
      \Level1\taintSink($tainted);
    }
  }

  public function map_size_2_test_fst_bad(): void {
    $tainted = \Level1\taintSource();

    $vec1 = vec[1, 2];
    $f = $x ==> $x + 1;
    $vec2 = Vec\map($vec1, $f);
    if ($vec2[0] == 2 && $vec2[1] == 3) {
      \Level1\taintSink($tainted);
    }
  }

  public function map_size_2_test_fst_ok(): void {
    $tainted = \Level1\taintSource();

    $vec1 = vec[1, 2];
    $f = $x ==> $x + 1;
    $vec2 = Vec\map($vec1, $f);
    if ($vec2[0] == 4 || $vec2[1] == 4) {
      \Level1\taintSink($tainted);
    }
  }

  public function map_size_3_test_fst_bad(): void {
    $tainted = \Level1\taintSource();

    $vec1 = vec[1, 2, 3];
    $f = $x ==> $x + 1;
    $vec2 = Vec\map($vec1, $f);
    if ($vec2[0] == 2 && $vec2[1] == 3) {
      \Level1\taintSink($tainted);
    }
  }

  public function map_size_3_test_5_fst_ok(): void {
    $tainted = \Level1\taintSource();

    $vec1 = vec[1, 2, 3];
    $f = $x ==> $x + 1;
    $vec2 = Vec\map($vec1, $f);
    if ($vec2[2] == 5) {
      \Level1\taintSink($tainted);
    }
  }

  public function FP_map_size_3_test_2_fst_ok(): void {
    $tainted = \Level1\taintSource();

    $vec1 = vec[1, 2, 3];
    $f = $x ==> $x + 1;
    $vec2 = Vec\map($vec1, $f);
    if ($vec2[2] == 2) {
      \Level1\taintSink($tainted);
    }
  }

}
