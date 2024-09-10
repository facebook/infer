// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Disjuncts;

class Branchs {

  public static function gen2(bool $b1): int {
    if ($b1) {
      return 1;
    } else {
      return 2;
    }
  }

  public static function gen4(bool $b1, bool $b2): int {
    // four branches
    if ($b1) {
      if ($b2) {
        return 1;
      } else {
        return 2;
      }
    } else {
      if ($b2) {
        return 3;
      } else {
        return 4;
      }
    }
  }

  public static function gen8(bool $b1, bool $b2, bool $b3, int $leaf): int {
    if ($b1) {
      if ($b2) {
        if ($b3) {
          return 1;
        } else {
          return 2;
        }
      } else {
        if ($b3) {
          return 3;
        } else {
          return 4;
        }
      }
    } else {
      if ($b2) {
        if ($b3) {
          return 5;
        } else {
          return 6;
        }
      } else {
        if ($b3) {
          return 7;
        } else {
          return $leaf; // 7th disjunct
        }
      }
    }
  }

}

class Flows {
  public function call_gen2_bad(bool $b1, bool $b2, bool $b3, bool $b4): void {
    if ($b4) {
      $v = \Level1\taintSource();
    } else {
      $v = 0;
    }
    $x = Branchs::gen2($b1);
    \Level1\taintSink($v);
  }

  public function call_gen4_bad(bool $b1, bool $b2, bool $b3, bool $b4): void {
    if ($b4) {
      $v = \Level1\taintSource();
    } else {
      $v = 0;
    }
    $x = Branchs::gen4($b1, $b2);
    \Level1\taintSink($v);
  }

  public function FN_call_gen8_bad(
    bool $b1,
    bool $b2,
    bool $b3,
    bool $b4,
  ): void {
    if ($b4) {
      $v = \Level1\taintSource();
    } else {
      $v = 0;
    }
    $x = Branchs::gen8($b1, $b2, $b3, 8);
    \Level1\taintSink($v);
  }

  public function call_gen8_many_unsat_cases_left_bad(
    bool $b1,
    bool $b2,
    bool $b3,
    bool $b4,
  ): void {
    if ($b4) {
      $v = \Level1\taintSource();
    } else {
      $v = 0;
      $b1 = true;
      $b2 = true;
      $b3 = true;
    }
    $v = Branchs::gen8($b1, $b2, $b3, $v);
    \Level1\taintSink($v);
  }

  public function call_gen8_many_unsat_cases_right_bad(
    bool $b1,
    bool $b2,
    bool $b3,
    bool $b4,
  ): void {
    if ($b4) {
      $v = 0;
      $b1 = true;
      $b2 = true;
      $b3 = true;
    } else {
      $v = \Level1\taintSource();
    }
    $v = Branchs::gen8($b1, $b2, $b3, $v);
    \Level1\taintSink($v);
  }
}
