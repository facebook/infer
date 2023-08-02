// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Closures;

class Delayed {
  public function startAndWait((function(): Awaitable<void>) $action) : void {
    \HH\Asio\join($action());
  }
}

class Utils {
  public function logDelayed(mixed $data) : void {
    new Delayed()->startAndWait(async () ==> {
      \Level1\taintSink($data);
    });
  }
}

class C1 {
  public function f1Bad(SensitiveClass $sc) : void {
    new Utils()->logDelayed($sc);
  }
}

class A {
  public static function id_fst(int $x, int $y) : int {
    $y = 0;
    return $x;
  }
}

class Main {

  public function id_fst_bad(int $u, int $v): void {
    $tainted = \Level1\taintSource();
    $x = 0;
    $y = $v;
    $f = (int $x): int ==> A::id_fst($x, $y);
    $i = $f($u);
    if ($i == $u && $y == $v) {
      \Level1\taintSink($tainted);
    }
  }

  public function id_fst_good(int $u, int $v): void {
    $tainted = \Level1\taintSource();
    $x = 0;
    $y = $v;
    $f = (int $x): int ==> A::id_fst($x, $y);
    $i = $f($u);
    if ($i != $u || $y != $v) {
      \Level1\taintSink($tainted);
    }
  }
}
