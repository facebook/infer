// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Closures;

class SensitiveClass {}

class Delayed {
  public function startAndWait((function(): Awaitable<void>) $action): void {
    \HH\Asio\join($action());
  }
}

class Utils {
  public function logDelayed(mixed $data): void {
    (new Delayed())->startAndWait(async () ==> {
      \Level1\taintSink($data);
    });
  }
}

class C1 {
  public function f1Bad(SensitiveClass $sc): void {
    (new Utils())->logDelayed($sc);
  }
}

class A {
  public static function id_fst(int $x, int $y): int {
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

class ClosuresAndDict {

  public static function main_bad(): void {
    $user = new User();
    $value = self::run(
      function($input) {
        return $input['arg']->getSource();
      },
      shape('arg' => $user->getUnsafe()),
    );
    \Level1\taintSink($value);
  }

  public static function main_ok(): void {
    $user = new User();
    $value = self::run(
      function($input) {
        return $input['arg']->getSource();
      },
      shape('arg' => $user->getSafe()),
    );
    \Level1\taintSink($value);
  }

  public static function run<TInput, TOutput>(
    (function(TInput): TOutput) $fun,
    TInput $input,
  ): TOutput {
    return $fun($input);
  }

}

class User {

  public Unsafe $unsafe;
  public Safe $safe;

  public function __construct() {
    $this->unsafe = new Unsafe();
    $this->safe = new Safe();
  }

  public function getSafe(): Safe {
    return $this->safe;
  }

  public function getUnsafe(): Unsafe {
    return $this->unsafe;
  }

}

interface I {
  public function getSource(): int;
}

class Safe implements I {

  public function getSource(): int {
    return 42;
  }
}

class Unsafe implements I {

  public function getSource(): int {
    return \Level1\taintSource();
  }
}
