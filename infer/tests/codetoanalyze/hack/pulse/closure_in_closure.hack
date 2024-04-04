// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace ClosureInClosure;

abstract class Base {

  public abstract function getImpl(int $i): I;
}

interface I {

  public function set(int $j): void;

  public function process(int $k): void;
}

class Utils {

  public static function compute(
    Base $o,
    int $arg1,
    int $arg2,
    int $arg3,
  ): void {
    Utils::run1(
      () ==> {
        $impl = $o->getImpl($arg1);
        $impl->set($arg2);
        Utils::run2(
          () ==> {
            $impl->process($arg3);
          },
        );
      },
    );
  }

  public static function run1((function(): void) $fun): void {
    $fun();
  }

  public static function run2((function(): void) $fun): void {
    $fun();
  }

}

class Unsafe extends Base {

  public function getImpl(int $i): I {
    return new B($i);
  }

}

class Safe extends Base {

  public function getImpl(int $i): I {
    return new B(0);
  }

}

class B implements I {

  public function __construct(private int $i, private int $j = 0) {}

  public function set(int $j): void {
    $this->j = $j;
  }

  public function process(int $k): void {
    \Level1\taintSink($this->i);
    \Level1\taintSink($this->j);
    \Level1\taintSink($k);
  }
}

class Main {
  public static function bad1(int $a, int $b): void {
    Utils::compute(new Unsafe(), \Level1\taintSource(), $a, $b);
  }
  public static function bad2(int $a, int $b): void {
    Utils::compute(new Unsafe(), $a, \Level1\taintSource(), $b);
  }
  public static function bad3(int $a, int $b): void {
    Utils::compute(new Unsafe(), $a, $b, \Level1\taintSource());
  }
  public static function good1(int $a, int $b, int $c): void {
    Utils::compute(new Unsafe(), $a, $b, $c);
  }
  public static function good2(int $a, int $b): void {
    Utils::compute(new Safe(), \Level1\taintSource(), $a, $b);
  }
}
