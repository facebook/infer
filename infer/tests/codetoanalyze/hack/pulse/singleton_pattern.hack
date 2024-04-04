// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace SingletonPattern;

final class C {
  public function __construct(int $n) {}

  public function getTainted(): int {
    return \Level1\taintSource();
  }

  public function getUntainted(): int {
    return 42;
  }
}

class Utils {
  public static function getUser(): int {
    return 42;
  }
}

class Singleton {

  private static ?C $instance = null;

  private string $user;

  public function __construct(string $user) {
    $this->user = $user;
  }

  public static function get(): C {
    if (!self::$instance) {
      self::$instance = new C(Utils::getUser());
    }

    return self::$instance;
  }

  public static function resetInstance(): void {
    self::$instance = null;
  }

}

class Main {

  public static function getBad(): void {
    \Level1\taintSink(Singleton::get()->getTainted());
  }

  public static function getGood(): void {
    \Level1\taintSink(Singleton::get()->getUntainted());
  }

}
