// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Sanitizers;

class Source {
  public function getTainted(): int {
    return 42;
  }
}

class Sink {
  public static function process(mixed $arg): void {}
}

class San {
  public static function sanitize(mixed $arg): void {
    Sink::process($arg);
  }
}

class Flows {
  public static function taintedSanitizedOk(Source $arg): void {
    $t = $arg->getTainted();
    San::sanitize($t);
    Sink::process($t);
  }

  public static function taintedNotSanitizedBad(Source $arg): void {
    $t = $arg->getTainted();
    Sink::process($t);
  }
}
