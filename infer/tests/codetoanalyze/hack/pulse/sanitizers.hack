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
  // We should not report an error here because the tainted value is simply sanitized.
  // The problem seems to be that:
  // 1. the sanitizer itself has 'MustNotBeTainted' attribute in the summary,
  // 2. sanitization happens **after** pre/post of the sanitizer is applied, thus
  // 3. we report a taint flow originating from the sanitizer itself which is not what we want.
  public static function FP_taintedSanitizedOk(Source $arg): void {
    $t = $arg->getTainted();
    San::sanitize($t);
    Sink::process($t);
  }

  public static function taintedNotSanitizedBad(Source $arg): void {
    $t = $arg->getTainted();
    Sink::process($t);
  }
}
