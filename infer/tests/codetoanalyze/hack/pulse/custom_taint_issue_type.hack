// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace CustomTaintIssueType;

class Source {
  public function get(): int {
    return 42;
  }
}

class Sink {
  public static function process(mixed $arg): void {}
}

class Flows {
  public static function taintedWithCustomIssueTypeBad(Source $arg): void {
    $t = $arg->get();
    Sink::process($t);
  }
}
