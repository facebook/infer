// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Traces;

class Source {
  public static function get(): string {
    return "tainted";
  }
}

class Helper {
  public static function f1(mixed $data): void {
    self::f2($data);
  }

  public static function f2(mixed $data): void {
    self::f3($data);
  }

  public static function f3(mixed $data): void {
    \Level1\taintSink($data);
  }
}

class Flows {
  public function flowBad(): void {
    Helper::f1(Source::get());
  }
}
