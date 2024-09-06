// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace ReturnTypeMatcher;

class Sensitive {

}

class Source1 {
  public static function getTainted(): Sensitive {
    return new Sensitive();
  }
}

class Source2 {
  public static function getTainted(): Sensitive {
    return new Sensitive();
  }
}

class NotSource {
  public static function getTainted(): int {
    return 42;
  }
}

class Sink {
  public static function process(mixed $arg): void {}
}

class Flows {
  public static function source1TaintFlowBad(): void {
    $source = Source1::getTainted();
    Sink::process($source);
  }

  public static function source2TaintFlowBad(): void {
    $source = Source2::getTainted();
    Sink::process($source);
  }

  public static function notSourceFlowOk(): void {
    $source = NotSource::getTainted();
    Sink::process($source);
  }
}
