// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Propagators;

class Source {
  public function getTainted(): int {
    return 42;
  }
}

class Sink {
  public static function process(mixed $arg): void {}
}

class Prop {
  public static function prop(string $arg): string {
    return $arg." propagated";
  }

  public static function propWithSink(string $arg): string {
    Sink::process($arg);
    return $arg;
  }
}

class Flows {
  public static function simpleTaintFlowBad(): void {
    $t = Source::getTainted();
    Sink::process($t);
  }
}
