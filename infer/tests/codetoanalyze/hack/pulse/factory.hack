// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Factory;

<<__ConsistentConstruct>>
class A {
  public int $x = 99;

  public function __construct(public int $y) {
  }
}

class B extends A {

  public static function create(classname<A> $class): A {
    return new $class(42);
  }

  public static async function genTestIt(): Awaitable<void> {
    $a = self::create(B::class);
    if ($a->x == 0) {
      // this should never happen 'cos we've just set the property to 99
      $_fail = async {
        return 9;
      };
    }
    return;
  }

}
