// Copyright (c) Facebook, Inc. and its affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

namespace Traversable;

class Main {

  private static function use_traversable_bad(
    KeyedContainer<arraykey, string> $item,
  ): void {
    echo $item['b']; //does not trigger typechecker error
  }

  private static function use_traversable_ok(
    KeyedContainer<arraykey, string> $item,
  ): void {
    echo $item[0];
  }

  public function call_use_traversable(): void {
    self::use_traversable_bad(vec['a']);
  }

  public function filter1_bad(): void {
    self::my_filter(
      vec[vec['x']],
      $item ==> {
        return $item['a'] === 'b'; //does not trigger typechecker error
      },
    );
  }

  public function filter1_ok(): void {
    self::my_filter(
      vec[vec['x']],
      $item ==> {
        return $item[0] === 'b';
      },
    );
  }

  private static function my_filter<T>(
    vec<T> $vec,
    (function(T): bool) $f,
  ): bool {
    return $f($vec[0]);
  }

  // we need to model \Vec\filter here
  public function FN_filter2_bad(): void {
    Vec\filter(
      vec[vec['x']],
      $item ==> {
        return $item['a'] === 'b'; //does not trigger typechecker error
      },
    );
  }

  public function filter2_ok(): void {
    Vec\filter(
      vec[vec['x']],
      $item ==> {
        return $item[0] === 'b';
      },
    );
  }

}
