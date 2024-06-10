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

  public function filter1(KeyedContainer<arraykey, string> $arg): void {
    self::my_filter(
      vec[$arg],
      $item ==> {
        return $item['key'] === 'b'; //does not trigger typechecker error
      },
    );
  }

  public function call_filter1_bad(): void {
    $this->filter1(vec['x']);
  }

  public function call_filter1_ok(): void {
    $this->filter1(dict['key' => 'x']);
  }

  private static function my_filter<T>(
    vec<T> $vec,
    (function(T): bool) $f,
  ): bool {
    return $f($vec[0]);
  }

  // we need to model \Vec\filter here
  public function filter2(KeyedContainer<arraykey, string> $arg): void {
    Vec\filter(
      vec[$arg],
      $item ==> {
        return $item['a'] === 'b'; //does not trigger typechecker error
      },
    );
  }

  public function FN_call_filter2_bad(): void {
    $this->filter2(vec['x']);
  }

  public function call_filter2_ok(): void {
    $this->filter2(dict['a' => 'x']);
  }

}
